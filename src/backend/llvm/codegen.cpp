#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/decl/fn_decl.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/types/alias.h"
#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/generic.h"
#include "backend/llvm/types/type_param.h"

#include "backend/llvm/abi.h"
#include "backend/llvm/codegen.h"

#include "options.h"

namespace cent::backend {

Codegen::Codegen(
    std::unique_ptr<ast::Module> program, std::string_view filename,
    const llvm::DataLayout& layout)
: m_module{std::make_unique<llvm::Module>("", m_context)}, m_builder{m_context},
  m_program{std::move(program)}, m_filename{filename},
  m_units{std::filesystem::absolute(filename).parent_path()} {
    m_module->setDataLayout(layout);

#if LLVM_VERSION_MAJOR >= 21
    m_module->setTargetTriple(llvm::Triple{g_options.target_triple});
#else
    m_module->setTargetTriple(g_options.target_triple);
#endif
}

std::unique_ptr<llvm::Module> Codegen::generate() {
    auto* int8 = llvm::Type::getInt8Ty(m_context);
    auto* int16 = llvm::Type::getInt16Ty(m_context);
    auto* int32 = llvm::Type::getInt32Ty(m_context);
    auto* int64 = llvm::Type::getInt64Ty(m_context);
    auto* void_type = llvm::Type::getVoidTy(m_context);

    m_size = m_module->getDataLayout().getIntPtrType(m_context);

    m_primitive_types["i8"] = std::make_unique<types::I8>(int8);
    m_primitive_types["i16"] = std::make_unique<types::I16>(int16);
    m_primitive_types["i32"] = std::make_unique<types::I32>(int32);
    m_primitive_types["i64"] = std::make_unique<types::I64>(int64);
    m_primitive_types["isize"] = std::make_unique<types::ISize>(m_size);
    m_primitive_types["u8"] = std::make_unique<types::U8>(int8);
    m_primitive_types["u16"] = std::make_unique<types::U16>(int16);
    m_primitive_types["u32"] = std::make_unique<types::U32>(int32);
    m_primitive_types["u64"] = std::make_unique<types::U64>(int64);
    m_primitive_types["usize"] = std::make_unique<types::USize>(m_size);

    m_primitive_types["rune"] = std::make_unique<types::Rune>(int32);

    m_primitive_types["f32"] =
        std::make_unique<types::F32>(llvm::Type::getFloatTy(m_context));

    m_primitive_types["f64"] =
        std::make_unique<types::F64>(llvm::Type::getDoubleTy(m_context));

    m_primitive_types["bool"] =
        std::make_unique<types::Bool>(llvm::Type::getInt1Ty(m_context));

    m_primitive_types["never"] = std::make_unique<types::Never>(void_type);

    m_null_type = std::make_unique<types::Null>(nullptr);
    m_undefined_type = std::make_unique<types::Undefined>(nullptr);

    m_void_type = std::make_unique<types::Void>(void_type);

    m_slice_type = llvm::StructType::create(
        {llvm::PointerType::get(m_context, 0), m_size});

    create_core();

    generate(*m_program);

    create_main();

    return std::move(m_module);
}

void Codegen::create_core() {
    auto* never = m_primitive_types["never"].get();
    auto* i32_type = m_primitive_types["i32"].get();
    auto* u8_slice = get_slice_type(m_primitive_types["u8"].get(), false);
    auto* bool_type = m_primitive_types["bool"].get();

    auto* debug =
        llvm::ConstantInt::get(bool_type->llvm_type, !g_options.release);

    m_core_module.scopes["env"].names["DEBUG"] = {
        .element = {.type = bool_type, .value = debug}, .is_public = true};

    auto panic_fn = create_core_panic();

    auto* assert_type = get_fn_type(m_void_type.get(), {bool_type});

    auto* assert_fn = llvm::Function::Create(
        static_cast<llvm::FunctionType*>(assert_type->llvm_type),
        llvm::Function::PrivateLinkage, "core::assert", *m_module);

    auto* assert_entry = llvm::BasicBlock::Create(m_context, "", assert_fn);

    m_builder.SetInsertPoint(assert_entry);

    auto* assert_fail = llvm::BasicBlock::Create(m_context, "", assert_fn);
    auto* assert_ok = llvm::BasicBlock::Create(m_context, "", assert_fn);

    m_builder.CreateCondBr(assert_fn->getArg(0), assert_ok, assert_fail);

    m_builder.SetInsertPoint(assert_fail);

    std::string_view assert_message = "assertion failed";

    auto* assert_arg = llvm::ConstantStruct::get(
        m_slice_type, {m_builder.CreateGlobalString(
                           assert_message, "", 0, m_module.get(), false),
                       llvm::ConstantInt::get(m_size, assert_message.size())});

    m_builder.CreateCall(
        static_cast<llvm::Function*>(panic_fn.value), assert_arg);

    m_builder.CreateUnreachable();

    m_builder.SetInsertPoint(assert_ok);

    m_builder.CreateRetVoid();

    auto* dbg_assert = g_options.release ? nullptr : assert_fn;

    m_core_module.names = {
        {"panic", {.element = panic_fn, .is_public = true}},
        {"assert",
         {.element = {.type = assert_type, .value = assert_fn},
          .is_public = true}},
        {"dbg_assert",
         {.element = {.type = assert_type, .value = dbg_assert},
          .is_public = true}}};

    create_core_mem();
}

Value Codegen::create_core_panic() {
    auto* never = m_primitive_types["never"].get();
    auto* i32_type = m_primitive_types["i32"].get();
    auto* u8_slice = get_slice_type(m_primitive_types["u8"].get(), false);

    auto* panic_type = get_fn_type(never, {u8_slice});

    auto* panic = llvm::Function::Create(
        static_cast<llvm::FunctionType*>(panic_type->llvm_type),
        llvm::Function::PrivateLinkage, "core::panic", *m_module);

    auto* panic_entry = llvm::BasicBlock::Create(m_context, "", panic);

    m_builder.SetInsertPoint(panic_entry);

    if (m_triple.getOS() != llvm::Triple::Linux) {
        m_builder.CreateUnreachable();
        return {.type = panic_type, .value = panic};
    }

    auto* ptr_type = llvm::PointerType::get(m_context, 0);

    auto* fputs_type = llvm::FunctionType::get(
        i32_type->llvm_type, {ptr_type, ptr_type}, false);

    auto* fputs_fn = llvm::Function::Create(
        fputs_type, llvm::Function::ExternalLinkage, "fputs", *m_module);

    auto* fwrite_type = llvm::FunctionType::get(
        m_size, {ptr_type, m_size, m_size, ptr_type}, false);

    auto* fwrite_fn = llvm::Function::Create(
        fwrite_type, llvm::Function::ExternalLinkage, "fwrite", *m_module);

    auto* fputc_type = llvm::FunctionType::get(
        i32_type->llvm_type, {i32_type->llvm_type, ptr_type}, false);

    auto* fputc_fn = llvm::Function::Create(
        fputc_type, llvm::Function::ExternalLinkage, "fputc", *m_module);

    auto* stderr_ = new llvm::GlobalVariable{
        *m_module, ptr_type, false, llvm::GlobalValue::ExternalLinkage,
        nullptr,   "stderr"};

    auto* exit_type = get_fn_type(never, {i32_type});

    auto* exit_fn = llvm::Function::Create(
        static_cast<llvm::FunctionType*>(exit_type->llvm_type),
        llvm::Function::ExternalLinkage, "exit", *m_module);

    auto* stderr_value = m_builder.CreateLoad(ptr_type, stderr_);

    auto* panic_begin_str =
        m_builder.CreateGlobalString("panic: ", "", 0, m_module.get());

    m_builder.CreateCall(fputs_fn, {panic_begin_str, stderr_value});

    Value panic_message_val{
        .type = u8_slice, .value = alloca_arg(0, u8_slice), .ptr_depth = 1};

    auto* message_len =
        load_struct_member(m_size, panic_message_val, slice_member_len);

    auto* message_ptr =
        load_struct_member(ptr_type, panic_message_val, slice_member_ptr);

    m_builder.CreateCall(
        fwrite_fn, {message_ptr, llvm::ConstantInt::get(m_size, 1), message_len,
                    stderr_value});

    m_builder.CreateCall(
        fputc_fn,
        {llvm::ConstantInt::get(i32_type->llvm_type, '\n'), stderr_value});

    m_builder.CreateCall(
        exit_fn, llvm::ConstantInt::getSigned(i32_type->llvm_type, 1));

    m_builder.CreateUnreachable();

    return {.type = panic_type, .value = panic};
}

void Codegen::create_core_mem() {
    auto* t_param = new types::TypeParam("T");
    auto* usize_type = m_primitive_types["usize"].get();

    m_core_module.scopes["mem"].generic_fns = {
        {"as_mut_slice",
         GenericFunction{
             .name = {.value = "as_mut_slice", .offset = 0},
             .return_type = get_slice_type(t_param, true),
             .params =
                 {
                     {.name = "ptr",
                      .type = get_ptr_type(t_param, true),
                      .is_mutable = false},
                     {.name = "len", .type = usize_type, .is_mutable = false},
                 },
             .type_params = {t_param},
             .kind = GenericFunction::FnKind::AsMutSlice}},
        {"as_slice",
         GenericFunction{
             .name = {.value = "as_slice", .offset = 0},
             .return_type = get_slice_type(t_param, false),
             .params =
                 {
                     {.name = "ptr",
                      .type = get_ptr_type(t_param, false),
                      .is_mutable = false},
                     {.name = "len", .type = usize_type, .is_mutable = false},
                 },
             .type_params = {t_param},
             .kind = GenericFunction::FnKind::AsSlice}}};
}

void Codegen::create_main() {
    if (g_options.emit_type == EmitType::Obj) {
        return;
    }

    if (m_module->getFunction("main")) {
        return;
    }

    const auto& user_main = m_scope.names["main"].element;

    if (!user_main.ok()) {
        return;
    }

    auto* user_main_fn = static_cast<types::Function*>(user_main.type);

    auto* int32 = llvm::Type::getInt32Ty(m_context);
    auto* pointer = llvm::PointerType::get(m_context, 0);

    auto* real_main = llvm::Function::Create(
        llvm::FunctionType::get(int32, {int32, pointer}, false),
        llvm::Function::ExternalLinkage, "main", *m_module);

    auto* entry = llvm::BasicBlock::Create(m_context, "", real_main);

    m_builder.SetInsertPoint(entry);

    auto invalid_signature = [&] {
        log::error("invalid `main` signature");
        m_had_error = true;
    };

    auto create_main_call = [&]() -> llvm::Value* {
        auto* llvm_fn = static_cast<llvm::Function*>(user_main.value);

        if (user_main_fn->param_types.empty()) {
            return m_builder.CreateCall(llvm_fn);
        }

        auto* arg_type = get_slice_type(m_primitive_types["u8"].get(), false);
        auto* args_type = get_slice_type(arg_type, false);

        if (user_main_fn->param_types.size() != 1 ||
            user_main_fn->param_types[0] != args_type) {
            invalid_signature();
            return nullptr;
        }

        auto* argc = real_main->getArg(0);
        auto* argv = real_main->getArg(1);

        auto* result_args = m_builder.CreateAlloca(arg_type->llvm_type, argc);
        auto* result_slice = m_builder.CreateAlloca(args_type->llvm_type);
        auto* index = m_builder.CreateAlloca(int32);

        m_builder.CreateStore(llvm::ConstantInt::getSigned(int32, 0), index);

        auto* cond = llvm::BasicBlock::Create(m_context, "", real_main);
        auto* body = llvm::BasicBlock::Create(m_context, "", real_main);
        auto* end = llvm::BasicBlock::Create(m_context, "", real_main);

        m_builder.CreateBr(cond);
        m_builder.SetInsertPoint(cond);

        auto* index_val = m_builder.CreateLoad(int32, index);

        m_builder.CreateCondBr(
            m_builder.CreateICmpSLT(index_val, argc), body, end);

        m_builder.SetInsertPoint(body);

        auto* result_arg =
            m_builder.CreateGEP(arg_type->llvm_type, result_args, index_val);

        auto* argv_arg = m_builder.CreateLoad(
            pointer, m_builder.CreateGEP(argv->getType(), argv, index_val));

        auto* strlen_fn = m_module->getFunction("strlen");

        if (!strlen_fn) {
            strlen_fn = llvm::Function::Create(
                llvm::FunctionType::get(m_size, pointer, false),
                llvm::Function::ExternalLinkage, "strlen", *m_module);
        }

        auto* result_arg_len = m_builder.CreateCall(strlen_fn, argv_arg);

        m_builder.CreateStore(
            argv_arg, m_builder.CreateStructGEP(
                          arg_type->llvm_type, result_arg, slice_member_ptr));

        m_builder.CreateStore(
            result_arg_len,
            m_builder.CreateStructGEP(
                arg_type->llvm_type, result_arg, slice_member_len));

        m_builder.CreateStore(
            m_builder.CreateAdd(
                m_builder.CreateLoad(int32, index),
                llvm::ConstantInt::getSigned(int32, 1)),
            index);

        m_builder.CreateBr(cond);
        m_builder.SetInsertPoint(end);

        m_builder.CreateStore(
            result_args,
            m_builder.CreateStructGEP(
                args_type->llvm_type, result_slice, slice_member_ptr));

        m_builder.CreateStore(
            m_builder.CreateSExt(argc, m_size),
            m_builder.CreateStructGEP(
                args_type->llvm_type, result_slice, slice_member_len));

        return m_builder.CreateCall(
            llvm_fn,
            {m_builder.CreateLoad(args_type->llvm_type, result_slice)});
    };

    llvm::Value* result = create_main_call();

    if (!result) {
        return;
    }

    auto* base_return_type = unwrap_type(user_main_fn->return_type);

    if (is<types::Void>(base_return_type)) {
        m_builder.CreateRet(llvm::ConstantInt::get(int32, 0));
        return;
    }

    if (is<types::I32>(base_return_type)) {
        m_builder.CreateRet(result);
        return;
    }

    invalid_signature();
}

void Codegen::generate(const ast::Module& module) {
    const auto iterator = m_generated_modules.find(module.path);

    if (iterator != m_generated_modules.end()) {
        *m_current_scope = iterator->second;
        return;
    }

    auto filename = m_filename;
    auto* scope = m_current_scope;
    auto scope_prefix = m_current_scope_prefix;
    auto unit = m_current_unit;

    m_current_scope->scopes["core"] = m_core_module;

    for (const auto& submodule : module.submodules) {
        m_filename = submodule->path.string();
        m_current_scope = &scope->scopes[*submodule->name];
        m_current_scope_prefix = *submodule->name + "::";
        m_current_unit = get_unit(m_filename);

        generate(*submodule);
    }

    m_filename = filename;
    m_current_scope = scope;
    m_current_scope_prefix = scope_prefix;
    m_current_unit = unit;

    for (const auto& type : module.types) {
        if (!matches_target(*type)) {
            continue;
        }

        type->codegen(*this);
    }

    for (const auto& variable : module.variables) {
        if (!matches_target(*variable)) {
            continue;
        }

        variable->codegen(*this);
    }

    for (const auto& function : module.functions) {
        if (!matches_target(*function)) {
            continue;
        }

        generate_fn_proto(*function);
    }

    for (const auto& for_block : module.for_blocks) {
        if (!matches_target(*for_block)) {
            continue;
        }

        generate_for_block_protos(*for_block);
    }

    for (const auto& function : module.functions) {
        if (!matches_target(*function)) {
            continue;
        }

        if (function->block && function->type_params.empty()) {
            function->codegen(*this);
        }
    }

    for (const auto& for_block : module.for_blocks) {
        if (!matches_target(*for_block)) {
            continue;
        }

        for_block->codegen(*this);
    }

    m_generated_modules[module.path] = *m_current_scope;
}

bool Codegen::deduce_type_arg(
    Type* param, Type* arg, const std::vector<types::TypeParam*>& type_params,
    std::vector<Type*>& deduced_args) {
    param = unwrap_type(param);
    arg = unwrap_type(arg);

    if (auto* t_param = dyn_cast<types::TypeParam>(param)) {
        for (std::size_t i = 0; i < type_params.size(); ++i) {
            if (type_params[i] == t_param) {
                if (deduced_args[i] && deduced_args[i] != arg) {
                    return false;
                }

                deduced_args[i] = arg;
                return true;
            }
        }

        return false;
    }

    if (auto* param_struct = dyn_cast<types::GenericStructInst>(param)) {
        auto* arg_struct = dyn_cast<types::Struct>(arg);

        if (!arg_struct || param_struct->type != arg_struct->origin) {
            return false;
        }

        for (std::size_t i = 0; i < param_struct->type->fields.size(); ++i) {
            auto& param_field = param_struct->type->fields[i];

            auto* resolved_field_type = inst_type_param(
                param_struct->type->type_params, param_struct->args,
                param_field.type);

            for (auto& [name, index] : m_members[static_cast<llvm::StructType*>(
                     arg_struct->llvm_type)]) {
                if (name == param_field.name) {
                    if (!deduce_type_arg(
                            resolved_field_type, arg_struct->fields[index],
                            type_params, deduced_args)) {
                        return false;
                    }

                    break;
                }
            }
        }

        return true;
    }

    if (auto* param_union = dyn_cast<types::GenericUnionInst>(param)) {
        auto* arg_union = dyn_cast<types::Union>(arg);

        if (!arg_union || param_union->type != arg_union->origin) {
            return false;
        }

        for (std::size_t i = 0; i < param_union->type->fields.size(); ++i) {
            auto& param_field = param_union->type->fields[i];

            auto* resolved_field_type = inst_type_param(
                param_union->type->type_params, param_union->args,
                param_field.type);

            auto* llvm_struct_type = arg_union->llvm_type;

            for (auto& [name, index] :
                 m_members[static_cast<llvm::StructType*>(llvm_struct_type)]) {
                if (name == param_field.name) {
                    if (!deduce_type_arg(
                            resolved_field_type, arg_union->fields[index],
                            type_params, deduced_args)) {
                        return false;
                    }

                    break;
                }
            }
        }

        return true;
    }

    if (auto* param_ptr = dyn_cast<types::Pointer>(param)) {
        if (auto* arg_ptr = dyn_cast<types::Pointer>(arg)) {
            return deduce_type_arg(
                param_ptr->type, arg_ptr->type, type_params, deduced_args);
        }

        return false;
    }

    if (auto* param_opt = dyn_cast<types::Optional>(param)) {
        if (auto* arg_opt = dyn_cast<types::Optional>(arg)) {
            return deduce_type_arg(
                param_opt->type, arg_opt->type, type_params, deduced_args);
        }

        return deduce_type_arg(param_opt->type, arg, type_params, deduced_args);
    }

    if (auto* param_slice = dyn_cast<types::Slice>(param)) {
        if (auto* arg_slice = dyn_cast<types::Slice>(arg)) {
            return deduce_type_arg(
                param_slice->type, arg_slice->type, type_params, deduced_args);
        }

        if (auto* arg_array = dyn_cast<types::Array>(arg)) {
            return deduce_type_arg(
                param_slice->type, arg_array->type, type_params, deduced_args);
        }

        if (auto* arg_array = dyn_cast<types::VarLenArray>(arg)) {
            return deduce_type_arg(
                param_slice->type, arg_array->type, type_params, deduced_args);
        }

        return false;
    }

    if (auto* param_array = dyn_cast<types::Array>(param)) {
        if (auto* arg_array = dyn_cast<types::Array>(arg)) {
            if (param_array->size != arg_array->size) {
                return false;
            }

            return deduce_type_arg(
                param_array->type, arg_array->type, type_params, deduced_args);
        }

        return false;
    }

    if (auto* param_range = dyn_cast<types::Range>(param)) {
        if (auto* arg_range = dyn_cast<types::Range>(arg)) {
            return deduce_type_arg(
                param_range->type, arg_range->type, type_params, deduced_args);
        }

        return false;
    }

    if (auto* param_tuple = dyn_cast<types::Tuple>(param)) {
        auto* arg_tuple = dyn_cast<types::Tuple>(arg);

        if (!arg_tuple ||
            param_tuple->types.size() != arg_tuple->types.size()) {
            return false;
        }

        for (std::size_t i = 0; i < param_tuple->types.size(); ++i) {
            if (!deduce_type_arg(
                    param_tuple->types[i], arg_tuple->types[i], type_params,
                    deduced_args)) {
                return false;
            }
        }

        return true;
    }

    if (auto* param_fn = dyn_cast<types::Function>(param)) {
        auto* arg_fn = dyn_cast<types::Function>(arg);

        if (!arg_fn ||
            param_fn->param_types.size() != arg_fn->param_types.size()) {
            return false;
        }

        if (!deduce_type_arg(
                param_fn->return_type, arg_fn->return_type, type_params,
                deduced_args)) {
            return false;
        }

        for (std::size_t i = 0; i < param_fn->param_types.size(); ++i) {
            if (!deduce_type_arg(
                    param_fn->param_types[i], arg_fn->param_types[i],
                    type_params, deduced_args)) {
                return false;
            }
        }

        return true;
    }

    return true;
}

Type* Codegen::inst_type_param(
    const std::vector<types::TypeParam*>& params,
    const std::vector<Type*>& args, Type* type) {
    auto* base_type = unwrap_type(type);

    if (auto* t_param = dyn_cast<types::TypeParam>(base_type)) {
        for (std::size_t i = 0; i < params.size(); ++i) {
            if (params[i] == t_param) {
                return args[i];
            }
        }

        return nullptr;
    }

    if (auto* t_struct = dyn_cast<types::GenericStructInst>(base_type)) {
        std::vector<Type*> new_args;
        new_args.reserve(t_struct->args.size());

        bool all_resolved = true;

        for (auto* arg : t_struct->args) {
            auto* resolved = inst_type_param(params, args, arg);

            if (!resolved) {
                return nullptr;
            }

            if (!resolved->llvm_type) {
                all_resolved = false;
            }

            new_args.push_back(resolved);
        }

        if (all_resolved) {
            return inst_generic_struct(t_struct->type, new_args);
        }

        m_named_types.push_back(
            std::make_unique<types::GenericStructInst>(
                t_struct->type, new_args));

        return m_named_types.back().get();
    }

    if (auto* t_union = dyn_cast<types::GenericUnionInst>(base_type)) {
        std::vector<Type*> new_args;
        new_args.reserve(t_union->args.size());

        bool all_resolved = true;

        for (auto* arg : t_union->args) {
            auto* resolved = inst_type_param(params, args, arg);

            if (!resolved) {
                return nullptr;
            }

            if (!resolved->llvm_type) {
                all_resolved = false;
            }

            new_args.push_back(resolved);
        }

        if (all_resolved) {
            return inst_generic_union(t_union->type, new_args);
        }

        m_named_types.push_back(
            std::make_unique<types::GenericUnionInst>(t_union->type, new_args));

        return m_named_types.back().get();
    }

    if (auto* ptr = dyn_cast<types::Pointer>(base_type)) {
        return get_ptr_type(
            inst_type_param(params, args, ptr->type), ptr->is_mutable);
    }

    if (auto* optional = dyn_cast<types::Optional>(base_type)) {
        return get_optional_type(inst_type_param(params, args, optional->type));
    }

    if (auto* range = dyn_cast<types::Range>(base_type)) {
        return get_range_type(
            inst_type_param(params, args, range->type), range->inclusive);
    }

    if (auto* array = dyn_cast<types::Array>(base_type)) {
        return get_array_type(
            inst_type_param(params, args, array->type), array->size);
    }

    if (auto* slice = dyn_cast<types::Slice>(base_type)) {
        return get_slice_type(
            inst_type_param(params, args, slice->type), slice->is_mutable);
    }

    if (auto* tuple = dyn_cast<types::Tuple>(base_type)) {
        std::vector<Type*> types;
        types.reserve(tuple->types.size());

        for (auto& element : tuple->types) {
            types.push_back(inst_type_param(params, args, element));
        }

        return get_tuple_type(types);
    }

    if (auto* func = dyn_cast<types::Function>(base_type)) {
        Type* return_type = inst_type_param(params, args, func->return_type);

        std::vector<Type*> param_types;
        param_types.reserve(func->param_types.size());

        for (auto& param_type : func->param_types) {
            param_types.push_back(inst_type_param(params, args, param_type));
        }

        return get_fn_type(
            return_type, param_types, func->default_args, func->variadic);
    }

    return type;
}

types::Struct* Codegen::inst_generic_struct(
    GenericStruct* type, const std::vector<Type*>& types) {
    auto& result = m_generic_struct_inst[type][types];

    if (result) {
        return result.get();
    }

    std::string name = m_current_scope_prefix + type->name + "(<";

    for (std::size_t i = 0; i < types.size(); ++i) {
        if (i > 0) {
            name += ",";
        }

        name += types[i]->to_string();
    }

    name += ">)";

    auto* llvm_struct_type = llvm::StructType::create(m_context, name);

    result = std::make_unique<types::Struct>(
        llvm_struct_type, name, std::vector<Type*>{}, false, type, types);

    struct Field {
        Type* type;
        std::string_view name;
        std::size_t size;
    };

    std::vector<Field> fields;
    fields.reserve(type->fields.size());

    for (const auto& field : type->fields) {
        auto* field_type =
            inst_type_param(type->type_params, types, field.type);

        auto size =
            m_module->getDataLayout().getTypeAllocSize(field_type->llvm_type);

        fields.emplace_back(field_type, field.name, size);
    }

    std::ranges::sort(fields, [](const auto& left, const auto& right) {
        return left.size > right.size;
    });

    std::vector<llvm::Type*> llvm_fields;
    std::vector<Type*> type_fields;
    llvm_fields.reserve(fields.size());
    type_fields.reserve(fields.size());

    std::size_t max_element = 0;
    std::size_t total_size = 0;

    for (std::size_t i = 0; i < fields.size(); ++i) {
        const auto& field = fields[i];

        llvm_fields.push_back(field.type->llvm_type);
        type_fields.push_back(field.type);

        max_element = std::max<std::size_t>(
            std::min<std::size_t>(
                field.size, m_module->getDataLayout().getTypeAllocSize(m_size)),
            max_element);

        total_size += field.size;
        m_members[llvm_struct_type][field.name] = i;
    }

    if (max_element > 0) {
        auto rem = total_size % max_element;

        if (rem != 0) {
            llvm_fields.push_back(
                llvm::ArrayType::get(
                    llvm::Type::getInt8Ty(m_context), max_element - rem));
        }
    }

    llvm_struct_type->setBody(llvm_fields, false);

    result->fields = type_fields;
    result->has_tail = llvm_fields.size() != type_fields.size();

    return result.get();
}

types::Union* Codegen::inst_generic_union(
    GenericUnion* type, const std::vector<Type*>& types) {
    auto& result = m_generic_union_inst[type][types];

    if (result) {
        return result.get();
    }

    std::string name = m_current_scope_prefix + type->name + "(<";

    for (std::size_t i = 0; i < types.size(); ++i) {
        if (i > 0) {
            name += ",";
        }

        name += types[i]->to_string();
    }

    name += ">)";

    auto* llvm_struct_type = llvm::StructType::create(m_context, name);

    std::vector<Type*> fields;
    fields.reserve(type->fields.size());

    for (std::size_t i = 0; i < type->fields.size(); ++i) {
        const auto& field = type->fields[i];

        auto* field_type =
            inst_type_param(type->type_params, types, field.type);

        fields.push_back(field_type);
        m_members[llvm_struct_type][field.name] = i;
    }

    std::size_t max = 0;
    llvm::Type* max_type = nullptr;

    auto layout = m_module->getDataLayout();

    for (const auto& field : fields) {
        auto size = layout.getTypeAllocSize(field->llvm_type);

        if (size > max) {
            max = size;
            max_type = field->llvm_type;
        }
    }

    if (!type->tag_type) {
        llvm_struct_type->setBody(max_type);

        result = std::make_unique<types::Union>(
            llvm_struct_type, name, std::move(fields), nullptr, type, types);

        return result.get();
    }

    llvm_struct_type->setBody({max_type, type->tag_type->llvm_type});

    result = std::make_unique<types::Union>(
        llvm_struct_type, name, std::move(fields), type->tag_type, type, types);

    return result.get();
}

Value Codegen::inst_generic_fn(
    GenericFunction* function, const std::vector<Type*>& types,
    const std::vector<Type*>& parent_types) {
    std::vector<Type*> type_args;

    type_args.reserve(parent_types.size() + types.size());

    type_args.insert(type_args.end(), parent_types.begin(), parent_types.end());
    type_args.insert(type_args.end(), types.begin(), types.end());

    auto& generic_fn_inst = m_generic_fns_inst[function];

    auto result = generic_fn_inst.find(type_args);

    if (result != generic_fn_inst.end()) {
        return result->second;
    }

    std::vector<types::TypeParam*> type_params;

    type_params.reserve(
        function->parent_type_params.size() + function->type_params.size());

    type_params.insert(
        type_params.end(), function->parent_type_params.begin(),
        function->parent_type_params.end());

    type_params.insert(
        type_params.end(), function->type_params.begin(),
        function->type_params.end());

    std::vector<Type*> param_types;
    param_types.reserve(function->params.size());

    for (const auto& param : function->params) {
        param_types.push_back(
            inst_type_param(type_params, type_args, param.type));
    }

    std::vector<llvm::Constant*> default_args;
    default_args.reserve(function->default_args.size());

    for (std::size_t i = 0; i < function->default_args.size(); ++i) {
        const auto* arg = function->default_args[i];
        auto value = arg->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        auto* type =
            param_types[i + param_types.size() - function->default_args.size()];

        auto val = cast_or_error(arg->offset, type, value);

        if (!val.ok()) {
            return Value::poisoned();
        }

        if (!llvm::isa<llvm::Constant>(val.value)) {
            error(arg->offset, "not a constant");
            return Value::poisoned();
        }

        default_args.push_back(static_cast<llvm::Constant*>(val.value));
    }

    std::string name = m_current_scope_prefix + function->name.value + "(<";

    for (std::size_t i = 0; i < type_args.size(); ++i) {
        if (i > 0) {
            name += ",";
        }

        name += type_args[i]->to_string();
    }

    name += ">)";

    auto* fn_type = get_fn_type(
        inst_type_param(type_params, type_args, function->return_type),
        std::move(param_types), std::move(default_args), false);

    auto* llvm_fn_type = static_cast<llvm::FunctionType*>(fn_type->llvm_type);

    auto* llvm_function = llvm::Function::Create(
        llvm_fn_type, llvm::Function::PrivateLinkage, name, *m_module);

    if (is<types::Never>(fn_type->return_type)) {
        llvm_function->addFnAttr(llvm::Attribute::NoReturn);
    }

    if (fn_type->sret) {
        llvm_function->addParamAttr(
            0, llvm::Attribute::getWithStructRetType(
                   m_context, fn_type->return_type->llvm_type));
    }

    auto* entry = llvm::BasicBlock::Create(m_context, "", llvm_function);

    auto* insert_point = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(entry);

    auto* current_function = m_current_function;
    m_current_function = fn_type;

    auto current_scope_names = m_current_scope->names;
    auto current_scope_types = m_current_scope->types;

    for (std::size_t i = 0; i < function->params.size(); ++i) {
        const auto& param = function->params[i];

        auto* variable =
            alloca_arg(fn_type->sret ? i + 1 : i, fn_type->param_types[i]);

        m_current_scope->names[param.name] = {
            .element =
                {.type = fn_type->param_types[i],
                 .value = variable,
                 .ptr_depth = 1,
                 .is_mutable = param.is_mutable},
            .unit = m_current_unit};
    }

    for (std::size_t i = 0; i < function->parent_type_params.size(); ++i) {
        m_named_types.push_back(
            std::make_unique<types::Alias>(
                type_args[i]->llvm_type, function->parent_type_params[i]->name,
                type_args[i], false));

        m_current_scope->types[function->parent_type_params[i]->name] = {
            .element = m_named_types.back().get(), .is_public = true};
    }

    for (std::size_t i = 0; i < function->type_params.size(); ++i) {
        auto index = function->parent_type_params.size() + i;

        m_named_types.push_back(
            std::make_unique<types::Alias>(
                type_args[index]->llvm_type, function->type_params[i]->name,
                type_args[index], false));

        m_current_scope->types[function->type_params[i]->name] = {
            .element = m_named_types.back().get(), .is_public = true};
    }

    if (function->self_type) {
        m_current_scope->types["Self"] = {
            .element =
                inst_type_param(type_params, type_args, function->self_type),
            .is_public = true};
    }

    auto filename = m_filename;

    if (!function->source_file.empty()) {
        m_filename = function->source_file;
    }

    auto scope_prefix = m_current_scope_prefix;
    m_current_scope_prefix = name + "::";

    function->block->codegen(*this);

    m_filename = filename;

    m_current_scope_prefix = scope_prefix;
    m_current_scope->names = current_scope_names;
    m_current_scope->types = current_scope_types;
    m_current_function = current_function;

    if (m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.SetInsertPoint(insert_point);
        generic_fn_inst[type_args] =
            Value{.type = fn_type, .value = llvm_function};

        return generic_fn_inst[type_args];
    }

    if (!m_current_fn_had_error &&
        !is<types::Void, types::Never>(fn_type->return_type)) {
        error(
            function->name.offset, "non-void function does not return a value");

        m_builder.SetInsertPoint(insert_point);

        return Value::poisoned();
    }

    if (is<types::Never>(fn_type->return_type)) {
        m_builder.CreateUnreachable();
    } else {
        m_builder.CreateRetVoid();
    }

    m_builder.SetInsertPoint(insert_point);

    generic_fn_inst[types] = Value{.type = fn_type, .value = llvm_function};
    return generic_fn_inst[types];
}

Value Codegen::primitive_cast(Type* type, const Value& value, bool implicit) {
    using enum llvm::Instruction::CastOps;

    auto* base_type = unwrap_type(type, !implicit);
    auto* base_value_type = unwrap_type(value.type, !implicit);

    bool value_is_rune = is<types::Rune>(base_value_type);
    bool type_is_rune = is<types::Rune>(base_type);

    if ((value_is_rune || type_is_rune) && implicit) {
        return Value::poisoned();
    }

    bool value_is_float = base_value_type->is_float();
    bool value_is_sint = base_value_type->is_sint();
    bool value_is_uint = base_value_type->is_uint() || value_is_rune;
    bool value_is_ptr = is<types::Pointer>(base_value_type);

    if (!implicit) {
        if (auto* type = dyn_cast<types::Enum>(base_value_type)) {
            auto* base_underlying = unwrap_type(type->type);

            value_is_sint = base_underlying->is_sint();
            value_is_uint = base_underlying->is_uint();
        }
    }

    if (!value_is_float && !value_is_sint && !value_is_uint && !value_is_ptr) {
        return Value::poisoned();
    }

    bool type_is_float = base_type->is_float();
    bool type_is_sint = base_type->is_sint();
    bool type_is_uint = base_type->is_uint() || type_is_rune;
    bool type_is_ptr = is<types::Pointer>(base_type);

    if (!implicit) {
        if (auto* type = dyn_cast<types::Enum>(base_type)) {
            type_is_sint = type->type->is_sint();
            type_is_uint = type->type->is_uint();
        }
    }

    if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value.value)) {
        if (type_is_sint || type_is_uint) {
            auto size_bits = base_type->llvm_type->getIntegerBitWidth();

            const auto& val = constant->getValue();

            bool non_lossy = [&] {
                if (value_is_uint && type_is_uint && val.isIntN(size_bits)) {
                    return true;
                }

                if (value_is_sint && type_is_sint &&
                    val.isSignedIntN(size_bits)) {
                    return true;
                }

                if (value_is_sint && type_is_uint && val.isNonNegative() &&
                    val.isIntN(size_bits)) {
                    return true;
                }

                return value_is_uint && type_is_sint &&
                       val.isSignedIntN(size_bits);
            }();

            if (non_lossy) {
                return {
                    .type = type,
                    .value = llvm::ConstantInt::get(
                        base_type->llvm_type, type_is_sint
                                                  ? val.getSExtValue()
                                                  : val.getZExtValue())};
            }
        }
    }

    auto layout = m_module->getDataLayout();

    auto from_size = layout.getTypeAllocSize(base_value_type->llvm_type);
    auto to_size = layout.getTypeAllocSize(base_type->llvm_type);

    llvm::Instruction::CastOps cast_op = CastOpsEnd;

    if (value_is_float) {
        if (type_is_float) {
            if (to_size > from_size) {
                cast_op = FPExt;
            } else if (!implicit) {
                cast_op = FPTrunc;
            }
        } else if (!implicit && type_is_sint) {
            cast_op = FPToSI;
        } else if (!implicit && type_is_uint) {
            cast_op = FPToUI;
        }
    } else if (value_is_sint) {
        if (type_is_float) {
            cast_op = SIToFP;
        } else if (type_is_sint) {
            if (to_size > from_size) {
                cast_op = SExt;
            } else if (!implicit) {
                cast_op = Trunc;
            }
        } else if (!implicit && type_is_uint) {
            if (to_size > from_size) {
                cast_op = SExt;
            } else if (to_size < from_size) {
                cast_op = Trunc;
            } else {
                return {.type = type, .value = load_rvalue(value).value};
            }
        } else if (!implicit && type_is_ptr) {
            cast_op = IntToPtr;
        }
    } else if (value_is_uint) {
        if (type_is_float) {
            cast_op = UIToFP;
        } else if (type_is_uint) {
            if (to_size > from_size) {
                cast_op = ZExt;
            } else if (!implicit) {
                cast_op = Trunc;
            }
        } else if (type_is_sint) {
            if (to_size > from_size) {
                cast_op = ZExt;
            } else if (!implicit) {
                if (to_size < from_size) {
                    cast_op = Trunc;
                } else {
                    return {.type = type, .value = load_rvalue(value).value};
                }
            }
        } else if (!implicit && type_is_ptr) {
            cast_op = IntToPtr;
        }
    } else if (value_is_ptr) {
        if (!implicit && (type_is_sint || type_is_uint)) {
            cast_op = PtrToInt;
        } else if (type_is_ptr) {
            if (!implicit) {
                return {
                    .type = type,
                    .value = value.value,
                    .ptr_depth = value.ptr_depth};
            }

            auto* value_ptr = static_cast<types::Pointer*>(base_value_type);
            auto* type_ptr = static_cast<types::Pointer*>(base_type);

            if (value_ptr->type == type_ptr->type &&
                (value_ptr->is_mutable || !type_ptr->is_mutable)) {
                return {
                    .type = type,
                    .value = value.value,
                    .ptr_depth = value.ptr_depth};
            }
        }
    }

    if (cast_op == CastOpsEnd) {
        return Value::poisoned();
    }

    return {
        .type = type,
        .value = m_builder.CreateCast(
            cast_op, load_rvalue(value).value, base_type->llvm_type)};
}

Value Codegen::cast(Type* type, const Value& value, bool implicit) {
    auto* base_type = unwrap_type(type, !implicit);
    auto* base_value_type = unwrap_type(value.type, !implicit);

    if (is<types::Never>(base_value_type)) {
        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateUnreachable();
        }

        if (is<types::Void, types::Never>(base_type)) {
            return {.type = type, .value = nullptr};
        }

        return {
            .type = type,
            .value = llvm::Constant::getNullValue(type->llvm_type)};
    }

    if (base_type == base_value_type) {
        return {
            .type = type,
            .value = value.value,
            .ptr_depth = value.ptr_depth,
            .memcpy = value.memcpy};
    }

    if (is<types::Void, types::Null, types::Never>(base_type)) {
        return Value::poisoned();
    }

    if (const auto* optional = dyn_cast<types::Optional>(base_type)) {
        auto* base_contained_type = unwrap_type(optional->type);

        if (is<types::Null>(base_value_type)) {
            return {
                .type = type,
                .value = llvm::Constant::getNullValue(base_type->llvm_type)};
        }

        if (base_contained_type != base_value_type) {
            if (implicit) {
                return Value::poisoned();
            }

            auto* value_optional = dyn_cast<types::Optional>(base_value_type);

            if (!value_optional) {
                return Value::poisoned();
            }

            if (!is<types::Pointer>(base_contained_type) ||
                !is<types::Pointer>(unwrap_type(value_optional->type))) {
                return Value::poisoned();
            }

            return {
                .type = type,
                .value = value.value,
                .ptr_depth = value.ptr_depth,
                .memcpy = value.memcpy};
        }

        if (is<types::Pointer>(base_contained_type)) {
            return {
                .type = type,
                .value =
                    is<types::Null>(base_value_type)
                        ? llvm::Constant::getNullValue(base_type->llvm_type)
                        : value.value,
                .ptr_depth = value.ptr_depth,
                .memcpy = value.memcpy};
        }

        if (auto* val = llvm::dyn_cast<llvm::Constant>(value.value)) {
            return {
                .type = type,
                .value = llvm::ConstantStruct::get(
                    static_cast<llvm::StructType*>(base_type->llvm_type),
                    {val, llvm::ConstantInt::get(
                              llvm::Type::getInt1Ty(m_context), true)})};
        }

        auto* variable = create_alloca(base_type);

        if (!variable) {
            return Value::poisoned();
        }

        auto* bool_ptr = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, optional_member_bool);

        auto* value_ptr = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, optional_member_value);

        m_builder.CreateStore(load_rvalue(value).value, value_ptr);

        m_builder.CreateStore(
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), true),
            bool_ptr);

        return {
            .type = type, .value = variable, .ptr_depth = 1, .memcpy = true};
    }

    if (const auto* slice = dyn_cast<types::Slice>(base_type)) {
        auto* base_slice_contained_type = unwrap_type(slice->type);

        if (auto* slice_value = dyn_cast<types::Slice>(base_value_type)) {
            if (base_slice_contained_type == unwrap_type(slice_value->type) &&
                (!slice->is_mutable || slice_value->is_mutable)) {
                return {
                    .type = type,
                    .value = value.value,
                    .ptr_depth = value.ptr_depth,
                    .memcpy = value.memcpy};
            }

            return Value::poisoned();
        }

        if (auto* array = dyn_cast<types::VarLenArray>(base_value_type)) {
            if (base_slice_contained_type != unwrap_type(array->type)) {
                return Value::poisoned();
            }

            auto* variable = create_alloca(base_type);

            if (!variable) {
                return Value::poisoned();
            }

            auto* ptr_member = m_builder.CreateStructGEP(
                base_type->llvm_type, variable, slice_member_ptr);

            auto* len_member = m_builder.CreateStructGEP(
                base_type->llvm_type, variable, slice_member_len);

            m_builder.CreateStore(load_lvalue(value).value, ptr_member);
            m_builder.CreateStore(array->size, len_member);

            return {
                .type = type,
                .value = variable,
                .ptr_depth = 1,
                .memcpy = true};
        }

        const auto* array_type = dyn_cast<types::Array>(base_value_type);

        if (!array_type) {
            return Value::poisoned();
        }

        if (base_slice_contained_type != unwrap_type(array_type->type)) {
            return Value::poisoned();
        }

        auto* variable = create_alloca(base_type);

        if (!variable) {
            return Value::poisoned();
        }

        auto* ptr_member = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, slice_member_ptr);

        auto* len_member = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, slice_member_len);

        auto* ptr_value = m_builder.CreateGEP(
            base_slice_contained_type->llvm_type, load_lvalue(value).value,
            llvm::ConstantInt::get(m_size, 0));

        m_builder.CreateStore(ptr_value, ptr_member);

        m_builder.CreateStore(
            llvm::ConstantInt::get(m_size, array_type->size), len_member);

        return {
            .type = type, .value = variable, .ptr_depth = 1, .memcpy = true};
    }

    return primitive_cast(type, value, implicit);
}

Value Codegen::cast_or_error(
    std::size_t offset, Type* type, const Value& value, bool implicit) {
    auto result = cast(type, value, implicit);

    if (!result.ok()) {
        type_mismatch(offset, type, value.type);
    }

    return result;
}

Value Codegen::create_call(
    std::size_t offset, types::Function* type, llvm::Value* function,
    const std::vector<OffsetValue<Value>>& arguments) {
    auto params_size = type->param_types.size();
    auto args_size = arguments.size();
    auto default_args_size = type->default_args.size();

    if (!type->variadic && (args_size < params_size - default_args_size ||
                            args_size > params_size)) {
        error(offset, "incorrect number of arguments passed");
        return Value::poisoned();
    }

    std::vector<llvm::Value*> llvm_args;
    llvm::Value* sret_result = nullptr;

    if (type->sret) {
        sret_result = create_alloca(type->return_type);

        llvm_args.reserve(params_size + 1);
        llvm_args.push_back(sret_result);
    } else {
        llvm_args.reserve(params_size);
    }

    auto layout = m_module->getDataLayout();

    for (std::size_t i = 0; i < args_size; ++i) {
        auto value = arguments[i].value;

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (type->variadic && i >= params_size) {
            static constexpr auto int_bitwidth = 32;
            auto* base_type = unwrap_type(value.type, true);

            if (auto* optional = dyn_cast<types::Optional>(base_type)) {
                if (is<types::Pointer>(optional->type)) {
                    llvm_args.push_back(load_rvalue(value).value);
                    continue;
                }
            }

            if (is<types::Pointer, types::F64>(base_type)) {
                llvm_args.push_back(load_rvalue(value).value);
                continue;
            }

            if (is<types::Bool>(base_type)) {
                llvm_args.push_back(
                    primitive_cast(m_primitive_types["i32"].get(), value, false)
                        .value);

                continue;
            }

            if (is<types::F32>(base_type)) {
                llvm_args.push_back(
                    primitive_cast(m_primitive_types["f64"].get(), value)
                        .value);

                continue;
            }

            if (!base_type->is_sint() && !base_type->is_uint()) {
                warning(
                    arguments[i].offset,
                    "passing argument of type {} to a variadic function is "
                    "undefined behavior",
                    log::quoted(value.type->to_string()));

                llvm_args.push_back(load_rvalue(value).value);
                continue;
            }

            auto bitwidth = base_type->llvm_type->getIntegerBitWidth();

            if (bitwidth >= int_bitwidth) {
                llvm_args.push_back(load_rvalue(value).value);
                continue;
            }

            auto val = primitive_cast(m_primitive_types["i32"].get(), value);

            if (!val.ok()) {
                val = primitive_cast(m_primitive_types["u32"].get(), value);
            }

            llvm_args.push_back(val.value);
            continue;
        }

        auto val =
            cast_or_error(arguments[i].offset, type->param_types[i], value);

        if (!val.ok()) {
            return Value::poisoned();
        }

        val = load_lvalue(val);

        if (get_abi(m_triple) != Abi::SysV) {
            llvm_args.push_back(load_rvalue(val).value);
            continue;
        }

        auto classification = sysv::classify(val.type->llvm_type, layout);

        if (classification.first == sysv::RegClass::Memory) {
            if (val.ptr_depth == 1) {
                llvm_args.push_back(val.value);
                continue;
            }

            auto* variable = create_alloca(val.type->llvm_type);
            m_builder.CreateStore(val.value, variable);
            llvm_args.push_back(variable);

            continue;
        }

        val = load_rvalue(val);

        auto* lowered =
            sysv::lower(val.type, classification, layout, m_context);

        if (val.type->llvm_type == lowered) {
            llvm_args.push_back(val.value);
            continue;
        }

        auto* variable = create_alloca(lowered);
        m_builder.CreateStore(val.value, variable);

        llvm_args.push_back(m_builder.CreateLoad(lowered, variable));
    }

    for (std::size_t i = default_args_size - (params_size - args_size);
         i < default_args_size; ++i) {
        auto* arg = type->default_args[i];

        auto* arg_type =
            type->param_types[(params_size - default_args_size) + i];

        auto classification = sysv::classify(arg_type->llvm_type, layout);

        if (classification.first == sysv::RegClass::Memory) {
            auto* variable = create_alloca(arg_type->llvm_type);
            m_builder.CreateStore(arg, variable);
            llvm_args.push_back(variable);

            continue;
        }

        auto* lowered =
            sysv::lower(arg_type, classification, layout, m_context);

        if (arg->getType() == lowered) {
            llvm_args.push_back(arg);
            continue;
        }

        auto* variable = create_alloca(lowered);
        m_builder.CreateStore(arg, variable);

        llvm_args.push_back(m_builder.CreateLoad(lowered, variable));
    }

    auto* call = m_builder.CreateCall(
        static_cast<llvm::FunctionType*>(type->llvm_type), function, llvm_args);

    if (!g_options.release) {
        if (is<types::Never>(type->return_type)) {
            create_panic("`never` function returned");
        }
    }

    if (sret_result) {
        return {
            .type = type->return_type,
            .value = sret_result,
            .ptr_depth = 1,
            .memcpy = type->sret};
    }

    if (is<types::Void, types::Never>(type->return_type)) {
        return {.type = type->return_type};
    }

    if (call->getType() == type->return_type->llvm_type) {
        return {.type = type->return_type, .value = call};
    }

    auto* variable = create_alloca(type->return_type->llvm_type);
    m_builder.CreateStore(call, variable);

    return {.type = type->return_type, .value = variable, .ptr_depth = 1};
}

Value Codegen::create_intrinsic_call(
    std::size_t offset, GenericFunction* function,
    const std::vector<Type*>& types,
    const std::vector<OffsetValue<Value>>& arguments) {
    std::vector<Type*> param_types;
    param_types.reserve(function->params.size());

    for (const auto& param : function->params) {
        param_types.push_back(
            inst_type_param(function->type_params, types, param.type));
    }

    std::vector<Value> args;
    args.reserve(arguments.size());

    for (std::size_t i = 0; i < arguments.size(); ++i) {
        auto val = cast_or_error(
            arguments[i].offset, param_types[i], arguments[i].value);

        if (!val.ok()) {
            return Value::poisoned();
        }

        args.push_back(val);
    }

    auto* slice_type = get_slice_type(
        types[0], function->kind == GenericFunction::FnKind::AsMutSlice);

    auto* variable = create_alloca(slice_type);

    auto* ptr_field =
        m_builder.CreateStructGEP(m_slice_type, variable, slice_member_ptr);

    auto* len_field =
        m_builder.CreateStructGEP(m_slice_type, variable, slice_member_len);

    m_builder.CreateStore(load_rvalue(args[0]).value, ptr_field);
    m_builder.CreateStore(load_rvalue(args[1]).value, len_field);

    return {
        .type = slice_type, .value = variable, .ptr_depth = 1, .memcpy = true};
}

Value Codegen::load_rvalue(const Value& value) {
    auto result = load_lvalue(value);

    if (result.ptr_depth == 1) {
        result.value =
            m_builder.CreateLoad(value.type->llvm_type, result.value);

        --result.ptr_depth;
        result.memcpy = false;
    }

    return result;
}

Value Codegen::load_lvalue(const Value& value) {
    auto result = value;

    while (result.ptr_depth > 1) {
        result.value = m_builder.CreateLoad(
            llvm::PointerType::get(m_context, 0), result.value);

        --result.ptr_depth;
    }

    return result;
}

llvm::Value* Codegen::create_alloca(llvm::Type* type, llvm::Value* size) {
    auto* insert_point = m_builder.GetInsertBlock();

    if (!insert_point) {
        return nullptr;
    }

    m_builder.SetInsertPointPastAllocas(insert_point->getParent());
    auto* result = m_builder.CreateAlloca(type, size);

    m_builder.SetInsertPoint(insert_point);
    return result;
}

llvm::Value* Codegen::create_alloca(Type* type) {
    if (auto* array = dyn_cast<types::VarLenArray>(type)) {
        return m_builder.CreateAlloca(array->type->llvm_type, array->size);
    }

    return create_alloca(type->llvm_type);
}

void Codegen::create_store(const Value& src, llvm::Value* dest) {
    if (src.memcpy) {
        const auto& layout = m_module->getDataLayout();

        m_builder.CreateMemCpy(
            dest, std::nullopt, src.value,
            layout.getPrefTypeAlign(src.type->llvm_type),
            layout.getTypeAllocSize(src.type->llvm_type));

        return;
    }

    m_builder.CreateStore(load_rvalue(src).value, dest);
}

void Codegen::zero_init(llvm::Value* value, Type* type) {
    if (auto* array = dyn_cast<types::VarLenArray>(type)) {
        const auto& layout = m_module->getDataLayout();

        auto* bytes = m_builder.CreateMul(
            array->size,
            llvm::ConstantInt::get(
                m_size, layout.getTypeAllocSize(array->type->llvm_type)));

        m_builder.CreateMemSet(
            value, llvm::ConstantInt::get(llvm::Type::getInt8Ty(m_context), 0),
            bytes, layout.getPrefTypeAlign(array->type->llvm_type));

        return;
    }

    m_builder.CreateStore(llvm::Constant::getNullValue(type->llvm_type), value);
}

void Codegen::create_out_of_bounds_check(
    llvm::Value* index, llvm::Value* len, bool inclusive) {
    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* out_of_bounds = llvm::BasicBlock::Create(m_context, "", function);

    auto* end = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateCondBr(
        inclusive ? m_builder.CreateICmpULE(index, len)
                  : m_builder.CreateICmpULT(index, len),
        end, out_of_bounds);

    m_builder.SetInsertPoint(out_of_bounds);

    create_panic("out of bounds access");

    m_builder.SetInsertPoint(end);
}

void Codegen::create_panic(std::string_view message) {
    auto* arg = llvm::ConstantStruct::get(
        m_slice_type,
        {m_builder.CreateGlobalString(message, "", 0, m_module.get(), false),
         llvm::ConstantInt::get(m_size, message.size())});

    m_builder.CreateCall(
        static_cast<llvm::Function*>(
            m_core_module.names["panic"].element.value),
        {arg});

    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateUnreachable();
    }
}

llvm::Value* Codegen::create_alloca_or_error(
    std::size_t offset, llvm::Type* type, llvm::Value* size) {
    if (auto* result = create_alloca(type, size)) {
        return result;
    }

    error(offset, "could not allocate outside of function");
    return nullptr;
}

llvm::Value* Codegen::create_alloca_or_error(std::size_t offset, Type* type) {
    if (auto* result = create_alloca(type)) {
        return result;
    }

    error(offset, "could not allocate outside of function");
    return nullptr;
}

llvm::Value* Codegen::get_optional_bool(const Value& value) {
    auto* optional = static_cast<types::Optional*>(value.type);

    if (is<types::Pointer>(optional->type)) {
        auto* null = llvm::Constant::getNullValue(optional->type->llvm_type);
        return m_builder.CreateICmpNE(load_rvalue(value).value, null);
    }

    return load_struct_member(
        llvm::Type::getInt1Ty(m_context), value, optional_member_bool);
}

llvm::Value* Codegen::get_optional_value(const Value& value) {
    auto* optional = static_cast<types::Optional*>(value.type);

    if (is<types::Pointer>(optional->type)) {
        return load_rvalue(value).value;
    }

    return load_struct_member(
        optional->type->llvm_type, value, optional_member_value);
}

Value Codegen::get_struct_member(
    Type* member_type, const Value& value, std::uint32_t index) {
    if (value.ptr_depth == 0) {
        return {
            .type = member_type,
            .value = m_builder.CreateExtractValue(value.value, index)};
    }

    auto val = load_lvalue(value);

    return {
        .type = member_type,
        .value =
            m_builder.CreateStructGEP(val.type->llvm_type, val.value, index),
        .ptr_depth = 1,
        .is_mutable = value.is_mutable};
}

llvm::Value* Codegen::load_struct_member(
    llvm::Type* member_type, const Value& value, std::uint32_t index) {
    if (value.ptr_depth == 0) {
        return m_builder.CreateExtractValue(value.value, index);
    }

    auto val = load_lvalue(value);

    return m_builder.CreateLoad(
        member_type,
        m_builder.CreateStructGEP(val.type->llvm_type, val.value, index));
}

Type* Codegen::get_type(
    std::size_t offset, std::string_view name, Scope& parent) {
    auto primitive = m_primitive_types.find(name);

    if (primitive != m_primitive_types.end()) {
        return primitive->second.get();
    }

    auto user = parent.types.find(name);

    if (user == parent.types.end()) {
        if (auto hint =
                did_you_mean_hint(name, parent.types, m_primitive_types)) {
            error_hint(offset, *hint, "undeclared type: {}", log::quoted(name));
        } else {
            error(offset, "undeclared type: {}", log::quoted(name));
        }

        return nullptr;
    }

    if (is_accessible(user->second, m_current_unit)) {
        return user->second.element;
    }

    error(offset, "{} is private", log::quoted(name));

    return nullptr;
}

Value*
Codegen::get_name(std::size_t offset, std::string_view name, Scope& parent) {
    auto iterator = parent.names.find(name);

    if (iterator == parent.names.end()) {
        if (auto hint = did_you_mean_hint(name, parent.names)) {
            error_hint(
                offset, *hint, "undeclared identifier: {}", log::quoted(name));
        } else {
            error(offset, "undeclared identifier: {}", log::quoted(name));
        }

        return nullptr;
    }

    if (is_accessible(iterator->second, m_current_unit)) {
        return &iterator->second.element;
    }

    error(offset, "{} is private", log::quoted(name));

    return nullptr;
}

Scope*
Codegen::get_scope(std::size_t offset, std::string_view name, Scope& parent) {
    auto iterator = parent.scopes.find(name);

    if (iterator == parent.scopes.end()) {
        if (auto hint = did_you_mean_hint(name, parent.scopes)) {
            error_hint(offset, *hint, "could not find {}", log::quoted(name));
        } else {
            error(offset, "could not find {}", log::quoted(name));
        }

        return nullptr;
    }

    return &iterator->second;
}

Codegen::GenericMethod
Codegen::get_generic_method(Type* type, std::string_view name) {
    if (auto* ptr = dyn_cast<types::Pointer>(type)) {
        type = ptr->type;
    } else if (auto* struct_type = dyn_cast<types::Struct>(type)) {
        if (struct_type->origin) {
            auto method = struct_type->origin->methods.find(name);

            if (method != struct_type->origin->methods.end()) {
                return {
                    .function = method->second,
                    .parent_args = struct_type->origin_args};
            }
        }
    } else if (auto* union_type = dyn_cast<types::Union>(type)) {
        if (union_type->origin) {
            auto method = union_type->origin->methods.find(name);

            if (method != union_type->origin->methods.end()) {
                return {
                    .function = method->second,
                    .parent_args = union_type->origin_args};
            }
        }
    } else if (auto* t_struct = dyn_cast<types::GenericStructInst>(type)) {
        auto method = t_struct->type->methods.find(name);

        if (method != t_struct->type->methods.end()) {
            return {.function = method->second, .parent_args = t_struct->args};
        }
    } else if (auto* t_union = dyn_cast<types::GenericUnionInst>(type)) {
        auto method = t_union->type->methods.find(name);

        if (method != t_union->type->methods.end()) {
            return {.function = method->second, .parent_args = t_union->args};
        }
    }

    auto method = type->generic_methods.find(name);

    if (method != type->generic_methods.end()) {
        return {.function = method->second};
    }

    return {.function = nullptr};
}

Scope*
Codegen::resolve_scope(const std::vector<OffsetValue<std::string>>& value) {
    auto* scope = m_current_scope;

    for (std::size_t i = 0; i < value.size() - 1; ++i) {
        scope = get_scope(value[i].offset, value[i].value, *scope);

        if (!scope) {
            return nullptr;
        }
    }

    return scope;
}

llvm::Value* Codegen::alloca_arg(std::size_t index, Type* type) {
    auto layout = m_module->getDataLayout();
    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* value = function->getArg(index);
    auto* variable = create_alloca(type);

    if (get_abi(m_triple) != Abi::SysV) {
        m_builder.CreateStore(value, variable);
        return variable;
    }

    auto classification = sysv::classify(type->llvm_type, layout);

    if (classification.first == sysv::RegClass::Memory) {
        m_builder.CreateMemCpy(
            variable, std::nullopt, value,
            layout.getPrefTypeAlign(type->llvm_type),
            layout.getTypeAllocSize(type->llvm_type));
    } else {
        m_builder.CreateStore(value, variable);
    }

    return variable;
}

void Codegen::type_mismatch(
    std::size_t offset, const Type* expected, const Type* got) {
    error(
        offset, "expected {} but got {}", log::quoted(expected->to_string()),
        log::quoted(got->to_string()));
}

void Codegen::report_invalid_attrs(
    const ast::Declaration& decl,
    std::initializer_list<std::string_view> allowed) {
    for (const auto& attribute : decl.attributes) {
        if (attr_to_os_type(attribute.name.value) ||
            attr_to_arch_type(attribute.name.value)) {
            continue;
        }

        bool valid = false;

        for (const auto& attr : allowed) {
            if (attribute.name.value == attr) {
                valid = true;
                break;
            }
        }

        if (!valid) {
            error(
                attribute.name.offset, "unexpected attribute {}",
                log::quoted(attribute.name.value));
        }
    }
}

bool Codegen::matches_target(const ast::Declaration& decl) {
    std::optional<bool> os_matches = std::nullopt;
    std::optional<bool> arch_matches = std::nullopt;

    for (const auto& attr : decl.attributes) {
        if (auto os_type = attr_to_os_type(attr.name.value)) {
            if (!os_matches.has_value()) {
                os_matches = false;
            }

            if (os_type == m_triple.getOS()) {
                os_matches = true;
            }

            continue;
        }

        if (auto arch_type = attr_to_arch_type(attr.name.value)) {
            if (!arch_matches.has_value()) {
                arch_matches = false;
            }

            if (arch_type == m_triple.getArch()) {
                arch_matches = true;
            }
        }
    }

    if (os_matches.has_value()) {
        if (!*os_matches) {
            return false;
        }
    }

    if (arch_matches.has_value()) {
        if (!*arch_matches) {
            return false;
        }
    }

    return true;
}

std::optional<OffsetValue<std::optional<std::string>>>
Codegen::decl_get_attr(const ast::Declaration& decl, std::string_view attr) {
    for (const auto& attribute : decl.attributes) {
        if (attribute.name.value == attr) {
            if (attribute.value) {
                return OffsetValue{
                    .value = std::optional{attribute.value},
                    .offset = attribute.name.offset};
            }

            return OffsetValue<std::optional<std::string>>{
                .value = std::nullopt, .offset = attribute.name.offset};
        }
    }

    return std::nullopt;
}

} // namespace cent::backend
