#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/decl/fn_decl.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/generic.h"

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
    m_module->setTargetTriple(g_options.target_triple);
}

std::unique_ptr<llvm::Module> Codegen::generate() {
    auto* int8 = llvm::Type::getInt8Ty(m_context);
    auto* int16 = llvm::Type::getInt16Ty(m_context);
    auto* int32 = llvm::Type::getInt32Ty(m_context);
    auto* int64 = llvm::Type::getInt64Ty(m_context);
    auto* size = m_module->getDataLayout().getIntPtrType(m_context);
    auto* void_type = llvm::Type::getVoidTy(m_context);

    m_primitive_types["i8"] = std::make_unique<types::I8>(int8);
    m_primitive_types["i16"] = std::make_unique<types::I16>(int16);
    m_primitive_types["i32"] = std::make_unique<types::I32>(int32);
    m_primitive_types["i64"] = std::make_unique<types::I64>(int64);
    m_primitive_types["isize"] = std::make_unique<types::ISize>(size);
    m_primitive_types["u8"] = std::make_unique<types::U8>(int8);
    m_primitive_types["u16"] = std::make_unique<types::U16>(int16);
    m_primitive_types["u32"] = std::make_unique<types::U32>(int32);
    m_primitive_types["u64"] = std::make_unique<types::U64>(int64);
    m_primitive_types["usize"] = std::make_unique<types::USize>(size);

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
        {llvm::PointerType::get(m_context, 0),
         m_module->getDataLayout().getIntPtrType(m_context)});

    generate(*m_program);

    create_main();

    return std::move(m_module);
}

void Codegen::create_main() {
    if (g_options.emit_type == EmitType::Obj) {
        return;
    }

    if (m_module->getFunction("main")) {
        return;
    }

    auto* user_main = m_module->getFunction("<main>::main");

    if (!user_main) {
        return;
    }

    auto* user_main_return = user_main->getReturnType();
    auto* int32 = llvm::Type::getInt32Ty(m_context);

    if (!user_main_return->isVoidTy() && user_main_return != int32) {
        log::error("invalid `main` signature");
        return;
    }

    auto* main = llvm::Function::Create(
        llvm::FunctionType::get(int32, false), llvm::Function::ExternalLinkage,
        "main", *m_module);

    auto* entry = llvm::BasicBlock::Create(m_context, "", main);

    m_builder.SetInsertPoint(entry);

    if (user_main_return->isVoidTy()) {
        m_builder.CreateCall(user_main);
        m_builder.CreateRet(llvm::ConstantInt::get(int32, 0));

        return;
    }

    auto* result = m_builder.CreateCall(user_main);
    m_builder.CreateRet(result);
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
        type->codegen(*this);
    }

    for (const auto& variable : module.variables) {
        variable->codegen(*this);
    }

    for (const auto& function : module.functions) {
        generate_fn_proto(*function);
    }

    for (const auto& function : module.functions) {
        if (function->block) {
            function->codegen(*this);
        }
    }

    m_generated_modules[module.path] = *m_current_scope;
}

Value Codegen::primitive_cast(Type* type, const Value& value, bool implicit) {
    using enum llvm::Instruction::CastOps;

    auto* base_type = unwrap_type(type);
    auto* base_value_type = unwrap_type(value.type);

    bool value_is_rune = is<types::Rune>(base_value_type);
    bool type_is_rune = is<types::Rune>(base_type);

    if ((value_is_rune || type_is_rune) && implicit) {
        return Value::poisoned();
    }

    bool value_is_float = is_float(base_value_type);
    bool value_is_sint = is_sint(base_value_type);
    bool value_is_uint = is_uint(base_value_type) || value_is_rune;
    bool value_is_ptr = is<types::Pointer>(base_value_type);
    bool value_is_enum = is<types::Enum>(base_value_type);

    if (!value_is_float && !value_is_sint && !value_is_uint && !value_is_ptr &&
        !value_is_enum) {
        return Value::poisoned();
    }

    bool type_is_float = is_float(base_type);
    bool type_is_sint = is_sint(base_type);
    bool type_is_uint = is_uint(base_type) || type_is_rune;
    bool type_is_ptr = is<types::Pointer>(base_type);
    bool type_is_enum = is<types::Enum>(base_type);

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
                return Value{
                    .type = type,
                    .value = llvm::ConstantInt::get(
                        base_type->llvm_type, val.getZExtValue())};
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
    } else if (
        value_is_sint ||
        (!implicit && value_is_enum &&
         is_sint(static_cast<types::Enum*>(base_value_type)->type))) {
        if (type_is_float) {
            cast_op = SIToFP;
        } else if (
            type_is_sint ||
            (!implicit && type_is_enum &&
             is_sint(static_cast<types::Enum*>(base_type)->type))) {
            if (to_size > from_size) {
                cast_op = SExt;
            } else if (!implicit) {
                cast_op = Trunc;
            }
        } else if (
            !implicit &&
            (type_is_uint ||
             (type_is_enum &&
              is_uint(static_cast<types::Enum*>(base_type)->type)))) {
            if (to_size > from_size) {
                cast_op = SExt;
            } else if (to_size < from_size) {
                cast_op = Trunc;
            } else {
                return Value{.type = type, .value = load_rvalue(value).value};
            }
        } else if (!implicit && type_is_ptr) {
            cast_op = IntToPtr;
        }
    } else if (
        value_is_uint ||
        (!implicit && value_is_enum &&
         is_uint(static_cast<types::Enum*>(base_value_type)->type))) {
        if (type_is_float) {
            cast_op = UIToFP;
        } else if (
            type_is_uint ||
            (!implicit && type_is_enum &&
             is_uint(static_cast<types::Enum*>(base_type)->type))) {
            if (to_size > from_size) {
                cast_op = ZExt;
            } else if (!implicit) {
                cast_op = Trunc;
            }
        } else if (
            type_is_sint ||
            (type_is_enum &&
             is_sint(static_cast<types::Enum*>(base_type)->type))) {
            if (to_size > from_size) {
                cast_op = ZExt;
            } else if (!implicit) {
                if (to_size < from_size) {
                    cast_op = Trunc;
                } else {
                    return Value{
                        .type = type, .value = load_rvalue(value).value};
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
                return Value{
                    .type = type,
                    .value = value.value,
                    .ptr_depth = value.ptr_depth};
            }

            auto* value_ptr = static_cast<types::Pointer*>(base_value_type);
            auto* type_ptr = static_cast<types::Pointer*>(base_type);

            if (value_ptr->type == type_ptr->type &&
                (value_ptr->is_mutable || !type_ptr->is_mutable)) {
                return Value{
                    .type = type,
                    .value = value.value,
                    .ptr_depth = value.ptr_depth};
            }
        }
    }

    if (cast_op == CastOpsEnd) {
        return Value::poisoned();
    }

    return Value{
        .type = type,
        .value = m_builder.CreateCast(
            cast_op, load_rvalue(value).value, base_type->llvm_type)};
}

Value Codegen::cast(Type* type, const Value& value, bool implicit) {
    auto* base_type = unwrap_type(type);
    auto* base_value_type = unwrap_type(value.type);

    if (is<types::Never>(base_value_type)) {
        m_builder.CreateUnreachable();
        return Value{.type = type, .value = value.value};
    }

    if (base_type == base_value_type) {
        return Value{
            .type = type, .value = value.value, .ptr_depth = value.ptr_depth};
    }

    if (is<types::TemplateParam>(base_type)) {
        return value;
    }

    if (is<types::Void>(base_type)) {
        return Value::poisoned();
    }

    if (const auto* optional = dyn_cast<types::Optional>(base_type)) {
        auto* base_contained_type = unwrap_type(optional->type);

        if (is<types::Null>(base_value_type)) {
            return Value{
                .type = type,
                .value = llvm::Constant::getNullValue(base_type->llvm_type)};
        }

        if (base_contained_type != base_value_type) {
            return Value::poisoned();
        }

        if (is<types::Pointer>(base_contained_type)) {
            return Value{
                .type = type,
                .value =
                    is<types::Null>(base_value_type)
                        ? llvm::Constant::getNullValue(base_type->llvm_type)
                        : value.value,
                .ptr_depth = value.ptr_depth};
        }

        if (auto* val = llvm::dyn_cast<llvm::Constant>(value.value)) {
            return Value{
                .type = type,
                .value = llvm::ConstantStruct::get(
                    static_cast<llvm::StructType*>(base_type->llvm_type),
                    {val, llvm::ConstantInt::get(
                              llvm::Type::getInt1Ty(m_context), true)})};
        }

        auto* variable = create_alloca(base_type->llvm_type);

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

        return Value{
            .type = type,
            .value = variable,
            .ptr_depth = 1,
            .stack_allocated = true};
    }

    if (const auto* slice = dyn_cast<types::Slice>(base_type)) {
        auto* base_slice_contained_type = unwrap_type(slice->type);

        const auto* array_type = dyn_cast<types::Array>(base_value_type);

        if (!array_type) {
            return Value::poisoned();
        }

        auto* base_array_contained_type = unwrap_type(array_type->type);

        if (base_slice_contained_type != base_array_contained_type) {
            return Value::poisoned();
        }

        auto* variable = create_alloca(base_type->llvm_type);

        if (!variable) {
            return Value::poisoned();
        }

        auto* ptr_member = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, slice_member_ptr);

        auto* len_member = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, slice_member_len);

        auto* intptr = m_module->getDataLayout().getIntPtrType(m_context);

        auto* ptr_value = m_builder.CreateGEP(
            base_slice_contained_type->llvm_type, value.value,
            llvm::ConstantInt::get(intptr, 0));

        m_builder.CreateStore(ptr_value, ptr_member);

        m_builder.CreateStore(
            llvm::ConstantInt::get(intptr, array_type->size), len_member);

        return Value{
            .type = type,
            .value = variable,
            .ptr_depth = 1,
            .stack_allocated = true};
    }

    return primitive_cast(type, value, implicit);
}

bool Codegen::cast_to_result(Type* type, const Value& value, bool implicit) {
    auto* base_type = unwrap_type(type);
    auto* base_value_type = unwrap_type(value.type);

    if (base_type == base_value_type) {
        m_builder.CreateStore(load_rvalue(value).value, m_current_result);
        return true;
    }

    if (is<types::Never>(base_value_type)) {
        m_builder.CreateUnreachable();
        return true;
    }

    if (is<types::Void>(base_type)) {
        return false;
    }

    if (const auto* optional = dyn_cast<types::Optional>(base_type)) {
        auto* base_contained_type = unwrap_type(optional->type);

        if (is<types::Null>(base_value_type)) {
            m_builder.CreateStore(
                llvm::Constant::getNullValue(base_type->llvm_type),
                m_current_result);

            return true;
        }

        if (base_contained_type != base_value_type) {
            return false;
        }

        if (is<types::Pointer>(base_contained_type)) {
            m_builder.CreateStore(
                is<types::Null>(base_value_type)
                    ? llvm::Constant::getNullValue(base_type->llvm_type)
                    : load_rvalue(value).value,
                m_current_result);

            return true;
        }

        if (auto* val = llvm::dyn_cast<llvm::Constant>(value.value)) {
            m_builder.CreateStore(
                llvm::ConstantStruct::get(
                    static_cast<llvm::StructType*>(base_type->llvm_type),
                    {val, llvm::ConstantInt::get(
                              llvm::Type::getInt1Ty(m_context), true)}),
                m_current_result);

            return true;
        }

        auto* bool_ptr = m_builder.CreateStructGEP(
            base_type->llvm_type, m_current_result, optional_member_bool);

        auto* value_ptr = m_builder.CreateStructGEP(
            base_type->llvm_type, m_current_result, optional_member_value);

        m_builder.CreateStore(load_rvalue(value).value, value_ptr);

        m_builder.CreateStore(
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), true),
            bool_ptr);

        return true;
    }

    if (const auto* slice = dyn_cast<types::Slice>(base_type)) {
        auto* base_slice_contained_type = unwrap_type(slice->type);

        const auto* array_type = dyn_cast<types::Array>(base_value_type);

        if (!array_type) {
            return false;
        }

        auto* base_array_contained_type = unwrap_type(array_type->type);

        if (base_slice_contained_type != base_array_contained_type) {
            return false;
        }

        auto* ptr_member = m_builder.CreateStructGEP(
            base_type->llvm_type, m_current_result, slice_member_ptr);

        auto* len_member = m_builder.CreateStructGEP(
            base_type->llvm_type, m_current_result, slice_member_len);

        auto* intptr = m_module->getDataLayout().getIntPtrType(m_context);

        auto* ptr_value = m_builder.CreateGEP(
            base_slice_contained_type->llvm_type, value.value,
            llvm::ConstantInt::get(intptr, 0));

        m_builder.CreateStore(ptr_value, ptr_member);

        m_builder.CreateStore(
            llvm::ConstantInt::get(intptr, array_type->size), len_member);

        return true;
    }

    if (auto val = primitive_cast(type, value, implicit); val.ok()) {
        m_builder.CreateStore(val.value, m_current_result);
        return true;
    }

    return false;
}

Value Codegen::cast_or_error(
    std::size_t offset, Type* type, const Value& value, bool implicit) {
    auto result = cast(type, value, implicit);

    if (!result.ok()) {
        type_mismatch(offset, type, value.type);
    }

    return result;
}

bool Codegen::cast_to_result_or_error(
    std::size_t offset, Type* type, const Value& value, bool implicit) {
    if (cast_to_result(type, value, implicit)) {
        return true;
    }

    type_mismatch(offset, type, value.type);
    return false;
}

Value Codegen::create_call(
    std::size_t offset, types::Function* type, llvm::Value* function,
    const std::vector<std::unique_ptr<ast::Expression>>& arguments) {
    auto params_size = type->param_types.size();
    auto args_size = arguments.size();
    auto default_args_size = type->default_args.size();

    if (!type->variadic && (args_size < params_size - default_args_size ||
                            args_size > params_size)) {
        error(offset, "incorrect number of arguments passed");

        return Value::poisoned();
    }

    std::vector<llvm::Value*> llvm_args;
    llvm_args.reserve(params_size);

    for (std::size_t i = 0; i < args_size; ++i) {
        auto value = arguments[i]->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (type->variadic && i >= params_size) {
            llvm_args.push_back(load_rvalue(value).value);
            continue;
        }

        if (auto val = cast_or_error(
                arguments[i]->offset, type->param_types[i], value);
            val.ok()) {
            llvm_args.push_back(load_rvalue(val).value);
        } else {
            return Value::poisoned();
        }
    }

    for (std::size_t i = default_args_size - (params_size - args_size);
         i < default_args_size; ++i) {
        llvm_args.push_back(type->default_args[i]);
    }

    return Value{
        .type = type->return_type,
        .value = m_builder.CreateCall(
            static_cast<llvm::FunctionType*>(type->llvm_type), function,
            llvm_args)};
}

Value Codegen::load_rvalue(const Value& value) {
    auto result = load_lvalue(value);

    if (result.ptr_depth == 1) {
        result.value =
            m_builder.CreateLoad(value.type->llvm_type, result.value);
        --result.ptr_depth;
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

llvm::Value* Codegen::create_alloca(llvm::Type* type) {
    auto* insert_point = m_builder.GetInsertBlock();

    if (!insert_point) {
        return nullptr;
    }

    m_builder.SetInsertPointPastAllocas(insert_point->getParent());
    auto* result = m_builder.CreateAlloca(type);
    m_builder.SetInsertPoint(insert_point);

    return result;
}

llvm::Value*
Codegen::create_alloca_or_error(std::size_t offset, llvm::Type* type) {
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

GenericFunction* Codegen::get_generic_fn(
    std::size_t offset, std::string_view name, Scope& parent) {
    auto iterator = parent.generic_fns.find(name);

    if (iterator == parent.generic_fns.end()) {
        if (auto hint = did_you_mean_hint(name, parent.names)) {
            error_hint(
                offset, *hint, "undeclared function: {}", log::quoted(name));
        } else {
            error(offset, "undeclared function: {}", log::quoted(name));
        }

        return nullptr;
    }

    if (is_accessible(iterator->second, m_current_unit)) {
        return iterator->second.element.get();
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

void Codegen::type_mismatch(
    std::size_t offset, const Type* expected, const Type* got) {
    error(
        offset, "expected {} but got {}", log::quoted(expected->to_string()),
        log::quoted(got->to_string()));
}

Attributes Codegen::parse_attrs(
    const ast::Declaration& decl, const std::set<std::string_view>& allowed) {
    Attributes result;

    for (const auto& attr : decl.attributes) {
        if (allowed.contains(attr.name)) {
            result.insert(attr.name);
            continue;
        }

        error(attr.offset, "unexpected attribute {}", log::quoted(attr.name));
    }

    return result;
}

bool Codegen::is_float(const Type* type) {
    return is<types::F32, types::F64>(type);
};

bool Codegen::is_sint(const Type* type) {
    return is<types::I8, types::I16, types::I32, types::I64, types::ISize>(
        type);
};

bool Codegen::is_uint(const Type* type) {
    return is<types::U8, types::U16, types::U32, types::U64, types::USize>(
        type);
};

} // namespace cent::backend
