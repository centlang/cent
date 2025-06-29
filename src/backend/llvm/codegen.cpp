#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/decl/fn_decl.h"
#include "ast/decl/type_alias.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/types/enum.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

std::unique_ptr<llvm::Module> Codegen::generate() {
    auto* int8 = llvm::Type::getInt8Ty(m_context);
    auto* int16 = llvm::Type::getInt16Ty(m_context);
    auto* int32 = llvm::Type::getInt32Ty(m_context);
    auto* int64 = llvm::Type::getInt64Ty(m_context);
    auto* size = m_module->getDataLayout().getIntPtrType(m_context);

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

    m_primitive_types["f32"] =
        std::make_unique<types::F32>(llvm::Type::getFloatTy(m_context));

    m_primitive_types["f64"] =
        std::make_unique<types::F64>(llvm::Type::getDoubleTy(m_context));

    m_primitive_types["bool"] =
        std::make_unique<types::Bool>(llvm::Type::getInt1Ty(m_context));

    m_null_type = std::make_unique<types::Null>(nullptr);
    m_undefined_type = std::make_unique<types::Undefined>(nullptr);

    m_void_type =
        std::make_unique<types::Void>(llvm::Type::getVoidTy(m_context));

    m_slice_type = llvm::StructType::create(
        {llvm::PointerType::get(m_context, 0),
         m_module->getDataLayout().getIntPtrType(m_context)});

    create_panic_fn();

    generate(*m_program);

    return std::move(m_module);
}

void Codegen::generate(const ast::Module& module, bool is_submodule) {
    const auto iterator = m_generated_modules.find(module.path);

    if (iterator != m_generated_modules.end()) {
        *m_current_scope = iterator->second;
        return;
    }

    auto filename = m_filename;
    auto* scope = m_current_scope;
    auto scope_prefix = m_current_scope_prefix;

    for (const auto& submodule : module.submodules) {
        m_filename = submodule->path.string();
        m_current_scope = &scope->scopes[*submodule->name];
        m_current_scope_prefix += *submodule->name + "::";

        generate(*submodule, true);
    }

    m_filename = filename;
    m_current_scope = scope;
    m_current_scope_prefix = scope_prefix;

    for (const auto& enum_decl : module.enums) {
        if (is_submodule && !enum_decl->is_public) {
            continue;
        }

        generate_enum(*enum_decl);
    }

    for (const auto& enum_decl : module.enums) {
        if (is_submodule && !enum_decl->is_public) {
            continue;
        }

        enum_decl->codegen(*this);
    }

    for (const auto& type : module.aliases) {
        if (is_submodule && !type->is_public) {
            continue;
        }

        type->codegen(*this);
    }

    for (const auto& struct_decl : module.structs) {
        if (is_submodule && !struct_decl->is_public) {
            continue;
        }

        struct_decl->codegen(*this);
    }

    for (const auto& union_decl : module.unions) {
        if (is_submodule && !union_decl->is_public) {
            continue;
        }

        union_decl->codegen(*this);
    }

    for (const auto& variable : module.variables) {
        if (is_submodule && !variable->is_public) {
            continue;
        }

        variable->codegen(*this);
    }

    for (const auto& function : module.functions) {
        if (is_submodule && !function->is_public) {
            continue;
        }

        generate_fn_proto(*function);
    }

    for (const auto& function : module.functions) {
        if (function->block && !is_submodule) {
            function->codegen(*this);
        }
    }

    m_generated_modules[module.path] = *m_current_scope;
}

std::optional<Value>
Codegen::cast(Type* type, const Value& value, bool implicit) {
    auto* base_type = unwrap_type(type);
    auto* base_value_type = unwrap_type(value.type);

    if (base_type == base_value_type) {
        return value;
    }

    if (is<types::Void>(base_type)) {
        return std::nullopt;
    }

    if (const auto* optional = dyn_cast<types::Optional>(base_type)) {
        auto* base_contained_type = unwrap_type(optional->type);

        if (is<types::Null>(base_value_type)) {
            return Value{
                type, llvm::Constant::getNullValue(base_type->llvm_type)};
        }

        if (base_contained_type != base_value_type) {
            return std::nullopt;
        }

        if (is<types::Pointer>(base_contained_type)) {
            return Value{
                type, is<types::Null>(base_value_type)
                          ? llvm::Constant::getNullValue(base_type->llvm_type)
                          : value.value};
        }

        if (auto* val = llvm::dyn_cast<llvm::Constant>(value.value)) {
            return Value{
                type, llvm::ConstantStruct::get(
                          static_cast<llvm::StructType*>(base_type->llvm_type),
                          {val, llvm::ConstantInt::get(
                                    llvm::Type::getInt1Ty(m_context), true)})};
        }

        auto* variable = create_alloca(base_type->llvm_type);

        auto* bool_ptr = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, optional_member_bool);

        auto* value_ptr = m_builder.CreateStructGEP(
            base_type->llvm_type, variable, optional_member_value);

        m_builder.CreateStore(load_value(value).value, value_ptr);

        m_builder.CreateStore(
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), true),
            bool_ptr);

        return Value{type, variable, false, false, false, true};
    }

    if (const auto* slice = dyn_cast<types::Slice>(base_type)) {
        auto* base_slice_contained_type = unwrap_type(slice->type);

        const auto* array_type = dyn_cast<types::Array>(base_value_type);

        if (!array_type) {
            return std::nullopt;
        }

        auto* base_array_contained_type = unwrap_type(array_type->type);

        if (base_slice_contained_type != base_array_contained_type) {
            return std::nullopt;
        }

        auto* variable = create_alloca(base_type->llvm_type);

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

        return Value{type, variable, false, false, false, true};
    }

    return primitive_cast(type, value, implicit);
}

std::optional<Value>
Codegen::primitive_cast(Type* type, const Value& value, bool implicit) {
    using enum llvm::Instruction::CastOps;

    auto* base_type = unwrap_type(type);
    auto* base_value_type = unwrap_type(value.type);

    bool value_is_float = is_float(base_value_type);
    bool value_is_sint = is_sint(base_value_type);
    bool value_is_uint = is_uint(base_value_type);
    bool value_is_ptr = is<types::Pointer>(base_value_type);
    bool value_is_enum = is<types::Enum>(base_value_type);

    if (!value_is_float && !value_is_sint && !value_is_uint && !value_is_ptr &&
        !value_is_enum) {
        return std::nullopt;
    }

    auto layout = m_module->getDataLayout();

    std::size_t from_size = layout.getTypeAllocSize(base_value_type->llvm_type);
    std::size_t to_size = layout.getTypeAllocSize(base_type->llvm_type);

    bool type_is_float = is_float(base_type);
    bool type_is_sint = is_sint(base_type);
    bool type_is_uint = is_uint(base_type);
    bool type_is_ptr = is<types::Pointer>(base_type);
    bool type_is_enum = is<types::Enum>(base_type);

    llvm::Instruction::CastOps cast_op = CastOpsEnd;

    if (value_is_float) {
        if (type_is_float) {
            if (to_size > from_size) {
                cast_op = FPExt;
            } else if (!implicit) {
                cast_op = FPTrunc;
            }
        } else if (!implicit) {
            if (type_is_sint) {
                cast_op = FPToSI;
            } else if (type_is_uint) {
                cast_op = FPToUI;
            }
        }
    } else if (
        value_is_sint ||
        (!implicit && value_is_enum &&
         is_sint(static_cast<types::Enum*>(base_value_type)->type))) {
        if (type_is_float) {
            cast_op = SIToFP;
        } else if (type_is_sint || type_is_uint || type_is_enum) {
            if (to_size > from_size) {
                cast_op = SExt;
            } else if (!implicit) {
                cast_op = Trunc;
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
        } else if (type_is_sint || type_is_uint || type_is_enum) {
            if (to_size > from_size) {
                cast_op = ZExt;
            } else if (!implicit) {
                cast_op = Trunc;
            }
        } else if (!implicit && type_is_ptr) {
            cast_op = IntToPtr;
        }
    } else if (value_is_ptr) {
        if (!implicit && (type_is_sint || type_is_uint)) {
            cast_op = PtrToInt;
        } else if (type_is_ptr) {
            if (!implicit) {
                return Value{type, value.value, false, value.is_ref};
            }

            auto* value_ptr = static_cast<types::Pointer*>(base_value_type);
            auto* type_ptr = static_cast<types::Pointer*>(base_type);

            if (value_ptr->type == type_ptr->type &&
                (value_ptr->is_mutable || !type_ptr->is_mutable)) {
                return Value{type, value.value, false, value.is_ref};
            }
        }
    }

    if (cast_op == CastOpsEnd) {
        return std::nullopt;
    }

    return Value{
        type, m_builder.CreateCast(
                  cast_op, load_value(value).value, base_type->llvm_type)};
}

bool Codegen::cast_to_result(Type* type, const Value& value, bool implicit) {
    auto* base_type = unwrap_type(type);
    auto* base_value_type = unwrap_type(value.type);

    if (base_type == base_value_type) {
        m_builder.CreateStore(load_value(value).value, m_current_result);

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
                    : value.value,
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

        m_builder.CreateStore(load_value(value).value, value_ptr);

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

    if (auto val = primitive_cast(type, value, implicit)) {
        m_builder.CreateStore(val->value, m_current_result);
        return true;
    }

    return false;
}

Value Codegen::load_value(const Value& value) {
    if (value.is_ref) {
        return value;
    }

    if (value.is_deref) {
        return Value{
            value.type,
            m_builder.CreateLoad(value.type->llvm_type, value.value)};
    }

    if (auto* variable =
            llvm::dyn_cast_or_null<llvm::AllocaInst>(value.value)) {
        return Value{
            value.type, m_builder.CreateLoad(value.type->llvm_type, variable)};
    }

    if (auto* variable =
            llvm::dyn_cast_or_null<llvm::GlobalVariable>(value.value)) {
        return Value{
            value.type, m_builder.CreateLoad(value.type->llvm_type, variable)};
    }

    if (auto* val = llvm::dyn_cast_or_null<llvm::LoadInst>(value.value)) {
        if (val->getType()->isPointerTy() && !is<types::Pointer>(value.type)) {
            return Value{
                value.type, m_builder.CreateLoad(value.type->llvm_type, val)};
        }
    }

    if (auto* ptr =
            llvm::dyn_cast_or_null<llvm::GetElementPtrInst>(value.value)) {
        return Value{
            value.type, m_builder.CreateLoad(value.type->llvm_type, ptr)};
    }

    if (auto* constant =
            llvm::dyn_cast_or_null<llvm::ConstantExpr>(value.value)) {
        if (constant->getOpcode() == llvm::Instruction::GetElementPtr) {
            return Value{
                value.type,
                m_builder.CreateLoad(value.type->llvm_type, constant)};
        }
    }

    return value;
}

llvm::Value* Codegen::create_alloca(llvm::Type* type) {
    auto* insert_point = m_builder.GetInsertBlock();

    m_builder.SetInsertPointPastAllocas(insert_point->getParent());
    auto* result = m_builder.CreateAlloca(type);
    m_builder.SetInsertPoint(insert_point);

    return result;
}

llvm::Value* Codegen::load_struct_member(
    llvm::Type* struct_type, llvm::Type* member_type, llvm::Value* value,
    std::uint32_t index) {
    if (value->getType()->isStructTy()) {
        return m_builder.CreateExtractValue(value, index);
    }

    return m_builder.CreateLoad(
        member_type, m_builder.CreateStructGEP(struct_type, value, index));
}

llvm::Value* Codegen::create_gep_or_extract(
    llvm::Type* struct_type, llvm::Value* value, std::uint32_t index) {
    if (value->getType()->isStructTy()) {
        return m_builder.CreateExtractValue(value, index);
    }

    return m_builder.CreateStructGEP(struct_type, value, index);
}

Type* Codegen::get_type(
    std::size_t offset, std::string_view name, Scope& parent) {
    auto primitive = m_primitive_types.find(name);

    if (primitive != m_primitive_types.end()) {
        return primitive->second.get();
    }

    auto user = parent.types.find(name);

    if (user == parent.types.end()) {
        error(offset, fmt::format("undeclared type: {}", log::quoted(name)));

        return nullptr;
    }

    return user->second;
}

Value*
Codegen::get_name(std::size_t offset, std::string_view name, Scope& parent) {
    auto iterator = parent.names.find(name);

    if (iterator == parent.names.end()) {
        error(
            offset,
            fmt::format("undeclared identifier: {}", log::quoted(name)));

        return nullptr;
    }

    return &iterator->second;
}

Scope*
Codegen::get_scope(std::size_t offset, std::string_view name, Scope& parent) {
    auto iterator = parent.scopes.find(name);

    if (iterator == parent.scopes.end()) {
        error(offset, fmt::format("could not find {}", log::quoted(name)));

        return nullptr;
    }

    return &iterator->second;
}

void Codegen::create_panic_fn() {
    auto* panic_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(m_context),
        llvm::Type::getInt8Ty(m_context)->getPointerTo(), false);

    m_panic_fn = llvm::Function::Create(
        panic_type, llvm::Function::PrivateLinkage, "panic", *m_module);

    auto* fputs_type = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(m_context),
        {llvm::Type::getInt8Ty(m_context)->getPointerTo(),
         llvm::PointerType::get(m_context, 0)},
        false);

    auto* fputs_fn = llvm::Function::Create(
        fputs_type, llvm::Function::ExternalLinkage, "fputs", *m_module);

    auto* stderr_ptr = new llvm::GlobalVariable{
        *m_module, llvm::PointerType::get(m_context, 0),
        false,     llvm::GlobalValue::ExternalLinkage,
        nullptr,   "stderr"};

    auto* exit_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(m_context), {llvm::Type::getInt32Ty(m_context)},
        false);

    auto* exit_fn = llvm::Function::Create(
        exit_type, llvm::Function::ExternalLinkage, "exit", *m_module);

    auto* entry = llvm::BasicBlock::Create(m_context, "", m_panic_fn);

    m_builder.SetInsertPoint(entry);

    auto* stderr_value =
        m_builder.CreateLoad(llvm::PointerType::get(m_context, 0), stderr_ptr);

    m_builder.CreateCall(fputs_fn, {m_panic_fn->getArg(0), stderr_value});

    m_builder.CreateCall(
        exit_fn,
        llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(m_context), 1));

    m_builder.CreateUnreachable();
}

void Codegen::type_mismatch(
    std::size_t offset, const Type* expected, const Type* got) {
    error(
        offset,
        fmt::format(
            "expected {} but got {}", log::quoted(expected->to_string()),
            log::quoted(got->to_string())));
}

Attributes Codegen::parse_attrs(
    const ast::Declaration& decl, const std::set<std::string_view>& allowed) {
    Attributes result;

    for (const auto& attr : decl.attributes) {
        if (allowed.contains(attr.name)) {
            result.insert(attr.name);
            continue;
        }

        error(
            attr.offset,
            fmt::format("unexpected attribute {}", log::quoted(attr.name)));
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
