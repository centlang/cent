#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/decl/fn_decl.h"
#include "ast/decl/type_alias.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/type.h"
#include "backend/llvm/value.h"

#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/primitive.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

std::unique_ptr<llvm::Module> Codegen::generate() {
    m_primitive_types = {
        {"i8", std::make_shared<types::I8>()},
        {"i16", std::make_shared<types::I16>()},
        {"i32", std::make_shared<types::I32>()},
        {"i64", std::make_shared<types::I64>()},
        {"isize", std::make_shared<types::ISize>()},
        {"u8", std::make_shared<types::U8>()},
        {"u16", std::make_shared<types::U16>()},
        {"u32", std::make_shared<types::U32>()},
        {"u64", std::make_shared<types::U64>()},
        {"usize", std::make_shared<types::USize>()},
        {"f32", std::make_shared<types::F32>()},
        {"f64", std::make_shared<types::F64>()},
        {"str", std::make_shared<types::Str>()},
        {"bool", std::make_shared<types::Bool>()}};

    m_null_type = std::make_shared<types::Null>();
    m_undefined_type = std::make_shared<types::Undefined>();
    m_void_type = std::make_shared<types::Void>();

    m_slice_type = llvm::StructType::create(
        {llvm::PointerType::get(m_context, 0),
         m_module->getDataLayout().getIntPtrType(m_context)});

    create_panic_fn();

    generate(*m_program);

    return std::move(m_module);
}

void Codegen::generate(ast::Module& module, bool is_submodule) {
    auto iterator = m_generated_modules.find(module.path);

    if (iterator != m_generated_modules.end()) {
        *m_current_scope = iterator->second;
        return;
    }

    auto filename = m_filename;
    auto source = m_source;

    auto* scope = m_current_scope;
    auto scope_prefix = m_current_scope_prefix;

    for (auto& submodule : module.submodules) {
        m_filename = submodule->path.string();
        auto src = read_file(submodule->path);

        if (!src) {
            return;
        }

        m_source = *src;

        m_current_scope = &scope->scopes[*submodule->name];
        m_current_scope_prefix += *submodule->name + "::";

        generate(*submodule, true);
    }

    m_filename = filename;
    m_source = source;

    m_current_scope = scope;
    m_current_scope_prefix = scope_prefix;

    for (auto& enum_decl : module.enums) {
        if (is_submodule && !enum_decl->is_public) {
            continue;
        }

        generate_enum(*enum_decl);
    }

    for (auto& struct_decl : module.structs) {
        if (is_submodule && !struct_decl->is_public) {
            continue;
        }

        generate_struct(*struct_decl);
    }

    for (auto& union_decl : module.unions) {
        if (is_submodule && !union_decl->is_public) {
            continue;
        }

        generate_union(*union_decl);
    }

    for (auto& enum_decl : module.enums) {
        if (is_submodule && !enum_decl->is_public) {
            continue;
        }

        enum_decl->codegen(*this);
    }

    for (auto& type : module.aliases) {
        if (is_submodule && !type->is_public) {
            continue;
        }

        type->codegen(*this);
    }

    for (auto& struct_decl : module.structs) {
        if (is_submodule && !struct_decl->is_public) {
            continue;
        }

        struct_decl->codegen(*this);
    }

    for (auto& union_decl : module.unions) {
        if (is_submodule && !union_decl->is_public) {
            continue;
        }

        union_decl->codegen(*this);
    }

    for (auto& variable : module.variables) {
        if (is_submodule && !variable->is_public) {
            continue;
        }

        variable->codegen(*this);
    }

    for (auto& function : module.functions) {
        if (is_submodule && !function->is_public) {
            continue;
        }

        generate_fn_proto(*function);
    }

    for (auto& function : module.functions) {
        if (function->block && !is_submodule) {
            function->codegen(*this);
        }
    }

    m_generated_modules[module.path] = *m_current_scope;
}

std::optional<Value>
Codegen::cast(std::shared_ptr<Type>& type, Value& value, bool implicit) {
    if (types_equal(*type, *value.type)) {
        return value;
    }

    if (is<types::Void>(*type)) {
        return std::nullopt;
    }

    auto* llvm_type = type->codegen(*this);

    if (auto* optional = dyn_cast<types::Optional>(*type)) {
        if (is<types::Null>(*value.type)) {
            return Value{type, llvm::Constant::getNullValue(llvm_type)};
        }

        if (!types_equal(*optional->type, *value.type)) {
            return std::nullopt;
        }

        if (is<types::Pointer>(*optional->type)) {
            return Value{
                type, is<types::Null>(*value.type)
                          ? llvm::Constant::getNullValue(llvm_type)
                          : value.value};
        }

        if (auto* val = llvm::dyn_cast<llvm::Constant>(value.value)) {
            return Value{
                type, llvm::ConstantStruct::get(
                          static_cast<llvm::StructType*>(llvm_type),
                          {val, llvm::ConstantInt::get(
                                    llvm::Type::getInt1Ty(m_context), true)})};
        }

        auto* variable = create_alloca(llvm_type);
        auto* bool_ptr = m_builder.CreateStructGEP(
            llvm_type, variable, optional_member_bool);

        auto* value_ptr = m_builder.CreateStructGEP(
            llvm_type, variable, optional_member_value);

        m_builder.CreateStore(load_value(value).value, value_ptr);

        m_builder.CreateStore(
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), true),
            bool_ptr);

        return Value{type, variable, false, false, false, true};
    }

    if (auto* slice = dyn_cast<types::Slice>(*type)) {
        auto* array_type = dyn_cast<types::Array>(*value.type);

        if (!array_type) {
            return std::nullopt;
        }

        auto* variable = create_alloca(llvm_type);

        if (!types_equal(*slice->type, *array_type->type)) {
            return std::nullopt;
        }

        auto* ptr_member =
            m_builder.CreateStructGEP(llvm_type, variable, slice_member_ptr);

        auto* len_member =
            m_builder.CreateStructGEP(llvm_type, variable, slice_member_len);

        auto* intptr = m_module->getDataLayout().getIntPtrType(m_context);

        auto* ptr_value = m_builder.CreateGEP(
            slice->type->codegen(*this), value.value,
            llvm::ConstantInt::get(intptr, 0));

        m_builder.CreateStore(ptr_value, ptr_member);

        m_builder.CreateStore(
            llvm::ConstantInt::get(intptr, array_type->size), len_member);

        return Value{type, variable, false, false, false, true};
    }

    return primitive_cast(type, llvm_type, value, implicit);
}

std::optional<Value> Codegen::primitive_cast(
    std::shared_ptr<Type>& type, llvm::Type* llvm_type, Value& value,
    bool implicit) {
    using enum llvm::Instruction::CastOps;

    bool value_is_float = is_float(*value.type);
    bool value_is_sint = is_sint(*value.type);
    bool value_is_uint = is_uint(*value.type);
    bool value_is_ptr = is<types::Pointer>(*value.type);
    bool value_is_enum = is<types::Enum>(*value.type);

    if (!value_is_float && !value_is_sint && !value_is_uint && !value_is_ptr &&
        !value_is_enum) {
        return std::nullopt;
    }

    auto layout = m_module->getDataLayout();

    std::size_t from_size = layout.getTypeAllocSize(value.type->codegen(*this));
    std::size_t to_size = layout.getTypeAllocSize(llvm_type);

    bool type_is_float = is_float(*type);
    bool type_is_sint = is_sint(*type);
    bool type_is_uint = is_uint(*type);
    bool type_is_ptr = is<types::Pointer>(*type);
    bool type_is_enum = is<types::Enum>(*type);

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
         is_sint(*static_cast<types::Enum&>(*value.type).type))) {
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
         is_uint(*static_cast<types::Enum&>(*value.type).type))) {
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

            auto& value_ptr = static_cast<types::Pointer&>(*value.type);
            auto& type_ptr = static_cast<types::Pointer&>(*type);

            if (types_equal(*value_ptr.type, *type_ptr.type) &&
                (value_ptr.is_mutable || !type_ptr.is_mutable)) {
                return Value{type, value.value, false, value.is_ref};
            }
        }
    }

    if (cast_op == CastOpsEnd) {
        return std::nullopt;
    }

    return Value{
        type,
        m_builder.CreateCast(cast_op, load_value(value).value, llvm_type)};
}

bool Codegen::cast_to_result(
    std::shared_ptr<Type>& type, Value& value, bool implicit) {
    if (types_equal(*type, *value.type)) {
        m_builder.CreateStore(load_value(value).value, m_current_result);

        return true;
    }

    auto* llvm_type = type->codegen(*this);

    if (auto* optional = dyn_cast<types::Optional>(*type)) {
        if (is<types::Null>(*value.type)) {
            m_builder.CreateStore(
                llvm::Constant::getNullValue(llvm_type), m_current_result);

            return true;
        }

        if (!types_equal(*optional->type, *value.type)) {
            return false;
        }

        if (is<types::Pointer>(*optional->type)) {
            m_builder.CreateStore(
                is<types::Null>(*value.type)
                    ? llvm::Constant::getNullValue(llvm_type)
                    : value.value,
                m_current_result);

            return true;
        }

        if (auto* val = llvm::dyn_cast<llvm::Constant>(value.value)) {
            m_builder.CreateStore(
                llvm::ConstantStruct::get(
                    static_cast<llvm::StructType*>(llvm_type),
                    {val, llvm::ConstantInt::get(
                              llvm::Type::getInt1Ty(m_context), true)}),
                m_current_result);

            return true;
        }

        auto* bool_ptr = m_builder.CreateStructGEP(
            llvm_type, m_current_result, optional_member_bool);

        auto* value_ptr = m_builder.CreateStructGEP(
            llvm_type, m_current_result, optional_member_value);

        m_builder.CreateStore(load_value(value).value, value_ptr);

        m_builder.CreateStore(
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), true),
            bool_ptr);

        return true;
    }

    if (auto* slice = dyn_cast<types::Slice>(*type)) {
        auto* array_type = dyn_cast<types::Array>(*value.type);

        if (!array_type) {
            return false;
        }

        if (!types_equal(*slice->type, *array_type->type)) {
            return false;
        }

        auto* ptr_member = m_builder.CreateStructGEP(
            llvm_type, m_current_result, slice_member_ptr);

        auto* len_member = m_builder.CreateStructGEP(
            llvm_type, m_current_result, slice_member_len);

        auto* intptr = m_module->getDataLayout().getIntPtrType(m_context);

        auto* ptr_value = m_builder.CreateGEP(
            slice->type->codegen(*this), value.value,
            llvm::ConstantInt::get(intptr, 0));

        m_builder.CreateStore(ptr_value, ptr_member);

        m_builder.CreateStore(
            llvm::ConstantInt::get(intptr, array_type->size), len_member);

        return true;
    }

    if (auto val = primitive_cast(type, llvm_type, value, implicit)) {
        m_builder.CreateStore(val->value, m_current_result);
        return true;
    }

    return false;
}

Value Codegen::load_value(Value& value) {
    if (value.is_ref) {
        return value;
    }

    if (value.is_deref) {
        return Value{
            value.type,
            m_builder.CreateLoad(value.type->codegen(*this), value.value)};
    }

    if (auto* variable =
            llvm::dyn_cast_or_null<llvm::AllocaInst>(value.value)) {
        return Value{
            value.type,
            m_builder.CreateLoad(value.type->codegen(*this), variable)};
    }

    if (auto* variable =
            llvm::dyn_cast_or_null<llvm::GlobalVariable>(value.value)) {
        if (!(variable->isConstant() && is<types::Str>(*value.type))) {
            return Value{
                value.type,
                m_builder.CreateLoad(value.type->codegen(*this), variable)};
        }
    }

    if (auto* val = llvm::dyn_cast_or_null<llvm::LoadInst>(value.value)) {
        auto* type = value.type->codegen(*this);

        if (val->getType()->isPointerTy() && !is<types::Pointer>(*value.type)) {
            return Value{value.type, m_builder.CreateLoad(type, val)};
        }
    }

    if (auto* ptr =
            llvm::dyn_cast_or_null<llvm::GetElementPtrInst>(value.value)) {
        return Value{
            value.type, m_builder.CreateLoad(value.type->codegen(*this), ptr)};
    }

    if (auto* constant =
            llvm::dyn_cast_or_null<llvm::ConstantExpr>(value.value)) {
        if (constant->getOpcode() == llvm::Instruction::GetElementPtr) {
            return Value{
                value.type,
                m_builder.CreateLoad(value.type->codegen(*this), constant)};
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

std::shared_ptr<Type>
Codegen::get_type(std::size_t offset, std::string_view name, Scope& parent) {
    auto primitive = m_primitive_types.find(name);

    if (primitive != m_primitive_types.end()) {
        return primitive->second;
    }

    auto user = parent.types.find(name);

    if (user == parent.types.end()) {
        error(
            offset,
            fmt::format("undeclared type: {}", log::bold(log::quoted(name))));

        return nullptr;
    }

    return user->second;
}

std::optional<Value>
Codegen::get_name(std::size_t offset, std::string_view name, Scope& parent) {
    auto iterator = parent.names.find(name);

    if (iterator == parent.names.end()) {
        error(
            offset,
            fmt::format(
                "undeclared identifier: {}", log::bold(log::quoted(name))));

        return std::nullopt;
    }

    return iterator->second;
}

Scope*
Codegen::get_scope(std::size_t offset, std::string_view name, Scope& parent) {
    auto iterator = parent.scopes.find(name);

    if (iterator == parent.scopes.end()) {
        error(
            offset,
            fmt::format("could not find {}", log::bold(log::quoted(name))));

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

void Codegen::type_mismatch(std::size_t offset, Type& expected, Type& got) {
    error(
        offset, fmt::format(
                    "expected {} but got {}",
                    log::bold(log::quoted(expected.to_string())),
                    log::bold(log::quoted(got.to_string()))));
}

Attributes Codegen::parse_attrs(
    ast::Declaration& decl, const std::set<std::string_view>& allowed) {
    Attributes result;

    for (auto& attr : decl.attributes) {
        if (allowed.contains(attr.name)) {
            result.insert(attr.name);
            continue;
        }

        error(
            attr.offset,
            fmt::format(
                "unexpected attribute {}", log::bold(log::quoted(attr.name))));
    }

    return result;
}

bool Codegen::is_float(const Type& type) {
    return is<types::F32, types::F64>(type);
};

bool Codegen::is_sint(const Type& type) {
    return is<types::I8, types::I16, types::I32, types::I64, types::ISize>(
        type);
};

bool Codegen::is_uint(const Type& type) {
    return is<types::U8, types::U16, types::U32, types::U64, types::USize>(
        type);
};

} // namespace cent::backend
