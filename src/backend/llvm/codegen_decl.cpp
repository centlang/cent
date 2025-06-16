#include <cstddef>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/decl/fn_decl.h"
#include "ast/decl/type_alias.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/type.h"
#include "backend/llvm/value.h"

#include "backend/llvm/types/alias.h"
#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/function.h"
#include "backend/llvm/types/primitive.h"
#include "backend/llvm/types/struct.h"
#include "backend/llvm/types/union.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

std::optional<Value> Codegen::generate(ast::FnDecl& decl) {
    if (m_current_function) {
        generate_fn_proto(decl);
    }

    auto type = decl.proto.type ? get_type(
                                      decl.proto.type->offset,
                                      decl.proto.type->value, *m_current_scope)
                                : nullptr;

    auto get_fn_type = [&]() -> std::shared_ptr<Type> {
        if (!decl.proto.type) {
            return m_scope.names[decl.proto.name.value].type;
        }

        auto method = m_methods[type].find(decl.proto.name.value);

        if (method != m_methods[type].end()) {
            return method->second.type;
        }

        return m_scope.scopes[decl.proto.type->value]
            .names[decl.proto.name.value]
            .type;
    };

    auto get_llvm_fn = [&]() {
        if (!decl.proto.type) {
            return static_cast<llvm::Function*>(
                m_scope.names[decl.proto.name.value].value);
        }

        auto method = m_methods[type].find(decl.proto.name.value);

        if (method != m_methods[type].end()) {
            return method->second.function;
        }

        return static_cast<llvm::Function*>(
            m_scope.scopes[decl.proto.type->value]
                .names[decl.proto.name.value]
                .value);
    };

    auto function_type = get_fn_type();
    auto* function = get_llvm_fn();

    auto* entry = llvm::BasicBlock::Create(
        m_context, "", static_cast<llvm::Function*>(function));

    auto* insert_point = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(entry);

    m_current_function = static_cast<types::Function*>(function_type.get());

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        auto& param = decl.proto.params[i];

        auto* value = function->getArg(i);
        auto* variable = create_alloca(value->getType());

        m_builder.CreateStore(value, variable);

        m_scope.names[param.name.value] = {
            m_current_function->param_types[i], variable, param.is_mutable};
    }

    decl.block->codegen(*this);

    m_last_alloca = nullptr;

    if (m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.SetInsertPoint(insert_point);
        m_current_function = nullptr;

        return std::nullopt;
    }

    if (!function->getReturnType()->isVoidTy()) {
        error(
            decl.proto.name.offset,
            "non-void function does not return a value");

        m_builder.SetInsertPoint(insert_point);
        m_current_function = nullptr;

        return std::nullopt;
    }

    m_builder.CreateRetVoid();

    m_builder.SetInsertPoint(insert_point);
    m_current_function = nullptr;

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::Struct& decl) {
    if (m_current_function) {
        generate_struct(decl);
    }

    auto* struct_type =
        llvm::StructType::getTypeByName(m_context, decl.name.value);

    std::vector<llvm::Type*> llvm_fields;
    std::vector<std::shared_ptr<Type>> fields;

    llvm_fields.reserve(decl.fields.size());
    fields.reserve(decl.fields.size());

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        auto& field = decl.fields[i];

        auto type = field.type->codegen(*this);

        if (!type) {
            return std::nullopt;
        }

        auto* llvm_type = type->codegen(*this);

        llvm_fields.push_back(llvm_type);
        fields.push_back(type);

        m_members[struct_type][field.name.value] = i;
    }

    struct_type->setBody(llvm_fields);

    m_current_scope->types[decl.name.value] = std::make_shared<types::Struct>(
        m_current_scope_prefix + decl.name.value, struct_type,
        std::move(fields));

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::Union& decl) {
    auto attrs = parse_attrs(decl, {"tagged"});
    bool tagged = attrs.contains("tagged");

    if (m_current_function) {
        generate_union(decl);
    }

    auto* struct_type =
        llvm::StructType::getTypeByName(m_context, decl.name.value);

    std::vector<std::shared_ptr<Type>> fields;
    fields.reserve(decl.fields.size());

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        auto& field = decl.fields[i];

        auto type = field.type->codegen(*this);

        if (!type) {
            return std::nullopt;
        }

        fields.push_back(type);
        m_members[struct_type][field.name.value] = i;
    }

    std::size_t max = 0;
    llvm::Type* max_type = nullptr;

    auto layout = m_module->getDataLayout();

    for (auto& field : decl.fields) {
        auto* type = field.type->codegen(*this)->codegen(*this);
        auto size = layout.getTypeAllocSize(type);

        if (size > max) {
            max = size;
            max_type = type;
        }
    }

    if (tagged) {
        auto tag_type = std::make_shared<types::Enum>(
            m_current_scope_prefix + decl.name.value + "(tag)",
            m_primitive_types["i32"]);

        for (std::size_t i = 0; i < decl.fields.size(); ++i) {
            m_current_scope->scopes[decl.name.value]
                .names[decl.fields[i].name.value] = {
                tag_type,
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), i)};
        }

        struct_type->setBody({max_type, tag_type->codegen(*this)});

        m_current_scope->types[decl.name.value] =
            std::make_shared<types::Union>(
                m_current_scope_prefix + decl.name.value, struct_type,
                std::move(fields), tag_type);
    } else {
        struct_type->setBody(max_type);

        m_current_scope->types[decl.name.value] =
            std::make_shared<types::Union>(
                m_current_scope_prefix + decl.name.value, struct_type,
                std::move(fields));
    }

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::EnumDecl& decl) {
    if (m_current_function) {
        generate_enum(decl);
    }

    auto& type = m_current_scope->types[decl.name.value];
    auto& enum_type = static_cast<types::Enum&>(*type);

    auto* llvm_type = type->codegen(*this);

    std::uint64_t number = 0;

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        auto& field = decl.fields[i];

        if (!field.value) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] =
                Value{
                    type, llvm::ConstantInt::get(
                              llvm_type, number++, type->is_signed_int())};

            continue;
        }

        auto value = field.value->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (auto val = cast(enum_type.type, *value)) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] =
                *val;

            if (auto* constant =
                    llvm::dyn_cast<llvm::ConstantInt>(val->value)) {
                number = enum_type.is_signed_int() ? constant->getSExtValue()
                                                   : constant->getZExtValue();
            } else {
                error(field.value->offset, "not a constant");

                return std::nullopt;
            }
        } else {
            type_mismatch(field.value->offset, *enum_type.type, *value->type);

            return std::nullopt;
        }

        ++number;
    }

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::TypeAlias& decl) {
    auto type = decl.type->codegen(*this);

    m_current_scope->types[decl.name.value] = std::make_shared<types::Alias>(
        m_current_scope_prefix + decl.name.value, type);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::VarDecl& decl) {
    if (decl.mutability == ast::VarDecl::Mut::Const) {
        if (!decl.value) {
            error(decl.name.offset, "constant has no value");
            return std::nullopt;
        }

        auto value = decl.value->codegen(*this);

        if (decl.type) {
            auto type = decl.type->codegen(*this);
            value = cast(type, *value);

            if (!value) {
                type_mismatch(decl.value->offset, *type, *value->type);
                return std::nullopt;
            }
        }

        if (!llvm::isa<llvm::Constant>(value->value)) {
            error(decl.value->offset, "not a constant");
            return std::nullopt;
        }

        m_scope.names[decl.name.value] = *value;
        return std::nullopt;
    }

    if (decl.mutability == ast::VarDecl::Mut::Immut && !decl.value) {
        error(
            decl.name.offset, fmt::format(
                                  "immutable variable {} must be initialized",
                                  log::quoted(log::bold(decl.name.value))));
        return std::nullopt;
    }

    auto global_var_init = [&] {
        error(
            decl.value->offset, fmt::format(
                                    "global variable {} cannot be initialized",
                                    log::quoted(log::bold(decl.name.value))));
    };

    std::optional<Value> value = std::nullopt;
    std::shared_ptr<Type> type = nullptr;

    llvm::Type* llvm_type = nullptr;

    if (decl.type) {
        type = decl.type->codegen(*this);

        if (!type) {
            return std::nullopt;
        }

        llvm_type = type->codegen(*this);
    }

    if (decl.value) {
        value = decl.value->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!type) {
            if (value->type->is_null() || value->type->is_undefined()) {
                error(
                    decl.name.offset,
                    fmt::format(
                        "cannot infer type for {}",
                        log::bold(log::quoted(decl.name.value))));

                return std::nullopt;
            }

            type = value->type;
            llvm_type = value->type->codegen(*this);
        } else {
            if (!m_current_function) {
                global_var_init();
                return std::nullopt;
            }

            if (value->type->is_undefined()) {
                m_scope.names[decl.name.value] = {
                    type, create_alloca(llvm_type),
                    decl.mutability == ast::VarDecl::Mut::Mut};

                return std::nullopt;
            }

            m_current_result = create_alloca(llvm_type);

            if (!cast_to_result(type, *value)) {
                type_mismatch(decl.value->offset, *type, *value->type);
            } else {
                m_scope.names[decl.name.value] = {
                    type, m_current_result,
                    decl.mutability == ast::VarDecl::Mut::Mut};
            }

            m_current_result = nullptr;

            return std::nullopt;
        }
    }

    if (value) {
        if (value->stack_allocated) {
            m_scope.names[decl.name.value] = {
                type, value->value, decl.mutability == ast::VarDecl::Mut::Mut};

            return std::nullopt;
        }

        if (m_current_function) {
            m_current_result = create_alloca(llvm_type);
        } else {
            global_var_init();
            return std::nullopt;
        }

        if (!value->type->is_undefined()) {
            m_builder.CreateStore(load_value(*value).value, m_current_result);
        }

        m_scope.names[decl.name.value] = {
            type, m_current_result, decl.mutability == ast::VarDecl::Mut::Mut};
    } else {
        if (m_current_function) {
            m_current_result = create_alloca(llvm_type);

            m_builder.CreateStore(
                llvm::Constant::getNullValue(llvm_type), m_current_result);

            m_scope.names[decl.name.value] = {
                type, m_current_result,
                decl.mutability == ast::VarDecl::Mut::Mut};
        } else {
            auto* global = new llvm::GlobalVariable{
                *m_module, llvm_type, false, llvm::GlobalValue::PrivateLinkage,
                llvm::Constant::getNullValue(llvm_type)};

            global->setAlignment(
                m_module->getDataLayout().getPreferredAlign(global));

            m_current_result = global;

            m_scope.names[decl.name.value] = {type, m_current_result, true};
        }
    }

    m_current_result = nullptr;

    return std::nullopt;
}

void Codegen::generate_fn_proto(ast::FnDecl& decl) {
    auto attrs = parse_attrs(decl, {"extern"});
    bool is_extern = attrs.contains("extern");

    if (!is_extern && !decl.block) {
        error(
            decl.proto.name.offset,
            fmt::format(
                "{} has no body",
                log::bold(log::quoted(decl.proto.name.value))));

        return;
    }

    auto& scope = decl.proto.type
                      ? m_current_scope->scopes[decl.proto.type->value]
                      : *m_current_scope;

    if (scope.names.contains(decl.proto.name.value)) {
        error(
            decl.proto.name.offset,
            fmt::format(
                "{} is already defined",
                log::bold(log::quoted(decl.proto.name.value))));

        return;
    }

    auto return_type = decl.proto.return_type
                           ? decl.proto.return_type->codegen(*this)
                           : m_void_type;

    if (!return_type) {
        return;
    }

    auto* llvm_return_type = return_type->codegen(*this);
    std::vector<llvm::Type*> llvm_param_types;

    std::vector<std::shared_ptr<Type>> param_types;
    std::vector<llvm::Constant*> default_args;

    llvm_param_types.reserve(decl.proto.params.size());
    param_types.reserve(decl.proto.params.size());

    for (const auto& parameter : decl.proto.params) {
        auto type = parameter.type->codegen(*this);

        if (!type) {
            return;
        }

        auto* llvm_type = type->codegen(*this);

        llvm_param_types.push_back(llvm_type);
        param_types.push_back(type);

        if (!parameter.value) {
            continue;
        }

        auto value = parameter.value->codegen(*this);

        if (!value) {
            return;
        }

        auto val = cast(type, *value);

        if (!llvm::isa<llvm::Constant>(val->value)) {
            error(parameter.value->offset, "not a constant");
            return;
        }

        default_args.push_back(static_cast<llvm::Constant*>(val->value));
    }

    auto* function_type = llvm::FunctionType::get(
        llvm_return_type, llvm_param_types, decl.proto.variadic);

    auto* function = llvm::Function::Create(
        function_type,
        (decl.is_public || is_extern) ? llvm::Function::ExternalLinkage
                                      : llvm::Function::PrivateLinkage,
        decl.proto.name.value, *m_module);

    if (!decl.proto.type) {
        scope.names[decl.proto.name.value] = Value{
            std::make_shared<types::Function>(
                return_type, std::move(param_types), std::move(default_args),
                decl.proto.variadic),
            function};

        return;
    }

    auto type = get_type(
        decl.proto.type->offset, decl.proto.type->value, *m_current_scope);

    auto func_type = std::make_shared<types::Function>(
        return_type, std::move(param_types), std::move(default_args),
        decl.proto.variadic);

    scope.names[decl.proto.name.value] = {func_type, function};

    if (!decl.proto.params.empty()) {
        auto param_type = decl.proto.params[0].type->codegen(*this);

        if (types_equal(*param_type, *type)) {
            m_methods[type][decl.proto.name.value] = {func_type, function};

            return;
        }

        if (param_type->is_pointer() &&
            types_equal(
                *static_cast<types::Pointer&>(*param_type).type, *type)) {
            m_methods[type][decl.proto.name.value] = {func_type, function};
        }
    }
}

void Codegen::generate_struct(ast::Struct& decl) {
    if (llvm::StructType::getTypeByName(m_context, decl.name.value)) {
        error(
            decl.name.offset, fmt::format(
                                  "{} is already defined",
                                  log::bold(log::quoted(decl.name.value))));

        return;
    }

    llvm::StructType::create(m_context, decl.name.value);
}

void Codegen::generate_union(ast::Union& decl) {
    if (llvm::StructType::getTypeByName(m_context, decl.name.value)) {
        error(
            decl.name.offset, fmt::format(
                                  "{} is already defined",
                                  log::bold(log::quoted(decl.name.value))));

        return;
    }

    llvm::StructType::create(m_context, decl.name.value);
}

void Codegen::generate_enum(ast::EnumDecl& decl) {
    if (m_current_scope->types.contains(decl.name.value)) {
        error(
            decl.name.offset, fmt::format(
                                  "{} is already defined",
                                  log::bold(log::quoted(decl.name.value))));

        return;
    }

    auto type =
        decl.type ? decl.type->codegen(*this) : m_primitive_types["i32"];

    if (!type->is_signed_int() && !type->is_unsigned_int() &&
        !type->is_bool()) {
        error(decl.type->offset, "type mismatch");
        return;
    }

    m_current_scope->types[decl.name.value] = std::make_shared<types::Enum>(
        m_current_scope_prefix + decl.name.value, type);
}

} // namespace cent::backend
