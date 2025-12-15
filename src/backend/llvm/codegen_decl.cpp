#include <cstddef>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/decl/enum_decl.h"
#include "ast/decl/fn_decl.h"
#include "ast/decl/struct.h"
#include "ast/decl/type_alias.h"
#include "ast/decl/union.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/types/alias.h"
#include "backend/llvm/types/struct.h"
#include "backend/llvm/types/union.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

Value Codegen::generate(const ast::FnDecl& decl) {
    if (m_current_function) {
        generate_fn_proto(decl);
    }

    Type* type = nullptr;

    if (decl.type) {
        type = get_type(decl.type->offset, decl.type->value, *m_current_scope);

        if (!type) {
            return Value::poisoned();
        }
    }

    auto get_fn_type = [&]() -> Type* {
        if (!decl.type) {
            auto iterator = m_current_scope->names.find(decl.name.value);

            if (iterator != m_current_scope->names.end()) {
                return iterator->second.element.type;
            }

            return nullptr;
        }

        auto iterator = m_current_scope->scopes[decl.type->value].names.find(
            decl.name.value);

        if (iterator != m_current_scope->scopes[decl.type->value].names.end()) {
            return iterator->second.element.type;
        }

        return nullptr;
    };

    auto get_llvm_fn = [&]() {
        if (!decl.type) {
            return static_cast<llvm::Function*>(
                m_current_scope->names[decl.name.value].element.value);
        }

        return static_cast<llvm::Function*>(
            m_current_scope->scopes[decl.type->value]
                .names[decl.name.value]
                .element.value);
    };

    auto* function_type = get_fn_type();

    if (!function_type) {
        return Value::poisoned();
    }

    auto* function = get_llvm_fn();

    auto* entry = llvm::BasicBlock::Create(m_context, "", function);

    auto* insert_point = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(entry);

    auto* current_function = m_current_function;
    m_current_function = static_cast<types::Function*>(function_type);

    auto current_scope_names = m_current_scope->names;

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        const auto& param = decl.proto.params[i];

        auto* value = function->getArg(i);
        auto* variable = create_alloca(value->getType());

        m_builder.CreateStore(value, variable);

        m_current_scope->names[param.name.value] = {
            .element =
                {.type = m_current_function->param_types[i],
                 .value = variable,
                 .ptr_depth = 1,
                 .is_mutable = param.is_mutable},
            .unit = m_current_unit};
    }

    auto scope_prefix = m_current_scope_prefix;
    m_current_scope_prefix += decl.name.value + "::__";

    decl.block->codegen(*this);

    m_current_scope_prefix = scope_prefix;
    m_current_function = current_function;
    m_current_scope->names = current_scope_names;

    if (m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.SetInsertPoint(insert_point);
        return Value::poisoned();
    }

    if (!m_current_fn_had_error && !function->getReturnType()->isVoidTy()) {
        error(decl.name.offset, "non-void function does not return a value");
        m_builder.SetInsertPoint(insert_point);

        return Value::poisoned();
    }

    m_builder.CreateRetVoid();
    m_builder.SetInsertPoint(insert_point);

    return Value::poisoned();
}

Value Codegen::generate(const ast::Struct& decl) {
    if (m_current_scope->types.contains(decl.name.value)) {
        error(
            decl.name.offset, "{} is already defined",
            log::quoted(decl.name.value));

        return Value::poisoned();
    }

    if (!decl.template_params.empty()) {
        not_implemented(decl.template_params[0].offset, "generic structs");
        return Value::poisoned();
    }

    auto* struct_type = llvm::StructType::create(
        m_context, m_current_scope_prefix + decl.name.value);

    std::vector<llvm::Type*> llvm_fields;
    std::vector<Type*> fields;

    llvm_fields.reserve(decl.fields.size());
    fields.reserve(decl.fields.size());

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        const auto& field = decl.fields[i];

        auto* type = field.type->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }

        llvm_fields.push_back(type->llvm_type);
        fields.push_back(type);

        m_members[struct_type][field.name.value] = i;
    }

    struct_type->setBody(llvm_fields);

    m_named_types.push_back(
        std::make_unique<types::Struct>(
            struct_type, m_current_scope_prefix + decl.name.value,
            std::move(fields)));

    m_current_scope->types[decl.name.value] = {
        .element = m_named_types.back().get(),
        .is_public = decl.is_public,
        .unit = m_current_unit};

    return Value::poisoned();
}

Value Codegen::generate(const ast::Union& decl) {
    auto attrs = parse_attrs(decl, {"untagged"});
    bool untagged = attrs.contains("untagged");

    if (m_current_scope->types.contains(decl.name.value)) {
        error(
            decl.name.offset, "{} is already defined",
            log::quoted(decl.name.value));

        return Value::poisoned();
    }

    if (!decl.template_params.empty()) {
        not_implemented(decl.template_params[0].offset, "generic unions");
        return Value::poisoned();
    }

    auto* struct_type = llvm::StructType::create(
        m_context, m_current_scope_prefix + decl.name.value);

    std::vector<Type*> fields;
    fields.reserve(decl.fields.size());

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        const auto& field = decl.fields[i];

        auto* type = field.type->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }

        fields.push_back(type);
        m_members[struct_type][field.name.value] = i;
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

    if (untagged) {
        struct_type->setBody(max_type);

        m_named_types.push_back(
            std::make_unique<types::Union>(
                struct_type, m_current_scope_prefix + decl.name.value,
                std::move(fields)));

        m_current_scope->types[decl.name.value] = {
            .element = m_named_types.back().get(),
            .is_public = decl.is_public,
            .unit = m_current_unit};

        return Value::poisoned();
    }

    auto* underlying = m_primitive_types["i32"].get();

    m_named_types.push_back(
        std::make_unique<types::Enum>(
            underlying->llvm_type,
            m_current_scope_prefix + decl.name.value + "(tag)", underlying));

    auto* tag_type = static_cast<types::Enum*>(m_named_types.back().get());

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        m_current_scope->scopes[decl.name.value]
            .names[decl.fields[i].name.value] = {
            .element =
                {.type = tag_type,
                 .value = llvm::ConstantInt::get(underlying->llvm_type, i)},
            .is_public = decl.is_public,
            .unit = m_current_unit};
    }

    struct_type->setBody({max_type, underlying->llvm_type});

    m_named_types.push_back(
        std::make_unique<types::Union>(
            struct_type, m_current_scope_prefix + decl.name.value,
            std::move(fields), tag_type));

    m_current_scope->types[decl.name.value] = {
        .element = m_named_types.back().get(),
        .is_public = decl.is_public,
        .unit = m_current_unit};

    return Value::poisoned();
}

Value Codegen::generate(const ast::EnumDecl& decl) {
    if (m_current_scope->types.contains(decl.name.value)) {
        error(
            decl.name.offset, "{} is already defined",
            log::quoted(decl.name.value));

        return Value::poisoned();
    }

    if (decl.type) {
        auto* type = decl.type->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }

        if (!is_sint(type) && !is_uint(type) && !is<types::Bool>(type)) {
            error(decl.type->offset, "type mismatch");
            return Value::poisoned();
        }

        m_named_types.push_back(
            std::make_unique<types::Enum>(
                type->llvm_type, m_current_scope_prefix + decl.name.value,
                type));

        m_current_scope->types[decl.name.value] = {
            .element = m_named_types.back().get(),
            .is_public = decl.is_public,
            .unit = m_current_unit};
    }

    auto* underlying = m_primitive_types["i32"].get();

    m_named_types.push_back(
        std::make_unique<types::Enum>(
            underlying->llvm_type, m_current_scope_prefix + decl.name.value,
            underlying));

    auto* type = static_cast<types::Enum*>(m_named_types.back().get());

    m_current_scope->types[decl.name.value] = {
        .element = type, .is_public = decl.is_public, .unit = m_current_unit};

    std::uint64_t number = 0;

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        const auto& field = decl.fields[i];

        if (!field.value) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] = {
                .element =
                    {.type = type,
                     .value = llvm::ConstantInt::get(
                         type->llvm_type, number++, is_sint(type))},
                .is_public = decl.is_public,
                .unit = m_current_unit};

            continue;
        }

        auto value = field.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (auto val = cast_or_error(field.value->offset, type->type, value);
            val.ok()) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] = {
                .element = val,
                .is_public = decl.is_public,
                .unit = m_current_unit};

            if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(val.value)) {
                number = is_sint(type) ? constant->getSExtValue()
                                       : constant->getZExtValue();
            } else {
                error(field.value->offset, "not a constant");

                return Value::poisoned();
            }
        } else {
            return Value::poisoned();
        }

        ++number;
    }

    return Value::poisoned();
}

Value Codegen::generate(const ast::TypeAlias& decl) {
    auto attrs = parse_attrs(decl, {"distinct"});
    bool distinct = attrs.contains("distinct");

    auto* type = decl.type->codegen(*this);

    if (!type) {
        return Value::poisoned();
    }

    m_named_types.push_back(
        std::make_unique<types::Alias>(
            type->llvm_type, m_current_scope_prefix + decl.name.value, type,
            distinct));

    m_current_scope->types[decl.name.value] = {
        .element = m_named_types.back().get(),
        .is_public = decl.is_public,
        .unit = m_current_unit};

    return Value::poisoned();
}

Value Codegen::generate(const ast::VarDecl& decl) {
    auto attrs = parse_attrs(decl, {"extern"});
    bool is_extern = attrs.contains("extern");

    auto& result = m_current_scope->names[decl.name.value];
    result = {.element = Value::poisoned()};

    if (decl.mutability == ast::VarDecl::Mut::Const) {
        if (!decl.value) {
            error(decl.name.offset, "constant has no value");
            return Value::poisoned();
        }

        auto value = decl.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (decl.type) {
            auto* type = decl.type->codegen(*this);

            if (!type) {
                return Value::poisoned();
            }

            value = cast_or_error(decl.value->offset, type, value);

            if (!value.ok()) {
                return Value::poisoned();
            }
        }

        if (!llvm::isa<llvm::Constant>(value.value)) {
            error(decl.value->offset, "not a constant");
            return Value::poisoned();
        }

        result = {
            .element = value,
            .is_public = decl.is_public,
            .unit = m_current_unit};

        return Value::poisoned();
    }

    Value value = Value::poisoned();
    Type* type = nullptr;

    if (!decl.type && !m_current_function) {
        error(
            decl.value->offset, "type of global variable {} was not specified",
            log::quoted(decl.name.value));

        return Value::poisoned();
    }

    if (decl.type) {
        type = decl.type->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }
    }

    if (!decl.value && decl.mutability == ast::VarDecl::Mut::Immut) {
        error(
            decl.name.offset, "immutable variable {} must be initialized",
            log::quoted(decl.name.value));

        return Value::poisoned();
    }

    if (decl.value) {
        value = decl.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (type) {
            if (!is<types::Undefined>(value.type)) {
                value = cast_or_error(decl.value->offset, type, value);

                if (!value.ok()) {
                    return Value::poisoned();
                }
            }
        } else {
            if (is<types::Null, types::Undefined>(value.type)) {
                error(
                    decl.name.offset, "cannot infer type for {}",
                    log::quoted(decl.name.value));

                return Value::poisoned();
            }

            type = value.type;
        }
    }

    if (!m_current_function) {
        llvm::Constant* constant = nullptr;

        if (value.ok()) {
            if (is<types::Undefined>(value.type)) {
                error(
                    decl.value->offset,
                    "cannot initialize global variable with `undefined`");

                return Value::poisoned();
            }

            constant = llvm::dyn_cast<llvm::Constant>(value.value);

            if (!constant) {
                error(decl.value->offset, "not a constant");
                return Value::poisoned();
            }
        }

        auto* global = new llvm::GlobalVariable{
            *m_module,
            type->llvm_type,
            false,
            (decl.is_public || is_extern) ? llvm::GlobalValue::ExternalLinkage
                                          : llvm::GlobalValue::PrivateLinkage,
            constant,
            is_extern ? decl.name.value
                      : m_current_scope_prefix + decl.name.value};

        global->setAlignment(
            m_module->getDataLayout().getPreferredAlign(global));

        result = {
            .element =
                {.type = type,
                 .value = global,
                 .ptr_depth = 1,
                 .is_mutable = true},
            .is_public = decl.is_public,
            .unit = m_current_unit};

        return Value::poisoned();
    }

    m_current_result = create_alloca(type->llvm_type);

    result = {
        .element =
            {.type = type,
             .value = m_current_result,
             .ptr_depth = 1,
             .is_mutable = decl.mutability == ast::VarDecl::Mut::Mut},
        .unit = m_current_unit};

    if (value.ok()) {
        if (is<types::Undefined>(value.type)) {
            m_current_result = nullptr;
            return Value::poisoned();
        }

        if (value.stack_allocated &&
            unwrap_type(type) == unwrap_type(value.type)) {
            result = {
                .element =
                    {.type = type,
                     .value = value.value,
                     .ptr_depth = value.ptr_depth,
                     .is_mutable = decl.mutability == ast::VarDecl::Mut::Mut},
                .unit = m_current_unit};

            return Value::poisoned();
        }

        cast_to_result_or_error(decl.value->offset, type, value);
    } else {
        m_builder.CreateStore(
            llvm::Constant::getNullValue(type->llvm_type), m_current_result);
    }

    m_current_result = nullptr;
    return Value::poisoned();
}

void Codegen::generate_fn_proto(const ast::FnDecl& decl) {
    auto attrs = parse_attrs(decl, {"extern"});
    bool is_extern = attrs.contains("extern");

    if (!is_extern && !decl.block) {
        error(decl.name.offset, "{} has no body", log::quoted(decl.name.value));
        return;
    }

    auto& scope = decl.type ? m_current_scope->scopes[decl.type->value]
                            : *m_current_scope;

    if (scope.names.contains(decl.name.value)) {
        error(
            decl.name.offset, "{} is already defined",
            log::quoted(decl.name.value));

        return;
    }

    if (!decl.template_params.empty()) {
        not_implemented(decl.template_params[0].offset, "generic functions");
        return;
    }

    auto* fn_type = generate_fn_type(decl.proto);

    if (!fn_type) {
        return;
    }

    auto* llvm_fn_type = static_cast<llvm::FunctionType*>(fn_type->llvm_type);

    llvm::Function* function = nullptr;

    if (is_extern) {
        if (auto* func = m_module->getFunction(decl.name.value)) {
            function = func;
        }
    }

    if (!function) {
        function = llvm::Function::Create(
            llvm_fn_type,
            (decl.is_public || is_extern) ? llvm::Function::ExternalLinkage
                                          : llvm::Function::PrivateLinkage,
            is_extern ? decl.name.value
                      : m_current_scope_prefix +
                            (decl.type ? decl.type->value + "::" : "") +
                            decl.name.value,
            *m_module);
    }

    scope.names[decl.name.value] = {
        .element = {.type = fn_type, .value = function},
        .is_public = decl.is_public,
        .unit = m_current_unit};

    if (!decl.type) {
        return;
    }

    auto* type =
        get_type(decl.type->offset, decl.type->value, *m_current_scope);

    if (!type) {
        return;
    }

    if (decl.proto.params.empty()) {
        return;
    }

    auto* param_type = decl.proto.params[0].type->codegen(*this);

    if (!param_type) {
        return;
    }

    if (param_type == type) {
        m_methods[type][decl.name.value] = {
            .type = fn_type, .function = function};

        return;
    }

    if (auto* pointer_type = dyn_cast<types::Pointer>(param_type)) {
        if (pointer_type->type == type) {
            m_methods[type][decl.name.value] = {
                .type = fn_type, .function = function};
        }
    }
}

} // namespace cent::backend
