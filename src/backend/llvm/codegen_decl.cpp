#include <cstddef>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/decl/enum_decl.h"
#include "ast/decl/fn_decl.h"
#include "ast/decl/struct.h"
#include "ast/decl/type_alias.h"
#include "ast/decl/union.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/types/alias.h"
#include "backend/llvm/types/generic.h"
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
                return iterator->second.type;
            }

            return nullptr;
        }

        auto iterator = m_current_scope->scopes[decl.type->value].names.find(
            decl.name.value);

        if (iterator != m_current_scope->scopes[decl.type->value].names.end()) {
            return iterator->second.type;
        }

        return nullptr;
    };

    auto get_llvm_fn = [&]() {
        if (!decl.type) {
            return static_cast<llvm::Function*>(
                m_current_scope->names[decl.name.value].value);
        }

        return static_cast<llvm::Function*>(
            m_current_scope->scopes[decl.type->value]
                .names[decl.name.value]
                .value);
    };

    auto* function_type = get_fn_type();

    if (!function_type) {
        return Value::poisoned();
    }

    auto* function = get_llvm_fn();

    auto* entry = llvm::BasicBlock::Create(
        m_context, "", static_cast<llvm::Function*>(function));

    auto* insert_point = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(entry);

    auto* current_function = m_current_function;
    m_current_function = static_cast<types::Function*>(function_type);

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        const auto& param = decl.proto.params[i];

        auto* value = function->getArg(i);
        auto* variable = create_alloca(value->getType());

        m_builder.CreateStore(value, variable);

        m_current_scope->names[param.name.value] = {
            m_current_function->param_types[i], variable, param.is_mutable};
    }

    auto scope_prefix = m_current_scope_prefix;
    m_current_scope_prefix += decl.name.value + "::__";

    decl.block->codegen(*this);

    m_current_scope_prefix = scope_prefix;

    m_current_function = current_function;

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
    if (m_current_scope->types.contains(decl.name.value) ||
        m_current_scope->generic_structs.contains(decl.name.value) ||
        m_current_scope->generic_unions.contains(decl.name.value)) {
        error(
            decl.name.offset,
            fmt::format("{} is already defined", log::quoted(decl.name.value)));

        return Value::poisoned();
    }

    if (!decl.template_params.empty()) {
        std::vector<types::TemplateParam*> template_params;
        auto current_scope_types = m_current_scope->types;

        for (const auto& param : decl.template_params) {
            m_named_types.push_back(
                std::make_unique<types::TemplateParam>(param.value));

            template_params.push_back(
                static_cast<types::TemplateParam*>(m_named_types.back().get()));

            m_current_scope->types[param.value] = template_params.back();
        }

        std::vector<GenericStruct::Field> fields;
        fields.reserve(decl.fields.size());

        for (const auto& field : decl.fields) {
            auto* type = field.type->codegen(*this);

            if (!type) {
                return Value::poisoned();
            }

            fields.emplace_back(field.name.value, type);
        }

        m_current_scope->types = current_scope_types;

        m_current_scope->generic_structs[decl.name.value] =
            std::make_shared<GenericStruct>(
                m_current_scope_prefix + decl.name.value, std::move(fields),
                std::move(template_params));

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

    m_named_types.push_back(std::make_unique<types::Struct>(
        struct_type, m_current_scope_prefix + decl.name.value,
        std::move(fields)));

    m_current_scope->types[decl.name.value] = m_named_types.back().get();

    return Value::poisoned();
}

Value Codegen::generate(const ast::Union& decl) {
    auto attrs = parse_attrs(decl, {"untagged"});
    bool untagged = attrs.contains("untagged");

    if (m_current_scope->types.contains(decl.name.value) ||
        m_current_scope->generic_structs.contains(decl.name.value) ||
        m_current_scope->generic_unions.contains(decl.name.value)) {
        error(
            decl.name.offset,
            fmt::format("{} is already defined", log::quoted(decl.name.value)));

        return Value::poisoned();
    }

    if (!decl.template_params.empty()) {
        std::vector<types::TemplateParam*> template_params;
        auto current_scope_types = m_current_scope->types;

        for (const auto& param : decl.template_params) {
            m_named_types.push_back(
                std::make_unique<types::TemplateParam>(param.value));

            template_params.push_back(
                static_cast<types::TemplateParam*>(m_named_types.back().get()));

            m_current_scope->types[param.value] = template_params.back();
        }

        std::vector<GenericUnion::Field> fields;
        fields.reserve(decl.fields.size());

        for (const auto& field : decl.fields) {
            auto* type = field.type->codegen(*this);

            if (!type) {
                return Value::poisoned();
            }

            fields.emplace_back(field.name.value, type);
        }

        m_current_scope->types = current_scope_types;

        types::Enum* tag_type = nullptr;

        if (!untagged) {
            auto* underlying = m_primitive_types["i32"].get();

            m_named_types.push_back(std::make_unique<types::Enum>(
                underlying->llvm_type,
                m_current_scope_prefix + decl.name.value + "(tag)",
                underlying));

            auto* type = static_cast<types::Enum*>(m_named_types.back().get());

            for (std::size_t i = 0; i < decl.fields.size(); ++i) {
                m_current_scope->scopes[decl.name.value]
                    .names[decl.fields[i].name.value] = {
                    tag_type, llvm::ConstantInt::get(underlying->llvm_type, i)};
            }

            tag_type = type;
        }

        m_current_scope->generic_unions[decl.name.value] =
            std::make_shared<GenericUnion>(
                m_current_scope_prefix + decl.name.value, std::move(fields),
                std::move(template_params), tag_type);

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

        m_named_types.push_back(std::make_unique<types::Union>(
            struct_type, m_current_scope_prefix + decl.name.value,
            std::move(fields)));

        m_current_scope->types[decl.name.value] = m_named_types.back().get();
    } else {
        auto* underlying = m_primitive_types["i32"].get();

        m_named_types.push_back(std::make_unique<types::Enum>(
            underlying->llvm_type,
            m_current_scope_prefix + decl.name.value + "(tag)", underlying));

        auto* tag_type = static_cast<types::Enum*>(m_named_types.back().get());

        for (std::size_t i = 0; i < decl.fields.size(); ++i) {
            m_current_scope->scopes[decl.name.value]
                .names[decl.fields[i].name.value] = {
                tag_type, llvm::ConstantInt::get(underlying->llvm_type, i)};
        }

        struct_type->setBody({max_type, underlying->llvm_type});

        m_named_types.push_back(std::make_unique<types::Union>(
            struct_type, m_current_scope_prefix + decl.name.value,
            std::move(fields), tag_type));

        m_current_scope->types[decl.name.value] = m_named_types.back().get();
    }

    return Value::poisoned();
}

Value Codegen::generate(const ast::EnumDecl& decl) {
    if (m_current_scope->types.contains(decl.name.value)) {
        error(
            decl.name.offset,
            fmt::format("{} is already defined", log::quoted(decl.name.value)));

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

        m_named_types.push_back(std::make_unique<types::Enum>(
            type->llvm_type, m_current_scope_prefix + decl.name.value, type));

        m_current_scope->types[decl.name.value] = m_named_types.back().get();
    }

    auto* underlying = m_primitive_types["i32"].get();

    m_named_types.push_back(std::make_unique<types::Enum>(
        underlying->llvm_type, m_current_scope_prefix + decl.name.value,
        underlying));

    auto* type = static_cast<types::Enum*>(m_named_types.back().get());

    m_current_scope->types[decl.name.value] = type;

    std::uint64_t number = 0;

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        const auto& field = decl.fields[i];

        if (!field.value) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] =
                Value{
                    type, llvm::ConstantInt::get(
                              type->llvm_type, number++, is_sint(type))};

            continue;
        }

        auto value = field.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (auto val = cast(type->type, value); val.ok()) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] =
                val;

            if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(val.value)) {
                number = is_sint(type) ? constant->getSExtValue()
                                       : constant->getZExtValue();
            } else {
                error(field.value->offset, "not a constant");

                return Value::poisoned();
            }
        } else {
            type_mismatch(field.value->offset, type->type, value.type);

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

    m_named_types.push_back(std::make_unique<types::Alias>(
        type->llvm_type, m_current_scope_prefix + decl.name.value, type,
        distinct));

    m_current_scope->types[decl.name.value] = m_named_types.back().get();

    return Value::poisoned();
}

Value Codegen::generate(const ast::VarDecl& decl) {
    auto& result = m_current_scope->names[decl.name.value];
    result = Value::poisoned();

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

            auto* value_type = value.type;
            value = cast(type, value);

            if (!value.ok()) {
                type_mismatch(decl.value->offset, type, value_type);
                return Value::poisoned();
            }
        }

        if (!llvm::isa<llvm::Constant>(value.value)) {
            error(decl.value->offset, "not a constant");
            return Value::poisoned();
        }

        result = value;
        return Value::poisoned();
    }

    if (decl.mutability == ast::VarDecl::Mut::Immut && !decl.value) {
        error(
            decl.name.offset, fmt::format(
                                  "immutable variable {} must be initialized",
                                  log::quoted(decl.name.value)));

        return Value::poisoned();
    }

    auto global_var_init = [&] {
        error(
            decl.value->offset, fmt::format(
                                    "global variable {} cannot be initialized",
                                    log::quoted(decl.name.value)));
    };

    Value value = Value::poisoned();
    Type* type = nullptr;

    llvm::Type* llvm_type = nullptr;

    if (decl.type) {
        type = decl.type->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }

        llvm_type = type->llvm_type;
    }

    if (decl.value) {
        value = decl.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (!type) {
            if (is<types::Null>(value.type) ||
                is<types::Undefined>(value.type)) {
                error(
                    decl.name.offset, fmt::format(
                                          "cannot infer type for {}",
                                          log::quoted(decl.name.value)));

                return Value::poisoned();
            }

            type = value.type;
            llvm_type = value.type->llvm_type;
        } else {
            if (!m_current_function) {
                global_var_init();
                return Value::poisoned();
            }

            if (is<types::Undefined>(value.type)) {
                result = {
                    type, create_alloca(llvm_type),
                    decl.mutability == ast::VarDecl::Mut::Mut};

                return Value::poisoned();
            }

            m_current_result = create_alloca(llvm_type);

            if (!cast_to_result(type, value)) {
                type_mismatch(decl.value->offset, type, value.type);
            } else {
                result = {
                    type, m_current_result,
                    decl.mutability == ast::VarDecl::Mut::Mut};
            }

            m_current_result = nullptr;

            return Value::poisoned();
        }
    }

    if (value.ok()) {
        if (value.stack_allocated) {
            result = {
                type, value.value, decl.mutability == ast::VarDecl::Mut::Mut};

            return Value::poisoned();
        }

        if (m_current_function) {
            m_current_result = create_alloca(llvm_type);
        } else {
            global_var_init();
            return Value::poisoned();
        }

        if (!is<types::Undefined>(value.type)) {
            m_builder.CreateStore(load_value(value).value, m_current_result);
        }

        result = {
            type, m_current_result, decl.mutability == ast::VarDecl::Mut::Mut};
    } else {
        if (m_current_function) {
            m_current_result = create_alloca(llvm_type);

            m_builder.CreateStore(
                llvm::Constant::getNullValue(llvm_type), m_current_result);

            result = {
                type, m_current_result,
                decl.mutability == ast::VarDecl::Mut::Mut};
        } else {
            auto* global = new llvm::GlobalVariable{
                *m_module, llvm_type, false, llvm::GlobalValue::PrivateLinkage,
                llvm::Constant::getNullValue(llvm_type)};

            global->setAlignment(
                m_module->getDataLayout().getPreferredAlign(global));

            m_current_result = global;

            result = {type, m_current_result, true};
        }
    }

    m_current_result = nullptr;

    return Value::poisoned();
}

void Codegen::generate_fn_proto(const ast::FnDecl& decl) {
    auto attrs = parse_attrs(decl, {"extern"});
    bool is_extern = attrs.contains("extern");

    if (!is_extern && !decl.block) {
        error(
            decl.name.offset,
            fmt::format("{} has no body", log::quoted(decl.name.value)));

        return;
    }

    auto& scope = decl.type ? m_current_scope->scopes[decl.type->value]
                            : *m_current_scope;

    if (scope.names.contains(decl.name.value)) {
        error(
            decl.name.offset,
            fmt::format("{} is already defined", log::quoted(decl.name.value)));

        return;
    }

    if (!decl.template_params.empty()) {
        std::vector<types::TemplateParam*> template_params;
        auto current_scope_types = m_current_scope->types;

        for (const auto& param : decl.template_params) {
            m_named_types.push_back(
                std::make_unique<types::TemplateParam>(param.value));

            template_params.emplace_back(
                static_cast<types::TemplateParam*>(m_named_types.back().get()));

            m_current_scope->types[param.value] = template_params.back();
        }

        Type* return_type = m_void_type.get();

        if (decl.proto.return_type) {
            return_type = decl.proto.return_type->codegen(*this);

            if (!return_type) {
                return;
            }
        }

        std::vector<GenericFunction::Param> params;
        std::vector<const ast::Expression*> default_args;

        params.reserve(decl.proto.params.size());

        for (const auto& parameter : decl.proto.params) {
            auto* type = parameter.type->codegen(*this);

            if (!type) {
                return;
            }

            params.emplace_back(
                parameter.name.value, type, parameter.is_mutable);

            if (!parameter.value) {
                continue;
            }

            default_args.push_back(parameter.value.get());
        }

        m_current_scope->types = current_scope_types;

        m_current_scope->generic_fns[decl.name.value] =
            std::make_shared<GenericFunction>(
                m_current_scope_prefix + decl.name.value, decl.name.offset,
                return_type, std::move(params), std::move(default_args),
                decl.block.get(), std::move(template_params));

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

    scope.names[decl.name.value] = {fn_type, function};

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
        m_methods[type][decl.name.value] = {fn_type, function};

        return;
    }

    if (auto* pointer_type = dyn_cast<types::Pointer>(param_type)) {
        if (pointer_type->type == type) {
            m_methods[type][decl.name.value] = {fn_type, function};
        }
    }
}

} // namespace cent::backend
