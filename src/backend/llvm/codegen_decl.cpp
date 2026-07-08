#include <algorithm>
#include <cstddef>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/decl/enum_decl.h"
#include "ast/decl/fn_decl.h"
#include "ast/decl/for_block.h"
#include "ast/decl/struct.h"
#include "ast/decl/type_alias.h"
#include "ast/decl/union.h"
#include "ast/decl/var_decl.h"

#include "backend/llvm/types/alias.h"
#include "backend/llvm/types/generic.h"
#include "backend/llvm/types/struct.h"
#include "backend/llvm/types/type_param.h"
#include "backend/llvm/types/union.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

void Codegen::generate_fn_body(
    const ast::FnDecl& decl, Type* fn_type, llvm::Value* fn_value) {
    auto* function_type = static_cast<types::Function*>(fn_type);
    auto* function = static_cast<llvm::Function*>(fn_value);

    auto* entry = llvm::BasicBlock::Create(m_context, "", function);

    auto* insert_point = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(entry);

    auto current_scope_names = m_current_scope->names;

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        const auto& param = decl.proto.params[i];
        const auto& type = function_type->param_types[i];

        auto* variable = alloca_arg(function_type->sret ? i + 1 : i, type);

        m_current_scope->names[param.name.value] = {
            .element =
                {.type = type,
                 .value = variable,
                 .ptr_depth = 1,
                 .is_mutable = param.is_mutable},
            .unit = m_current_unit};
    }

    auto scope_prefix = m_current_scope_prefix;
    m_current_scope_prefix += decl.name.value + "::__";

    auto* current_function = m_current_function;
    m_current_function = function_type;

    decl.block->codegen(*this);

    m_current_scope_prefix = scope_prefix;
    m_current_function = current_function;
    m_current_scope->names = current_scope_names;

    if (m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.SetInsertPoint(insert_point);
        return;
    }

    if (!m_current_fn_had_error &&
        !is<types::Void, types::Never>(function_type->return_type)) {
        error(decl.name.offset, "non-void function does not return a value");
        m_builder.SetInsertPoint(insert_point);
        return;
    }

    if (is<types::Never>(function_type->return_type)) {
        m_builder.CreateUnreachable();
    } else {
        m_builder.CreateRetVoid();
    }

    m_builder.SetInsertPoint(insert_point);
}

Value Codegen::generate(const ast::FnDecl& decl) {
    auto iterator = m_current_scope->names.find(decl.name.value);

    if (iterator == m_current_scope->names.end()) {
        return Value::poisoned();
    }

    generate_fn_body(
        decl, iterator->second.element.type, iterator->second.element.value);

    return Value::poisoned();
}

Value Codegen::generate(const ast::Struct& decl) {
    auto [is_extern, packed] = parse_attrs_validate(decl, "extern", "packed");

    if (m_current_scope->types.contains(decl.name.value) ||
        m_current_scope->generic_structs.contains(decl.name.value) ||
        m_current_scope->generic_unions.contains(decl.name.value)) {
        error(
            decl.name.offset, "{} is already defined",
            log::quoted(decl.name.value));

        return Value::poisoned();
    }

    if (!decl.type_params.empty()) {
        auto& generic_struct =
            m_current_scope->generic_structs[decl.name.value];

        auto current_scope_types = m_current_scope->types;

        for (const auto& param : decl.type_params) {
            m_named_types.push_back(
                std::make_unique<types::TypeParam>(param.value));

            generic_struct.type_params.push_back(
                static_cast<types::TypeParam*>(m_named_types.back().get()));

            m_current_scope->types[param.value] = {
                .element = generic_struct.type_params.back(),
                .is_public = true};
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

        generic_struct.name = decl.name.value;
        generic_struct.fields = std::move(fields);

        return Value::poisoned();
    }

    auto* llvm_struct_type = llvm::StructType::create(
        m_context, m_current_scope_prefix + decl.name.value);

    m_named_types.push_back(
        std::make_unique<types::Struct>(
            llvm_struct_type, m_current_scope_prefix + decl.name.value,
            std::vector<Type*>{}, false));

    auto* struct_type = static_cast<types::Struct*>(m_named_types.back().get());

    m_current_scope->types[decl.name.value] = {
        .element = struct_type,
        .is_public = decl.is_public,
        .unit = m_current_unit};

    struct Field {
        Type* type;
        std::string_view name;
        std::size_t size;
    };

    std::vector<Field> fields;
    fields.reserve(decl.fields.size());

    for (const auto& field : decl.fields) {
        auto* type = field.type->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }

        auto size = m_module->getDataLayout().getTypeAllocSize(type->llvm_type);
        fields.emplace_back(type, field.name.value, size);
    }

    if (!is_extern && !packed) {
        std::ranges::sort(fields, [](const auto& left, const auto& right) {
            return left.size > right.size;
        });
    }

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

    if (!is_extern && !packed && max_element > 0) {
        auto rem = total_size % max_element;

        if (rem != 0) {
            llvm_fields.push_back(
                llvm::ArrayType::get(
                    llvm::Type::getInt8Ty(m_context), max_element - rem));
        }
    }

    llvm_struct_type->setBody(llvm_fields, packed.has_value());

    struct_type->fields = type_fields;
    struct_type->has_tail = llvm_fields.size() != type_fields.size();

    return Value::poisoned();
}

Value Codegen::generate(const ast::Union& decl) {
    auto [untagged] = parse_attrs_validate(decl, "untagged");

    if (m_current_scope->types.contains(decl.name.value) ||
        m_current_scope->generic_structs.contains(decl.name.value) ||
        m_current_scope->generic_unions.contains(decl.name.value)) {
        error(
            decl.name.offset, "{} is already defined",
            log::quoted(decl.name.value));

        return Value::poisoned();
    }

    if (!decl.type_params.empty()) {
        auto& generic_union = m_current_scope->generic_unions[decl.name.value];

        auto current_scope_types = m_current_scope->types;

        for (const auto& param : decl.type_params) {
            m_named_types.push_back(
                std::make_unique<types::TypeParam>(param.value));

            generic_union.type_params.push_back(
                static_cast<types::TypeParam*>(m_named_types.back().get()));

            m_current_scope->types[param.value] = {
                .element = generic_union.type_params.back(), .is_public = true};
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

        generic_union.name = decl.name.value;
        generic_union.fields = std::move(fields);

        if (untagged) {
            generic_union.tag_type = nullptr;
        } else {
            auto* underlying = m_primitive_types["i32"].get();

            m_named_types.push_back(
                std::make_unique<types::Enum>(
                    underlying->llvm_type,
                    m_current_scope_prefix + decl.name.value + "(tag)",
                    underlying));

            auto* tag_type =
                static_cast<types::Enum*>(m_named_types.back().get());

            for (std::size_t i = 0; i < decl.fields.size(); ++i) {
                m_current_scope->scopes[decl.name.value]
                    .names[decl.fields[i].name.value] = {
                    .element =
                        {.type = tag_type,
                         .value =
                             llvm::ConstantInt::get(underlying->llvm_type, i)},
                    .is_public = decl.is_public,
                    .unit = m_current_unit};
            }

            generic_union.tag_type = tag_type;
        }

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

    auto* underlying = m_primitive_types["i32"].get();

    if (decl.type) {
        auto* type = decl.type->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }

        auto* base_type = unwrap_type(type);

        if (!base_type->is_sint() && !base_type->is_uint() &&
            !is<types::Bool>(base_type)) {
            error(decl.type->offset, "type mismatch");
            return Value::poisoned();
        }

        underlying = type;
    }

    m_named_types.push_back(
        std::make_unique<types::Enum>(
            underlying->llvm_type, m_current_scope_prefix + decl.name.value,
            underlying));

    m_current_scope->types[decl.name.value] = {
        .element = m_named_types.back().get(),
        .is_public = decl.is_public,
        .unit = m_current_unit};

    auto* type = static_cast<types::Enum*>(m_named_types.back().get());

    std::uint64_t number = 0;

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        const auto& field = decl.fields[i];

        if (!field.value) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] = {
                .element =
                    {.type = type,
                     .value = llvm::ConstantInt::get(
                         underlying->llvm_type, number++,
                         underlying->is_sint())},
                .is_public = decl.is_public,
                .unit = m_current_unit};

            continue;
        }

        auto value = field.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (auto val = cast_or_error(field.value->offset, underlying, value);
            val.ok()) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] = {
                .element = {.type = type, .value = val.value},
                .is_public = decl.is_public,
                .unit = m_current_unit};

            if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(val.value)) {
                number = type->is_sint() ? constant->getSExtValue()
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
    auto [distinct] = parse_attrs_validate(decl, "distinct");
    auto* type = decl.type->codegen(*this);

    if (!type) {
        return Value::poisoned();
    }

    m_named_types.push_back(
        std::make_unique<types::Alias>(
            type->llvm_type, m_current_scope_prefix + decl.name.value, type,
            distinct.has_value()));

    m_current_scope->types[decl.name.value] = {
        .element = m_named_types.back().get(),
        .is_public = decl.is_public,
        .unit = m_current_unit};

    return Value::poisoned();
}

Value Codegen::generate(const ast::VarDecl& decl) {
    auto [is_extern] = parse_attrs_validate(decl, "extern");

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

        if (!llvm::isa_and_nonnull<llvm::Constant>(value.value)) {
            error(decl.value->offset, "not a constant");
            return Value::poisoned();
        }

        m_current_scope->names[decl.name.value] = {
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

        if (!is_extern && !constant) {
            constant = llvm::Constant::getNullValue(type->llvm_type);
        }

        llvm::GlobalVariable* global = nullptr;

        if (is_extern) {
            global = m_module->getGlobalVariable(decl.name.value);
        }

        if (!global) {
            global = new llvm::GlobalVariable{
                *m_module,
                type->llvm_type,
                false,
                (decl.is_public || is_extern)
                    ? llvm::GlobalValue::ExternalLinkage
                    : llvm::GlobalValue::PrivateLinkage,
                constant,
                is_extern ? decl.name.value
                          : m_current_scope_prefix + decl.name.value};

            global->setAlignment(
                m_module->getDataLayout().getPreferredAlign(global));
        }

        m_current_scope->names[decl.name.value] = {
            .element =
                {.type = type,
                 .value = global,
                 .ptr_depth = 1,
                 .is_mutable = true},
            .is_public = decl.is_public,
            .unit = m_current_unit};

        return Value::poisoned();
    }

    auto* variable = create_alloca(type);

    m_current_scope->names[decl.name.value] = {
        .element =
            {.type = type,
             .value = variable,
             .ptr_depth = 1,
             .is_mutable = decl.mutability == ast::VarDecl::Mut::Mut},
        .unit = m_current_unit};

    if (value.ok()) {
        if (is<types::Undefined>(value.type)) {
            return Value::poisoned();
        }

        auto val = cast_or_error(decl.value->offset, type, value);

        if (!val.ok()) {
            return Value::poisoned();
        }

        create_store(val, variable);
        return Value::poisoned();
    }

    if (is<types::Pointer>(type)) {
        error(decl.type->offset, "cannot zero-initialize a pointer");
        return Value::poisoned();
    }

    zero_init(variable, type);
    return Value::poisoned();
}

void Codegen::generate_method_proto(
    const ast::FnDecl& method,
    std::map<std::string_view, GenericFunction*>& methods, Type* self_type,
    const std::vector<types::TypeParam*>& parent_params,
    const ast::ForBlock& for_block) {
    if (!matches_target(method)) {
        return;
    }

    auto& type_scope = m_current_scope->scopes[for_block.type.value];

    if (type_scope.names.contains(method.name.value) ||
        type_scope.generic_fns.contains(method.name.value)) {
        error(
            method.name.offset, "{} is already defined",
            log::quoted(method.name.value));

        return;
    }

    auto scope_types = m_current_scope->types;
    std::vector<types::TypeParam*> type_args;

    for (const auto& param : method.type_params) {
        m_named_types.push_back(
            std::make_unique<types::TypeParam>(param.value));

        type_args.push_back(
            static_cast<types::TypeParam*>(m_named_types.back().get()));

        m_current_scope->types[param.value] = {
            .element = type_args.back(), .is_public = true};
    }

    Type* return_type = m_void_type.get();

    if (method.proto.return_type) {
        return_type = method.proto.return_type->codegen(*this);

        if (!return_type) {
            m_current_scope->types = scope_types;
            return;
        }
    }

    std::vector<GenericFunction::Param> params;

    for (const auto& parameter : method.proto.params) {
        auto* type = parameter.type->codegen(*this);

        if (!type) {
            m_current_scope->types = scope_types;
            return;
        }

        params.emplace_back(parameter.name.value, type, parameter.is_mutable);
    }

    m_current_scope->types = scope_types;

    if (!for_block.type_params.empty() || !method.type_params.empty()) {
        auto& gen = type_scope.generic_fns[method.name.value];

        gen = GenericFunction{
            .name = method.name,
            .return_type = return_type,
            .params = std::move(params),
            .block = method.block.get(),
            .type_params = std::move(type_args),
            .parent_type_params = parent_params,
            .self_type = self_type,
            .source_file = m_filename};

        methods[method.name.value] = &gen;
        return;
    }

    auto* fn_type = generate_fn_type(method.proto);

    if (!fn_type) {
        return;
    }

    auto llvm_name = m_current_scope_prefix + for_block.type.value +
                     "::" + method.name.value;

    auto* function = llvm::Function::Create(
        static_cast<llvm::FunctionType*>(fn_type->llvm_type),
        method.is_public ? llvm::Function::ExternalLinkage
                         : llvm::Function::PrivateLinkage,
        llvm_name, *m_module);

    if (is<types::Never>(fn_type->return_type)) {
        function->addFnAttr(llvm::Attribute::NoReturn);
    }

    if (fn_type->sret) {
        function->addParamAttr(
            0, llvm::Attribute::getWithStructRetType(
                   m_context, fn_type->return_type->llvm_type));
    }

    type_scope.names[method.name.value] = {
        .element = {.type = fn_type, .value = function},
        .is_public = method.is_public,
        .unit = m_current_unit};

    if (self_type && !method.proto.params.empty()) {
        auto* param_type = method.proto.params[0].type->codegen(*this);
        if (param_type &&
            (param_type == self_type ||
             (dyn_cast<types::Pointer>(param_type) &&
              static_cast<types::Pointer*>(param_type)->type == self_type))) {
            self_type->methods[method.name.value] = {
                .type = fn_type, .function = function};
        }
    }
}

void Codegen::generate_for_block_protos(const ast::ForBlock& decl) {
    if (decl.is_public) {
        error(decl.offset, "for-block cannot be public");
    }

    auto num_of_args = [&] {
        error(
            decl.type.offset, "incorrect number of type parameters for {}",
            log::quoted(decl.type.value));
    };

    auto scope_types = m_current_scope->types;
    std::vector<types::TypeParam*> type_params;

    for (const auto& param : decl.type_params) {
        m_named_types.push_back(
            std::make_unique<types::TypeParam>(param.value));

        type_params.push_back(
            static_cast<types::TypeParam*>(m_named_types.back().get()));

        m_current_scope->types[param.value] = {
            .element = type_params.back(), .is_public = true};
    }

    auto struct_it = m_current_scope->generic_structs.find(decl.type.value);

    if (struct_it != m_current_scope->generic_structs.end()) {
        auto& gen = struct_it->second;

        if (decl.type_params.size() != gen.type_params.size()) {
            num_of_args();
            return;
        }

        m_named_types.push_back(
            std::make_unique<types::GenericStructInst>(
                &gen,
                std::vector<Type*>(type_params.begin(), type_params.end())));

        m_current_scope->types["Self"] = {
            .element = m_named_types.back().get(), .is_public = true};

        for (const auto& method : decl.methods) {
            generate_method_proto(
                *method, gen.methods, m_named_types.back().get(), type_params,
                decl);
        }

        m_current_scope->types = scope_types;
        return;
    }

    auto union_it = m_current_scope->generic_unions.find(decl.type.value);

    if (union_it != m_current_scope->generic_unions.end()) {
        auto& gen = union_it->second;

        if (decl.type_params.size() != gen.type_params.size()) {
            num_of_args();
            return;
        }

        m_named_types.push_back(
            std::make_unique<types::GenericUnionInst>(
                &gen,
                std::vector<Type*>(type_params.begin(), type_params.end())));

        m_current_scope->types["Self"] = {
            .element = m_named_types.back().get(), .is_public = true};

        for (const auto& method : decl.methods) {
            generate_method_proto(
                *method, gen.methods, m_named_types.back().get(), type_params,
                decl);
        }

        m_current_scope->types = scope_types;
        return;
    }

    auto* type = get_type(decl.type.offset, decl.type.value, *m_current_scope);

    if (!type) {
        return;
    }

    if (!decl.type_params.empty()) {
        error(
            decl.type.offset, "{} is not a generic type",
            log::quoted(decl.type.value));

        return;
    }

    m_current_scope->types["Self"] = {.element = type, .is_public = true};

    for (const auto& method : decl.methods) {
        generate_method_proto(
            *method, type->generic_methods, type,
            std::vector<types::TypeParam*>{}, decl);
    }

    m_current_scope->types = scope_types;
}

Value Codegen::generate(const ast::ForBlock& decl) {
    auto& type_scope = m_current_scope->scopes[decl.type.value];

    if (!m_current_scope->types.contains(decl.type.value)) {
        return Value::poisoned();
    }

    auto scope_types = m_current_scope->types;

    m_current_scope->types["Self"] = {
        .element = m_current_scope->types[decl.type.value].element,
        .is_public = true};

    for (const auto& method : decl.methods) {
        if (!method->block || !method->type_params.empty() ||
            !decl.type_params.empty()) {
            continue;
        }

        auto method_it = type_scope.names.find(method->name.value);

        if (method_it != type_scope.names.end()) {
            generate_fn_body(
                *method, method_it->second.element.type,
                method_it->second.element.value);
        }
    }

    m_current_scope->types = scope_types;

    return Value::poisoned();
}

void Codegen::generate_fn_proto(const ast::FnDecl& decl) {
    auto [is_extern, alwaysinline, symbol] =
        parse_attrs_validate(decl, "extern", "alwaysinline", "symbol");

    if (!is_extern && !decl.block) {
        error(decl.name.offset, "{} has no body", log::quoted(decl.name.value));
        return;
    }

    if (is_extern && alwaysinline) {
        error(
            decl.name.offset, "extern function {} cannot be `alwaysinline`",
            log::quoted(decl.name.value));

        return;
    }

    if (symbol && !symbol->value) {
        error(symbol->offset, "`symbol` requires a string value");
        return;
    }

    if (m_current_scope->names.contains(decl.name.value) ||
        m_current_scope->generic_fns.contains(decl.name.value)) {
        error(
            decl.name.offset, "{} is already defined",
            log::quoted(decl.name.value));

        return;
    }

    if (!decl.type_params.empty()) {
        auto& generic_fn = m_current_scope->generic_fns[decl.name.value];

        auto current_scope_types = m_current_scope->types;

        for (const auto& param : decl.type_params) {
            m_named_types.push_back(
                std::make_unique<types::TypeParam>(param.value));

            generic_fn.type_params.emplace_back(
                static_cast<types::TypeParam*>(m_named_types.back().get()));

            m_current_scope->types[param.value] = {
                .element = generic_fn.type_params.back(), .is_public = true};
        }

        Type* return_type = m_void_type.get();

        if (decl.proto.return_type) {
            return_type = decl.proto.return_type->codegen(*this);

            if (!return_type) {
                return;
            }
        }

        generic_fn.params.reserve(decl.proto.params.size());

        for (const auto& parameter : decl.proto.params) {
            auto* type = parameter.type->codegen(*this);

            if (!type) {
                return;
            }

            generic_fn.params.emplace_back(
                parameter.name.value, type, parameter.is_mutable);

            if (!parameter.value) {
                continue;
            }

            generic_fn.default_args.push_back(parameter.value.get());
        }

        m_current_scope->types = current_scope_types;

        generic_fn.name = decl.name;
        generic_fn.return_type = return_type;
        generic_fn.block = decl.block.get();
        generic_fn.source_file = m_filename;

        return;
    }

    auto* fn_type = generate_fn_type(decl.proto);

    if (!fn_type) {
        return;
    }

    auto* llvm_fn_type = static_cast<llvm::FunctionType*>(fn_type->llvm_type);

    std::string llvm_name;

    if (symbol) {
        llvm_name = *symbol->value;
    } else if (is_extern) {
        llvm_name = decl.name.value;
    } else {
        llvm_name = m_current_scope_prefix + decl.name.value;
    }

    llvm::Function* function = nullptr;

    if (is_extern) {
        function = m_module->getFunction(llvm_name);
    }

    if (!function) {
        function = llvm::Function::Create(
            llvm_fn_type,
            (decl.is_public || is_extern) ? llvm::Function::ExternalLinkage
                                          : llvm::Function::PrivateLinkage,
            llvm_name, *m_module);
    }

    if (alwaysinline) {
        function->addFnAttr(llvm::Attribute::AlwaysInline);
    }

    if (is<types::Never>(fn_type->return_type)) {
        function->addFnAttr(llvm::Attribute::NoReturn);
    }

    if (fn_type->sret) {
        function->addParamAttr(
            0, llvm::Attribute::getWithStructRetType(
                   m_context, fn_type->return_type->llvm_type));
    }

    m_current_scope->names[decl.name.value] = {
        .element = {.type = fn_type, .value = function},
        .is_public = decl.is_public,
        .unit = m_current_unit};
}

} // namespace cent::backend
