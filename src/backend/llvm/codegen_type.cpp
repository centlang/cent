#include <cstddef>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/type/array_type.h"
#include "ast/type/fn_pointer.h"
#include "ast/type/named_type.h"
#include "ast/type/optional.h"
#include "ast/type/pointer.h"
#include "ast/type/slice_type.h"
#include "ast/type/tuple_type.h"

#include "ast/stmt/block_stmt.h"

#include "backend/llvm/types/alias.h"
#include "backend/llvm/types/generic.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

Type* Codegen::generate(const ast::NamedType& type) {
    auto* scope = resolve_scope(type.value);

    if (!scope) {
        return nullptr;
    }

    auto [name, offset] = type.value.back();

    if (!type.template_args.empty()) {
        std::vector<Type*> args;
        args.reserve(type.template_args.size());

        bool has_generic_arg = false;

        for (const auto& arg : type.template_args) {
            auto* type = arg->codegen(*this);

            if (!type) {
                return nullptr;
            }

            if (!type->llvm_type) {
                has_generic_arg = true;
            }

            args.push_back(type);
        }

        if (has_generic_arg) {
            auto generic = scope->generic_structs.find(name);

            if (generic != scope->generic_structs.end()) {
                if (generic->second.element->template_params.size() !=
                    type.template_args.size()) {
                    error(
                        offset,
                        "incorrect number of template arguments passed");

                    return nullptr;
                }

                m_named_types.push_back(
                    std::make_unique<types::TemplateStructInst>(
                        generic->second.element.get(), std::move(args)));

                return m_named_types.back().get();
            }

            auto generic_union = scope->generic_unions.find(name);

            if (generic_union == scope->generic_unions.end()) {
                error(offset, "undeclared type: {}", log::quoted(name));

                return nullptr;
            }

            if (generic_union->second.element->template_params.size() !=
                type.template_args.size()) {
                error(offset, "incorrect number of template arguments passed");
                return nullptr;
            }

            m_named_types.push_back(
                std::make_unique<types::TemplateUnionInst>(
                    generic_union->second.element.get(), std::move(args)));

            return m_named_types.back().get();
        }

        auto generic = scope->generic_structs.find(name);

        if (generic != scope->generic_structs.end()) {
            if (generic->second.element->template_params.size() !=
                type.template_args.size()) {
                error(offset, "incorrect number of template arguments passed");
                return nullptr;
            }

            return inst_generic_struct(generic->second.element.get(), args);
        }

        auto generic_union = scope->generic_unions.find(name);

        if (generic_union == scope->generic_unions.end()) {
            error(offset, "undeclared type: {}", log::quoted(name));

            return nullptr;
        }

        if (generic_union->second.element->template_params.size() !=
            type.template_args.size()) {
            error(offset, "incorrect number of template arguments passed");
            return nullptr;
        }

        return inst_generic_union(generic_union->second.element.get(), args);
    }

    return get_type(offset, name, *scope);
}

Type* Codegen::generate(const ast::Pointer& type) {
    auto* points_to = type.type->codegen(*this);

    if (!points_to) {
        return nullptr;
    }

    return get_ptr_type(points_to, type.is_mutable);
}

Type* Codegen::generate(const ast::Optional& type) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return get_optional_type(contained);
}

Type* Codegen::generate(const ast::ArrayType& type) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    auto size = type.size->codegen(*this);

    if (!size.ok()) {
        return nullptr;
    }

    auto value = cast(m_primitive_types["usize"].get(), size);

    if (!value.ok()) {
        return nullptr;
    }

    if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value.value)) {
        return get_array_type(contained, constant->getZExtValue());
    }

    error(type.offset, "not a constant");

    return nullptr;
}

Type* Codegen::generate(const ast::ArrayType& type, std::size_t size) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return get_array_type(contained, size);
}

Type* Codegen::generate(const ast::SliceType& type) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return get_slice_type(contained, type.is_mutable);
}

Type* Codegen::generate(const ast::TupleType& type) {
    std::vector<Type*> types;
    types.reserve(type.types.size());

    for (const auto& element_type : type.types) {
        auto* el_type = element_type->codegen(*this);

        if (!el_type) {
            return nullptr;
        }

        types.push_back(el_type);
    }

    return get_tuple_type(types);
}

Type* Codegen::generate(const ast::FnPointer& type) {
    return generate_fn_type(type.proto);
}

types::Function* Codegen::generate_fn_type(const ast::FnProto& proto) {
    Type* return_type = m_void_type.get();

    if (proto.return_type) {
        return_type = proto.return_type->codegen(*this);

        if (!return_type) {
            return nullptr;
        }
    }

    std::vector<Type*> param_types;
    std::vector<llvm::Constant*> default_args;

    param_types.reserve(proto.params.size());

    for (const auto& parameter : proto.params) {
        auto* type = parameter.type->codegen(*this);

        if (!type) {
            return nullptr;
        }

        param_types.push_back(type);

        if (!parameter.value) {
            continue;
        }

        auto value = parameter.value->codegen(*this);

        if (!value.ok()) {
            return nullptr;
        }

        auto val = cast_or_error(parameter.value->offset, type, value);

        if (!val.ok()) {
            return nullptr;
        }

        if (!llvm::isa<llvm::Constant>(val.value)) {
            error(parameter.value->offset, "not a constant");
            return nullptr;
        }

        default_args.push_back(static_cast<llvm::Constant*>(val.value));
    }

    return get_fn_type(
        return_type, std::move(param_types), std::move(default_args),
        proto.variadic);
}

types::Function* Codegen::get_fn_type(
    Type* return_type, std::vector<Type*> param_types,
    std::vector<llvm::Constant*> default_args, bool variadic) {
    auto& result = m_fn_types[std::make_tuple(
        return_type, param_types, default_args, variadic)];

    if (result) {
        return result.get();
    }

    std::vector<llvm::Type*> llvm_param_types;
    llvm_param_types.reserve(param_types.size());

    for (const auto& parameter : param_types) {
        llvm_param_types.push_back(parameter->llvm_type);
    }

    auto* llvm_type = llvm::FunctionType::get(
        return_type->llvm_type, llvm_param_types, variadic);

    result = std::make_unique<types::Function>(
        llvm_type, return_type, std::move(param_types), std::move(default_args),
        variadic);

    return result.get();
}

types::Struct* Codegen::inst_generic_struct(
    GenericStruct* type, const std::vector<Type*>& types) {
    auto& result = m_generic_struct_inst[type][types];

    if (result) {
        return result.get();
    }

    std::string name = type->mangled_name + "(<";

    for (std::size_t i = 0; i < types.size(); ++i) {
        name += types[i]->to_string();

        if (i + 1 != types.size()) {
            name += ",";
        }
    }

    name += ">)";

    auto* struct_type = llvm::StructType::create(m_context, name);

    std::vector<llvm::Type*> llvm_fields;
    std::vector<Type*> fields;

    llvm_fields.reserve(type->fields.size());
    fields.reserve(type->fields.size());

    for (std::size_t i = 0; i < type->fields.size(); ++i) {
        const auto& field = type->fields[i];

        auto* field_type =
            inst_template_param(type->template_params, types, field.type);

        if (!field_type) {
            return nullptr;
        }

        llvm_fields.push_back(field_type->llvm_type);
        fields.push_back(field_type);

        m_members[struct_type][field.name] = i;
    }

    struct_type->setBody(llvm_fields);

    result =
        std::make_unique<types::Struct>(struct_type, name, std::move(fields));

    return result.get();
}

types::Union* Codegen::inst_generic_union(
    GenericUnion* type, const std::vector<Type*>& types) {
    auto& result = m_generic_union_inst[type][types];

    if (result) {
        return result.get();
    }

    std::string name = type->mangled_name + "(<";

    for (std::size_t i = 0; i < types.size(); ++i) {
        name += types[i]->to_string();

        if (i + 1 != types.size()) {
            name += ",";
        }
    }

    name += ">)";

    auto* struct_type = llvm::StructType::create(m_context, name);

    std::vector<Type*> fields;
    fields.reserve(type->fields.size());

    for (std::size_t i = 0; i < type->fields.size(); ++i) {
        const auto& field = type->fields[i];

        auto* field_type =
            inst_template_param(type->template_params, types, field.type);

        if (!field_type) {
            return nullptr;
        }

        fields.push_back(field_type);
        m_members[struct_type][field.name] = i;
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
        struct_type->setBody(max_type);

        result = std::make_unique<types::Union>(
            struct_type, name, std::move(fields));

        return result.get();
    }

    auto* tag_type = type->tag_type;

    struct_type->setBody({max_type, tag_type->llvm_type});

    result = std::make_unique<types::Union>(
        struct_type, name, std::move(fields), tag_type);

    return result.get();
}

Value Codegen::inst_generic_fn(
    GenericFunction* function, const std::vector<Type*>& types) {
    auto& generic_fn_inst = m_generic_fns_inst[function];

    if (auto result = generic_fn_inst.find(types);
        result != generic_fn_inst.end()) {
        return result->second;
    }

    std::vector<Type*> param_types;
    param_types.reserve(function->params.size());

    for (const auto& param : function->params) {
        if (auto* type = inst_template_param(
                function->template_params, types, param.type)) {
            param_types.push_back(type);
            continue;
        }

        return Value::poisoned();
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

    std::string name = function->mangled_name + "(<";

    for (std::size_t i = 0; i < types.size(); ++i) {
        name += types[i]->to_string();

        if (i + 1 != types.size()) {
            name += ",";
        }
    }

    name += ">)";

    auto* return_type = inst_template_param(
        function->template_params, types, function->return_type);

    if (!return_type) {
        return Value::poisoned();
    }

    auto* fn_type = get_fn_type(
        return_type, std::move(param_types), std::move(default_args), false);

    auto* llvm_fn_type = static_cast<llvm::FunctionType*>(fn_type->llvm_type);

    auto* llvm_function = llvm::Function::Create(
        llvm_fn_type, llvm::Function::PrivateLinkage, name, *m_module);

    generic_fn_inst[types] = {fn_type, llvm_function};

    auto* entry = llvm::BasicBlock::Create(m_context, "", llvm_function);

    auto* insert_point = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(entry);

    auto* current_function = m_current_function;
    m_current_function = fn_type;

    auto current_scope = *m_current_scope;

    for (std::size_t i = 0; i < function->params.size(); ++i) {
        const auto& param = function->params[i];

        auto* value = llvm_function->getArg(i);
        auto* variable = create_alloca(value->getType());

        m_builder.CreateStore(value, variable);

        m_current_scope->names[param.name] = {
            m_current_function->param_types[i], variable, 1, param.is_mutable};
    }

    for (std::size_t i = 0; i < function->template_params.size(); ++i) {
        m_named_types.push_back(
            std::make_unique<types::Alias>(
                types[i]->llvm_type, function->template_params[i]->name,
                types[i], false));

        m_current_scope->types[function->template_params[i]->name] = {
            m_named_types.back().get()};
    }

    auto scope_prefix = m_current_scope_prefix;
    m_current_scope_prefix = name + "::__";

    function->block->codegen(*this);

    m_current_scope_prefix = scope_prefix;
    *m_current_scope = current_scope;
    m_current_function = current_function;

    if (m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.SetInsertPoint(insert_point);
        return Value{fn_type, llvm_function};
    }

    if (!m_current_fn_had_error &&
        !llvm_function->getReturnType()->isVoidTy()) {
        error(
            function->name_offset, "non-void function does not return a value");

        m_builder.SetInsertPoint(insert_point);

        return Value::poisoned();
    }

    m_builder.CreateRetVoid();
    m_builder.SetInsertPoint(insert_point);

    return Value{fn_type, llvm_function};
}

Value Codegen::inst_generic_fn(
    GenericFunction* function, const std::vector<Value>& args) {
    std::map<types::TemplateParam*, Type*> deduced;

    for (std::size_t i = 0; i < args.size(); ++i) {
        if (!deduce_template_arg(
                function->params[i].type, args[i].type, deduced)) {
            error(
                function->name_offset, "cannot deduce type for {}",
                log::quoted(function->params[i].name));

            return Value::poisoned();
        }
    }

    std::vector<Type*> types;
    types.reserve(function->template_params.size());

    for (auto* param : function->template_params) {
        auto type = deduced.find(param);

        if (type == deduced.end()) {
            error(
                function->name_offset, "cannot deduce type for {}",
                log::quoted(param->name));

            return Value::poisoned();
        }

        types.push_back(type->second);
    }

    return inst_generic_fn(function, types);
}

bool Codegen::deduce_template_arg(
    Type* param, Type* arg, std::map<types::TemplateParam*, Type*>& deduced) {
    if (auto* t_param = dyn_cast<types::TemplateParam>(param)) {
        auto type = deduced.find(t_param);

        if (type == deduced.end()) {
            deduced[t_param] = arg;
            return true;
        }

        return arg == type->second;
    }

    if (param == arg) {
        return true;
    }

    if (param->kind != arg->kind) {
        return false;
    }

    if (auto* struct_param = dyn_cast<types::TemplateStructInst>(param)) {
        auto* struct_arg = static_cast<types::TemplateStructInst*>(arg);

        if (struct_param->type != struct_arg->type ||
            struct_param->args.size() != struct_arg->args.size()) {
            return false;
        }

        for (std::size_t i = 0; i < struct_param->args.size(); ++i) {
            if (!deduce_template_arg(
                    struct_param->args[i], struct_arg->args[i], deduced)) {
                return false;
            }
        }

        return true;
    }

    if (auto* union_param = dyn_cast<types::TemplateUnionInst>(param)) {
        auto* union_arg = static_cast<types::TemplateUnionInst*>(arg);

        if (union_param->type != union_arg->type ||
            union_param->args.size() != union_arg->args.size()) {
            return false;
        }

        for (std::size_t i = 0; i < union_param->args.size(); ++i) {
            if (!deduce_template_arg(
                    union_param->args[i], union_arg->args[i], deduced)) {
                return false;
            }
        }

        return true;
    }

    if (auto* ptr_param = dyn_cast<types::Pointer>(param)) {
        auto* ptr_arg = static_cast<types::Pointer*>(arg);

        return deduce_template_arg(ptr_param->type, ptr_arg->type, deduced);
    }

    if (auto* optional_param = dyn_cast<types::Optional>(param)) {
        auto* optional_arg = static_cast<types::Optional*>(arg);

        return deduce_template_arg(
            optional_param->type, optional_arg->type, deduced);
    }

    if (auto* range_param = dyn_cast<types::Range>(param)) {
        auto* range_arg = static_cast<types::Range*>(arg);

        return deduce_template_arg(range_param->type, range_arg->type, deduced);
    }

    if (auto* array_param = dyn_cast<types::Array>(param)) {
        auto* array_arg = static_cast<types::Array*>(arg);

        if (array_param->size != array_arg->size) {
            return false;
        }

        return deduce_template_arg(array_param->type, array_arg->type, deduced);
    }

    if (auto* slice_param = dyn_cast<types::Slice>(param)) {
        auto* slice_arg = static_cast<types::Slice*>(arg);

        return deduce_template_arg(slice_param->type, slice_arg->type, deduced);
    }

    if (auto* tuple_param = dyn_cast<types::Tuple>(param)) {
        auto* tuple_arg = static_cast<types::Tuple*>(arg);

        if (tuple_param->types.size() != tuple_arg->types.size()) {
            return false;
        }

        for (std::size_t i = 0; i < tuple_param->types.size(); ++i) {
            if (!deduce_template_arg(
                    tuple_param->types[i], tuple_arg->types[i], deduced)) {
                return false;
            }
        }

        return true;
    }

    if (auto* fn_param = dyn_cast<types::Function>(param)) {
        auto* fn_arg = static_cast<types::Function*>(arg);

        if (fn_param->param_types.size() != fn_arg->param_types.size() ||
            fn_param->variadic != fn_arg->variadic) {
            return false;
        }

        if (!deduce_template_arg(
                fn_param->return_type, fn_arg->return_type, deduced)) {
            return false;
        }

        for (std::size_t i = 0; i < fn_param->param_types.size(); ++i) {
            if (!deduce_template_arg(
                    fn_param->param_types[i], fn_arg->param_types[i],
                    deduced)) {
                return false;
            }
        }

        return true;
    }

    return false;
}

Type* Codegen::inst_template_param(
    const std::vector<types::TemplateParam*>& params,
    const std::vector<Type*>& args, Type* type) {
    auto* base_type = unwrap_type(type);

    if (auto* t_param = dyn_cast<types::TemplateParam>(base_type)) {
        for (std::size_t i = 0; i < params.size(); ++i) {
            if (params[i] == t_param) {
                return args[i];
            }
        }

        return nullptr;
    }

    if (auto* t_struct = dyn_cast<types::TemplateStructInst>(base_type)) {
        std::vector<Type*> new_args;
        new_args.reserve(t_struct->args.size());

        for (auto* arg : t_struct->args) {
            if (auto* new_arg = inst_template_param(params, args, arg)) {
                new_args.push_back(new_arg);
                continue;
            }

            return nullptr;
        }

        return inst_generic_struct(t_struct->type, new_args);
    }

    if (auto* t_union = dyn_cast<types::TemplateUnionInst>(base_type)) {
        std::vector<Type*> new_args;
        new_args.reserve(t_union->args.size());

        for (auto* arg : t_union->args) {
            if (auto* new_arg = inst_template_param(params, args, arg)) {
                new_args.push_back(new_arg);
                continue;
            }

            return nullptr;
        }

        return inst_generic_union(t_union->type, new_args);
    }

    if (auto* ptr = dyn_cast<types::Pointer>(base_type)) {
        auto* type = inst_template_param(params, args, ptr->type);

        if (!type) {
            return nullptr;
        }

        return get_ptr_type(type, ptr->is_mutable);
    }

    if (auto* optional = dyn_cast<types::Optional>(base_type)) {
        auto* type = inst_template_param(params, args, optional->type);

        if (!type) {
            return nullptr;
        }

        return get_optional_type(type);
    }

    if (auto* range = dyn_cast<types::Range>(base_type)) {
        auto* type = inst_template_param(params, args, range->type);

        if (!type) {
            return nullptr;
        }

        return get_range_type(type, range->inclusive);
    }

    if (auto* array = dyn_cast<types::Array>(base_type)) {
        auto* type = inst_template_param(params, args, array->type);

        if (!type) {
            return nullptr;
        }

        return get_array_type(type, array->size);
    }

    if (auto* slice = dyn_cast<types::Slice>(base_type)) {
        auto* type = inst_template_param(params, args, slice->type);

        if (!type) {
            return nullptr;
        }

        return get_ptr_type(type, slice->is_mutable);
    }

    if (auto* tuple = dyn_cast<types::Tuple>(base_type)) {
        std::vector<Type*> types;
        types.reserve(tuple->types.size());

        for (auto& element : tuple->types) {
            if (auto* type = inst_template_param(params, args, element)) {
                types.push_back(type);
                continue;
            }

            return nullptr;
        }

        return get_tuple_type(types);
    }

    if (auto* func = dyn_cast<types::Function>(base_type)) {
        Type* return_type =
            inst_template_param(params, args, func->return_type);

        if (!return_type) {
            return nullptr;
        }

        std::vector<Type*> param_types;
        param_types.reserve(func->param_types.size());

        for (auto& param_type : func->param_types) {
            if (auto* type = inst_template_param(params, args, param_type)) {
                param_types.push_back(type);
                continue;
            }

            return nullptr;
        }

        return get_fn_type(
            return_type, param_types, func->default_args, func->variadic);
    }

    return type;
}

types::Pointer* Codegen::get_ptr_type(Type* type, bool is_mutable) {
    auto& result = m_ptr_types[std::make_pair(type, is_mutable)];

    if (!result) {
        result = std::make_unique<types::Pointer>(
            type->llvm_type ? llvm::PointerType::get(m_context, 0) : nullptr,
            type, is_mutable);
    }

    return result.get();
}

types::Slice* Codegen::get_slice_type(Type* type, bool is_mutable) {
    auto& result = m_slice_types[std::make_pair(type, is_mutable)];

    if (!result) {
        result = std::make_unique<types::Slice>(
            type->llvm_type ? m_slice_type : nullptr, type, is_mutable);
    }

    return result.get();
}

types::Array* Codegen::get_array_type(Type* type, std::size_t size) {
    auto& result = m_array_types[std::make_pair(type, size)];

    if (!result) {
        result = std::make_unique<types::Array>(
            type->llvm_type ? llvm::ArrayType::get(type->llvm_type, size)
                            : nullptr,
            type, size);
    }

    return result.get();
}

types::Tuple* Codegen::get_tuple_type(const std::vector<Type*>& types) {
    auto& result = m_tuple_types[types];

    if (!result) {
        std::vector<llvm::Type*> llvm_types;
        llvm_types.reserve(types.size());

        bool has_nullptr = false;

        for (auto* element_type : types) {
            if (!element_type->llvm_type) {
                has_nullptr = true;
                break;
            }

            llvm_types.push_back(element_type->llvm_type);
        }

        result = std::make_unique<types::Tuple>(
            has_nullptr ? nullptr : llvm::StructType::create(llvm_types),
            types);
    }

    return result.get();
}

types::Optional* Codegen::get_optional_type(Type* type) {
    auto& result = m_optional_types[type];

    if (!result) {
        if (is<types::Pointer>(type)) {
            result = std::make_unique<types::Optional>(type->llvm_type, type);
        } else {
            result = std::make_unique<types::Optional>(
                type->llvm_type
                    ? llvm::StructType::create(
                          {type->llvm_type, llvm::Type::getInt1Ty(m_context)})
                    : nullptr,
                type);
        }
    }

    return result.get();
}

types::Range* Codegen::get_range_type(Type* type, bool inclusive) {
    auto& result = m_range_types[std::make_pair(type, inclusive)];

    if (!result) {
        result = std::make_unique<types::Range>(
            type->llvm_type
                ? llvm::StructType::create({type->llvm_type, type->llvm_type})
                : nullptr,
            inclusive, type);
    }

    return result.get();
}

Type* Codegen::unwrap_type(Type* type) {
    if (auto* alias = dyn_cast<types::Alias>(type)) {
        if (alias->distinct) {
            return type;
        }

        return unwrap_type(alias->type);
    }

    if (auto* ptr = dyn_cast<types::Pointer>(type)) {
        return get_ptr_type(unwrap_type(ptr->type), ptr->is_mutable);
    }

    if (auto* optional = dyn_cast<types::Optional>(type)) {
        return get_optional_type(unwrap_type(optional->type));
    }

    if (auto* range = dyn_cast<types::Range>(type)) {
        return get_range_type(unwrap_type(range->type), range->inclusive);
    }

    if (auto* array = dyn_cast<types::Array>(type)) {
        return get_array_type(unwrap_type(array->type), array->size);
    }

    if (auto* slice = dyn_cast<types::Slice>(type)) {
        return get_slice_type(unwrap_type(slice->type), slice->is_mutable);
    }

    if (auto* tuple = dyn_cast<types::Tuple>(type)) {
        std::vector<Type*> types;
        types.reserve(tuple->types.size());

        for (auto& element : tuple->types) {
            types.push_back(unwrap_type(element));
        }

        return get_tuple_type(types);
    }

    if (auto* func = dyn_cast<types::Function>(type)) {
        Type* return_type = unwrap_type(func->return_type);

        std::vector<Type*> param_types;
        param_types.reserve(func->param_types.size());

        for (auto& param_type : func->param_types) {
            param_types.push_back(unwrap_type(param_type));
        }

        return get_fn_type(
            return_type, param_types, func->default_args, func->variadic);
    }

    return type;
}

} // namespace cent::backend
