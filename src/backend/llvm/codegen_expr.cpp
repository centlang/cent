#include <charconv>
#include <cstddef>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/expr/as_expr.h"
#include "ast/expr/binary_expr.h"
#include "ast/expr/call_expr.h"
#include "ast/expr/identifier.h"
#include "ast/expr/index_expr.h"
#include "ast/expr/literals.h"
#include "ast/expr/member_expr.h"
#include "ast/expr/method_expr.h"
#include "ast/expr/sizeof_expr.h"
#include "ast/expr/slice_expr.h"
#include "ast/expr/unary_expr.h"

#include "ast/type/named_type.h"

#include "backend/llvm/types/struct.h"
#include "backend/llvm/types/union.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

Value Codegen::generate(const ast::BinaryExpr& expr) {
    return generate_bin_logical_expr(*expr.lhs, *expr.rhs, expr.oper);
}

Value Codegen::generate(const ast::UnaryExpr& expr) {
    using enum frontend::Token::Type;

    auto value = expr.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    auto* base_type = unwrap_type(value.type);

    switch (expr.oper.value) {
    case Minus:
        if (is_float(base_type)) {
            return Value{
                value.type, m_builder.CreateFNeg(load_value(value).value)};
        }

        if (!is_sint(base_type) && !is_uint(base_type)) {
            error(expr.offset, "cannot apply `-` to a non-number type");

            return Value::poisoned();
        }

        return Value{value.type, m_builder.CreateNeg(load_value(value).value)};
    case Bang:
        if (!is<types::Bool>(base_type)) {
            error(expr.offset, "cannot apply `!` to a non-boolean type");

            return Value::poisoned();
        }

        return Value{value.type, m_builder.CreateNot(load_value(value).value)};
    case Star: {
        auto* pointer = dyn_cast<types::Pointer>(base_type);

        if (!pointer) {
            error(expr.offset, "dereference of a non-pointer type");

            return Value::poisoned();
        }

        return Value{
            pointer->type, value.value, pointer->is_mutable, false, true};
    }
    case And:
        if (llvm::isa<llvm::Function>(value.value)) {
            return Value{get_ptr_type(value.type, false), value.value};
        }

        if (!llvm::isa<llvm::AllocaInst>(value.value) &&
            !llvm::isa<llvm::GetElementPtrInst>(value.value)) {
            error(expr.offset, "taking the reference of a non-variable value");

            return Value::poisoned();
        }

        return Value{
            get_ptr_type(value.type, value.is_mutable), value.value, false,
            true};
    case Not:
        if (!is_sint(base_type) && !is_uint(base_type)) {
            error(expr.offset, "cannot apply `~` to a non-integer type");

            return Value::poisoned();
        }

        return Value{value.type, m_builder.CreateNot(load_value(value).value)};
    default:
        return Value::poisoned();
    }
}

Value Codegen::generate(const ast::IntLiteral& expr) {
    std::string_view literal = expr.value;

    std::uint8_t base = [&] {
        enum { Bin = 2, Dec = 10, Oct = 8, Hex = 16 };

        if (literal.starts_with("0x")) {
            literal = literal.substr(2);

            return Hex;
        }

        if (literal.starts_with("0b")) {
            literal = literal.substr(2);

            return Bin;
        }

        if (literal.starts_with("0o")) {
            literal = literal.substr(2);

            return Oct;
        }

        return Dec;
    }();

    std::int32_t value{};

    auto [pointer, result] =
        std::from_chars(literal.cbegin(), literal.cend(), value, base);

    if (result == std::errc::result_out_of_range) {
        error(expr.offset, "integer out of range");
        return Value::poisoned();
    }

    return Value{
        m_primitive_types["i32"].get(),
        llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(m_context), value)};
}

Value Codegen::generate(const ast::FloatLiteral& expr) {
    float value{};

    auto [pointer, result] = std::from_chars(
        expr.value.data(), expr.value.data() + expr.value.size(), value);

    if (result == std::errc::result_out_of_range) {
        error(expr.offset, "float out of range");
        return Value::poisoned();
    }

    return Value{
        m_primitive_types["f32"].get(),
        llvm::ConstantFP::get(llvm::Type::getFloatTy(m_context), value)};
}

Value Codegen::generate(const ast::StrLiteral& expr) {
    return Value{
        get_array_type(m_primitive_types["u8"].get(), expr.value.size() + 1),
        m_builder.CreateGlobalString(expr.value)};
}

Value Codegen::generate(const ast::BoolLiteral& expr) {
    return Value{
        m_primitive_types["bool"].get(),
        llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), expr.value)};
}

Value Codegen::generate([[maybe_unused]] const ast::NullLiteral& expr) {
    return Value{m_null_type.get(), nullptr};
}

Value Codegen::generate([[maybe_unused]] const ast::Undefined& expr) {
    return Value{m_undefined_type.get(), nullptr};
}

Value Codegen::generate(const ast::RangeLiteral& expr) {
    auto begin = expr.begin->codegen(*this);
    auto end = expr.end->codegen(*this);

    if (!begin.ok() || !end.ok()) {
        return Value::poisoned();
    }

    auto begin_value = load_value(begin);
    auto end_value = load_value(end);

    auto value_x = begin_value;
    auto value_y = cast(begin_value.type, end_value);

    if (!value_y.ok()) {
        value_x = cast_or_error(expr.end->offset, end_value.type, begin_value);

        if (!value_x.ok()) {
            return Value::poisoned();
        }

        value_y = end_value;
    }

    auto* type = get_range_type(value_x.type);

    if (auto* begin_constant = llvm::dyn_cast<llvm::Constant>(value_x.value)) {
        if (auto* end_constant =
                llvm::dyn_cast<llvm::Constant>(value_y.value)) {
            return Value{
                type, llvm::ConstantStruct::get(
                          static_cast<llvm::StructType*>(type->llvm_type),
                          {begin_constant, end_constant})};
        }
    }

    bool stack_allocated = m_current_result == nullptr;

    auto* variable =
        m_current_result ? m_current_result : create_alloca(type->llvm_type);

    auto* begin_ptr = m_builder.CreateStructGEP(
        type->llvm_type, variable, range_member_begin);

    auto* end_ptr =
        m_builder.CreateStructGEP(type->llvm_type, variable, range_member_end);

    m_builder.CreateStore(value_x.value, begin_ptr);
    m_builder.CreateStore(value_y.value, end_ptr);

    return Value{type, variable, false, false, false, stack_allocated};
}

Value Codegen::generate(const ast::StructLiteral& expr) {
    auto* type = expr.type->codegen(*this);

    if (!type) {
        return Value::poisoned();
    }

    auto get_index = [&](const std::map<std::string_view, std::size_t>& members,
                         const ast::StructLiteral::Field& field)
        -> std::optional<std::size_t> {
        auto iterator = members.find(field.name.value);

        if (iterator == members.end()) {
            error(
                field.name.offset,
                fmt::format(
                    "no such member: {}", log::quoted(field.name.value)));

            return std::nullopt;
        }

        return iterator->second;
    };

    if (auto* union_type = dyn_cast<types::Union>(type)) {
        bool stack_allocated = m_current_result == nullptr;

        if (expr.fields.size() != 1) {
            error(
                expr.type->offset,
                "union literals must have exactly one field");

            return Value::poisoned();
        }

        const auto& field = expr.fields[0];
        auto value = field.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        auto* variable = m_current_result
                             ? m_current_result
                             : create_alloca(union_type->llvm_type);

        auto index = get_index(
            m_members[static_cast<llvm::StructType*>(union_type->llvm_type)],
            field);

        if (!index) {
            return Value::poisoned();
        }

        m_current_result = m_builder.CreateStructGEP(
            union_type->llvm_type, variable, union_member_value);

        if (!cast_to_result_or_error(
                field.value->offset, union_type->fields[*index], value)) {
            m_current_result = nullptr;
            return Value::poisoned();
        }

        if (union_type->tag_type) {
            auto* tag_member = m_builder.CreateStructGEP(
                union_type->llvm_type, variable, union_member_tag);

            m_builder.CreateStore(
                llvm::ConstantInt::get(union_type->tag_type->llvm_type, *index),
                tag_member);
        }

        m_current_result = nullptr;

        return Value{type, variable, false, false, false, stack_allocated};
    }

    auto* struct_type = dyn_cast<types::Struct>(type);

    if (!struct_type) {
        error(expr.type->offset, "not a structure");

        return Value::poisoned();
    }

    bool is_const = true;

    bool stack_allocated = m_current_result == nullptr;

    if (expr.fields.size() != struct_type->fields.size()) {
        error(expr.type->offset, "incorrect number of fields initialized");

        return Value::poisoned();
    }

    std::vector<Value> values;
    values.reserve(expr.fields.size());

    for (const auto& field : expr.fields) {
        auto value = field.value->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (!llvm::isa<llvm::Constant>(value.value)) {
            is_const = false;
        } else {
            auto index = get_index(
                m_members[static_cast<llvm::StructType*>(
                    struct_type->llvm_type)],
                field);

            if (!index) {
                return Value::poisoned();
            }

            if (struct_type->fields[*index] != value.type) {
                is_const = false;
            }
        }

        values.push_back(value);
    }

    if (is_const) {
        std::vector<llvm::Constant*> llvm_values;
        llvm_values.reserve(expr.fields.size());

        for (auto& value : values) {
            llvm_values.push_back(static_cast<llvm::Constant*>(value.value));
        }

        return Value{
            type, llvm::ConstantStruct::get(
                      static_cast<llvm::StructType*>(struct_type->llvm_type),
                      llvm_values)};
    }

    auto* variable = m_current_result ? m_current_result
                                      : create_alloca(struct_type->llvm_type);

    for (std::size_t i = 0; i < expr.fields.size(); ++i) {
        const auto& field = expr.fields[i];
        auto& value = values[i];

        auto index = get_index(
            m_members[static_cast<llvm::StructType*>(struct_type->llvm_type)],
            field);

        if (!index) {
            return Value::poisoned();
        }

        m_current_result =
            m_builder.CreateStructGEP(struct_type->llvm_type, variable, *index);

        if (!cast_to_result_or_error(
                field.value->offset, struct_type->fields[*index], value)) {
            m_current_result = nullptr;
            return Value::poisoned();
        }

        m_current_result = nullptr;
    }

    return Value{type, variable, false, false, false, stack_allocated};
}

Value Codegen::generate(const ast::ArrayLiteral& expr) {
    auto* type = expr.type->codegen(*this);

    if (!type) {
        return Value::poisoned();
    }

    auto* array_type = dyn_cast<types::Array>(type);

    if (!array_type) {
        error(expr.type->offset, "not an array type");

        return Value::poisoned();
    }

    auto* llvm_type = static_cast<llvm::ArrayType*>(type->llvm_type);

    bool stack_allocated = m_current_result == nullptr;

    bool is_const = true;

    if (expr.elements.size() != array_type->size) {
        error(expr.type->offset, "incorrect number of elements");

        return Value::poisoned();
    }

    std::vector<Value> values;
    values.reserve(expr.elements.size());

    for (const auto& element : expr.elements) {
        auto value = element->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (!llvm::isa<llvm::Constant>(value.value) ||
            array_type != value.type) {
            is_const = false;
        }

        values.push_back(value);
    }

    if (is_const) {
        std::vector<llvm::Constant*> llvm_values;
        llvm_values.reserve(expr.elements.size());

        for (auto& value : values) {
            llvm_values.push_back(static_cast<llvm::Constant*>(value.value));
        }

        return Value{type, llvm::ConstantArray::get(llvm_type, llvm_values)};
    }

    auto* variable =
        m_current_result ? m_current_result : create_alloca(llvm_type);

    for (std::size_t i = 0; i < expr.elements.size(); ++i) {
        auto& value = values[i];

        auto* intptr = m_module->getDataLayout().getIntPtrType(m_context);

        m_current_result = m_builder.CreateGEP(
            llvm_type, variable,
            {llvm::ConstantInt::get(intptr, 0),
             llvm::ConstantInt::get(intptr, i)});

        if (!cast_to_result_or_error(
                expr.elements[i]->offset, array_type->type, value)) {
            m_current_result = nullptr;
            return Value::poisoned();
        }

        m_current_result = nullptr;
    }

    return Value{type, variable, false, false, false, stack_allocated};
}

Value Codegen::generate(const ast::TupleLiteral& expr) {
    bool is_const = true;

    std::vector<Value> values;
    values.reserve(expr.elements.size());

    for (const auto& element : expr.elements) {
        auto value = element->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (!llvm::isa<llvm::Constant>(value.value)) {
            is_const = false;
        }

        values.push_back(value);
    }

    std::vector<Type*> types;
    std::vector<llvm::Type*> llvm_types;

    types.reserve(expr.elements.size());
    llvm_types.reserve(expr.elements.size());

    for (auto& value : values) {
        types.push_back(value.type);
    }

    for (auto& value : values) {
        llvm_types.push_back(value.type->llvm_type);
    }

    auto* struct_type = llvm::StructType::create(llvm_types);

    bool stack_allocated = m_current_result == nullptr;

    if (is_const) {
        std::vector<llvm::Constant*> llvm_values;
        llvm_values.reserve(expr.elements.size());

        for (auto& value : values) {
            llvm_values.push_back(static_cast<llvm::Constant*>(value.value));
        }

        return Value{
            get_tuple_type(types),
            llvm::ConstantStruct::get(struct_type, llvm_values)};
    }

    auto* variable =
        m_current_result ? m_current_result : create_alloca(struct_type);

    for (std::size_t i = 0; i < values.size(); ++i) {
        auto* pointer = m_builder.CreateStructGEP(struct_type, variable, i);

        m_builder.CreateStore(load_value(values[i]).value, pointer);
    }

    return Value{get_tuple_type(types), variable, false, false, false,
                 stack_allocated};
}

Value Codegen::generate(const ast::Identifier& expr) {
    auto* scope = resolve_scope(expr.value);

    if (!scope) {
        return Value::poisoned();
    }

    auto [name, offset] = expr.value.back();

    auto* result = get_name(offset, name, *scope);

    if (!result) {
        return Value::poisoned();
    }

    return *result;
}

Value Codegen::generate(const ast::CallExpr& expr) {
    auto not_a_function = [&] {
        error(expr.identifier->offset, "not a function");
    };

    if (auto* identifier =
            dynamic_cast<ast::Identifier*>(expr.identifier.get())) {
        auto* scope = resolve_scope(identifier->value);

        if (!scope) {
            return Value::poisoned();
        }

        auto [name, offset] = identifier->value.back();

        if (auto value = scope->names.find(name); value != scope->names.end()) {
            if (!is_accessible(value->second, m_current_unit)) {
                error(offset, fmt::format("{} is private", log::quoted(name)));
                return Value::poisoned();
            }

            auto function = load_value(value->second.element);

            if (auto* type = dyn_cast<types::Function>(function.type)) {
                return create_call(
                    expr.identifier->offset, type, function.value,
                    expr.arguments);
            }

            if (auto* pointer = dyn_cast<types::Pointer>(function.type)) {
                if (auto* type = dyn_cast<types::Function>(pointer->type)) {
                    return create_call(
                        expr.identifier->offset, type, function.value,
                        expr.arguments);
                }
            }

            not_a_function();
            return Value::poisoned();
        }

        auto* generic_fn = get_generic_fn(offset, name, *scope);

        if (!generic_fn) {
            return Value::poisoned();
        }

        auto params_size = generic_fn->params.size();
        auto args_size = expr.arguments.size();
        auto default_args_size = generic_fn->default_args.size();

        if (args_size < params_size - default_args_size ||
            args_size > params_size) {
            error(
                expr.identifier->offset,
                "incorrect number of arguments passed");

            return Value::poisoned();
        }

        std::vector<Value> arguments;
        arguments.reserve(params_size);

        for (std::size_t i = 0; i < args_size; ++i) {
            auto value = expr.arguments[i]->codegen(*this);

            if (!value.ok()) {
                return Value::poisoned();
            }

            if (auto val = cast_or_error(
                    expr.arguments[i]->offset, generic_fn->params[i].type,
                    value);
                val.ok()) {
                arguments.push_back(load_value(val));
            } else {
                return Value::poisoned();
            }
        }

        for (std::size_t i = default_args_size - (params_size - args_size);
             i < default_args_size; ++i) {
            auto arg = generic_fn->default_args[i]->codegen(*this);

            if (!arg.ok()) {
                return Value::poisoned();
            }

            arguments.push_back(arg);
        }

        auto function = inst_generic_fn(generic_fn, arguments);

        if (!function.ok()) {
            return Value::poisoned();
        }

        auto* type = static_cast<types::Function*>(function.type);

        std::vector<llvm::Value*> llvm_args;
        llvm_args.reserve(params_size);

        for (auto arg : arguments) {
            llvm_args.push_back(arg.value);
        }

        return Value{
            type->return_type,
            m_builder.CreateCall(
                static_cast<llvm::FunctionType*>(type->llvm_type),
                function.value, llvm_args)};
    }

    auto value = expr.identifier->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    value = load_value(value);

    if (auto* type = dyn_cast<types::Function>(value.type)) {
        return create_call(
            expr.identifier->offset, type, value.value, expr.arguments);
    }

    if (auto* pointer = dyn_cast<types::Pointer>(value.type)) {
        if (auto* type = dyn_cast<types::Function>(pointer->type)) {
            return create_call(
                expr.identifier->offset, type, value.value, expr.arguments);
        }
    }

    not_a_function();
    return Value::poisoned();
}

Value Codegen::generate(const ast::CallExprGeneric& expr) {
    auto* scope = resolve_scope(expr.identifier->value);

    if (!scope) {
        return Value::poisoned();
    }

    auto [name, offset] = expr.identifier->value.back();

    auto* generic_fn = get_generic_fn(offset, name, *scope);

    if (!generic_fn) {
        return Value::poisoned();
    }

    std::vector<Type*> template_args;
    template_args.reserve(expr.template_args.size());

    for (const auto& arg : expr.template_args) {
        auto* type = arg->codegen(*this);

        if (!type) {
            return Value::poisoned();
        }

        template_args.push_back(type);
    }

    auto function = inst_generic_fn(generic_fn, template_args);

    if (!function.ok()) {
        return Value::poisoned();
    }

    return create_call(
        expr.identifier->offset, static_cast<types::Function*>(function.type),
        function.value, expr.arguments);
}

Value Codegen::generate(const ast::MethodExpr& expr) {
    auto value = expr.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    auto iterator = m_methods[value.type].find(expr.name.value);

    auto no_such_method = [&] {
        error(
            expr.name.offset,
            fmt::format("no such method: {}", log::quoted(expr.name.value)));
    };

    if (iterator == m_methods[value.type].end()) {
        auto* type = dyn_cast<types::Pointer>(value.type);

        if (!type) {
            no_such_method();

            return Value::poisoned();
        }

        iterator = m_methods[type->type].find(expr.name.value);

        if (iterator == m_methods[type->type].end()) {
            no_such_method();

            return Value::poisoned();
        }
    }

    auto arg_size = iterator->second.function->arg_size();

    if (arg_size - 1 != expr.arguments.size()) {
        error(expr.name.offset, "incorrect number of arguments passed");

        return Value::poisoned();
    }

    std::vector<llvm::Value*> arguments;
    arguments.reserve(arg_size);

    if (auto* type =
            dyn_cast<types::Pointer>(iterator->second.type->param_types[0])) {
        if (!value.is_mutable && type->is_mutable) {
            error(
                expr.name.offset,
                fmt::format(
                    "cannot call method {} on an immutable value",
                    log::quoted(expr.name.value)));

            return Value::poisoned();
        }

        if (is<types::Pointer>(value.type)) {
            arguments.push_back(load_value(value).value);
        } else {
            if (!value.value->getType()->isPointerTy()) {
                error(expr.value->offset, "type mismatch");

                return Value::poisoned();
            }

            arguments.push_back(value.value);
        }
    } else {
        auto val = load_value(value);

        if (auto* pointer = dyn_cast<types::Pointer>(value.type)) {
            arguments.push_back(
                m_builder.CreateLoad(pointer->type->llvm_type, val.value));
        } else {
            arguments.push_back(val.value);
        }
    }

    for (std::size_t i = 0; i < expr.arguments.size(); ++i) {
        auto value = expr.arguments[i]->codegen(*this);

        if (!value.ok()) {
            return Value::poisoned();
        }

        if (auto val = cast_or_error(
                expr.arguments[i]->offset,
                iterator->second.type->param_types[i + 1], value);
            val.ok()) {
            arguments.push_back(load_value(val).value);
        } else {
            return Value::poisoned();
        }
    }

    return Value{
        iterator->second.type->return_type,
        m_builder.CreateCall(iterator->second.function, arguments)};
}

Value Codegen::generate(const ast::MemberExpr& expr) {
    auto parent = expr.parent->codegen(*this);

    if (!parent.ok()) {
        return Value::poisoned();
    }

    auto no_such_member = [&] {
        error(
            expr.member.offset,
            fmt::format("no such member: {}", log::quoted(expr.member.value)));
    };

    auto get_member =
        [&](llvm::StructType* type) -> std::optional<std::size_t> {
        auto iterator = m_members[type].find(expr.member.value);

        if (iterator == m_members[type].end()) {
            no_such_member();

            return std::nullopt;
        }

        return iterator->second;
    };

    if (auto* tuple = dyn_cast<types::Tuple>(parent.type)) {
        if (expr.member.type != frontend::Token::Type::IntLiteral) {
            no_such_member();

            return Value::poisoned();
        }

        std::size_t value{};
        std::string_view literal = expr.member.value;

        std::from_chars(literal.cbegin(), literal.cend(), value);

        if (value >= tuple->types.size()) {
            no_such_member();

            return Value::poisoned();
        }

        return Value{
            tuple->types[value],
            create_gep_or_extract(tuple->llvm_type, parent.value, value),
            parent.is_mutable};
    }

    auto not_a_struct = [&] {
        error(expr.member.offset, "member access of a non-structure type");
    };

    if (auto* slice = dyn_cast<types::Slice>(parent.type)) {
        if (expr.member.value == "len") {
            return Value{
                m_primitive_types["usize"].get(),
                m_builder.CreateStructGEP(
                    m_slice_type, parent.value, slice_member_len)};
        }

        not_a_struct();
        return Value::poisoned();
    }

    if (auto* union_type = dyn_cast<types::Union>(parent.type)) {
        auto index =
            get_member(static_cast<llvm::StructType*>(union_type->llvm_type));

        if (!index) {
            return Value::poisoned();
        }

        return Value{
            union_type->fields[*index],
            create_gep_or_extract(
                union_type->llvm_type, parent.value, union_member_value)};
    }

    if (parent.value->getType()->isStructTy()) {
        auto* type = static_cast<types::Struct*>(parent.type);
        auto index =
            get_member(static_cast<llvm::StructType*>(type->llvm_type));

        if (!index) {
            return Value::poisoned();
        }

        return Value{
            type->fields[*index],
            m_builder.CreateExtractValue(parent.value, *index)};
    }

    bool is_mutable = false;

    types::Struct* type = nullptr;

    if (auto* pointer = dyn_cast<types::Pointer>(parent.type)) {
        type = dyn_cast<types::Struct>(pointer->type);

        if (!type) {
            not_a_struct();

            return Value::poisoned();
        }

        is_mutable = pointer->is_mutable;
    } else {
        type = dyn_cast<types::Struct>(parent.type);

        if (!type) {
            not_a_struct();

            return Value::poisoned();
        }

        is_mutable = parent.is_mutable;
    }

    auto* value = parent.value;

    if (is<types::Pointer>(parent.type)) {
        if (auto* variable = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            value = m_builder.CreateLoad(variable->getAllocatedType(), value);
        }
    }

    auto index = get_member(static_cast<llvm::StructType*>(type->llvm_type));

    if (!index) {
        return Value::poisoned();
    }

    return Value{
        type->fields[*index],
        m_builder.CreateStructGEP(type->llvm_type, value, *index), is_mutable};
}

Value Codegen::generate(const ast::IndexExpr& expr) {
    auto value = expr.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    auto index = expr.index->codegen(*this);

    if (!index.ok()) {
        return Value::poisoned();
    }

    auto index_val = load_value(index);

    auto val = cast_or_error(
        expr.index->offset, m_primitive_types["usize"].get(), index_val);

    if (!val.ok()) {
        return Value::poisoned();
    }

    if (auto* type = dyn_cast<types::Slice>(value.type)) {
        auto* ptr_value = load_struct_member(
            type->llvm_type, llvm::PointerType::get(m_context, 0), value.value,
            slice_member_ptr);

        return Value{
            type->type,
            m_builder.CreateGEP(type->type->llvm_type, ptr_value, val.value),
            type->is_mutable};
    }

    auto* type = dyn_cast<types::Array>(value.type);

    if (!type) {
        error(expr.value->offset, "index access of a non-array type");

        return Value::poisoned();
    }

    return Value{
        type->type,
        m_builder.CreateGEP(
            type->llvm_type, value.value,
            {llvm::ConstantInt::get(
                 m_module->getDataLayout().getIntPtrType(m_context), 0),
             val.value}),
        value.is_mutable};
}

Value Codegen::generate(const ast::SliceExpr& expr) {
    auto value = expr.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    Value low = Value::poisoned();

    if (expr.low) {
        low = expr.low->codegen(*this);

        if (!low.ok()) {
            return Value::poisoned();
        }

        low = load_value(low);

        auto low_val = cast_or_error(
            expr.low->offset, m_primitive_types["usize"].get(), low);

        if (!low_val.ok()) {
            return Value::poisoned();
        }

        low = low_val;
    } else {
        low = {
            m_primitive_types["usize"].get(),
            llvm::ConstantInt::get(
                m_module->getDataLayout().getIntPtrType(m_context), 0)};
    }

    Value high = Value::poisoned();

    if (expr.high) {
        high = expr.high->codegen(*this);

        if (!high.ok()) {
            return Value::poisoned();
        }

        high = load_value(high);

        auto high_val = cast_or_error(
            expr.high->offset, m_primitive_types["usize"].get(), high);

        if (!high_val.ok()) {
            return Value::poisoned();
        }

        high = high_val;
    }

    if (auto* type = dyn_cast<types::Array>(value.type)) {
        if (!high.ok()) {
            high = {
                m_primitive_types["usize"].get(),
                llvm::ConstantInt::get(
                    m_module->getDataLayout().getIntPtrType(m_context),
                    type->size)};
        }

        auto* ptr_value =
            m_builder.CreateGEP(type->type->llvm_type, value.value, low.value);

        auto* len_value = m_builder.CreateSub(high.value, low.value);

        auto* variable =
            m_current_result ? m_current_result : create_alloca(m_slice_type);

        auto* ptr_member =
            m_builder.CreateStructGEP(m_slice_type, variable, slice_member_ptr);

        auto* len_member =
            m_builder.CreateStructGEP(m_slice_type, variable, slice_member_len);

        m_builder.CreateStore(ptr_value, ptr_member);
        m_builder.CreateStore(len_value, len_member);

        return Value{
            get_slice_type(type->type, value.is_mutable),
            variable,
            false,
            false,
            false,
            m_current_result == nullptr};
    }

    auto* type = dyn_cast<types::Slice>(value.type);

    if (!type) {
        error(expr.value->offset, "slice expression of a non-slice type");

        return Value::poisoned();
    }

    auto* llvm_type = type->llvm_type;

    if (!high.ok()) {
        auto* len_member =
            m_builder.CreateStructGEP(llvm_type, value.value, slice_member_len);

        high = {
            m_primitive_types["usize"].get(),
            m_builder.CreateLoad(
                m_module->getDataLayout().getIntPtrType(m_context),
                len_member)};
    }

    auto* ptr_value = load_struct_member(
        llvm_type, llvm::PointerType::get(m_context, 0), value.value,
        slice_member_ptr);

    auto* new_ptr_value =
        m_builder.CreateGEP(type->type->llvm_type, ptr_value, low.value);

    auto* new_len_value = m_builder.CreateSub(high.value, low.value);

    auto* variable =
        m_current_result ? m_current_result : create_alloca(llvm_type);

    auto* new_ptr_member =
        m_builder.CreateStructGEP(llvm_type, variable, slice_member_ptr);

    auto* new_len_member =
        m_builder.CreateStructGEP(llvm_type, variable, slice_member_len);

    m_builder.CreateStore(new_ptr_value, new_ptr_member);
    m_builder.CreateStore(new_len_value, new_len_member);

    return Value{value.type, variable, false,
                 false,      false,    m_current_result == nullptr};
}

Value Codegen::generate(const ast::AsExpr& expr) {
    auto value = expr.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    auto* type = expr.type->codegen(*this);

    if (!type) {
        return Value::poisoned();
    }

    return cast_or_error(expr.type->offset, type, value, false);
}

Value Codegen::generate(const ast::SizeofExpr& expr) {
    auto* type = expr.type->codegen(*this);

    if (!type) {
        return Value::poisoned();
    }

    auto* usize = m_primitive_types["usize"].get();
    auto size = m_module->getDataLayout().getTypeAllocSize(type->llvm_type);

    return Value{usize, llvm::ConstantInt::get(usize->llvm_type, size)};
}

Value Codegen::generate_bin_logical_expr(
    const ast::Expression& lhs, const ast::Expression& rhs,
    ast::OffsetValue<frontend::Token::Type> oper) {
    using enum frontend::Token::Type;

    auto lhs_value = lhs.codegen(*this);

    if (!lhs_value.ok()) {
        return Value::poisoned();
    }

    auto* lhs_base_type = unwrap_type(lhs_value.type);

    if (oper.value == AndAnd || oper.value == OrOr) {
        if (!is<types::Bool>(lhs_base_type)) {
            error(lhs.offset, "type mismatch");

            return Value::poisoned();
        }

        auto* root = m_builder.GetInsertBlock();
        auto* function = root->getParent();

        auto* next = llvm::BasicBlock::Create(m_context, "", function);
        auto* end = llvm::BasicBlock::Create(m_context, "", function);

        if (oper.value == AndAnd) {
            m_builder.CreateCondBr(load_value(lhs_value).value, next, end);
        } else {
            m_builder.CreateCondBr(load_value(lhs_value).value, end, next);
        }

        m_builder.SetInsertPoint(next);

        auto rhs_value = rhs.codegen(*this);

        if (!rhs_value.ok()) {
            return Value::poisoned();
        }

        auto value_y =
            cast_or_error(rhs.offset, lhs_value.type, load_value(rhs_value));

        if (!value_y.ok()) {
            return Value::poisoned();
        }

        m_builder.CreateBr(end);

        m_builder.SetInsertPoint(end);

        auto* phi = m_builder.CreatePHI(lhs_base_type->llvm_type, 2);

        phi->addIncoming(
            llvm::ConstantInt::get(
                lhs_base_type->llvm_type, oper.value == OrOr),
            root);

        phi->addIncoming(value_y.value, next);

        return Value{lhs_value.type, phi};
    }

    if (oper.value == QuestionQuestion) {
        auto* optional = dyn_cast<types::Optional>(lhs_base_type);

        if (!optional) {
            error(lhs.offset, "type mismatch");

            return Value::poisoned();
        }

        auto* root = m_builder.GetInsertBlock();
        auto* function = root->getParent();

        auto* bool_true = llvm::BasicBlock::Create(m_context, "", function);
        auto* bool_false = llvm::BasicBlock::Create(m_context, "", function);
        auto* end = llvm::BasicBlock::Create(m_context, "", function);

        auto* optional_bool = get_optional_bool(lhs_value);

        m_builder.CreateCondBr(optional_bool, bool_true, bool_false);

        m_builder.SetInsertPoint(bool_true);

        auto* optional_value = get_optional_value(lhs_value);

        m_builder.CreateBr(end);

        m_builder.SetInsertPoint(bool_false);

        auto rhs_value = rhs.codegen(*this);

        if (!rhs_value.ok()) {
            return Value::poisoned();
        }

        auto value_y =
            cast_or_error(rhs.offset, optional->type, load_value(rhs_value));

        if (!value_y.ok()) {
            return Value::poisoned();
        }

        m_builder.CreateBr(end);

        m_builder.SetInsertPoint(end);

        auto* phi = m_builder.CreatePHI(optional->type->llvm_type, 2);

        phi->addIncoming(optional_value, bool_true);
        phi->addIncoming(value_y.value, bool_false);

        return Value{optional->type, phi};
    }

    auto rhs_value = rhs.codegen(*this);

    if (!rhs_value.ok()) {
        return Value::poisoned();
    }

    return generate_bin_expr(
        ast::OffsetValue<const Value&>{lhs_value, lhs.offset},
        ast::OffsetValue<const Value&>{rhs_value, rhs.offset}, oper);
}

Value Codegen::generate_bin_expr(
    ast::OffsetValue<const Value&> lhs, ast::OffsetValue<const Value&> rhs,
    ast::OffsetValue<frontend::Token::Type> oper) {
    using enum frontend::Token::Type;

    auto* lhs_base_type = unwrap_type(lhs.value.type);
    auto* rhs_base_type = unwrap_type(rhs.value.type);

    if (oper.value == EqualEqual || oper.value == BangEqual) {
        Value value = Value::poisoned();
        backend::Type* base_type = nullptr;

        if (is<types::Optional>(lhs_base_type) &&
            is<types::Null>(rhs_base_type)) {
            value = lhs.value;
            base_type = lhs_base_type;
        } else if (
            is<types::Optional>(rhs_base_type) &&
            is<types::Null>(lhs_base_type)) {
            value = rhs.value;
            base_type = rhs_base_type;
        }

        if (value.ok()) {
            auto* val = get_optional_bool(value);

            if (oper.value == EqualEqual) {
                val = m_builder.CreateNot(val);
            }

            return Value{m_primitive_types["bool"].get(), val};
        }
    }

    auto lhs_value = load_value(lhs.value);
    auto rhs_value = load_value(rhs.value);

    auto* value_base_type = lhs_base_type;
    auto* value_type = lhs.value.type;

    auto value_x = lhs_value;
    auto value_y = cast(lhs_base_type, rhs_value);

    if (!value_y.ok()) {
        value_y = cast_or_error(rhs.offset, rhs_base_type, lhs_value);

        if (!value_y.ok()) {
            return Value::poisoned();
        }

        value_x = rhs_value;
        value_base_type = rhs_base_type;
        value_type = rhs.value.type;
    }

    switch (oper.value) {
    case Plus:
    case Minus:
    case Star:
    case Slash:
    case Less:
    case Greater:
    case GreaterEqual:
    case LessEqual: {
        if (!is_sint(value_base_type) && !is_uint(value_base_type) &&
            !is_float(value_base_type)) {
            error(lhs.offset, "type mismatch");

            return Value::poisoned();
        }

        break;
    }
    case Percent:
    case And:
    case Or:
    case Xor:
        if (!is_sint(value_base_type) && !is_uint(value_base_type)) {
            error(lhs.offset, "type mismatch");

            return Value::poisoned();
        }

        break;
    default:
        break;
    };

    switch (oper.value) {
    case Plus:
        return Value{
            value_type,
            is_float(value_base_type)
                ? m_builder.CreateFAdd(value_x.value, value_y.value)
                : m_builder.CreateAdd(value_x.value, value_y.value)};
    case Minus:
        return Value{
            value_type,
            is_float(value_base_type)
                ? m_builder.CreateFSub(value_x.value, value_y.value)
                : m_builder.CreateSub(value_x.value, value_y.value)};
    case Star:
        return Value{
            value_type,
            is_float(value_base_type)
                ? m_builder.CreateFMul(value_x.value, value_y.value)
                : m_builder.CreateMul(value_x.value, value_y.value)};
    case Slash:
        if (is_float(value_base_type)) {
            return Value{
                value_type, m_builder.CreateFDiv(value_x.value, value_y.value)};
        }

        return Value{
            value_type,
            is_sint(value_base_type)
                ? m_builder.CreateSDiv(value_x.value, value_y.value)
                : m_builder.CreateUDiv(value_x.value, value_y.value)};
    case Percent:
        return Value{
            value_type,
            is_sint(value_base_type)
                ? m_builder.CreateSRem(value_x.value, value_y.value)
                : m_builder.CreateURem(value_x.value, value_y.value)};
    case And:
        return Value{
            value_type, m_builder.CreateAnd(value_x.value, value_y.value)};
    case Or:
        return Value{
            value_type, m_builder.CreateOr(value_x.value, value_y.value)};
    case Xor:
        return Value{
            value_type, m_builder.CreateXor(value_x.value, value_y.value)};
    case Less:
        if (is_float(value_base_type)) {
            return Value{
                m_primitive_types["bool"].get(),
                m_builder.CreateFCmpULT(value_x.value, value_y.value)};
        }

        return Value{
            m_primitive_types["bool"].get(),
            is_sint(value_base_type)
                ? m_builder.CreateICmpSLT(value_x.value, value_y.value)
                : m_builder.CreateICmpULT(value_x.value, value_y.value)};
    case Greater:
        if (is_float(value_base_type)) {
            return Value{
                m_primitive_types["bool"].get(),
                m_builder.CreateFCmpUGT(value_x.value, value_y.value)};
        }

        return Value{
            m_primitive_types["bool"].get(),
            is_sint(value_base_type)
                ? m_builder.CreateICmpSGT(value_x.value, value_y.value)
                : m_builder.CreateICmpUGT(value_x.value, value_y.value)};
    case EqualEqual:
        return Value{
            m_primitive_types["bool"].get(),
            is_float(value_base_type)
                ? m_builder.CreateFCmpUEQ(value_x.value, value_y.value)
                : m_builder.CreateICmpEQ(value_x.value, value_y.value)};
    case BangEqual:
        return Value{
            m_primitive_types["bool"].get(),
            is_float(value_base_type)
                ? m_builder.CreateFCmpUNE(value_x.value, value_y.value)
                : m_builder.CreateICmpNE(value_x.value, value_y.value)};
    case GreaterEqual:
        if (is_float(value_base_type)) {
            return Value{
                m_primitive_types["bool"].get(),
                m_builder.CreateFCmpUGE(value_x.value, value_y.value)};
        }

        return Value{
            m_primitive_types["bool"].get(),
            is_sint(value_base_type)
                ? m_builder.CreateICmpSGE(value_x.value, value_y.value)
                : m_builder.CreateICmpUGE(value_x.value, value_y.value)};
    case LessEqual:
        if (is_float(value_base_type)) {
            return Value{
                m_primitive_types["bool"].get(),
                m_builder.CreateFCmpULE(value_x.value, value_y.value)};
        }

        return Value{
            m_primitive_types["bool"].get(),
            is_sint(value_base_type)
                ? m_builder.CreateICmpSLE(value_x.value, value_y.value)
                : m_builder.CreateICmpULE(value_x.value, value_y.value)};
    default:
        return Value::poisoned();
    }
}

} // namespace cent::backend
