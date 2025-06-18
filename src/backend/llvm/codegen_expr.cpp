#include <charconv>
#include <cstddef>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/expr/as_expr.h"
#include "ast/expr/binary_expr.h"
#include "ast/expr/call_expr.h"
#include "ast/expr/identifier.h"
#include "ast/expr/index_expr.h"
#include "ast/expr/literals.h"
#include "ast/expr/member_expr.h"
#include "ast/expr/method_expr.h"
#include "ast/expr/slice_expr.h"
#include "ast/expr/unary_expr.h"

#include "ast/type/named_type.h"

#include "backend/llvm/type.h"
#include "backend/llvm/value.h"

#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/function.h"
#include "backend/llvm/types/primitive.h"
#include "backend/llvm/types/struct.h"
#include "backend/llvm/types/union.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

std::optional<Value> Codegen::generate(ast::BinaryExpr& expr) {
    using enum frontend::Token::Type;

    auto lhs = expr.lhs->codegen(*this);
    auto rhs = expr.rhs->codegen(*this);

    if (!lhs || !rhs) {
        return std::nullopt;
    }

    return generate_bin_expr(
        ast::OffsetValue<Value&>{*lhs, expr.lhs->offset},
        ast::OffsetValue<Value&>{*rhs, expr.rhs->offset}, expr.oper);
}

std::optional<Value> Codegen::generate(ast::UnaryExpr& expr) {
    using enum frontend::Token::Type;

    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    switch (expr.oper.value) {
    case Minus:
        if (is_float(*value->type)) {
            return Value{
                value->type, m_builder.CreateFNeg(load_value(*value).value)};
        }

        if (!is_sint(*value->type) && !is_uint(*value->type)) {
            error(
                expr.offset,
                fmt::format(
                    "cannot apply {} to a non-number type", log::bold("'-'")));

            return std::nullopt;
        }

        return Value{
            value->type, m_builder.CreateNeg(load_value(*value).value)};
    case Bang:
        if (!is<types::Bool>(*value->type)) {
            error(
                expr.offset,
                fmt::format(
                    "cannot apply {} to a non-boolean type", log::bold("'!'")));

            return std::nullopt;
        }

        return Value{
            value->type, m_builder.CreateNot(load_value(*value).value)};
    case Star: {
        if (!is<types::Pointer>(*value->type)) {
            error(expr.offset, "dereference of a non-pointer type");

            return std::nullopt;
        }

        auto& pointer = static_cast<types::Pointer&>(*value->type);

        return Value{
            pointer.type, value->value, pointer.is_mutable, false, true};
    }
    case And:
        if (llvm::isa<llvm::Function>(value->value)) {
            return Value{
                std::make_shared<types::Pointer>(value->type, false),
                value->value};
        }

        if (!llvm::isa<llvm::AllocaInst>(value->value) &&
            !llvm::isa<llvm::GetElementPtrInst>(value->value)) {
            error(expr.offset, "taking the reference of a non-variable value");

            return std::nullopt;
        }

        return Value{
            std::make_shared<types::Pointer>(value->type, value->is_mutable),
            value->value, false, true};
    case Not:
        if (!is_sint(*value->type) && !is_uint(*value->type)) {
            error(
                expr.offset,
                fmt::format(
                    "cannot apply {} to a non-integer type", log::bold("'~'")));

            return std::nullopt;
        }

        return Value{
            value->type, m_builder.CreateNot(load_value(*value).value)};
    default:
        return std::nullopt;
    }
}

std::optional<Value> Codegen::generate(ast::IntLiteral& expr) {
    bool failed = false;
    std::string_view literal = expr.value;

    std::uint8_t base = [&] {
        static constexpr auto hex = 16;
        static constexpr auto oct = 8;
        static constexpr auto bin = 2;
        static constexpr auto dec = 10;

        if (literal.starts_with("0x")) {
            literal = literal.substr(2);

            return hex;
        }

        if (literal.starts_with("0o")) {
            literal = literal.substr(2);

            return oct;
        }

        if (literal.starts_with("0b")) {
            literal = literal.substr(2);

            return bin;
        }

        return dec;
    }();

    auto with_type_suffix = [&]<typename Type>(
                                std::string_view suffix,
                                bool is_signed) -> std::optional<Value> {
        if (!literal.ends_with(suffix)) {
            return std::nullopt;
        }

        Type value{};
        literal = literal.substr(0, literal.size() - suffix.size());

        auto [pointer, result] =
            std::from_chars(literal.cbegin(), literal.cend(), value, base);

        if (result == std::errc::result_out_of_range) {
            error(expr.offset, "integer out of range");

            failed = true;
            return std::nullopt;
        }

        auto& type = m_primitive_types[suffix];
        auto* llvm_type = type->codegen(*this);

        return Value{type, llvm::ConstantInt::get(llvm_type, value, is_signed)};
    };

    if (auto value = with_type_suffix.operator()<std::int8_t>("i8", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::int16_t>("i16", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::int32_t>("i32", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::int64_t>("i64", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value =
            with_type_suffix.operator()<std::ptrdiff_t>("isize", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint8_t>("u8", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint16_t>("u16", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint32_t>("u32", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint64_t>("u64", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::size_t>("usize", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    std::int32_t value{};

    auto [pointer, result] =
        std::from_chars(literal.cbegin(), literal.cend(), value, base);

    if (result == std::errc::result_out_of_range) {
        error(expr.offset, "integer out of range");

        return std::nullopt;
    }

    return Value{
        m_primitive_types["i32"],
        llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(m_context), value)};
}

std::optional<Value> Codegen::generate(ast::FloatLiteral& expr) {
    bool failed = false;

    std::string_view literal = expr.value;

    auto with_type_suffix =
        [&]<typename Type>(std::string_view suffix) -> std::optional<Value> {
        if (!expr.value.ends_with(suffix)) {
            return std::nullopt;
        }

        Type value{};

        literal = literal.substr(0, literal.size() - suffix.size());

        auto [pointer, result] =
            std::from_chars(literal.cbegin(), literal.cend(), value);

        if (result == std::errc::result_out_of_range) {
            error(expr.offset, "float out of range");

            failed = true;
            return std::nullopt;
        }

        auto& type = m_primitive_types[suffix];
        auto* llvm_type = type->codegen(*this);

        return Value{type, llvm::ConstantFP::get(llvm_type, value)};
    };

    if (auto value = with_type_suffix.operator()<float>("f32")) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<double>("f64")) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    float value{};

    auto [pointer, result] =
        std::from_chars(literal.cbegin(), literal.cend(), value);

    if (result == std::errc::result_out_of_range) {
        error(expr.offset, "float out of range");

        return std::nullopt;
    }

    return Value{
        m_primitive_types["f32"],
        llvm::ConstantFP::get(llvm::Type::getFloatTy(m_context), value)};
}

std::optional<Value> Codegen::generate(ast::StrLiteral& expr) {
    return Value{
        m_primitive_types["str"], m_builder.CreateGlobalString(expr.value)};
}

std::optional<Value> Codegen::generate(ast::BoolLiteral& expr) {
    return Value{
        m_primitive_types["bool"],
        llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), expr.value)};
}

std::optional<Value>
Codegen::generate([[maybe_unused]] ast::NullLiteral& expr) {
    return Value{m_null_type, nullptr};
}

std::optional<Value> Codegen::generate([[maybe_unused]] ast::Undefined& expr) {
    return Value{m_undefined_type, nullptr};
}

std::optional<Value> Codegen::generate(ast::StructLiteral& expr) {
    auto type = expr.type->codegen(*this);

    if (!type) {
        return std::nullopt;
    }

    if (auto* union_type = dyn_cast<types::Union>(*type)) {
        auto& members = m_members[union_type->type];
        bool stack_allocated = m_current_result == nullptr;

        if (expr.fields.size() != 1) {
            error(
                expr.type->offset,
                "union literals must have exactly one field");

            return std::nullopt;
        }

        auto& field = expr.fields[0];
        auto value = field.value->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        auto* variable = m_current_result ? m_current_result
                                          : create_alloca(union_type->type);

        auto iterator = members.find(field.name.value);

        if (iterator == members.end()) {
            error(
                field.name.offset,
                fmt::format(
                    "no such member: {}",
                    log::bold(log::quoted(field.name.value))));

            return std::nullopt;
        }

        auto index = iterator->second;

        m_current_result = m_builder.CreateStructGEP(
            union_type->type, variable, union_member_value);

        if (!cast_to_result(union_type->fields[index], *value)) {
            type_mismatch(
                field.value->offset, *union_type->fields[index], *value->type);

            m_current_result = nullptr;

            return std::nullopt;
        }

        if (union_type->tag_type) {
            auto* tag_member = m_builder.CreateStructGEP(
                union_type->type, variable, union_member_tag);

            m_builder.CreateStore(
                llvm::ConstantInt::get(
                    union_type->tag_type->codegen(*this), index),
                tag_member);
        }

        m_current_result = nullptr;

        return Value{type, variable, false, false, false, stack_allocated};
    }

    auto* struct_type = dyn_cast<types::Struct>(*type);

    if (!struct_type) {
        error(expr.type->offset, "not a structure");

        return std::nullopt;
    }

    auto& members = m_members[struct_type->type];
    bool is_const = true;

    bool stack_allocated = m_current_result == nullptr;

    if (expr.fields.size() != struct_type->fields.size()) {
        error(expr.type->offset, "incorrect number of fields initialized");

        return std::nullopt;
    }

    std::vector<Value> values;
    values.reserve(expr.fields.size());

    for (auto& field : expr.fields) {
        auto value = field.value->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!llvm::isa<llvm::Constant>(value->value)) {
            is_const = false;
        }

        values.push_back(std::move(*value));
    }

    if (is_const) {
        std::vector<llvm::Constant*> llvm_values;
        llvm_values.reserve(expr.fields.size());

        for (auto& value : values) {
            llvm_values.push_back(static_cast<llvm::Constant*>(value.value));
        }

        return Value{
            type, llvm::ConstantStruct::get(struct_type->type, llvm_values)};
    }

    auto* variable =
        m_current_result ? m_current_result : create_alloca(struct_type->type);

    for (std::size_t i = 0; i < expr.fields.size(); ++i) {
        auto& field = expr.fields[i];
        auto& value = values[i];

        auto iterator = members.find(field.name.value);

        if (iterator == members.end()) {
            error(
                field.name.offset,
                fmt::format(
                    "no such member: {}",
                    log::bold(log::quoted(field.name.value))));

            return std::nullopt;
        }

        auto index = iterator->second;

        m_current_result =
            m_builder.CreateStructGEP(struct_type->type, variable, index);

        if (!cast_to_result(struct_type->fields[index], value)) {
            type_mismatch(
                field.value->offset, *struct_type->fields[index], *value.type);

            m_current_result = nullptr;

            return std::nullopt;
        }

        m_current_result = nullptr;
    }

    return Value{type, variable, false, false, false, stack_allocated};
}

std::optional<Value> Codegen::generate(ast::ArrayLiteral& expr) {
    auto type = expr.type->codegen(*this);

    if (!type) {
        return std::nullopt;
    }

    auto& array_type = static_cast<types::Array&>(*type);
    auto* llvm_type = static_cast<llvm::ArrayType*>(type->codegen(*this));

    bool stack_allocated = m_current_result == nullptr;

    bool is_const = true;

    if (expr.elements.size() != array_type.size) {
        error(expr.type->offset, "incorrect number of elements");

        return std::nullopt;
    }

    std::vector<Value> values;
    values.reserve(expr.elements.size());

    for (auto& element : expr.elements) {
        auto value = element->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!llvm::isa<llvm::Constant>(value->value)) {
            is_const = false;
        }

        values.push_back(std::move(*value));
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

        if (!cast_to_result(array_type.type, value)) {
            type_mismatch(
                expr.elements[i]->offset, *array_type.type, *value.type);

            m_current_result = nullptr;

            return std::nullopt;
        }

        m_current_result = nullptr;
    }

    return Value{type, variable, false, false, false, stack_allocated};
}

std::optional<Value> Codegen::generate(ast::TupleLiteral& expr) {
    bool is_const = true;

    std::vector<Value> values;
    values.reserve(expr.elements.size());

    for (auto& element : expr.elements) {
        auto value = element->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!llvm::isa<llvm::Constant>(value->value)) {
            is_const = false;
        }

        values.push_back(*value);
    }

    std::vector<std::shared_ptr<backend::Type>> types;
    std::vector<llvm::Type*> llvm_types;

    types.reserve(expr.elements.size());
    llvm_types.reserve(expr.elements.size());

    for (auto& value : values) {
        types.push_back(value.type);
    }

    for (auto& value : values) {
        llvm_types.push_back(value.type->codegen(*this));
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
            std::make_shared<types::Tuple>(struct_type, std::move(types)),
            llvm::ConstantStruct::get(struct_type, llvm_values)};
    }

    auto* variable =
        m_current_result ? m_current_result : create_alloca(struct_type);

    for (std::size_t i = 0; i < values.size(); ++i) {
        auto* pointer = m_builder.CreateStructGEP(struct_type, variable, i);

        m_builder.CreateStore(load_value(values[i]).value, pointer);
    }

    return Value{
        std::make_shared<types::Tuple>(struct_type, std::move(types)),
        variable,
        false,
        false,
        false,
        stack_allocated};
}

std::optional<Value> Codegen::generate(ast::Identifier& expr) {
    auto* scope = m_current_scope;
    std::size_t last_index = expr.value.size() - 1;

    for (std::size_t i = 0; i < last_index; ++i) {
        scope = get_scope(expr.value[i].offset, expr.value[i].value, *scope);

        if (!scope) {
            return std::nullopt;
        }
    }

    return get_name(
        expr.value[last_index].offset, expr.value[last_index].value, *scope);
}

std::optional<Value> Codegen::generate(ast::CallExpr& expr) {
    auto value = expr.identifier->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    value = load_value(*value);

    auto not_a_function = [&] {
        error(expr.identifier->offset, "not a function");
    };

    auto* type = dyn_cast<types::Function>(*value->type);

    if (!type) {
        auto* pointer = dyn_cast<types::Pointer>(*value->type);

        if (!pointer) {
            not_a_function();
            return std::nullopt;
        }

        type = dyn_cast<types::Function>(*pointer->type);

        if (!type) {
            not_a_function();
            return std::nullopt;
        }
    }

    auto arg_size = type->param_types.size();
    auto passed_args_size = expr.arguments.size();
    auto default_args_size = type->default_args.size();

    if (!type->variadic && (passed_args_size < arg_size - default_args_size ||
                            passed_args_size > arg_size)) {
        error(expr.identifier->offset, "incorrect number of arguments passed");

        return std::nullopt;
    }

    std::vector<llvm::Value*> arguments;
    arguments.reserve(arg_size);

    for (std::size_t i = 0; i < passed_args_size; ++i) {
        auto value = expr.arguments[i]->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (type->variadic && i >= arg_size) {
            arguments.push_back(load_value(*value).value);
            continue;
        }

        if (auto val = cast(type->param_types[i], *value)) {
            arguments.push_back(load_value(*val).value);
        } else {
            type_mismatch(
                expr.arguments[i]->offset, *type->param_types[i], *value->type);

            return std::nullopt;
        }
    }

    for (std::size_t i = default_args_size - (arg_size - passed_args_size);
         i < default_args_size; ++i) {
        arguments.push_back(type->default_args[i]);
    }

    return Value{
        type->return_type,
        m_builder.CreateCall(
            static_cast<llvm::FunctionType*>(type->codegen(*this)),
            value->value, arguments)};
}

std::optional<Value> Codegen::generate(ast::MethodExpr& expr) {
    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    auto iterator = m_methods[value->type].find(expr.name.value);

    auto no_such_method = [&] {
        error(
            expr.name.offset,
            fmt::format(
                "no such method: {}", log::bold(log::quoted(expr.name.value))));
    };

    if (iterator == m_methods[value->type].end()) {
        auto* type = dyn_cast<types::Pointer>(*value->type);

        if (!type) {
            no_such_method();

            return std::nullopt;
        }

        iterator = m_methods[type->type].find(expr.name.value);

        if (iterator == m_methods[type->type].end()) {
            no_such_method();

            return std::nullopt;
        }
    }

    auto arg_size = iterator->second.function->arg_size();

    if (arg_size - 1 != expr.arguments.size()) {
        error(expr.name.offset, "incorrect number of arguments passed");

        return std::nullopt;
    }

    std::vector<llvm::Value*> arguments;
    arguments.reserve(arg_size);

    if (auto* type =
            dyn_cast<types::Pointer>(*iterator->second.type->param_types[0])) {
        if (!value->is_mutable && type->is_mutable) {
            error(
                expr.name.offset,
                fmt::format(
                    "cannot call method {} on an immutable value",
                    log::bold(log::quoted(expr.name.value))));

            return std::nullopt;
        }

        if (is<types::Pointer>(*value->type)) {
            arguments.push_back(load_value(*value).value);
        } else {
            if (!value->value->getType()->isPointerTy()) {
                error(expr.value->offset, "type mismatch");

                return std::nullopt;
            }

            arguments.push_back(value->value);
        }
    } else {
        auto val = load_value(*value);

        if (auto* pointer = dyn_cast<types::Pointer>(*value->type)) {
            arguments.push_back(
                m_builder.CreateLoad(pointer->type->codegen(*this), val.value));
        } else {
            arguments.push_back(val.value);
        }
    }

    for (std::size_t i = 0; i < expr.arguments.size(); ++i) {
        auto value = expr.arguments[i]->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (auto val =
                cast(iterator->second.type->param_types[i + 1], *value)) {
            arguments.push_back(load_value(*val).value);
        } else {
            type_mismatch(
                expr.arguments[i]->offset,
                *iterator->second.type->param_types[i + 1], *value->type);

            return std::nullopt;
        }
    }

    return Value{
        iterator->second.type->return_type,
        m_builder.CreateCall(iterator->second.function, arguments)};
}

std::optional<Value> Codegen::generate(ast::MemberExpr& expr) {
    auto parent = expr.parent->codegen(*this);

    if (!parent) {
        return std::nullopt;
    }

    auto no_such_member = [&] {
        error(
            expr.member.offset, fmt::format(
                                    "no such member: {}",
                                    log::bold(log::quoted(expr.member.value))));
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

    if (auto* tuple = dyn_cast<types::Tuple>(*parent->type)) {
        if (expr.member.type != frontend::Token::Type::IntLiteral) {
            no_such_member();

            return std::nullopt;
        }

        std::size_t value{};
        std::string_view literal = expr.member.value;

        std::from_chars(literal.cbegin(), literal.cend(), value);

        if (value >= tuple->types.size()) {
            no_such_member();

            return std::nullopt;
        }

        return Value{
            tuple->types[value],
            parent->value->getType()->isStructTy()
                ? m_builder.CreateExtractValue(parent->value, value)
                : m_builder.CreateStructGEP(tuple->type, parent->value, value),
            parent->is_mutable};
    }

    auto not_a_struct = [&] {
        error(expr.member.offset, "member access of a non-structure type");
    };

    if (auto* slice = dyn_cast<types::Slice>(*parent->type)) {
        if (expr.member.value == "ptr") {
            return Value{
                std::make_shared<types::Pointer>(
                    slice->type, slice->is_mutable),
                m_builder.CreateStructGEP(
                    m_slice_type, parent->value, slice_member_ptr)};
        }

        if (expr.member.value == "len") {
            return Value{
                m_primitive_types["usize"],
                m_builder.CreateStructGEP(
                    m_slice_type, parent->value, slice_member_len)};
        }

        not_a_struct();
        return std::nullopt;
    }

    if (auto* union_type = dyn_cast<types::Union>(*parent->type)) {
        auto index = get_member(union_type->type);

        if (!index) {
            return std::nullopt;
        }

        return Value{
            union_type->fields[*index],
            parent->value->getType()->isStructTy()
                ? m_builder.CreateExtractValue(
                      parent->value, union_member_value)
                : m_builder.CreateStructGEP(
                      union_type->type, parent->value, union_member_value)};
    }

    if (parent->value->getType()->isStructTy()) {
        auto* type = static_cast<types::Struct*>(parent->type.get());
        auto index = get_member(type->type);

        if (!index) {
            return std::nullopt;
        }

        return Value{
            type->fields[*index],
            m_builder.CreateExtractValue(parent->value, *index)};
    }

    bool is_mutable = false;

    types::Struct* type = nullptr;

    if (auto* pointer = dyn_cast<types::Pointer>(*parent->type)) {
        type = dyn_cast<types::Struct>(*pointer->type);

        if (!type) {
            not_a_struct();

            return std::nullopt;
        }

        is_mutable = pointer->is_mutable;
    } else {
        type = dyn_cast<types::Struct>(*parent->type);

        if (!type) {
            not_a_struct();

            return std::nullopt;
        }

        is_mutable = parent->is_mutable;
    }

    auto* value = parent->value;

    if (is<types::Pointer>(*parent->type)) {
        if (auto* variable = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            value = m_builder.CreateLoad(variable->getAllocatedType(), value);
        }
    }

    auto index = get_member(type->type);

    if (!index) {
        return std::nullopt;
    }

    return Value{
        type->fields[*index],
        m_builder.CreateStructGEP(type->type, value, *index), is_mutable};
}

std::optional<Value> Codegen::generate(ast::IndexExpr& expr) {
    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    auto index = expr.index->codegen(*this);

    if (!index) {
        return std::nullopt;
    }

    auto index_val = load_value(*index);
    auto val = cast(m_primitive_types["usize"], index_val);

    if (!val) {
        type_mismatch(
            expr.index->offset, *m_primitive_types["usize"], *index->type);

        return std::nullopt;
    }

    if (auto* type = dyn_cast<types::Slice>(*value->type)) {
        auto* ptr_member = m_builder.CreateStructGEP(
            type->codegen(*this), value->value, slice_member_ptr);

        auto* ptr_value = m_builder.CreateLoad(
            llvm::PointerType::get(m_context, 0), ptr_member);

        return Value{
            type->type,
            m_builder.CreateGEP(
                type->type->codegen(*this), ptr_value, val->value),
            type->is_mutable};
    }

    auto* type = dyn_cast<types::Array>(*value->type);

    if (!type) {
        error(expr.value->offset, "index access of a non-array type");

        return std::nullopt;
    }

    return Value{
        type->type,
        m_builder.CreateGEP(
            type->codegen(*this), value->value,
            {llvm::ConstantInt::get(
                 m_module->getDataLayout().getIntPtrType(m_context), 0),
             val->value}),
        value->is_mutable};
}

std::optional<Value> Codegen::generate(ast::SliceExpr& expr) {
    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    auto* type = dyn_cast<types::Slice>(*value->type);

    if (!type) {
        error(expr.value->offset, "slice expression of a non-slice type");

        return std::nullopt;
    }

    auto* llvm_type = type->codegen(*this);

    std::optional<Value> low = std::nullopt;

    if (expr.low) {
        low = expr.low->codegen(*this);

        if (!low) {
            return std::nullopt;
        }

        low = load_value(*low);
        auto low_val = cast(m_primitive_types["usize"], *low);

        if (!low_val) {
            type_mismatch(
                expr.low->offset, *m_primitive_types["usize"], *low->type);

            return std::nullopt;
        }

        low = low_val;
    } else {
        low = {
            m_primitive_types["usize"],
            llvm::ConstantInt::get(
                m_module->getDataLayout().getIntPtrType(m_context), 0)};
    }

    std::optional<Value> high = std::nullopt;

    if (expr.high) {
        high = expr.high->codegen(*this);

        if (!high) {
            return std::nullopt;
        }

        high = load_value(*high);
        auto high_val = cast(m_primitive_types["usize"], *high);

        if (!high_val) {
            type_mismatch(
                expr.high->offset, *m_primitive_types["usize"], *high->type);

            return std::nullopt;
        }

        high = high_val;
    } else {
        auto* len_member = m_builder.CreateStructGEP(
            llvm_type, value->value, slice_member_len);

        high = {
            m_primitive_types["usize"],
            m_builder.CreateLoad(
                m_module->getDataLayout().getIntPtrType(m_context),
                len_member)};
    }

    auto* ptr_member =
        m_builder.CreateStructGEP(llvm_type, value->value, slice_member_ptr);

    auto* ptr_value =
        m_builder.CreateLoad(llvm::PointerType::get(m_context, 0), ptr_member);

    auto* new_ptr_value =
        m_builder.CreateGEP(type->type->codegen(*this), ptr_value, low->value);

    auto* new_len_value = m_builder.CreateSub(high->value, low->value);

    auto* variable =
        m_current_result ? m_current_result : create_alloca(llvm_type);

    auto* new_ptr_member =
        m_builder.CreateStructGEP(llvm_type, variable, slice_member_ptr);

    auto* new_len_member =
        m_builder.CreateStructGEP(llvm_type, variable, slice_member_len);

    m_builder.CreateStore(new_ptr_value, new_ptr_member);
    m_builder.CreateStore(new_len_value, new_len_member);

    return Value{value->type, variable, false,
                 false,       false,    m_current_result == nullptr};
}

std::optional<Value> Codegen::generate(ast::AsExpr& expr) {
    using enum llvm::Instruction::CastOps;

    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    auto type = expr.type->codegen(*this);

    if (!type) {
        return std::nullopt;
    }

    if (is<types::Pointer>(*value->type) && is<types::Pointer>(*type)) {
        return Value{type, value->value};
    }

    if (auto val = cast(type, *value, false)) {
        return val;
    }

    type_mismatch(expr.type->offset, *type, *value->type);

    return std::nullopt;
}

std::optional<Value> Codegen::generate_bin_expr(
    ast::OffsetValue<Value&> lhs, ast::OffsetValue<Value&> rhs,
    ast::OffsetValue<frontend::Token::Type> oper) {
    using enum frontend::Token::Type;

    if (oper.value == EqualEqual || oper.value == BangEqual) {
        std::optional<Value> value = std::nullopt;

        if (is<types::Optional>(*lhs.value.type) &&
            is<types::Null>(*rhs.value.type)) {
            value = lhs.value;
        } else if (
            is<types::Optional>(*rhs.value.type) &&
            is<types::Null>(*lhs.value.type)) {
            value = rhs.value;
        }

        if (value) {
            auto& type = static_cast<types::Optional&>(*value->type);

            if (is<types::Pointer>(*type.type)) {
                auto* null =
                    llvm::Constant::getNullValue(value->value->getType());

                auto* llvm_value = load_value(*value).value;

                auto* val = oper.value == EqualEqual
                                ? m_builder.CreateICmpEQ(llvm_value, null)
                                : m_builder.CreateICmpNE(llvm_value, null);

                return Value{m_primitive_types["bool"], val};
            }

            llvm::Value* val = nullptr;

            if (value->value->getType()->isStructTy()) {
                val = m_builder.CreateExtractValue(
                    value->value, optional_member_bool);
            } else {
                auto* bool_ptr = m_builder.CreateStructGEP(
                    type.codegen(*this), value->value, optional_member_bool);

                val = m_builder.CreateLoad(
                    llvm::Type::getInt1Ty(m_context), bool_ptr);
            }

            if (oper.value == EqualEqual) {
                val = m_builder.CreateNot(val);
            }

            return Value{m_primitive_types["bool"], val};
        }
    }

    auto lhs_value = load_value(lhs.value);
    auto rhs_value = load_value(rhs.value);

    auto right = cast(lhs_value.type, rhs_value);

    if (!right) {
        type_mismatch(lhs.offset, *lhs_value.type, *rhs_value.type);

        return std::nullopt;
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
        auto type = lhs_value.type;

        if (!is_sint(*type) && !is_uint(*type) && !is_float(*type)) {
            error(lhs.offset, "type mismatch");

            return std::nullopt;
        }

        break;
    }
    case Percent:
    case And:
    case Or:
    case Xor:
        if (!is_sint(*lhs_value.type) && !is_uint(*lhs_value.type)) {
            error(lhs.offset, "type mismatch");

            return std::nullopt;
        }

        break;
    case AndAnd:
    case OrOr:
        if (!is<types::Bool>(*lhs_value.type)) {
            error(lhs.offset, "type mismatch");

            return std::nullopt;
        }

        break;
    default:
        break;
    };

    switch (oper.value) {
    case Plus:
        return Value{
            lhs_value.type,
            is_float(*lhs_value.type)
                ? m_builder.CreateFAdd(lhs_value.value, right->value)
                : m_builder.CreateAdd(lhs_value.value, right->value)};
    case Minus:
        return Value{
            lhs_value.type,
            is_float(*lhs_value.type)
                ? m_builder.CreateFSub(lhs_value.value, right->value)
                : m_builder.CreateSub(lhs_value.value, right->value)};
    case Star:
        return Value{
            lhs_value.type,
            is_float(*lhs_value.type)
                ? m_builder.CreateFMul(lhs_value.value, right->value)
                : m_builder.CreateMul(lhs_value.value, right->value)};
    case Slash:
        if (is_float(*lhs_value.type)) {
            return Value{
                lhs_value.type,
                m_builder.CreateFDiv(lhs_value.value, right->value)};
        }

        return Value{
            lhs_value.type,
            is_sint(*lhs_value.type)
                ? m_builder.CreateSDiv(lhs_value.value, right->value)
                : m_builder.CreateUDiv(lhs_value.value, right->value)};
    case Percent:
        return Value{
            lhs_value.type,
            is_sint(*lhs_value.type)
                ? m_builder.CreateSRem(lhs_value.value, right->value)
                : m_builder.CreateURem(lhs_value.value, right->value)};
    case And:
        return Value{
            lhs_value.type, m_builder.CreateAnd(lhs_value.value, right->value)};
    case Or:
        return Value{
            lhs_value.type, m_builder.CreateOr(lhs_value.value, right->value)};
    case Xor:
        return Value{
            lhs_value.type, m_builder.CreateXor(lhs_value.value, right->value)};
    case AndAnd:
        return Value{
            m_primitive_types["bool"],
            m_builder.CreateLogicalAnd(lhs_value.value, right->value)};
    case OrOr:
        return Value{
            m_primitive_types["bool"],
            m_builder.CreateLogicalOr(lhs_value.value, right->value)};
    case Less:
        if (is_float(*lhs_value.type)) {
            return Value{
                m_primitive_types["bool"],
                m_builder.CreateFCmpULT(lhs_value.value, right->value)};
        }

        return Value{
            m_primitive_types["bool"],
            is_sint(*lhs_value.type)
                ? m_builder.CreateICmpSLT(lhs_value.value, right->value)
                : m_builder.CreateICmpULT(lhs_value.value, right->value)};
    case Greater:
        if (is_float(*lhs_value.type)) {
            return Value{
                m_primitive_types["bool"],
                m_builder.CreateFCmpUGT(lhs_value.value, right->value)};
        }

        return Value{
            m_primitive_types["bool"],
            is_sint(*lhs_value.type)
                ? m_builder.CreateICmpSGT(lhs_value.value, right->value)
                : m_builder.CreateICmpUGT(lhs_value.value, right->value)};
    case EqualEqual:
        return Value{
            m_primitive_types["bool"],
            is_float(*lhs_value.type)
                ? m_builder.CreateFCmpUEQ(lhs_value.value, right->value)
                : m_builder.CreateICmpEQ(lhs_value.value, right->value)};
    case BangEqual:
        return Value{
            m_primitive_types["bool"],
            is_float(*lhs_value.type)
                ? m_builder.CreateFCmpUNE(lhs_value.value, right->value)
                : m_builder.CreateICmpNE(lhs_value.value, right->value)};
    case GreaterEqual:
        if (is_float(*lhs_value.type)) {
            return Value{
                m_primitive_types["bool"],
                m_builder.CreateFCmpUGE(lhs_value.value, right->value)};
        }

        return Value{
            m_primitive_types["bool"],
            is_sint(*lhs_value.type)
                ? m_builder.CreateICmpSGE(lhs_value.value, right->value)
                : m_builder.CreateICmpUGE(lhs_value.value, right->value)};
    case LessEqual:
        if (is_float(*lhs_value.type)) {
            return Value{
                m_primitive_types["bool"],
                m_builder.CreateFCmpULE(lhs_value.value, right->value)};
        }

        return Value{
            m_primitive_types["bool"],
            is_sint(*lhs_value.type)
                ? m_builder.CreateICmpSLE(lhs_value.value, right->value)
                : m_builder.CreateICmpULE(lhs_value.value, right->value)};
    default:
        return std::nullopt;
    }
}

} // namespace cent::backend
