#ifndef CENT_AST_LITERALS_H
#define CENT_AST_LITERALS_H

#include <memory>
#include <string>
#include <vector>

#include "cent/span.h"

#include "cent/ast/array_type.h"
#include "cent/ast/named_type.h"
#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct IntLiteral : detail::Expr<IntLiteral> {
    [[nodiscard]] IntLiteral(Span span, std::string value) noexcept
    : Expr{span}, value{std::move(value)} {}

    std::string value;
};

struct FloatLiteral : detail::Expr<FloatLiteral> {
    [[nodiscard]] FloatLiteral(Span span, std::string value) noexcept
    : Expr{span}, value{std::move(value)} {}

    std::string value;
};

struct StrLiteral : detail::Expr<StrLiteral> {
    [[nodiscard]] StrLiteral(Span span, std::string value) noexcept
    : Expr{span}, value{std::move(value)} {}

    std::string value;
};

struct BoolLiteral : detail::Expr<BoolLiteral> {
    [[nodiscard]] BoolLiteral(Span span, bool value) noexcept
    : Expr{span}, value{value} {}

    bool value;
};

struct NullLiteral : detail::Expr<NullLiteral> {
    using Expr::Expr;
};

struct StructLiteral : detail::Expr<StructLiteral> {
    struct Field {
        SpanValue<std::string> name;
        std::unique_ptr<Expression> value;
    };

    [[nodiscard]] StructLiteral(
        Span span, std::unique_ptr<NamedType> type,
        std::vector<Field> fields) noexcept
    : Expr{span}, type{std::move(type)}, fields{std::move(fields)} {}

    std::unique_ptr<NamedType> type;
    std::vector<Field> fields;
};

struct ArrayLiteral : detail::Expr<ArrayLiteral> {
    [[nodiscard]] ArrayLiteral(
        Span span, std::unique_ptr<ArrayType> type,
        std::vector<std::unique_ptr<Expression>> elements) noexcept
    : Expr{span}, type{std::move(type)}, elements{std::move(elements)} {}

    std::unique_ptr<ArrayType> type;
    std::vector<std::unique_ptr<Expression>> elements;
};

struct TupleLiteral : detail::Expr<TupleLiteral> {
    [[nodiscard]] TupleLiteral(
        Span span, std::vector<std::unique_ptr<Expression>> elements) noexcept
    : Expr{span}, elements{std::move(elements)} {}

    std::vector<std::unique_ptr<Expression>> elements;
};

} // namespace cent::ast

#endif
