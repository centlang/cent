#ifndef CENT_AST_LITERALS_H
#define CENT_AST_LITERALS_H

#include <string_view>
#include <vector>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct IntLiteral : detail::Expr<IntLiteral> {
    [[nodiscard]] IntLiteral(Span span, std::string_view value) noexcept
    : Expr{span}, value{value} {}

    std::string_view value;
};

struct FloatLiteral : detail::Expr<FloatLiteral> {
    [[nodiscard]] FloatLiteral(Span span, std::string_view value) noexcept
    : Expr{span}, value{value} {}

    std::string_view value;
};

struct BoolLiteral : detail::Expr<BoolLiteral> {
    [[nodiscard]] BoolLiteral(Span span, bool value) noexcept
    : Expr{span}, value{value} {}

    bool value;
};

struct StructLiteral : detail::Expr<StructLiteral> {
    struct Field {
        SpanValue<std::string_view> name;
        std::unique_ptr<Expression> value;
    };

    [[nodiscard]] StructLiteral(
        Span span, SpanValue<std::string_view> name,
        std::vector<Field> fields) noexcept
    : Expr{span}, name{name}, fields{std::move(fields)} {}

    SpanValue<std::string_view> name;
    std::vector<Field> fields;
};

} // namespace cent::ast

#endif
