#ifndef CENT_AST_LITERALS_H
#define CENT_AST_LITERALS_H

#include <string>
#include <vector>

#include "cent/span.h"

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

struct StructLiteral : detail::Expr<StructLiteral> {
    struct Field {
        SpanValue<std::string> name;
        std::unique_ptr<Expression> value;
    };

    [[nodiscard]] StructLiteral(
        Span span, SpanValue<std::string> name,
        std::vector<Field> fields) noexcept
    : Expr{span}, name{std::move(name)}, fields{std::move(fields)} {}

    SpanValue<std::string> name;
    std::vector<Field> fields;
};

} // namespace cent::ast

#endif
