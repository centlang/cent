#ifndef CENT_AST_AS_EXPR_H
#define CENT_AST_AS_EXPR_H

#include <memory>
#include <utility>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct AsExpr : detail::Expr<AsExpr> {
    [[nodiscard]] AsExpr(
        Span span, std::unique_ptr<Expression> value,
        SpanValue<std::string_view> type) noexcept
    : Expr{span}, value{std::move(value)}, type{type} {}

    std::unique_ptr<Expression> value;
    SpanValue<std::string_view> type;
};

} // namespace cent::ast

#endif
