#ifndef CENT_AST_UNARY_EXPR_H
#define CENT_AST_UNARY_EXPR_H

#include <memory>
#include <utility>

#include "cent/frontend/token.h"
#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent {

struct UnaryExpr : detail::Expr<UnaryExpr> {
    [[nodiscard]] UnaryExpr(
        Span span, SpanValue<Token::Type> oper,
        std::unique_ptr<Expression> value) noexcept
    : Expr{span}, oper{oper}, value{std::move(value)} {}

    SpanValue<Token::Type> oper;
    std::unique_ptr<Expression> value;
};

} // namespace cent

#endif
