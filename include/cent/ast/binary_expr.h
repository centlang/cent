#ifndef CENT_AST_BINARY_EXPR_H
#define CENT_AST_BINARY_EXPR_H

#include <memory>
#include <utility>

#include "cent/frontend/token.h"
#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent {

struct BinaryExpr : detail::Expr<BinaryExpr> {
    [[nodiscard]] BinaryExpr(
        Span span, SpanValue<Token::Type> oper, std::unique_ptr<Expression> lhs,
        std::unique_ptr<Expression> rhs) noexcept
    : Expr{span}, oper{oper}, lhs{std::move(lhs)}, rhs{std::move(rhs)} {}

    SpanValue<Token::Type> oper;

    std::unique_ptr<Expression> lhs;
    std::unique_ptr<Expression> rhs;
};

} // namespace cent

#endif
