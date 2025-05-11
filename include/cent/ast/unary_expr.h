#ifndef CENT_AST_UNARY_EXPR_H
#define CENT_AST_UNARY_EXPR_H

#include <memory>
#include <utility>

#include "cent/frontend/token.h"
#include "cent/offset_value.h"

#include "cent/ast/node.h"

namespace cent::ast {

struct UnaryExpr : detail::Expr<UnaryExpr> {
    [[nodiscard]] UnaryExpr(
        std::size_t offset, OffsetValue<frontend::Token::Type> oper,
        std::unique_ptr<Expression> value) noexcept
    : Expr{offset}, oper{oper}, value{std::move(value)} {}

    OffsetValue<frontend::Token::Type> oper;
    std::unique_ptr<Expression> value;
};

} // namespace cent::ast

#endif
