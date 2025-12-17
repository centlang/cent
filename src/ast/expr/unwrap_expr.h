#ifndef CENT_AST_UNWRAP_EXPR_H
#define CENT_AST_UNWRAP_EXPR_H

#include <memory>
#include <utility>

#include "ast/node.h"

namespace cent::ast {

struct UnwrapExpr : detail::Expr<UnwrapExpr> {
    [[nodiscard]] UnwrapExpr(
        std::size_t offset, std::unique_ptr<Expression> value)
    : Expr{offset}, value{std::move(value)} {}

    std::unique_ptr<Expression> value;
};

} // namespace cent::ast

#endif
