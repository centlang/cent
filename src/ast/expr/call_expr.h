#ifndef CENT_AST_CALL_EXPR_H
#define CENT_AST_CALL_EXPR_H

#include <memory>
#include <utility>
#include <vector>

#include "ast/node.h"

namespace cent::ast {

struct CallExpr : detail::Expr<CallExpr> {
    [[nodiscard]] CallExpr(
        std::size_t offset, std::unique_ptr<Expression> identifier,
        std::vector<std::unique_ptr<Expression>> arguments)
    : Expr{offset}, identifier{std::move(identifier)},
      arguments{std::move(arguments)} {}

    std::unique_ptr<Expression> identifier;
    std::vector<std::unique_ptr<Expression>> arguments;
};

} // namespace cent::ast

#endif
