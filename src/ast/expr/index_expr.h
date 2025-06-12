#ifndef CENT_AST_INDEX_EXPR_H
#define CENT_AST_INDEX_EXPR_H

#include <memory>

#include "ast/node.h"

namespace cent::ast {

struct IndexExpr : detail::Expr<IndexExpr> {
    [[nodiscard]] IndexExpr(
        std::size_t offset, std::unique_ptr<Expression> value,
        std::unique_ptr<Expression> index)
    : Expr{offset}, value{std::move(value)}, index{std::move(index)} {}

    std::unique_ptr<Expression> value;
    std::unique_ptr<Expression> index;
};

} // namespace cent::ast

#endif
