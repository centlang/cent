#ifndef CENT_AST_INDEX_EXPR_H
#define CENT_AST_INDEX_EXPR_H

#include <memory>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct IndexExpr : detail::Expr<IndexExpr> {
    [[nodiscard]] IndexExpr(
        Span span, std::unique_ptr<Expression> value,
        std::unique_ptr<Expression> index) noexcept
    : Expr{span}, value{std::move(value)}, index{std::move(index)} {}

    std::unique_ptr<Expression> value;
    std::unique_ptr<Expression> index;
};

} // namespace cent::ast

#endif
