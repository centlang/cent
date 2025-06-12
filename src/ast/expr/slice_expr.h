#ifndef CENT_AST_SLICE_EXPR_H
#define CENT_AST_SLICE_EXPR_H

#include <memory>

#include "ast/node.h"

namespace cent::ast {

struct SliceExpr : detail::Expr<SliceExpr> {
    [[nodiscard]] SliceExpr(
        std::size_t offset, std::unique_ptr<Expression> value,
        std::unique_ptr<Expression> low, std::unique_ptr<Expression> high)
    : Expr{offset}, value{std::move(value)}, low{std::move(low)},
      high{std::move(high)} {}

    std::unique_ptr<Expression> value;

    std::unique_ptr<Expression> low;
    std::unique_ptr<Expression> high;
};

} // namespace cent::ast

#endif
