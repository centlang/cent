#ifndef CENT_AST_AS_EXPR_H
#define CENT_AST_AS_EXPR_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"

namespace cent::ast {

struct AsExpr : detail::Expr<AsExpr> {
    [[nodiscard]] AsExpr(
        std::size_t offset, std::unique_ptr<Expression> value,
        std::unique_ptr<Type> type) noexcept
    : Expr{offset}, value{std::move(value)}, type{std::move(type)} {}

    std::unique_ptr<Expression> value;
    std::unique_ptr<Type> type;
};

} // namespace cent::ast

#endif
