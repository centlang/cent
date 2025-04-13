#ifndef CENT_AST_AS_EXPR_H
#define CENT_AST_AS_EXPR_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct AsExpr : detail::Expr<AsExpr> {
    [[nodiscard]] AsExpr(
        Span span, std::unique_ptr<Expression> value,
        std::unique_ptr<Type> type) noexcept
    : Expr{span}, value{std::move(value)}, type{std::move(type)} {}

    std::unique_ptr<Expression> value;
    std::unique_ptr<Type> type;
};

} // namespace cent::ast

#endif
