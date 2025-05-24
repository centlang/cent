#ifndef CENT_AST_BINARY_EXPR_H
#define CENT_AST_BINARY_EXPR_H

#include <memory>
#include <utility>

#include "cent/frontend/token.h"
#include "cent/offset_value.h"

#include "cent/ast/node.h"

namespace cent::ast {

struct BinaryExpr : detail::Expr<BinaryExpr> {
    [[nodiscard]] BinaryExpr(
        std::size_t offset, OffsetValue<frontend::Token::Type> oper,
        std::unique_ptr<Expression> lhs, std::unique_ptr<Expression> rhs)
    : Expr{offset}, oper{oper}, lhs{std::move(lhs)}, rhs{std::move(rhs)} {}

    OffsetValue<frontend::Token::Type> oper;

    std::unique_ptr<Expression> lhs;
    std::unique_ptr<Expression> rhs;
};

} // namespace cent::ast

#endif
