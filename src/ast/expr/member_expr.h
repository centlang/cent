#ifndef CENT_AST_MEMBER_EXPR_H
#define CENT_AST_MEMBER_EXPR_H

#include <memory>

#include "ast/node.h"
#include "frontend/token.h"

namespace cent::ast {

struct MemberExpr : detail::Expr<MemberExpr> {
    [[nodiscard]] MemberExpr(
        std::size_t offset, std::unique_ptr<Expression> parent,
        frontend::Token member)
    : Expr{offset}, parent{std::move(parent)}, member{std::move(member)} {}

    std::unique_ptr<Expression> parent;
    frontend::Token member;
};

} // namespace cent::ast

#endif
