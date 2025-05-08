#ifndef CENT_AST_MEMBER_EXPR_H
#define CENT_AST_MEMBER_EXPR_H

#include <memory>

#include "cent/frontend/token.h"
#include "cent/span.h"

#include "cent/ast/node.h"

namespace cent::ast {

struct MemberExpr : detail::Expr<MemberExpr> {
    [[nodiscard]] MemberExpr(
        Span span, std::unique_ptr<Expression> parent,
        frontend::Token member) noexcept
    : Expr{span}, parent{std::move(parent)}, member{std::move(member)} {}

    std::unique_ptr<Expression> parent;
    frontend::Token member;
};

} // namespace cent::ast

#endif
