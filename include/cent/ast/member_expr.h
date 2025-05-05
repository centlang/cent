#ifndef CENT_AST_MEMBER_EXPR_H
#define CENT_AST_MEMBER_EXPR_H

#include <memory>
#include <string>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct MemberExpr : detail::Expr<MemberExpr> {
    [[nodiscard]] MemberExpr(
        Span span, std::unique_ptr<Expression> parent,
        SpanValue<std::string> member) noexcept
    : Expr{span}, parent{std::move(parent)}, member{std::move(member)} {}

    std::unique_ptr<Expression> parent;
    SpanValue<std::string> member;
};

} // namespace cent::ast

#endif
