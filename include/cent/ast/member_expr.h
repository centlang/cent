#ifndef CENT_AST_MEMBER_EXPR_H
#define CENT_AST_MEMBER_EXPR_H

#include <memory>
#include <string_view>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct MemberExpr : detail::Expr<MemberExpr> {
    [[nodiscard]] MemberExpr(
        Span span, std::unique_ptr<Expression> parent,
        SpanValue<std::string_view> member) noexcept
    : Expr{span}, parent{std::move(parent)}, member{member} {}

    std::unique_ptr<Expression> parent;
    SpanValue<std::string_view> member;
};

} // namespace cent::ast

#endif
