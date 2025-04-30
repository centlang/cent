#ifndef CENT_AST_MEMBER_EXPR_H
#define CENT_AST_MEMBER_EXPR_H

#include <memory>
#include <string>
#include <vector>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct MemberExpr : detail::Expr<MemberExpr> {
    [[nodiscard]] MemberExpr(
        Span span, std::unique_ptr<Expression> value,
        std::vector<SpanValue<std::string>> path) noexcept
    : Expr{span}, value{std::move(value)}, path{std::move(path)} {}

    std::unique_ptr<Expression> value;
    std::vector<SpanValue<std::string>> path;
};

} // namespace cent::ast

#endif
