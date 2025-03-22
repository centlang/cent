#ifndef CENT_AST_CALL_EXPR_H
#define CENT_AST_CALL_EXPR_H

#include <memory>
#include <string_view>
#include <utility>
#include <vector>

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent {

struct CallExpr : detail::Expr<CallExpr> {
    [[nodiscard]] CallExpr(
        Span span, SpanValue<std::string_view> identifier,
        std::vector<std::unique_ptr<Expression>> arguments) noexcept
    : Expr{span}, identifier{identifier}, arguments{std::move(arguments)} {}

    SpanValue<std::string_view> identifier;
    std::vector<std::unique_ptr<Expression>> arguments;
};

} // namespace cent

#endif
