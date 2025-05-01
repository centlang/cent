#ifndef CENT_AST_METHOD_EXPR_H
#define CENT_AST_METHOD_EXPR_H

#include <memory>
#include <utility>
#include <vector>

#include "cent/ast/node.h"

namespace cent::ast {

struct MethodExpr : detail::Expr<MethodExpr> {
    [[nodiscard]] MethodExpr(
        Span span, std::unique_ptr<Expression> value,
        SpanValue<std::string> name,
        std::vector<std::unique_ptr<Expression>> arguments) noexcept
    : Expr{span}, value{std::move(value)}, name{std::move(name)},
      arguments{std::move(arguments)} {}

    std::unique_ptr<Expression> value;
    SpanValue<std::string> name;

    std::vector<std::unique_ptr<Expression>> arguments;
};

} // namespace cent::ast

#endif
