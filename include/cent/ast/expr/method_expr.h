#ifndef CENT_AST_METHOD_EXPR_H
#define CENT_AST_METHOD_EXPR_H

#include <memory>
#include <utility>
#include <vector>

#include "cent/ast/node.h"

namespace cent::ast {

struct MethodExpr : detail::Expr<MethodExpr> {
    [[nodiscard]] MethodExpr(
        std::size_t offset, std::unique_ptr<Expression> value,
        OffsetValue<std::string> name,
        std::vector<std::unique_ptr<Expression>> arguments)
    : Expr{offset}, value{std::move(value)}, name{std::move(name)},
      arguments{std::move(arguments)} {}

    std::unique_ptr<Expression> value;
    OffsetValue<std::string> name;

    std::vector<std::unique_ptr<Expression>> arguments;
};

} // namespace cent::ast

#endif
