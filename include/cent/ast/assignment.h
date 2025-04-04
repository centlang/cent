#ifndef CENT_AST_ASSIGNMENT_H
#define CENT_AST_ASSIGNMENT_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent {

struct Assignment : detail::Stmt<Assignment> {
    [[nodiscard]] Assignment(
        Span span, std::unique_ptr<Expression> variable,
        std::unique_ptr<Expression> value) noexcept
    : Stmt{span}, variable{std::move(variable)}, value{std::move(value)} {}

    std::unique_ptr<Expression> variable;
    std::unique_ptr<Expression> value;
};

} // namespace cent

#endif
