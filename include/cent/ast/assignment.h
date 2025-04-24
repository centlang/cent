#ifndef CENT_AST_ASSIGNMENT_H
#define CENT_AST_ASSIGNMENT_H

#include <memory>
#include <utility>

#include "cent/frontend/token.h"
#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct Assignment : detail::Stmt<Assignment> {
    [[nodiscard]] Assignment(
        Span span, std::unique_ptr<Expression> variable,
        std::unique_ptr<Expression> value,
        SpanValue<frontend::Token::Type> oper) noexcept
    : Stmt{span}, variable{std::move(variable)}, value{std::move(value)},
      oper{oper} {}

    std::unique_ptr<Expression> variable;
    std::unique_ptr<Expression> value;

    SpanValue<frontend::Token::Type> oper;
};

} // namespace cent::ast

#endif
