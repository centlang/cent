#ifndef CENT_AST_ASSIGNMENT_H
#define CENT_AST_ASSIGNMENT_H

#include <memory>
#include <utility>

#include "frontend/token.h"
#include "offset_value.h"

#include "ast/node.h"

namespace cent::ast {

struct Assignment : detail::Stmt<Assignment> {
    [[nodiscard]] Assignment(
        std::size_t offset, std::unique_ptr<Expression> variable,
        std::unique_ptr<Expression> value,
        OffsetValue<frontend::Token::Type> oper)
    : Stmt{offset}, variable{std::move(variable)}, value{std::move(value)},
      oper{oper} {}

    std::unique_ptr<Expression> variable;
    std::unique_ptr<Expression> value;

    OffsetValue<frontend::Token::Type> oper;
};

} // namespace cent::ast

#endif
