#ifndef CENT_AST_RETURN_STMT_H
#define CENT_AST_RETURN_STMT_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct ReturnStmt : detail::Stmt<ReturnStmt> {
    [[nodiscard]] ReturnStmt(
        Span span, std::unique_ptr<Expression> value) noexcept
    : Stmt{span}, value{std::move(value)} {}

    std::unique_ptr<Expression> value;
};

} // namespace cent::ast

#endif
