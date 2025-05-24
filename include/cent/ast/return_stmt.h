#ifndef CENT_AST_RETURN_STMT_H
#define CENT_AST_RETURN_STMT_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"

namespace cent::ast {

struct ReturnStmt : detail::Stmt<ReturnStmt> {
    [[nodiscard]] ReturnStmt(
        std::size_t offset, std::unique_ptr<Expression> value)
    : Stmt{offset}, value{std::move(value)} {}

    std::unique_ptr<Expression> value;
};

} // namespace cent::ast

#endif
