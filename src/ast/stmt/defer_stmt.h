#ifndef CENT_AST_DEFER_STMT_H
#define CENT_AST_DEFER_STMT_H

#include <memory>
#include <utility>

#include "ast/node.h"

namespace cent::ast {

struct DeferStmt : detail::Stmt<DeferStmt> {
    [[nodiscard]] DeferStmt(std::size_t offset, std::unique_ptr<Statement> stmt)
    : Stmt{offset}, stmt{std::move(stmt)} {}

    std::unique_ptr<Statement> stmt;
};

} // namespace cent::ast

#endif
