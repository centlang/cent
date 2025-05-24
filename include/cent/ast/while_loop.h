#ifndef CENT_AST_WHILE_LOOP_H
#define CENT_AST_WHILE_LOOP_H

#include <memory>
#include <utility>

#include "cent/ast/block_stmt.h"
#include "cent/ast/node.h"

namespace cent::ast {

struct WhileLoop : detail::Stmt<WhileLoop> {
    [[nodiscard]] WhileLoop(
        std::size_t offset, std::unique_ptr<Expression> condition,
        std::unique_ptr<BlockStmt> body)
    : Stmt{offset}, condition{std::move(condition)}, body{std::move(body)} {}

    std::unique_ptr<Expression> condition;
    std::unique_ptr<BlockStmt> body;
};

} // namespace cent::ast

#endif
