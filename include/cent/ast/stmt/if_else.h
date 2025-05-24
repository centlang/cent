#ifndef CENT_AST_IF_ELSE_H
#define CENT_AST_IF_ELSE_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"
#include "cent/ast/stmt/block_stmt.h"

namespace cent::ast {

struct IfElse : detail::Stmt<IfElse> {
    [[nodiscard]] IfElse(
        std::size_t offset, std::unique_ptr<Expression> condition,
        std::unique_ptr<BlockStmt> if_block,
        std::unique_ptr<Statement> else_block = nullptr)
    : Stmt{offset}, condition{std::move(condition)},
      if_block{std::move(if_block)}, else_block{std::move(else_block)} {}

    std::unique_ptr<Expression> condition;

    std::unique_ptr<BlockStmt> if_block;
    std::unique_ptr<Statement> else_block;
};

} // namespace cent::ast

#endif
