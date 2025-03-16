#ifndef CENT_AST_IF_ELSE_H
#define CENT_AST_IF_ELSE_H

#include <memory>
#include <utility>

#include "cent/span.h"

#include "cent/ast/block_stmt.h"
#include "cent/ast/node.h"

namespace cent {

struct IfElse : Statement {
    [[nodiscard]] IfElse(
        Span span, std::unique_ptr<Expression> condition,
        std::unique_ptr<BlockStmt> if_block,
        std::unique_ptr<BlockStmt> else_block) noexcept
    : Statement{span}, condition{std::move(condition)},
      if_block{std::move(if_block)}, else_block{std::move(else_block)} {}

    std::unique_ptr<Expression> condition;

    std::unique_ptr<BlockStmt> if_block;
    std::unique_ptr<BlockStmt> else_block;
};

} // namespace cent

#endif
