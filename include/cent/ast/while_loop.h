#ifndef CENT_AST_WHILE_LOOP_H
#define CENT_AST_WHILE_LOOP_H

#include <memory>
#include <utility>

#include "cent/span.h"

#include "cent/ast/block_stmt.h"
#include "cent/ast/node.h"

namespace cent {

struct WhileLoop : Statement {
    [[nodiscard]] WhileLoop(
        Span span, std::unique_ptr<Expression> condition,
        std::unique_ptr<BlockStmt> body) noexcept
    : Statement{span}, condition{std::move(condition)}, body{std::move(body)} {}

    std::unique_ptr<Expression> condition;
    std::unique_ptr<BlockStmt> body;
};

} // namespace cent

#endif
