#ifndef CENT_AST_SWITCH_H
#define CENT_AST_SWITCH_H

#include <memory>
#include <utility>

#include "ast/node.h"
#include "ast/stmt/block_stmt.h"

namespace cent::ast {

struct Switch : detail::Stmt<Switch> {
    struct Case {
        std::vector<std::unique_ptr<Expression>> values;
        std::unique_ptr<BlockStmt> block;
    };

    [[nodiscard]] Switch(
        std::size_t offset, std::unique_ptr<Expression> value,
        std::vector<Case> cases, std::unique_ptr<BlockStmt> else_block)
    : Stmt{offset}, value{std::move(value)}, cases{std::move(cases)},
      else_block{std::move(else_block)} {}

    std::unique_ptr<Expression> value;

    std::vector<Case> cases;
    std::unique_ptr<BlockStmt> else_block;
};

} // namespace cent::ast

#endif
