#ifndef CENT_AST_FOR_LOOP_H
#define CENT_AST_FOR_LOOP_H

#include <memory>
#include <utility>

#include "offset_value.h"

#include "ast/node.h"
#include "ast/stmt/block_stmt.h"

namespace cent::ast {

struct ForLoop : detail::Stmt<ForLoop> {
    [[nodiscard]] ForLoop(
        std::size_t offset, bool is_mutable, OffsetValue<std::string> name,
        std::unique_ptr<Expression> value, std::unique_ptr<BlockStmt> body)
    : Stmt{offset}, is_mutable{is_mutable}, name{std::move(name)},
      value{std::move(value)}, body{std::move(body)} {}

    bool is_mutable;
    OffsetValue<std::string> name;

    std::unique_ptr<Expression> value;
    std::unique_ptr<BlockStmt> body;
};

} // namespace cent::ast

#endif
