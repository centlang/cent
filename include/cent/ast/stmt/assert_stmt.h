#ifndef CENT_AST_ASSERT_STMT_H
#define CENT_AST_ASSERT_STMT_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"

namespace cent::ast {

struct AssertStmt : detail::Stmt<AssertStmt> {
    [[nodiscard]] AssertStmt(
        std::size_t offset, std::unique_ptr<Expression> condition)
    : Stmt{offset}, condition{std::move(condition)} {}

    std::unique_ptr<Expression> condition;
};

} // namespace cent::ast

#endif
