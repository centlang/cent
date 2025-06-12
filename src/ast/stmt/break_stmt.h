#ifndef CENT_AST_BREAK_STMT_H
#define CENT_AST_BREAK_STMT_H

#include "ast/node.h"

namespace cent::ast {

struct BreakStmt : detail::Stmt<BreakStmt> {
    using Stmt::Stmt;
};

} // namespace cent::ast

#endif
