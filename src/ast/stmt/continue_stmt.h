#ifndef CENT_AST_CONTINUE_STMT_H
#define CENT_AST_CONTINUE_STMT_H

#include "ast/node.h"

namespace cent::ast {

struct ContinueStmt : detail::Stmt<ContinueStmt> {
    using Stmt::Stmt;
};

} // namespace cent::ast

#endif
