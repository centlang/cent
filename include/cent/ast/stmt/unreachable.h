#ifndef CENT_AST_UNREACHABLE_H
#define CENT_AST_UNREACHABLE_H

#include "cent/ast/node.h"

namespace cent::ast {

struct Unreachable : detail::Stmt<Unreachable> {
    using Stmt::Stmt;
};

} // namespace cent::ast

#endif
