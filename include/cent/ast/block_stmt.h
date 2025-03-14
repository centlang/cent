#ifndef CENT_AST_BLOCK_STMT_H
#define CENT_AST_BLOCK_STMT_H

#include <memory>
#include <vector>

#include "cent/ast/node.h"

namespace cent {

struct BlockStmt : Statement {
    using Statement::Statement;

    std::vector<std::unique_ptr<Statement>> body;
};

} // namespace cent

#endif
