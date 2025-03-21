#ifndef CENT_AST_RETURN_STMT_H
#define CENT_AST_RETURN_STMT_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent {

struct ReturnStmt : Statement {
    [[nodiscard]] ReturnStmt(
        Span span, std::unique_ptr<Expression> value) noexcept
    : Statement{span}, value{std::move(value)} {}

    std::unique_ptr<Expression> value;
};

} // namespace cent

#endif
