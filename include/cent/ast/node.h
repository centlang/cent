#ifndef CENT_AST_NODE_H
#define CENT_AST_NODE_H

#include "cent/span.h"

namespace cent {

struct Node {
    [[nodiscard]] Node() noexcept = default;
    virtual ~Node() noexcept = default;

    Node(const Node&) = delete;
    Node(Node&&) = delete;

    auto operator=(const Node&) = delete;
    auto operator=(Node&&) = delete;
};

struct Statement : Node {
    [[nodiscard]] Statement(Span span) noexcept : span{span} {}

    Span span;
};

struct Expression : Statement {
    using Statement::Statement;
};

struct Declaration : Statement {
    using Statement::Statement;
};

} // namespace cent

#endif
