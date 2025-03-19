#ifndef CENT_AST_ASSIGNMENT_H
#define CENT_AST_ASSIGNMENT_H

#include <memory>
#include <string_view>
#include <utility>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent {

struct Assignment : Statement {
    [[nodiscard]] Assignment(
        Span span, SpanValue<std::string_view> identifier,
        std::unique_ptr<Expression> value) noexcept
    : Statement{span}, identifier{identifier}, value{std::move(value)} {}

    SpanValue<std::string_view> identifier;
    std::unique_ptr<Expression> value;
};

} // namespace cent

#endif
