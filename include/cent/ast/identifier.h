#ifndef CENT_AST_IDENTIFIER_H
#define CENT_AST_IDENTIFIER_H

#include <string_view>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent {

struct Identifier : Expression {
    [[nodiscard]] Identifier(Span span, std::string_view value) noexcept
    : Expression{span}, value{value} {}

    std::string_view value;
};

} // namespace cent

#endif
