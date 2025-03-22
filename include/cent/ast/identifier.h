#ifndef CENT_AST_IDENTIFIER_H
#define CENT_AST_IDENTIFIER_H

#include <string_view>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent {

struct Identifier : detail::Expr<Identifier> {
    [[nodiscard]] Identifier(Span span, std::string_view value) noexcept
    : Expr{span}, value{value} {}

    std::string_view value;
};

} // namespace cent

#endif
