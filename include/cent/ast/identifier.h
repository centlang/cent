#ifndef CENT_AST_IDENTIFIER_H
#define CENT_AST_IDENTIFIER_H

#include <string>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct Identifier : detail::Expr<Identifier> {
    [[nodiscard]] Identifier(Span span, std::string value) noexcept
    : Expr{span}, value{std::move(value)} {}

    std::string value;
};

} // namespace cent::ast

#endif
