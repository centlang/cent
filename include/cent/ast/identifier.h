#ifndef CENT_AST_IDENTIFIER_H
#define CENT_AST_IDENTIFIER_H

#include <string>
#include <vector>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct Identifier : detail::Expr<Identifier> {
    [[nodiscard]] Identifier(
        Span span, std::vector<SpanValue<std::string>> value) noexcept
    : Expr{span}, value{std::move(value)} {}

    std::vector<SpanValue<std::string>> value;
};

} // namespace cent::ast

#endif
