#ifndef CENT_AST_IDENTIFIER_H
#define CENT_AST_IDENTIFIER_H

#include <string>
#include <vector>

#include "ast/node.h"
#include "offset_value.h"

namespace cent::ast {

struct Identifier : detail::Expr<Identifier> {
    [[nodiscard]] Identifier(
        std::size_t offset, std::vector<OffsetValue<std::string>> value)
    : Expr{offset}, value{std::move(value)} {}

    std::vector<OffsetValue<std::string>> value;
};

} // namespace cent::ast

#endif
