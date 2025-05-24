#ifndef CENT_AST_NAMED_TYPE_H
#define CENT_AST_NAMED_TYPE_H

#include <string>

#include "cent/ast/node.h"
#include "cent/offset_value.h"

namespace cent::ast {

struct NamedType : detail::Type<NamedType> {
    [[nodiscard]] NamedType(
        std::size_t offset, std::vector<OffsetValue<std::string>> value)
    : Type{offset}, value{std::move(value)} {}

    std::vector<OffsetValue<std::string>> value;
};

} // namespace cent::ast

#endif
