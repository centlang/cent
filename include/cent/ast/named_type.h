#ifndef CENT_AST_NAMED_TYPE_H
#define CENT_AST_NAMED_TYPE_H

#include <string_view>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct NamedType : detail::Type<NamedType> {
    [[nodiscard]] NamedType(Span span, std::string_view value) noexcept
    : Type{span}, value{value} {}

    std::string_view value;
};

} // namespace cent::ast

#endif
