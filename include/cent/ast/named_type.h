#ifndef CENT_AST_NAMED_TYPE_H
#define CENT_AST_NAMED_TYPE_H

#include <string>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct NamedType : detail::Type<NamedType> {
    [[nodiscard]] NamedType(
        Span span, std::vector<SpanValue<std::string>> value) noexcept
    : Type{span}, value{std::move(value)} {}

    std::vector<SpanValue<std::string>> value;
};

} // namespace cent::ast

#endif
