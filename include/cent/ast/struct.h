#ifndef CENT_AST_STRUCT_H
#define CENT_AST_STRUCT_H

#include <string_view>
#include <vector>

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct Struct : detail::Decl<Struct> {
    struct Field {
        SpanValue<std::string_view> name;
        SpanValue<std::string_view> type;
    };

    [[nodiscard]] Struct(
        Span span, SpanValue<std::string_view> name,
        std::vector<Field> fields) noexcept
    : Decl{span}, name{name}, fields{std::move(fields)} {}

    SpanValue<std::string_view> name;
    std::vector<Field> fields;
};

} // namespace cent::ast

#endif
