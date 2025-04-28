#ifndef CENT_AST_STRUCT_H
#define CENT_AST_STRUCT_H

#include <memory>
#include <string>
#include <vector>

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct Struct : detail::Decl<Struct> {
    struct Field {
        SpanValue<std::string> name;
        std::unique_ptr<Type> type;
    };

    [[nodiscard]] Struct(
        Span span, SpanValue<std::string> name,
        std::vector<Field> fields) noexcept
    : Decl{span}, name{std::move(name)}, fields{std::move(fields)} {}

    SpanValue<std::string> name;
    std::vector<Field> fields;
};

} // namespace cent::ast

#endif
