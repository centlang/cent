#ifndef CENT_AST_STRUCT_H
#define CENT_AST_STRUCT_H

#include <memory>
#include <string>
#include <vector>

#include "ast/node.h"
#include "offset_value.h"

namespace cent::ast {

struct Struct : detail::Decl<Struct> {
    struct Field {
        OffsetValue<std::string> name;
        std::unique_ptr<Type> type;
    };

    [[nodiscard]] Struct(
        std::size_t offset, OffsetValue<std::string> name,
        std::vector<Field> fields, std::vector<Attribute> attributes,
        bool is_public = false)
    : Decl{offset, std::move(attributes), is_public}, name{std::move(name)},
      fields{std::move(fields)} {}

    OffsetValue<std::string> name;
    std::vector<Field> fields;
};

} // namespace cent::ast

#endif
