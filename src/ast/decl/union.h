#ifndef CENT_AST_UNION_H
#define CENT_AST_UNION_H

#include <string>
#include <vector>

#include "offset_value.h"

#include "ast/decl/struct.h"
#include "ast/node.h"

namespace cent::ast {

struct Union : detail::Decl<Union> {
    using Field = Struct::Field;

    [[nodiscard]] Union(
        std::size_t offset, OffsetValue<std::string> name,
        std::vector<Field> fields,
        std::vector<OffsetValue<std::string>> template_params,
        std::vector<Attribute> attributes, bool is_public = false)
    : Decl{offset, std::move(attributes), is_public}, name{std::move(name)},
      fields{std::move(fields)}, template_params{std::move(template_params)} {}

    OffsetValue<std::string> name;
    std::vector<Field> fields;

    std::vector<OffsetValue<std::string>> template_params;
};

} // namespace cent::ast

#endif
