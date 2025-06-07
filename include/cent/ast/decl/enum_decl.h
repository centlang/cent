#ifndef CENT_AST_ENUM_DECL_H
#define CENT_AST_ENUM_DECL_H

#include <memory>
#include <string>
#include <vector>

#include "cent/ast/node.h"
#include "cent/offset_value.h"

namespace cent::ast {

struct EnumDecl : detail::Decl<EnumDecl> {
    struct Field {
        OffsetValue<std::string> name;
        std::unique_ptr<Expression> value;
    };

    [[nodiscard]] EnumDecl(
        std::size_t offset, OffsetValue<std::string> name,
        std::unique_ptr<Type> type, std::vector<Field> fields,
        std::vector<Attribute> attributes, bool is_public = false)
    : Decl{offset, std::move(attributes), is_public}, name{std::move(name)},
      type{std::move(type)}, fields{std::move(fields)} {}

    OffsetValue<std::string> name;
    std::unique_ptr<Type> type;

    std::vector<Field> fields;
};

} // namespace cent::ast

#endif
