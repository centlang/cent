#ifndef CENT_AST_ENUM_DECL_H
#define CENT_AST_ENUM_DECL_H

#include <string>
#include <vector>

#include "cent/ast/node.h"
#include "cent/offset_value.h"

namespace cent::ast {

struct EnumDecl : detail::Decl<EnumDecl> {
    [[nodiscard]] EnumDecl(
        std::size_t offset, OffsetValue<std::string> name,
        std::vector<OffsetValue<std::string>> fields,
        bool is_public = false) noexcept
    : Decl{offset, is_public}, name{std::move(name)},
      fields{std::move(fields)} {}

    OffsetValue<std::string> name;
    std::vector<OffsetValue<std::string>> fields;
};

} // namespace cent::ast

#endif
