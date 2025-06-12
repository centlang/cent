#ifndef CENT_AST_TYPE_ALIAS_H
#define CENT_AST_TYPE_ALIAS_H

#include <memory>
#include <string>
#include <vector>

#include "ast/node.h"
#include "offset_value.h"

namespace cent::ast {

struct TypeAlias : detail::Decl<TypeAlias> {
    [[nodiscard]] TypeAlias(
        std::size_t offset, OffsetValue<std::string> name,
        std::unique_ptr<Type> type, std::vector<Attribute> attributes,
        bool is_public = false)
    : Decl{offset, std::move(attributes), is_public}, name{std::move(name)},
      type{std::move(type)} {}

    OffsetValue<std::string> name;
    std::unique_ptr<Type> type;
};

} // namespace cent::ast

#endif
