#ifndef CENT_AST_NAMED_TYPE_H
#define CENT_AST_NAMED_TYPE_H

#include <string>

#include "ast/node.h"
#include "offset_value.h"

namespace cent::ast {

struct NamedType : detail::Type<NamedType> {
    [[nodiscard]] NamedType(
        std::size_t offset, std::vector<OffsetValue<std::string>> value,
        std::vector<std::unique_ptr<ast::Type>> template_args)
    : Type{offset}, value{std::move(value)},
      template_args{std::move(template_args)} {}

    std::vector<OffsetValue<std::string>> value;
    std::vector<std::unique_ptr<ast::Type>> template_args;
};

} // namespace cent::ast

#endif
