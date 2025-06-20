#ifndef CENT_AST_RANGE_TYPE_H
#define CENT_AST_RANGE_TYPE_H

#include <memory>

#include "ast/node.h"

namespace cent::ast {

struct RangeType : detail::Type<RangeType> {
    [[nodiscard]] RangeType(std::size_t offset, std::unique_ptr<ast::Type> type)
    : Type{offset}, type{std::move(type)} {}

    std::unique_ptr<ast::Type> type;
};

} // namespace cent::ast

#endif
