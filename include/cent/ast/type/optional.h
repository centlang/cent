#ifndef CENT_AST_OPTIONAL_H
#define CENT_AST_OPTIONAL_H

#include <memory>

#include "cent/ast/node.h"

namespace cent::ast {

struct Optional : detail::Type<Optional> {
    [[nodiscard]] Optional(std::size_t offset, std::unique_ptr<ast::Type> type)
    : Type{offset}, type{std::move(type)} {}

    std::unique_ptr<ast::Type> type;
};

} // namespace cent::ast

#endif
