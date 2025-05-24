#ifndef CENT_AST_TUPLE_TYPE_H
#define CENT_AST_TUPLE_TYPE_H

#include <memory>
#include <utility>
#include <vector>

#include "cent/ast/node.h"

namespace cent::ast {

struct TupleType : detail::Type<TupleType> {
    [[nodiscard]] TupleType(
        std::size_t offset, std::vector<std::unique_ptr<ast::Type>> types)
    : Type{offset}, types{std::move(types)} {}

    std::vector<std::unique_ptr<ast::Type>> types;
};

} // namespace cent::ast

#endif
