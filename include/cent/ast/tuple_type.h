#ifndef CENT_AST_TUPLE_TYPE_H
#define CENT_AST_TUPLE_TYPE_H

#include <memory>
#include <utility>
#include <vector>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct TupleType : detail::Type<TupleType> {
    [[nodiscard]] TupleType(
        Span span, std::vector<std::unique_ptr<ast::Type>> types) noexcept
    : Type{span}, types{std::move(types)} {}

    std::vector<std::unique_ptr<ast::Type>> types;
};

} // namespace cent::ast

#endif
