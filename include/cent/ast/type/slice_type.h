#ifndef CENT_AST_SLICE_TYPE_H
#define CENT_AST_SLICE_TYPE_H

#include <memory>
#include <utility>

#include "cent/ast/node.h"

namespace cent::ast {

struct SliceType : detail::Type<SliceType> {
    [[nodiscard]] SliceType(
        std::size_t offset, std::unique_ptr<ast::Type> type, bool is_mutable)
    : Type{offset}, type{std::move(type)}, is_mutable{is_mutable} {}

    std::unique_ptr<ast::Type> type;
    bool is_mutable;
};

} // namespace cent::ast

#endif
