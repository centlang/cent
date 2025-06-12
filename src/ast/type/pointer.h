#ifndef CENT_AST_POINTER_H
#define CENT_AST_POINTER_H

#include <memory>

#include "ast/node.h"

namespace cent::ast {

struct Pointer : detail::Type<Pointer> {
    [[nodiscard]] Pointer(
        std::size_t offset, std::unique_ptr<ast::Type> type, bool is_mutable)
    : Type{offset}, type{std::move(type)}, is_mutable{is_mutable} {}

    std::unique_ptr<ast::Type> type;
    bool is_mutable;
};

} // namespace cent::ast

#endif
