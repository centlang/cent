#ifndef CENT_AST_POINTER_H
#define CENT_AST_POINTER_H

#include <memory>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct Pointer : detail::Type<Pointer> {
    [[nodiscard]] Pointer(
        Span span, std::unique_ptr<ast::Type> type, bool is_mutable) noexcept
    : Type{span}, type{std::move(type)}, is_mutable{is_mutable} {}

    std::unique_ptr<ast::Type> type;
    bool is_mutable;
};

} // namespace cent::ast

#endif
