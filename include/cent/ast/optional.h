#ifndef CENT_AST_OPTIONAL_H
#define CENT_AST_OPTIONAL_H

#include <memory>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct Optional : detail::Type<Optional> {
    [[nodiscard]] Optional(Span span, std::unique_ptr<ast::Type> type) noexcept
    : Type{span}, type{std::move(type)} {}

    std::unique_ptr<ast::Type> type;
};

} // namespace cent::ast

#endif
