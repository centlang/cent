#ifndef CENT_AST_ARRAY_TYPE_H
#define CENT_AST_ARRAY_TYPE_H

#include <memory>
#include <utility>

#include "cent/ast/literals.h"
#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent::ast {

struct ArrayType : detail::Type<ArrayType> {
    [[nodiscard]] ArrayType(
        Span span, std::unique_ptr<ast::Type> type,
        std::unique_ptr<IntLiteral> size) noexcept
    : Type{span}, type{std::move(type)}, size{std::move(size)} {}

    std::unique_ptr<ast::Type> type;
    std::unique_ptr<IntLiteral> size;
};

} // namespace cent::ast

#endif
