#ifndef CENT_AST_ARRAY_TYPE_H
#define CENT_AST_ARRAY_TYPE_H

#include <memory>
#include <utility>

#include "ast/node.h"

namespace cent::ast {

struct ArrayType : detail::Type<ArrayType> {
    [[nodiscard]] ArrayType(
        std::size_t offset, std::unique_ptr<ast::Type> type,
        std::unique_ptr<Expression> size)
    : Type{offset}, type{std::move(type)}, size{std::move(size)} {}

    std::unique_ptr<ast::Type> type;
    std::unique_ptr<Expression> size;
};

} // namespace cent::ast

#endif
