#ifndef CENT_AST_SIZEOF_EXPR_H
#define CENT_AST_SIZEOF_EXPR_H

#include <memory>
#include <utility>

#include "ast/node.h"

namespace cent::ast {

struct SizeofExpr : detail::Expr<SizeofExpr> {
    [[nodiscard]] SizeofExpr(std::size_t offset, std::unique_ptr<Type> type)
    : Expr{offset}, type{std::move(type)} {}

    std::unique_ptr<Type> type;
};

} // namespace cent::ast

#endif
