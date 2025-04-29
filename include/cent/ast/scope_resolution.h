#ifndef CENT_AST_SCOPE_RESOLUTION_H
#define CENT_AST_SCOPE_RESOLUTION_H

#include <memory>
#include <utility>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct ScopeResolutionExpr : detail::Expr<ScopeResolutionExpr> {
    [[nodiscard]] ScopeResolutionExpr(
        Span span, SpanValue<std::string> name,
        std::unique_ptr<Expression> value) noexcept
    : Expr{span}, name{std::move(name)}, value{std::move(value)} {}

    SpanValue<std::string> name;
    std::unique_ptr<Expression> value;
};

struct ScopeResolutionType : detail::Type<ScopeResolutionType> {
    [[nodiscard]] ScopeResolutionType(
        Span span, SpanValue<std::string> name,
        std::unique_ptr<ast::Type> value) noexcept
    : Type{span}, name{std::move(name)}, value{std::move(value)} {}

    SpanValue<std::string> name;
    std::unique_ptr<ast::Type> value;
};

} // namespace cent::ast

#endif
