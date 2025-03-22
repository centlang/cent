#ifndef CENT_AST_LITERALS_H
#define CENT_AST_LITERALS_H

#include <string_view>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent {

struct IntLiteral : detail::Expr<IntLiteral> {
    [[nodiscard]] IntLiteral(Span span, std::string_view value) noexcept
    : Expr{span}, value{value} {}

    std::string_view value;
};

struct FloatLiteral : detail::Expr<FloatLiteral> {
    [[nodiscard]] FloatLiteral(Span span, std::string_view value) noexcept
    : Expr{span}, value{value} {}

    std::string_view value;
};

struct BoolLiteral : detail::Expr<BoolLiteral> {
    [[nodiscard]] BoolLiteral(Span span, bool value) noexcept
    : Expr{span}, value{value} {}

    bool value;
};

} // namespace cent

#endif
