#ifndef CENT_AST_LITERALS_H
#define CENT_AST_LITERALS_H

#include <string_view>

#include "cent/ast/node.h"
#include "cent/span.h"

namespace cent {

struct IntLiteral : Expression {
    [[nodiscard]] IntLiteral(Span span, std::string_view value) noexcept
    : Expression{span}, value{value} {}

    std::string_view value;
};

struct FloatLiteral : Expression {
    [[nodiscard]] FloatLiteral(Span span, std::string_view value) noexcept
    : Expression{span}, value{value} {}

    std::string_view value;
};

struct BoolLiteral : Expression {
    [[nodiscard]] BoolLiteral(Span span, bool value) noexcept
    : Expression{span}, value{value} {}

    bool value;
};

} // namespace cent

#endif
