#ifndef CENT_AST_LITERALS_H
#define CENT_AST_LITERALS_H

#include <memory>
#include <string>
#include <vector>

#include "cent/ast/node.h"
#include "cent/offset_value.h"

#include "cent/ast/type/array_type.h"
#include "cent/ast/type/named_type.h"

namespace cent::ast {

struct IntLiteral : detail::Expr<IntLiteral> {
    [[nodiscard]] IntLiteral(std::size_t offset, std::string value)
    : Expr{offset}, value{std::move(value)} {}

    std::string value;
};

struct FloatLiteral : detail::Expr<FloatLiteral> {
    [[nodiscard]] FloatLiteral(std::size_t offset, std::string value)
    : Expr{offset}, value{std::move(value)} {}

    std::string value;
};

struct StrLiteral : detail::Expr<StrLiteral> {
    [[nodiscard]] StrLiteral(std::size_t offset, std::string value)
    : Expr{offset}, value{std::move(value)} {}

    std::string value;
};

struct BoolLiteral : detail::Expr<BoolLiteral> {
    [[nodiscard]] BoolLiteral(std::size_t offset, bool value)
    : Expr{offset}, value{value} {}

    bool value;
};

struct NullLiteral : detail::Expr<NullLiteral> {
    using Expr::Expr;
};

struct Undefined : detail::Expr<Undefined> {
    using Expr::Expr;
};

struct StructLiteral : detail::Expr<StructLiteral> {
    struct Field {
        OffsetValue<std::string> name;
        std::unique_ptr<Expression> value;
    };

    [[nodiscard]] StructLiteral(
        std::size_t offset, std::unique_ptr<NamedType> type,
        std::vector<Field> fields)
    : Expr{offset}, type{std::move(type)}, fields{std::move(fields)} {}

    std::unique_ptr<NamedType> type;
    std::vector<Field> fields;
};

struct ArrayLiteral : detail::Expr<ArrayLiteral> {
    [[nodiscard]] ArrayLiteral(
        std::size_t offset, std::unique_ptr<Type> type,
        std::vector<std::unique_ptr<Expression>> elements)
    : Expr{offset}, type{std::move(type)}, elements{std::move(elements)} {}

    std::unique_ptr<Type> type;
    std::vector<std::unique_ptr<Expression>> elements;
};

struct TupleLiteral : detail::Expr<TupleLiteral> {
    [[nodiscard]] TupleLiteral(
        std::size_t offset, std::vector<std::unique_ptr<Expression>> elements)
    : Expr{offset}, elements{std::move(elements)} {}

    std::vector<std::unique_ptr<Expression>> elements;
};

} // namespace cent::ast

#endif
