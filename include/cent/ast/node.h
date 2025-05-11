#ifndef CENT_AST_NODE_H
#define CENT_AST_NODE_H

#include <memory>
#include <optional>

#include "cent/backend/codegen.h"
#include "cent/backend/type.h"
#include "cent/backend/value.h"

namespace cent::ast {

struct Node {
    [[nodiscard]] Node() noexcept = default;
    virtual ~Node() noexcept = default;

    Node(const Node&) = delete;
    Node(Node&&) = delete;

    auto operator=(const Node&) = delete;
    auto operator=(Node&&) = delete;
};

struct Type : Node {
    [[nodiscard]] Type(std::size_t offset) noexcept : offset{offset} {}

    virtual std::shared_ptr<backend::Type>
    codegen(backend::Codegen& codegen) noexcept = 0;

    std::size_t offset;
};

struct Statement : Node {
    [[nodiscard]] Statement(std::size_t offset) noexcept : offset{offset} {}

    virtual std::optional<backend::Value>
    codegen(backend::Codegen& codegen) noexcept = 0;

    std::size_t offset;
};

struct Expression : Statement {
    using Statement::Statement;
};

struct Declaration : Statement {
    [[nodiscard]] Declaration(
        std::size_t offset, bool is_public = false) noexcept
    : Statement{offset}, is_public{is_public} {}

    bool is_public;
};

namespace detail {

template <typename Derived> struct Type : ast::Type {
    using ast::Type::Type;

    std::shared_ptr<backend::Type>
    codegen(backend::Codegen& codegen) noexcept override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

template <typename Derived> struct Stmt : Statement {
    using Statement::Statement;

    std::optional<backend::Value>
    codegen(backend::Codegen& codegen) noexcept override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

template <typename Derived> struct Expr : Expression {
    using Expression::Expression;

    [[nodiscard]] std::optional<backend::Value>
    codegen(backend::Codegen& codegen) noexcept override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

template <typename Derived> struct Decl : Declaration {
    using Declaration::Declaration;

    std::optional<backend::Value>
    codegen(backend::Codegen& codegen) noexcept override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

} // namespace detail

} // namespace cent::ast

#endif
