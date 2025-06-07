#ifndef CENT_AST_NODE_H
#define CENT_AST_NODE_H

#include <memory>
#include <optional>
#include <vector>

#include "cent/backend/llvm/codegen.h"
#include "cent/backend/llvm/type.h"
#include "cent/backend/llvm/value.h"

#include "cent/ast/attribute.h"

namespace cent::ast {

struct Node {
    [[nodiscard]] Node() = default;
    virtual ~Node() = default;

    Node(const Node&) = delete;
    Node(Node&&) = delete;

    auto operator=(const Node&) = delete;
    auto operator=(Node&&) = delete;
};

struct Type : Node {
    [[nodiscard]] Type(std::size_t offset) : offset{offset} {}

    virtual std::shared_ptr<backend::Type>
    codegen(backend::Codegen& codegen) = 0;

    std::size_t offset;
};

struct Statement : Node {
    [[nodiscard]] Statement(std::size_t offset) : offset{offset} {}

    virtual std::optional<backend::Value>
    codegen(backend::Codegen& codegen) = 0;

    std::size_t offset;
};

struct Expression : Statement {
    using Statement::Statement;
};

struct Declaration : Statement {
    [[nodiscard]] Declaration(
        std::size_t offset, std::vector<Attribute> attributes,
        bool is_public = false)
    : Statement{offset}, attributes{std::move(attributes)},
      is_public{is_public} {}

    std::vector<Attribute> attributes;
    bool is_public;
};

namespace detail {

template <typename Derived> struct Type : ast::Type {
    using ast::Type::Type;

    std::shared_ptr<backend::Type> codegen(backend::Codegen& codegen) override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

template <typename Derived> struct Stmt : Statement {
    using Statement::Statement;

    std::optional<backend::Value> codegen(backend::Codegen& codegen) override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

template <typename Derived> struct Expr : Expression {
    using Expression::Expression;

    [[nodiscard]] std::optional<backend::Value>
    codegen(backend::Codegen& codegen) override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

template <typename Derived> struct Decl : Declaration {
    using Declaration::Declaration;

    std::optional<backend::Value> codegen(backend::Codegen& codegen) override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

} // namespace detail

} // namespace cent::ast

#endif
