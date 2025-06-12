#ifndef CENT_AST_VAR_DECL_H
#define CENT_AST_VAR_DECL_H

#include <cstdint>
#include <memory>
#include <string>
#include <utility>

#include "ast/node.h"
#include "offset_value.h"

namespace cent::ast {

struct VarDecl : detail::Decl<VarDecl> {
    enum struct Mut : std::uint8_t { Immut, Mut, Const };

    [[nodiscard]] VarDecl(
        std::size_t offset, Mut mutability, OffsetValue<std::string> name,
        std::unique_ptr<Type> type, std::unique_ptr<Expression> value,
        std::vector<Attribute> attributes)
    : Decl{offset, std::move(attributes)}, mutability{mutability},
      name{std::move(name)}, type{std::move(type)}, value{std::move(value)} {}

    Mut mutability;

    OffsetValue<std::string> name;
    std::unique_ptr<Type> type;

    std::unique_ptr<Expression> value;
};

} // namespace cent::ast

#endif
