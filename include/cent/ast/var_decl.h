#ifndef CENT_AST_VAR_DECL_H
#define CENT_AST_VAR_DECL_H

#include <memory>
#include <string>
#include <utility>

#include "cent/ast/node.h"
#include "cent/offset_value.h"

namespace cent::ast {

struct VarDecl : detail::Decl<VarDecl> {
    [[nodiscard]] VarDecl(
        std::size_t offset, bool is_mutable, OffsetValue<std::string> name,
        std::unique_ptr<Type> type, std::unique_ptr<Expression> value) noexcept
    : Decl{offset}, is_mutable{is_mutable}, name{std::move(name)},
      type{std::move(type)}, value{std::move(value)} {}

    bool is_mutable;

    OffsetValue<std::string> name;
    std::unique_ptr<Type> type;

    std::unique_ptr<Expression> value;
};

} // namespace cent::ast

#endif
