#ifndef CENT_AST_FN_DECL_H
#define CENT_AST_FN_DECL_H

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "offset_value.h"

#include "ast/node.h"
#include "ast/stmt/block_stmt.h"

namespace cent::ast {

struct FnDecl : detail::Decl<FnDecl> {
    struct Param {
        OffsetValue<std::string> name;
        std::unique_ptr<Type> type;

        std::unique_ptr<Expression> value;
        bool is_mutable;
    };

    struct Proto {
        std::optional<OffsetValue<std::string>> type;
        OffsetValue<std::string> name;

        std::vector<Param> params;
        std::unique_ptr<Type> return_type;

        bool variadic;
    };

    [[nodiscard]] FnDecl(
        std::size_t offset, Proto proto, std::unique_ptr<BlockStmt> block,
        std::vector<Attribute> attributes, bool is_public = false)
    : Decl{offset, std::move(attributes), is_public}, proto{std::move(proto)},
      block{std::move(block)} {}

    Proto proto;
    std::unique_ptr<BlockStmt> block;
};

} // namespace cent::ast

#endif
