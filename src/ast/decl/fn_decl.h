#ifndef CENT_AST_FN_DECL_H
#define CENT_AST_FN_DECL_H

#include <memory>
#include <utility>
#include <vector>

#include "ast/node.h"

#include "ast/fn_proto.h"
#include "ast/stmt/block_stmt.h"

namespace cent::ast {

struct FnDecl : detail::Decl<FnDecl> {
    [[nodiscard]] FnDecl(
        std::size_t offset, OffsetValue<std::string> name, FnProto proto,
        std::unique_ptr<BlockStmt> block,
        std::vector<OffsetValue<std::string>> type_params,
        std::vector<Attribute> attributes, bool is_public = false)
    : Decl{offset, std::move(attributes), is_public}, name{std::move(name)},
      proto{std::move(proto)}, block{std::move(block)},
      type_params{std::move(type_params)} {}

    OffsetValue<std::string> name;

    FnProto proto;
    std::unique_ptr<BlockStmt> block;

    std::vector<OffsetValue<std::string>> type_params;
};

} // namespace cent::ast

#endif
