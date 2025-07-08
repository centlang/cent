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
        std::size_t offset, std::optional<OffsetValue<std::string>> type,
        OffsetValue<std::string> name, FnProto proto,
        std::unique_ptr<BlockStmt> block,
        std::vector<OffsetValue<std::string>> template_params,
        std::vector<Attribute> attributes, bool is_public = false)
    : Decl{offset, std::move(attributes), is_public}, type{std::move(type)},
      name{std::move(name)}, proto{std::move(proto)}, block{std::move(block)},
      template_params{std::move(template_params)} {}

    std::optional<OffsetValue<std::string>> type;
    OffsetValue<std::string> name;

    FnProto proto;
    std::unique_ptr<BlockStmt> block;

    std::vector<OffsetValue<std::string>> template_params;
};

} // namespace cent::ast

#endif
