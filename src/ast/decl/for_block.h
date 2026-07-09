#ifndef CENT_AST_FOR_BLOCK_H
#define CENT_AST_FOR_BLOCK_H

#include <memory>
#include <string>
#include <vector>

#include "ast/node.h"
#include "offset_value.h"

#include "ast/decl/fn_decl.h"

namespace cent::ast {

struct ForBlock : detail::Decl<ForBlock> {
    [[nodiscard]] ForBlock(
        std::size_t offset, std::vector<OffsetValue<std::string>> type_params,
        OffsetValue<std::string> type_name,
        std::vector<std::unique_ptr<FnDecl>> fns,
        std::vector<Attribute> attributes, bool is_public = false)
    : Decl{offset, std::move(attributes), is_public},
      type_params{std::move(type_params)}, type{std::move(type_name)},
      fns{std::move(fns)} {}

    std::vector<OffsetValue<std::string>> type_params;
    OffsetValue<std::string> type;

    std::vector<std::unique_ptr<FnDecl>> fns;
};

} // namespace cent::ast

#endif
