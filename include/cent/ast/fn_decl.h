#ifndef CENT_AST_FN_DECL_H
#define CENT_AST_FN_DECL_H

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "cent/span.h"

#include "cent/ast/block_stmt.h"
#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent::ast {

struct FnDecl : detail::Decl<FnDecl> {
    struct Param {
        SpanValue<std::string> name;
        std::unique_ptr<Type> type;
    };

    struct Proto {
        SpanValue<std::string> name;

        std::vector<Param> params;
        std::unique_ptr<Type> return_type;
    };

    [[nodiscard]] FnDecl(
        Span span, Proto proto, std::unique_ptr<BlockStmt> block,
        bool is_public = false, bool is_extern = false) noexcept
    : Decl{span, is_public}, proto{std::move(proto)}, block{std::move(block)},
      is_extern{is_extern} {}

    Proto proto;
    std::unique_ptr<BlockStmt> block;

    bool is_extern;
};

} // namespace cent::ast

#endif
