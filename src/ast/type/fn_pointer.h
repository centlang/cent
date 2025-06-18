#ifndef CENT_AST_FN_POINTER_H
#define CENT_AST_FN_POINTER_H

#include <utility>

#include "ast/fn_proto.h"
#include "ast/node.h"

namespace cent::ast {

struct FnPointer : detail::Type<FnPointer> {
    [[nodiscard]] FnPointer(std::size_t offset, FnProto proto)
    : Type{offset}, proto{std::move(proto)} {}

    FnProto proto;
};

} // namespace cent::ast

#endif
