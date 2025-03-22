#ifndef CENT_AST_VAR_DECL_H
#define CENT_AST_VAR_DECL_H

#include <memory>
#include <string_view>
#include <utility>

#include "cent/span.h"

#include "cent/ast/node.h"
#include "cent/ast/span_value.h"

namespace cent {

struct VarDecl : detail::Decl<VarDecl> {
    [[nodiscard]] VarDecl(
        Span span, bool is_mutable, SpanValue<std::string_view> name,
        SpanValue<std::string_view> type,
        std::unique_ptr<Expression> value) noexcept
    : Decl{span}, is_mutable{is_mutable}, name{name}, type{type},
      value{std::move(value)} {}

    bool is_mutable;

    SpanValue<std::string_view> name;
    SpanValue<std::string_view> type;

    std::unique_ptr<Expression> value;
};

} // namespace cent

#endif
