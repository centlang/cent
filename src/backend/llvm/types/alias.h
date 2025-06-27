#ifndef CENT_BACKEND_TYPES_ALIAS_H
#define CENT_BACKEND_TYPES_ALIAS_H

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Alias : detail::Type<Alias, Type::Kind::Alias> {
    [[nodiscard]] Alias(
        llvm::Type* llvm_type, std::string name, backend::Type* type,
        bool distinct)
    : Type{llvm_type}, name{std::move(name)}, type{type}, distinct{distinct} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;

    backend::Type* type;
    bool distinct;
};

} // namespace cent::backend::types

#endif
