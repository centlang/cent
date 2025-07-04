#ifndef CENT_BACKEND_TYPES_ALIAS_H
#define CENT_BACKEND_TYPES_ALIAS_H

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Alias : detail::Ty<Alias, Type::Kind::Alias> {
    [[nodiscard]] Alias(
        llvm::Type* llvm_type, std::string name, Type* type, bool distinct)
    : Ty{llvm_type}, name{std::move(name)}, type{type}, distinct{distinct} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;

    Type* type;
    bool distinct;
};

} // namespace cent::backend::types

#endif
