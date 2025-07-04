#ifndef CENT_BACKEND_TYPES_ENUM_H
#define CENT_BACKEND_TYPES_ENUM_H

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Enum : detail::Ty<Enum, Type::Kind::Enum> {
    [[nodiscard]] Enum(llvm::Type* llvm_type, std::string name, Type* type)
    : Ty{llvm_type}, name{std::move(name)}, type{type} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
    Type* type;
};

} // namespace cent::backend::types

#endif
