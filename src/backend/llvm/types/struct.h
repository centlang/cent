#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Struct : detail::Type<Struct, Type::Kind::Struct> {
    [[nodiscard]] Struct(
        llvm::Type* llvm_type, std::string name,
        std::vector<backend::Type*> fields)
    : Type{llvm_type}, name{std::move(name)}, fields{std::move(fields)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
    std::vector<backend::Type*> fields;
};

} // namespace cent::backend::types

#endif
