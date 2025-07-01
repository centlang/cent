#ifndef CENT_BACKEND_TYPES_UNION_H
#define CENT_BACKEND_TYPES_UNION_H

#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "backend/llvm/type.h"
#include "backend/llvm/types/enum.h"

namespace cent::backend::types {

struct Union : detail::Type<Union, Type::Kind::Union> {
    [[nodiscard]] Union(
        llvm::Type* llvm_type, std::string name,
        std::vector<backend::Type*> fields, Enum* tag_type = nullptr)
    : Type{llvm_type}, name{std::move(name)}, fields{std::move(fields)},
      tag_type{tag_type} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
    std::vector<backend::Type*> fields;

    Enum* tag_type;
};

} // namespace cent::backend::types

#endif
