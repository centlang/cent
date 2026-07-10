#ifndef CENT_BACKEND_TYPES_UNION_H
#define CENT_BACKEND_TYPES_UNION_H

#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "backend/llvm/generics.h"
#include "backend/llvm/type.h"
#include "backend/llvm/types/enum.h"

namespace cent::backend::types {

struct Union : detail::Ty<Union, Type::Kind::Union> {
    [[nodiscard]] Union(
        llvm::Type* llvm_type, std::string name, std::vector<Type*> fields,
        Enum* tag_type = nullptr, bool implicit = false,
        GenericUnion* origin = nullptr, std::vector<Type*> origin_args = {})
    : Ty{llvm_type}, name{std::move(name)}, fields{std::move(fields)},
      tag_type{tag_type}, implicit{implicit}, origin{origin},
      origin_args{std::move(origin_args)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
    std::vector<Type*> fields;

    Enum* tag_type;
    bool implicit;

    GenericUnion* origin;
    std::vector<Type*> origin_args;
};

} // namespace cent::backend::types

#endif
