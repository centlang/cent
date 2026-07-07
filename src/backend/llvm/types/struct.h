#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "backend/llvm/generics.h"
#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Struct : detail::Ty<Struct, Type::Kind::Struct> {
    [[nodiscard]] Struct(
        llvm::Type* llvm_type, std::string name, std::vector<Type*> fields,
        bool has_tail, GenericStruct* origin = nullptr,
        std::vector<Type*> origin_args = {})
    : Ty{llvm_type}, name{std::move(name)}, fields{std::move(fields)},
      has_tail{has_tail}, origin{origin}, origin_args{std::move(origin_args)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
    std::vector<Type*> fields;

    bool has_tail;

    GenericStruct* origin;
    std::vector<Type*> origin_args;
};

} // namespace cent::backend::types

#endif
