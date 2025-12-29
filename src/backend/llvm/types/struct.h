#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Struct : detail::Ty<Struct, Type::Kind::Struct> {
    [[nodiscard]] Struct(
        llvm::Type* llvm_type, std::string name, std::vector<Type*> fields,
        bool has_tail)
    : Ty{llvm_type}, name{std::move(name)}, fields{std::move(fields)},
      has_tail{has_tail} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
    std::vector<Type*> fields;

    bool has_tail;
};

} // namespace cent::backend::types

#endif
