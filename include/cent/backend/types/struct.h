#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Struct : detail::Type<Struct> {
    [[nodiscard]] Struct(
        llvm::StructType* type, std::vector<backend::Type*> fields) noexcept
    : type{type}, fields{std::move(fields)} {}

    llvm::StructType* type;
    std::vector<backend::Type*> fields;
};

} // namespace cent::backend::types

#endif
