#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <llvm/IR/DerivedTypes.h>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Struct : detail::Type<Struct> {
    [[nodiscard]] Struct(llvm::StructType* type) noexcept : type{type} {}

    llvm::StructType* type;
};

} // namespace cent::backend::types

#endif
