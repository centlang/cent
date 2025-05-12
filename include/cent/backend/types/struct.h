#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <memory>
#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Struct : detail::Type<Struct> {
    [[nodiscard]] Struct(
        std::string name, llvm::StructType* type,
        std::vector<std::shared_ptr<backend::Type>> fields) noexcept
    : name{std::move(name)}, type{type}, fields{std::move(fields)} {}

    std::string to_string() noexcept override { return name; }

    bool is_struct() noexcept override { return true; };

    std::string name;

    llvm::StructType* type;
    std::vector<std::shared_ptr<backend::Type>> fields;
};

} // namespace cent::backend::types

#endif
