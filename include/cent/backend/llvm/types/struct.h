#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <memory>
#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "cent/backend/llvm/type.h"

namespace cent::backend::types {

struct Struct : detail::Type<Struct> {
    [[nodiscard]] Struct(
        std::string name, llvm::StructType* type,
        std::vector<std::shared_ptr<backend::Type>> fields)
    : name{std::move(name)}, type{type}, fields{std::move(fields)} {}

    std::string to_string() override { return name; }

    bool is_struct() override { return true; };

    std::string name;

    llvm::StructType* type;
    std::vector<std::shared_ptr<backend::Type>> fields;
};

} // namespace cent::backend::types

#endif
