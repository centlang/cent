#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <memory>
#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Struct : detail::Type<Struct, Type::Kind::Struct> {
    [[nodiscard]] Struct(
        std::string name, llvm::StructType* type,
        std::vector<std::shared_ptr<backend::Type>> fields)
    : name{std::move(name)}, type{type}, fields{std::move(fields)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;

    llvm::StructType* type;
    std::vector<std::shared_ptr<backend::Type>> fields;
};

} // namespace cent::backend::types

#endif
