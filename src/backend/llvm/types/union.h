#ifndef CENT_BACKEND_TYPES_UNION_H
#define CENT_BACKEND_TYPES_UNION_H

#include <memory>
#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Union : detail::Type<Union> {
    [[nodiscard]] Union(
        std::string name, llvm::StructType* type,
        std::vector<std::shared_ptr<backend::Type>> fields,
        std::shared_ptr<Enum> tag_type = nullptr)
    : name{std::move(name)}, type{type}, fields{std::move(fields)},
      tag_type{std::move(tag_type)} {}

    std::string to_string() override { return name; }

    bool is_union() override { return true; };

    std::string name;

    llvm::StructType* type;
    std::vector<std::shared_ptr<backend::Type>> fields;

    std::shared_ptr<Enum> tag_type;
};

} // namespace cent::backend::types

#endif
