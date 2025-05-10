#ifndef CENT_BACKEND_TYPES_STRUCT_H
#define CENT_BACKEND_TYPES_STRUCT_H

#include <memory>
#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Struct : detail::Type<Struct> {
    [[nodiscard]] Struct(
        llvm::StructType* type,
        std::vector<std::shared_ptr<backend::Type>> fields) noexcept
    : type{type}, fields{std::move(fields)} {}

    std::string to_string() noexcept override {
        std::string result = "struct {";

        for (std::size_t i = 0; i < fields.size(); ++i) {
            result += " " + fields[i]->to_string();

            if (i + 1 != fields.size()) {
                result += ",";
            }
        }

        return result + " }";
    }

    bool is_struct() noexcept override { return true; };

    llvm::StructType* type;
    std::vector<std::shared_ptr<backend::Type>> fields;
};

} // namespace cent::backend::types

#endif
