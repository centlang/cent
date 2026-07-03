#ifndef CENT_BACKEND_TYPES_TYPE_PARAM_H
#define CENT_BACKEND_TYPES_TYPE_PARAM_H

#include <string>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct TypeParam : detail::Ty<TypeParam, Type::Kind::TypeParam> {
    [[nodiscard]] TypeParam(std::string name)
    : Ty{nullptr}, name{std::move(name)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
};

} // namespace cent::backend::types

#endif
