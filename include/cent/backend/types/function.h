#ifndef CENT_BACKEND_TYPES_FUNCTION_H
#define CENT_BACKEND_TYPES_FUNCTION_H

#include <vector>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Function : detail::Type<Function> {
    [[nodiscard]] Function(
        Type* return_type, std::vector<Type*> param_types) noexcept
    : return_type{return_type}, param_types{std::move(param_types)} {}

    Type* return_type;
    std::vector<Type*> param_types;
};

} // namespace cent::backend::types

#endif
