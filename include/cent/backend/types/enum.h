#ifndef CENT_BACKEND_TYPES_ENUM_H
#define CENT_BACKEND_TYPES_ENUM_H

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Enum : detail::Type<Enum> {
    [[nodiscard]] Enum(std::string name) noexcept : name{std::move(name)} {}

    std::string to_string() noexcept override { return name; }

    std::string name;
};

} // namespace cent::backend::types

#endif
