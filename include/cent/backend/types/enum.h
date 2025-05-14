#ifndef CENT_BACKEND_TYPES_ENUM_H
#define CENT_BACKEND_TYPES_ENUM_H

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Enum : detail::Type<Enum> {
    [[nodiscard]] Enum(
        std::string name, std::shared_ptr<backend::Type> type) noexcept
    : name{std::move(name)}, type{std::move(type)} {}

    std::string to_string() noexcept override { return name; }

    bool is_enum() noexcept override { return true; };

    std::string name;
    std::shared_ptr<backend::Type> type;
};

} // namespace cent::backend::types

#endif
