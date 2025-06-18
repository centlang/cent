#ifndef CENT_BACKEND_TYPES_ENUM_H
#define CENT_BACKEND_TYPES_ENUM_H

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Enum : detail::Type<Enum, Type::Kind::Enum> {
    [[nodiscard]] Enum(std::string name, std::shared_ptr<backend::Type> type)
    : name{std::move(name)}, type{std::move(type)} {}

    std::string to_string() override { return name; }

    std::string name;
    std::shared_ptr<backend::Type> type;
};

} // namespace cent::backend::types

#endif
