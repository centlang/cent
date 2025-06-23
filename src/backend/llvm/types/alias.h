#ifndef CENT_BACKEND_TYPES_ALIAS_H
#define CENT_BACKEND_TYPES_ALIAS_H

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Alias : detail::Type<Alias, Type::Kind::Alias> {
    [[nodiscard]] Alias(std::string name, std::shared_ptr<backend::Type> type)
    : name{std::move(name)}, type{std::move(type)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
    std::shared_ptr<backend::Type> type;
};

} // namespace cent::backend::types

#endif
