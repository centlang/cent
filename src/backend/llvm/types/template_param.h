#ifndef CENT_BACKEND_TYPES_TEMPLATE_PARAM_H
#define CENT_BACKEND_TYPES_TEMPLATE_PARAM_H

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct TemplateParam : detail::Type<TemplateParam, Type::Kind::TemplateParam> {
    [[nodiscard]] TemplateParam(std::string name)
    : Type{nullptr}, name{std::move(name)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
};

} // namespace cent::backend::types

#endif
