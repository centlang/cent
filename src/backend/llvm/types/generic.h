#ifndef CENT_BACKEND_TYPES_TEMPLATE_PARAM_H
#define CENT_BACKEND_TYPES_TEMPLATE_PARAM_H

#include "backend/llvm/generic_types.h"
#include "backend/llvm/type.h"

namespace cent::backend::types {

struct TemplateParam : detail::Type<TemplateParam, Type::Kind::TemplateParam> {
    [[nodiscard]] TemplateParam(std::string name)
    : Type{nullptr}, name{std::move(name)} {}

    [[nodiscard]] std::string to_string() const override { return name; }

    std::string name;
};

struct TemplateStructInst
: detail::Type<TemplateStructInst, Type::Kind::TemplateStructInst> {
    [[nodiscard]] TemplateStructInst(
        GenericStruct* type, std::vector<backend::Type*> args)
    : Type{nullptr}, type{type}, args{std::move(args)} {}

    [[nodiscard]] std::string to_string() const override { return type->name; }

    GenericStruct* type;
    std::vector<backend::Type*> args;
};

struct TemplateUnionInst
: detail::Type<TemplateUnionInst, Type::Kind::TemplateUnionInst> {
    [[nodiscard]] TemplateUnionInst(
        GenericUnion* type, std::vector<backend::Type*> args)
    : Type{nullptr}, type{type}, args{std::move(args)} {}

    [[nodiscard]] std::string to_string() const override { return type->name; }

    GenericUnion* type;
    std::vector<backend::Type*> args;
};

} // namespace cent::backend::types

#endif
