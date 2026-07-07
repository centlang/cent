#ifndef CENT_BACKEND_TYPES_GENERIC_H
#define CENT_BACKEND_TYPES_GENERIC_H

#include <string>
#include <vector>

#include "backend/llvm/generics.h"
#include "backend/llvm/type.h"

namespace cent::backend::types {

struct GenericStructInst
: detail::Ty<GenericStructInst, Type::Kind::GenericStructInst> {
    [[nodiscard]] GenericStructInst(
        GenericStruct* type, std::vector<Type*> args)
    : Ty{nullptr}, type{type}, args{std::move(args)} {}

    [[nodiscard]] std::string to_string() const override {
        std::string result = type->name + "(<";

        for (std::size_t i = 0; i < args.size(); ++i) {
            if (i > 0) {
                result += ",";
            }

            result += args[i]->to_string();
        }

        return result + ">)";
    }

    GenericStruct* type;
    std::vector<Type*> args;
};

struct GenericUnionInst
: detail::Ty<GenericUnionInst, Type::Kind::GenericUnionInst> {
    [[nodiscard]] GenericUnionInst(GenericUnion* type, std::vector<Type*> args)
    : Ty{nullptr}, type{type}, args{std::move(args)} {}

    [[nodiscard]] std::string to_string() const override {
        std::string result = type->name + "(<";

        for (std::size_t i = 0; i < args.size(); ++i) {
            if (i > 0) {
                result += ",";
            }

            result += args[i]->to_string();
        }

        return result + ">)";
    }

    GenericUnion* type;
    std::vector<Type*> args;
};

} // namespace cent::backend::types

#endif
