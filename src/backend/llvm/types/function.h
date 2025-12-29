#ifndef CENT_BACKEND_TYPES_FUNCTION_H
#define CENT_BACKEND_TYPES_FUNCTION_H

#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Function.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Function : detail::Ty<Function, Type::Kind::Function> {
    [[nodiscard]] Function(
        llvm::Type* llvm_type, Type* return_type,
        std::vector<Type*> param_types,
        std::vector<llvm::Constant*> default_args, bool variadic, bool sret)
    : Ty{llvm_type}, return_type{return_type},
      param_types{std::move(param_types)},
      default_args{std::move(default_args)}, variadic{variadic}, sret{sret} {}

    [[nodiscard]] std::string to_string() const override {
        std::string result = "fn(";

        for (std::size_t i = 0; i < param_types.size(); ++i) {
            result += param_types[i]->to_string();

            if (i + 1 != param_types.size()) {
                result += ", ";
            }
        }

        return result + ") " + return_type->to_string();
    }

    Type* return_type;
    std::vector<Type*> param_types;

    std::vector<llvm::Constant*> default_args;
    bool variadic;

    bool sret;
};

} // namespace cent::backend::types

#endif
