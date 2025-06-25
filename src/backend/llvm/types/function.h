#ifndef CENT_BACKEND_TYPES_FUNCTION_H
#define CENT_BACKEND_TYPES_FUNCTION_H

#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Function.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct Function : detail::Type<Function, Type::Kind::Function> {
    [[nodiscard]] Function(
        llvm::Type* llvm_type, backend::Type* return_type,
        std::vector<backend::Type*> param_types,
        std::vector<llvm::Constant*> default_args, bool variadic)
    : Type{llvm_type}, return_type{return_type},
      param_types{std::move(param_types)},
      default_args{std::move(default_args)}, variadic{variadic} {}

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

    backend::Type* return_type;
    std::vector<backend::Type*> param_types;

    std::vector<llvm::Constant*> default_args;
    bool variadic;
};

} // namespace cent::backend::types

#endif
