#ifndef CENT_BACKEND_TYPES_FUNCTION_H
#define CENT_BACKEND_TYPES_FUNCTION_H

#include <memory>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Function.h>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Function : detail::Type<Function> {
    [[nodiscard]] Function(
        std::shared_ptr<backend::Type> return_type,
        std::vector<std::shared_ptr<backend::Type>> param_types,
        std::vector<llvm::Constant*> default_args) noexcept
    : return_type{std::move(return_type)}, param_types{std::move(param_types)},
      default_args{std::move(default_args)} {}

    std::string to_string() noexcept override {
        std::string result = "fn(";

        for (std::size_t i = 0; i < param_types.size(); ++i) {
            result += param_types[i]->to_string();

            if (i + 1 != param_types.size()) {
                result += ", ";
            }
        }

        return result + ") " + return_type->to_string();
    }

    bool is_function() noexcept override { return true; };

    std::shared_ptr<backend::Type> return_type;
    std::vector<std::shared_ptr<backend::Type>> param_types;

    std::vector<llvm::Constant*> default_args;
};

} // namespace cent::backend::types

#endif
