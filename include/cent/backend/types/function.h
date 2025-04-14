#ifndef CENT_BACKEND_TYPES_FUNCTION_H
#define CENT_BACKEND_TYPES_FUNCTION_H

#include <memory>
#include <vector>

#include <llvm/IR/DerivedTypes.h>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct Function : detail::Type<Function> {
    [[nodiscard]] Function(
        llvm::FunctionType* function,
        std::shared_ptr<backend::Type> return_type,
        std::vector<std::shared_ptr<backend::Type>> param_types) noexcept
    : function{function}, return_type{std::move(return_type)},
      param_types{std::move(param_types)} {}

    llvm::FunctionType* function;

    std::shared_ptr<backend::Type> return_type;
    std::vector<std::shared_ptr<backend::Type>> param_types;
};

} // namespace cent::backend::types

#endif
