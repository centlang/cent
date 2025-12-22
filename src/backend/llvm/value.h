#ifndef CENT_BACKEND_VALUE_H
#define CENT_BACKEND_VALUE_H

#include <llvm/IR/Value.h>

namespace cent::backend {

struct Type;

struct Value {
    Type* type{nullptr};
    llvm::Value* value{nullptr};

    std::uint8_t ptr_depth = 0;

    bool is_mutable = false;
    bool memcpy = false;

    [[nodiscard]] bool ok() const { return type; }

    [[nodiscard]] static Value poisoned() { return {.type = nullptr}; }
};

} // namespace cent::backend

#endif
