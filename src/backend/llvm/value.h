#ifndef CENT_BACKEND_VALUE_H
#define CENT_BACKEND_VALUE_H

#include <llvm/IR/Value.h>

namespace cent::backend {

struct Type;

struct Value {
    Type* type{nullptr};
    llvm::Value* value{nullptr};

    bool is_mutable = false;
    bool is_ref = false;
    bool is_deref = false;
    bool stack_allocated = false;

    [[nodiscard]] bool is_poisoned() const { return !type; }

    [[nodiscard]] static Value poisoned() { return {.type = nullptr}; }
};

} // namespace cent::backend

#endif
