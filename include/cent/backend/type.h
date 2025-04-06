#ifndef CENT_BACKEND_TYPE_H
#define CENT_BACKEND_TYPE_H

#include <llvm/IR/Type.h>

#include "cent/backend/codegen.h"

namespace cent::backend {

struct Type {
    [[nodiscard]] Type() noexcept = default;
    virtual ~Type() noexcept = default;

    Type(const Type&) = delete;
    Type(Type&&) = delete;

    auto operator=(const Type&) = delete;
    auto operator=(Type&&) = delete;

    virtual llvm::Type* codegen(backend::Codegen& codegen) noexcept = 0;
};

} // namespace cent::backend

#endif
