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

    virtual bool is_bool() noexcept { return false; }
    virtual bool is_signed_int() noexcept { return false; };
    virtual bool is_unsigned_int() noexcept { return false; };
    virtual bool is_float() noexcept { return false; };
    virtual bool is_pointer() noexcept { return false; }
    virtual bool is_optional() noexcept { return false; }

    virtual bool is_function() noexcept { return false; }
    virtual bool is_struct() noexcept { return false; }
};

namespace detail {

template <typename Derived> struct Type : backend::Type {
    [[nodiscard]] llvm::Type*
    codegen(backend::Codegen& codegen) noexcept override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

} // namespace detail

} // namespace cent::backend

#endif
