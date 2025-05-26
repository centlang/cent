#ifndef CENT_BACKEND_TYPE_H
#define CENT_BACKEND_TYPE_H

#include <string>

#include <llvm/IR/Type.h>

#include "cent/backend/llvm/codegen.h"

namespace cent::backend {

struct Type {
    [[nodiscard]] Type() = default;
    virtual ~Type() = default;

    Type(const Type&) = delete;
    Type(Type&&) = delete;

    auto operator=(const Type&) = delete;
    auto operator=(Type&&) = delete;

    virtual llvm::Type* codegen(backend::Codegen& codegen) = 0;
    virtual std::string to_string() = 0;

    virtual bool is_bool() { return false; }
    virtual bool is_null() { return false; }
    virtual bool is_signed_int() { return false; };
    virtual bool is_unsigned_int() { return false; };
    virtual bool is_float() { return false; };
    virtual bool is_pointer() { return false; }
    virtual bool is_optional() { return false; }
    virtual bool is_array() { return false; }
    virtual bool is_slice() { return false; }
    virtual bool is_tuple() { return false; }

    virtual bool is_function() { return false; }
    virtual bool is_struct() { return false; }
    virtual bool is_enum() { return false; }
};

namespace detail {

template <typename Derived> struct Type : backend::Type {
    [[nodiscard]] llvm::Type* codegen(backend::Codegen& codegen) override {
        return codegen.generate(static_cast<Derived&>(*this));
    }
};

} // namespace detail

} // namespace cent::backend

#endif
