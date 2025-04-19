#ifndef CENT_BACKEND_TYPES_PRIMITIVE_H
#define CENT_BACKEND_TYPES_PRIMITIVE_H

#include <memory>
#include <utility>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct I8 : detail::Type<I8> {
    bool is_signed_int() noexcept override { return true; };
};

struct I16 : detail::Type<I16> {
    bool is_signed_int() noexcept override { return true; };
};

struct I32 : detail::Type<I32> {
    bool is_signed_int() noexcept override { return true; };
};

struct I64 : detail::Type<I64> {
    bool is_signed_int() noexcept override { return true; };
};

struct U8 : detail::Type<U8> {
    bool is_unsigned_int() noexcept override { return true; };
};

struct U16 : detail::Type<U16> {
    bool is_unsigned_int() noexcept override { return true; };
};

struct U32 : detail::Type<U32> {
    bool is_unsigned_int() noexcept override { return true; };
};

struct U64 : detail::Type<U64> {
    bool is_unsigned_int() noexcept override { return true; };
};

struct F32 : detail::Type<F32> {
    bool is_float() noexcept override { return true; };
};

struct F64 : detail::Type<F64> {
    bool is_float() noexcept override { return true; };
};

struct Bool : detail::Type<Bool> {
    bool is_bool() noexcept override { return true; };
};

struct Void : detail::Type<Void> {};

struct Pointer : detail::Type<Pointer> {
    [[nodiscard]] Pointer(
        std::shared_ptr<backend::Type> type, bool is_mutable) noexcept
    : type{std::move(type)}, is_mutable{is_mutable} {}

    bool is_pointer() noexcept override { return true; };

    std::shared_ptr<backend::Type> type;
    bool is_mutable;
};

} // namespace cent::backend::types

#endif
