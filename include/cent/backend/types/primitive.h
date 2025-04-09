#ifndef CENT_BACKEND_TYPES_PRIMITIVE_H
#define CENT_BACKEND_TYPES_PRIMITIVE_H

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

struct F32 : detail::Type<F32> {};
struct F64 : detail::Type<F64> {};

struct Bool : detail::Type<Bool> {};
struct Void : detail::Type<Void> {};

} // namespace cent::backend::types

#endif
