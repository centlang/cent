#ifndef CENT_BACKEND_TYPES_PRIMITIVE_H
#define CENT_BACKEND_TYPES_PRIMITIVE_H

#include "cent/backend/type.h"

namespace cent::backend::types {

struct I8 : detail::Type<I8> {};
struct I16 : detail::Type<I16> {};
struct I32 : detail::Type<I32> {};
struct I64 : detail::Type<I64> {};

struct F32 : detail::Type<F32> {};
struct F64 : detail::Type<F64> {};

struct Bool : detail::Type<Bool> {};
struct Void : detail::Type<Void> {};

} // namespace cent::backend::types

#endif
