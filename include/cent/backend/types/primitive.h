#ifndef CENT_BACKEND_TYPES_PRIMITIVE_H
#define CENT_BACKEND_TYPES_PRIMITIVE_H

#include "cent/backend/type.h"

namespace cent::backend::types {

struct I32 : detail::Type<I32> {};

struct F32 : detail::Type<F32> {};

struct Bool : detail::Type<Bool> {};

struct Void : detail::Type<Void> {};

} // namespace cent::backend::types

#endif
