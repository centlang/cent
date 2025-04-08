#ifndef CENT_BACKEND_VARIABLE_H
#define CENT_BACKEND_VARIABLE_H

#include <llvm/IR/Value.h>

#include "cent/backend/value.h"

namespace cent::backend {

struct Variable {
    Value value;
    bool is_mutable;
};

} // namespace cent::backend

#endif
