#ifndef CENT_BACKEND_VALUE_H
#define CENT_BACKEND_VALUE_H

#include <llvm/IR/Value.h>

#include "cent/backend/type.h"

namespace cent::backend {

struct Value {
    Type* type;
    llvm::Value* value;
};

} // namespace cent::backend

#endif
