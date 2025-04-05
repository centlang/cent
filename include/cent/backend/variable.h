#ifndef CENT_BACKEND_VARIABLE_H
#define CENT_BACKEND_VARIABLE_H

#include <llvm/IR/Value.h>

namespace cent::backend {

struct Variable {
    llvm::Value* value;
    bool is_mutable;
};

} // namespace cent::backend

#endif
