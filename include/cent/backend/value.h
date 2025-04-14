#ifndef CENT_BACKEND_VALUE_H
#define CENT_BACKEND_VALUE_H

#include <memory>

#include <llvm/IR/Value.h>

namespace cent::backend {

struct Type;

struct Value {
    std::shared_ptr<Type> type;
    llvm::Value* value;
};

} // namespace cent::backend

#endif
