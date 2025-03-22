#include <cstdint>

#include <llvm/IR/Constants.h>

#include "cent/ast/literals.h"
#include "cent/backend/codegen.h"

namespace cent {

llvm::Value* Codegen::generate(IntLiteral& expr) noexcept {
    return llvm::ConstantInt::getSigned(
        llvm::Type::getInt32Ty(m_context),
        from_string<std::int32_t>(expr.value));
}

llvm::Value* Codegen::generate(FloatLiteral& expr) noexcept {
    return llvm::ConstantFP::get(
        llvm::Type::getFloatTy(m_context), from_string<float>(expr.value));
}

llvm::Value* Codegen::generate(BoolLiteral& expr) noexcept {
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), expr.value);
}

} // namespace cent
