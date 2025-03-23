#include <cstdint>

#include <llvm/IR/Constants.h>

#include "cent/ast/binary_expr.h"
#include "cent/ast/literals.h"

#include "cent/backend/codegen.h"

namespace cent {

llvm::Value* Codegen::generate(BinaryExpr& expr) noexcept {
    using enum Token::Type;

    auto* lhs = expr.lhs->codegen(*this);
    auto* rhs = expr.rhs->codegen(*this);

    switch (expr.oper.value) {
    case Plus:
        return m_builder.CreateAdd(lhs, rhs);
    case Minus:
        return m_builder.CreateSub(lhs, rhs);
    case Star:
        return m_builder.CreateMul(lhs, rhs);
    case Slash:
        return m_builder.CreateSDiv(lhs, rhs);
    case And:
        return m_builder.CreateAnd(lhs, rhs);
    case Or:
        return m_builder.CreateOr(lhs, rhs);
    case Less:
        return m_builder.CreateICmpSLT(lhs, rhs);
    case Greater:
        return m_builder.CreateICmpSGT(lhs, rhs);
    case EqualEqual:
        return m_builder.CreateICmpEQ(lhs, rhs);
    case BangEqual:
        return m_builder.CreateICmpNE(lhs, rhs);
    case GreaterEqual:
        return m_builder.CreateICmpSGE(lhs, rhs);
    case LessEqual:
        return m_builder.CreateICmpSLE(lhs, rhs);
    default:
        return nullptr;
    }
}

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
