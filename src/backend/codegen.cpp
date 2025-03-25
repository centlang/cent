#include <cstdint>

#include <llvm/IR/Constants.h>

#include "cent/ast/binary_expr.h"
#include "cent/ast/block_stmt.h"
#include "cent/ast/fn_decl.h"
#include "cent/ast/literals.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/unary_expr.h"

#include "cent/backend/codegen.h"

namespace cent {

llvm::Value* Codegen::generate(BlockStmt& stmt) noexcept {
    for (auto& statement : stmt.body) {
        statement->codegen(*this);
    }

    return nullptr;
}

llvm::Value* Codegen::generate(ReturnStmt& stmt) noexcept {
    m_builder.CreateRet(stmt.value->codegen(*this));

    return nullptr;
}

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

llvm::Value* Codegen::generate(UnaryExpr& expr) noexcept {
    using enum Token::Type;

    auto* value = expr.value->codegen(*this);

    switch (expr.oper.value) {
    case Minus:
        return m_builder.CreateNeg(value);
    case Bang:
        return m_builder.CreateNot(value);
    default:
        return nullptr;
    }
}

llvm::Value* Codegen::generate(IntLiteral& expr) noexcept {
    return llvm::ConstantInt::getSigned(
        get_i32_type(), from_string<std::int32_t>(expr.value));
}

llvm::Value* Codegen::generate(FloatLiteral& expr) noexcept {
    return llvm::ConstantFP::get(
        get_f32_type(), from_string<float>(expr.value));
}

llvm::Value* Codegen::generate(BoolLiteral& expr) noexcept {
    return llvm::ConstantInt::get(get_bool_type(), expr.value);
}

void Codegen::generate_fn_proto(FnDecl& decl) noexcept {
    llvm::Function::Create(
        get_fn_type(decl), llvm::Function::ExternalLinkage,
        decl.proto.name.value, *m_module);
}

llvm::FunctionType* Codegen::get_fn_type(FnDecl& decl) noexcept {
    auto* return_type = get_type(decl.proto.return_type.value);

    std::vector<llvm::Type*> param_types;
    param_types.reserve(decl.proto.params.size());

    for (const auto& parameter : decl.proto.params) {
        param_types.push_back(get_type(parameter.type.value));
    }

    return llvm::FunctionType::get(return_type, param_types, false);
}

} // namespace cent
