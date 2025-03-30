#include <cstdint>

#include <llvm/IR/Constants.h>

#include "cent/ast/assignment.h"
#include "cent/ast/binary_expr.h"
#include "cent/ast/block_stmt.h"
#include "cent/ast/call_expr.h"
#include "cent/ast/fn_decl.h"
#include "cent/ast/identifier.h"
#include "cent/ast/if_else.h"
#include "cent/ast/literals.h"
#include "cent/ast/program.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/unary_expr.h"
#include "cent/ast/var_decl.h"

#include "cent/backend/codegen.h"

namespace cent {

std::unique_ptr<llvm::Module> Codegen::generate() noexcept {
    for (auto& function : m_program->functions) {
        generate_fn_proto(*function);
    }

    for (auto& function : m_program->functions) {
        function->codegen(*this);
    }

    return std::move(m_module);
}

llvm::Value* Codegen::generate(Assignment& stmt) noexcept {
    auto* value = stmt.value->codegen(*this);

    if (!value) {
        return nullptr;
    }

    auto iterator = m_locals.find(stmt.identifier.value);

    if (iterator == m_locals.end()) {
        error(
            stmt.identifier.span.begin, m_filename,
            fmt::format("undeclared variable: '{}'", stmt.identifier.value));

        return nullptr;
    }

    if (!iterator->second.is_mutable) {
        error(
            stmt.identifier.span.begin, m_filename,
            fmt::format("'{}' is immutable", stmt.identifier.value));

        return nullptr;
    }

    auto* variable = llvm::dyn_cast<llvm::AllocaInst>(iterator->second.value);

    if (!variable) {
        return nullptr;
    }

    if (variable->getAllocatedType() != value->getType()) {
        error(stmt.value->span.begin, m_filename, "type mismatch");

        return nullptr;
    }

    m_builder.CreateStore(value, variable);

    return nullptr;
}

llvm::Value* Codegen::generate(BlockStmt& stmt) noexcept {
    auto locals = m_locals;

    for (auto& statement : stmt.body) {
        statement->codegen(*this);
    }

    m_locals = std::move(locals);

    return nullptr;
}

llvm::Value* Codegen::generate(IfElse& stmt) noexcept {
    auto* condition = stmt.condition->codegen(*this);

    if (!condition) {
        return nullptr;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* if_block = llvm::BasicBlock::Create(m_context, "", function);
    auto* end = llvm::BasicBlock::Create(m_context, "", function);

    if (!stmt.else_block) {
        m_builder.CreateCondBr(condition, if_block, end);

        m_builder.SetInsertPoint(if_block);
        stmt.if_block->codegen(*this);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(end);
        }

        m_builder.SetInsertPoint(end);

        return nullptr;
    }

    auto* else_block = llvm::BasicBlock::Create(m_context, "", function);
    m_builder.CreateCondBr(condition, if_block, else_block);

    m_builder.SetInsertPoint(if_block);
    stmt.if_block->codegen(*this);

    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateBr(end);
    }

    m_builder.SetInsertPoint(else_block);
    stmt.else_block->codegen(*this);

    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateBr(end);
    }

    m_builder.SetInsertPoint(end);

    return nullptr;
}

llvm::Value* Codegen::generate(ReturnStmt& stmt) noexcept {
    auto* value = stmt.value->codegen(*this);

    if (!value) {
        return nullptr;
    }

    if (value->getType() != m_builder.getCurrentFunctionReturnType()) {
        error(stmt.value->span.begin, m_filename, "type mismatch");

        return nullptr;
    }

    m_builder.CreateRet(value);

    return nullptr;
}

llvm::Value* Codegen::generate(BinaryExpr& expr) noexcept {
    using enum Token::Type;

    auto* lhs = expr.lhs->codegen(*this);
    auto* rhs = expr.rhs->codegen(*this);

    if (!lhs || !rhs) {
        return nullptr;
    }

    if (lhs->getType() != rhs->getType()) {
        error(expr.lhs->span.begin, m_filename, "type mismatch");

        return nullptr;
    }

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

    if (!value) {
        return nullptr;
    }

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

llvm::Value* Codegen::generate(Identifier& expr) noexcept {
    auto iterator = m_locals.find(expr.value);

    if (iterator == m_locals.end()) {
        error(
            expr.span.begin, m_filename,
            fmt::format("undeclared variable: '{}'", expr.value));

        return nullptr;
    }

    auto* value = iterator->second.value;

    if (auto* variable = llvm::dyn_cast<llvm::AllocaInst>(value)) {
        return m_builder.CreateLoad(variable->getAllocatedType(), variable);
    }

    return value;
}

llvm::Value* Codegen::generate(CallExpr& expr) noexcept {
    auto* callee = m_module->getFunction(expr.identifier.value);

    if (!callee) {
        error(
            expr.identifier.span.begin, m_filename,
            fmt::format("undeclared function: '{}'", expr.identifier.value));

        return nullptr;
    }

    auto arg_size = callee->arg_size();

    if (arg_size != expr.arguments.size()) {
        error(
            expr.identifier.span.begin, m_filename,
            "incorrect number of arguments passed");

        return nullptr;
    }

    std::vector<llvm::Value*> arguments;
    arguments.reserve(arg_size);

    for (std::size_t i = 0; i < arg_size; ++i) {
        auto* value = expr.arguments[i]->codegen(*this);

        if (!value) {
            return nullptr;
        }

        if (value->getType() != callee->getFunctionType()->getParamType(i)) {
            error(expr.arguments[i]->span.begin, m_filename, "type mismatch");

            return nullptr;
        }

        arguments.push_back(value);
    }

    return m_builder.CreateCall(callee, arguments);
}

llvm::Value* Codegen::generate(FnDecl& decl) noexcept {
    auto* function = m_module->getFunction(decl.proto.name.value);
    auto* entry = llvm::BasicBlock::Create(m_context, "", function);

    m_locals.clear();

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        m_locals[decl.proto.params[i].name.value] = {
            function->getArg(i), false};
    }

    m_builder.SetInsertPoint(entry);
    decl.block->codegen(*this);

    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateRetVoid();
    }

    return nullptr;
}

llvm::Value* Codegen::generate(VarDecl& decl) noexcept {
    auto* value = decl.value->codegen(*this);

    if (!value) {
        return nullptr;
    }

    auto* type = get_type(decl.type.span, decl.type.value);

    if (!type) {
        return nullptr;
    }

    if (type != value->getType()) {
        error(decl.value->span.begin, m_filename, "type mismatch");

        return nullptr;
    }

    auto* variable = m_builder.CreateAlloca(type);
    m_builder.CreateStore(value, variable);

    m_locals[decl.name.value] = {variable, decl.is_mutable};

    return nullptr;
}

void Codegen::generate_fn_proto(FnDecl& decl) noexcept {
    auto* type = get_fn_type(decl);

    if (type) {
        llvm::Function::Create(
            type, llvm::Function::ExternalLinkage, decl.proto.name.value,
            *m_module);
    }
}

llvm::FunctionType* Codegen::get_fn_type(FnDecl& decl) noexcept {
    auto* return_type =
        get_type(decl.proto.return_type.span, decl.proto.return_type.value);

    if (!return_type) {
        return nullptr;
    }

    std::vector<llvm::Type*> param_types;
    param_types.reserve(decl.proto.params.size());

    for (const auto& parameter : decl.proto.params) {
        auto* type = get_type(parameter.type.span, parameter.type.value);

        if (!type) {
            return nullptr;
        }

        if (type->isVoidTy()) {
            error(
                parameter.name.span.begin, m_filename,
                fmt::format(
                    "'{}' cannot be of type 'void'", parameter.name.value));

            return nullptr;
        }

        param_types.push_back(type);
    }

    return llvm::FunctionType::get(return_type, param_types, false);
}

} // namespace cent
