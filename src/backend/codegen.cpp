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
#include "cent/ast/member_expr.h"
#include "cent/ast/program.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/unary_expr.h"
#include "cent/ast/var_decl.h"
#include "cent/ast/while_loop.h"

#include "cent/backend/codegen.h"

namespace cent {

std::unique_ptr<llvm::Module> Codegen::generate() noexcept {
    for (auto& struct_decl : m_program->structs) {
        llvm::StructType::create(m_context, struct_decl->name.value);
    }

    for (auto& struct_decl : m_program->structs) {
        struct_decl->codegen(*this);
    }

    for (auto& function : m_program->functions) {
        generate_fn_proto(*function);
    }

    for (auto& function : m_program->functions) {
        function->codegen(*this);
    }

    return std::move(m_module);
}

llvm::Value* Codegen::generate(Assignment& stmt) noexcept {
    auto* value = generate(*stmt.value);

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
    auto* condition = generate(*stmt.condition);

    if (!condition) {
        return nullptr;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* if_block = llvm::BasicBlock::Create(m_context, "", function);

    if (!stmt.else_block) {
        auto* end = llvm::BasicBlock::Create(m_context, "", function);

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

    llvm::BasicBlock* end = nullptr;

    if (!m_builder.GetInsertBlock()->getTerminator()) {
        end = llvm::BasicBlock::Create(m_context, "", function);
        m_builder.CreateBr(end);
    }

    m_builder.SetInsertPoint(else_block);
    stmt.else_block->codegen(*this);

    if (m_builder.GetInsertBlock()->getTerminator()) {
        if (end) {
            m_builder.SetInsertPoint(end);
        }

        return nullptr;
    }

    if (!end) {
        end = llvm::BasicBlock::Create(m_context, "", function);
    }

    m_builder.CreateBr(end);
    m_builder.SetInsertPoint(end);

    return nullptr;
}

llvm::Value* Codegen::generate(ReturnStmt& stmt) noexcept {
    if (!stmt.value) {
        if (!m_builder.getCurrentFunctionReturnType()->isVoidTy()) {
            error(stmt.span.begin, m_filename, "type mismatch");

            return nullptr;
        }

        m_builder.CreateRetVoid();

        return nullptr;
    }

    auto* value = generate(*stmt.value);

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

llvm::Value* Codegen::generate(WhileLoop& stmt) noexcept {
    auto* condition = generate(*stmt.condition);

    if (!condition) {
        return nullptr;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* body = llvm::BasicBlock::Create(m_context, "", function);
    auto* end = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateCondBr(condition, body, end);

    m_builder.SetInsertPoint(body);
    stmt.body->codegen(*this);

    condition = generate(*stmt.condition);
    m_builder.CreateCondBr(condition, body, end);

    m_builder.SetInsertPoint(end);

    return nullptr;
}

llvm::Value* Codegen::generate(BinaryExpr& expr) noexcept {
    using enum Token::Type;

    auto* lhs = generate(*expr.lhs);
    auto* rhs = generate(*expr.rhs);

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

    auto* value = generate(*expr.value);

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

    return iterator->second.value;
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
        auto* value = generate(*expr.arguments[i]);

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

llvm::Value* Codegen::generate(MemberExpr& expr) noexcept {
    auto* variable =
        llvm::dyn_cast<llvm::AllocaInst>(expr.parent->codegen(*this));

    if (!variable) {
        return nullptr;
    }

    auto* struct_type =
        llvm::dyn_cast<llvm::StructType>(variable->getAllocatedType());

    if (!struct_type) {
        error(
            expr.member.span.begin, m_filename,
            "member access of a non-structure type");

        return nullptr;
    }

    auto iterator = m_members[struct_type].find(expr.member.value);

    if (iterator == m_members[struct_type].end()) {
        error(
            expr.member.span.begin, m_filename,
            fmt::format("no such member: '{}'", expr.member.value));

        return nullptr;
    }

    return m_builder.CreateStructGEP(struct_type, variable, iterator->second);
}

llvm::Value* Codegen::generate(FnDecl& decl) noexcept {
    auto* function = m_module->getFunction(decl.proto.name.value);
    auto* entry = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.SetInsertPoint(entry);

    m_locals.clear();

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        auto* value = function->getArg(i);
        auto* variable = m_builder.CreateAlloca(value->getType());

        m_builder.CreateStore(value, variable);
        m_locals[decl.proto.params[i].name.value] = {variable, false};
    }

    decl.block->codegen(*this);

    if (m_builder.GetInsertBlock()->getTerminator()) {
        return nullptr;
    }

    if (!function->getReturnType()->isVoidTy()) {
        error(
            decl.proto.name.span.begin, m_filename,
            "non-void function does not return a value");

        return nullptr;
    }

    m_builder.CreateRetVoid();

    return nullptr;
}

llvm::Value* Codegen::generate(Struct& decl) noexcept {
    auto* struct_type =
        llvm::StructType::getTypeByName(m_context, decl.name.value);

    std::vector<llvm::Type*> fields;
    fields.reserve(decl.fields.size());

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        auto field = decl.fields[i];

        auto* type = get_type(field.type.span, field.type.value);

        if (!type) {
            return nullptr;
        }

        if (type->isVoidTy()) {
            error(
                field.name.span.begin, m_filename,
                fmt::format("'{}' cannot be of type 'void'", field.name.value));

            return nullptr;
        }

        fields.push_back(type);
        m_members[struct_type][field.name.value] = i;
    }

    struct_type->setBody(fields);

    return nullptr;
}

llvm::Value* Codegen::generate(VarDecl& decl) noexcept {
    auto* type = get_type(decl.type.span, decl.type.value);

    if (!type) {
        return nullptr;
    }

    if (type->isVoidTy()) {
        error(
            decl.name.span.begin, m_filename,
            fmt::format("'{}' cannot be of type 'void'", decl.name.value));

        return nullptr;
    }

    auto* variable = m_builder.CreateAlloca(type);
    llvm::Value* value = nullptr;

    if (decl.value) {
        value = generate(*decl.value);

        if (!value) {
            return nullptr;
        }

        if (type != value->getType()) {
            error(decl.value->span.begin, m_filename, "type mismatch");

            return nullptr;
        }
    }

    if (!value) {
        value = llvm::Constant::getNullValue(type);
    }

    m_builder.CreateStore(value, variable);
    m_locals[decl.name.value] = {variable, decl.is_mutable};

    return nullptr;
}

llvm::Value* Codegen::generate(Expression& expr) noexcept {
    auto* result = expr.codegen(*this);

    if (auto* variable = llvm::dyn_cast_or_null<llvm::AllocaInst>(result)) {
        return m_builder.CreateLoad(variable->getAllocatedType(), variable);
    }

    if (auto* ptr = llvm::dyn_cast_or_null<llvm::GetElementPtrInst>(result)) {
        return m_builder.CreateLoad(ptr->getResultElementType(), ptr);
    }

    return result;
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
