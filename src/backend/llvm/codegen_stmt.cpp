#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/stmt/assert_stmt.h"
#include "ast/stmt/assignment.h"
#include "ast/stmt/block_stmt.h"
#include "ast/stmt/break_stmt.h"
#include "ast/stmt/continue_stmt.h"
#include "ast/stmt/if_else.h"
#include "ast/stmt/return_stmt.h"
#include "ast/stmt/switch.h"
#include "ast/stmt/unreachable.h"
#include "ast/stmt/while_loop.h"

#include "backend/llvm/codegen.h"
#include "backend/llvm/types/function.h"
#include "backend/llvm/value.h"

namespace cent::backend {

std::optional<Value> Codegen::generate([[maybe_unused]] ast::Assignment& stmt) {
    auto var = stmt.variable->codegen(*this);

    if (!var) {
        return std::nullopt;
    }

    auto value = stmt.value->codegen(*this);

    auto without_equal = [](frontend::Token::Type type) {
        using enum frontend::Token::Type;

        switch (type) {
        case PlusEqual:
            return Plus;
        case MinusEqual:
            return Minus;
        case StarEqual:
            return Star;
        case SlashEqual:
            return Slash;
        case PercentEqual:
            return Percent;
        case AndEqual:
            return And;
        case OrEqual:
            return Or;
        case XorEqual:
            return Xor;
        default:
            return Eof;
        }
    };

    if (stmt.oper.value != frontend::Token::Type::Equal) {
        value = generate_bin_expr(
            ast::OffsetValue<Value&>{*var, stmt.variable->offset},
            ast::OffsetValue<Value&>{*value, stmt.value->offset},
            ast::OffsetValue{without_equal(stmt.oper.value), stmt.oper.offset});
    }

    if (!value) {
        return std::nullopt;
    }

    if (!var->is_deref && !llvm::isa<llvm::AllocaInst>(var->value) &&
        !llvm::isa<llvm::GlobalVariable>(var->value) &&
        !llvm::isa<llvm::LoadInst>(var->value) &&
        !llvm::isa<llvm::GetElementPtrInst>(var->value)) {
        error(stmt.variable->offset, "cannot assign to a value");

        return std::nullopt;
    }

    if (!var->is_mutable) {
        error(stmt.value->offset, "cannot assign to an immutable value");

        return std::nullopt;
    }

    m_current_result = var->value;

    if (!cast_to_result(var->type, *value)) {
        type_mismatch(stmt.value->offset, *var->type, *value->type);
    }

    m_current_result = nullptr;

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::BlockStmt& stmt) {
    auto scope = m_scope;

    for (auto& statement : stmt.body) {
        statement->codegen(*this);
    }

    m_scope = std::move(scope);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::IfElse& stmt) {
    auto condition = stmt.condition->codegen(*this);

    if (!condition) {
        return std::nullopt;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* if_block = llvm::BasicBlock::Create(m_context, "", function);

    if (!stmt.else_block) {
        auto* end = llvm::BasicBlock::Create(m_context, "", function);

        m_builder.CreateCondBr(load_value(*condition).value, if_block, end);

        m_builder.SetInsertPoint(if_block);
        stmt.if_block->codegen(*this);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(end);
        }

        m_builder.SetInsertPoint(end);

        return std::nullopt;
    }

    auto* else_block = llvm::BasicBlock::Create(m_context, "", function);
    m_builder.CreateCondBr(load_value(*condition).value, if_block, else_block);

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

        return std::nullopt;
    }

    if (!end) {
        end = llvm::BasicBlock::Create(m_context, "", function);
    }

    m_builder.CreateBr(end);
    m_builder.SetInsertPoint(end);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::Switch& stmt) {
    auto value = stmt.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    auto val = load_value(*value);

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* end = llvm::BasicBlock::Create(m_context, "", function);

    auto* else_block = stmt.else_block
                           ? llvm::BasicBlock::Create(m_context, "", function)
                           : nullptr;

    auto* switch_inst = m_builder.CreateSwitch(
        val.value, else_block ? else_block : end, stmt.cases.size());

    bool all_terminate = true;

    for (auto& case_stmt : stmt.cases) {
        auto value = case_stmt.value->codegen(*this);

        if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value->value)) {
            auto* block = llvm::BasicBlock::Create(m_context, "", function);
            switch_inst->addCase(constant, block);

            m_builder.SetInsertPoint(block);
            case_stmt.block->codegen(*this);

            if (!m_builder.GetInsertBlock()->getTerminator()) {
                m_builder.CreateBr(end);
                all_terminate = false;
            }

            continue;
        }

        error(case_stmt.value->offset, "not a constant");
    }

    if (stmt.else_block) {
        m_builder.SetInsertPoint(else_block);
        stmt.else_block->codegen(*this);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(end);
            all_terminate = false;
        }
    }

    if (!all_terminate) {
        m_builder.SetInsertPoint(end);
    } else {
        end->eraseFromParent();
    }

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::ReturnStmt& stmt) {
    auto* function = m_builder.GetInsertBlock()->getParent();

    if (!stmt.value) {
        if (!function->getReturnType()->isVoidTy()) {
            error(stmt.offset, "type mismatch");

            return std::nullopt;
        }

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateRetVoid();
        }

        return std::nullopt;
    }

    auto value = stmt.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    if (auto val = cast(m_current_function->return_type, *value)) {
        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateRet(load_value(*val).value);
        }
    } else {
        type_mismatch(
            stmt.offset, *m_current_function->return_type, *value->type);
    }

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::WhileLoop& stmt) {
    auto condition = stmt.condition->codegen(*this);

    if (!condition) {
        return std::nullopt;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    m_loop_body = llvm::BasicBlock::Create(m_context, "", function);
    m_loop_end = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateCondBr(
        load_value(*condition).value, m_loop_body, m_loop_end);

    m_builder.SetInsertPoint(m_loop_body);
    stmt.body->codegen(*this);

    condition = stmt.condition->codegen(*this);

    m_builder.CreateCondBr(
        load_value(*condition).value, m_loop_body, m_loop_end);

    m_builder.SetInsertPoint(m_loop_end);

    m_loop_body = nullptr;
    m_loop_end = nullptr;

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::BreakStmt& stmt) {
    if (!m_loop_end) {
        error(stmt.offset, fmt::format("{} not in loop", log::bold("'break'")));

        return std::nullopt;
    }

    m_builder.CreateBr(m_loop_end);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::ContinueStmt& stmt) {
    if (!m_loop_body) {
        error(
            stmt.offset,
            fmt::format("{} not in loop", log::bold("'continue'")));

        return std::nullopt;
    }

    m_builder.CreateBr(m_loop_body);

    return std::nullopt;
}

std::optional<Value>
Codegen::generate([[maybe_unused]] ast::Unreachable& stmt) {
    m_builder.CreateUnreachable();

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::AssertStmt& stmt) {
    auto condition = stmt.condition->codegen(*this);

    if (!condition) {
        return std::nullopt;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* success = llvm::BasicBlock::Create(m_context, "", function);
    auto* failure = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateCondBr(condition->value, success, failure);

    m_builder.SetInsertPoint(failure);

    m_builder.CreateCall(
        m_panic_fn, {m_builder.CreateGlobalString("Assertion failed\n")});

    m_builder.CreateUnreachable();

    m_builder.SetInsertPoint(success);

    return std::nullopt;
}

} // namespace cent::backend
