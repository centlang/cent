#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/stmt/assert_stmt.h"
#include "ast/stmt/assignment.h"
#include "ast/stmt/block_stmt.h"
#include "ast/stmt/break_stmt.h"
#include "ast/stmt/continue_stmt.h"
#include "ast/stmt/for_loop.h"
#include "ast/stmt/if_else.h"
#include "ast/stmt/return_stmt.h"
#include "ast/stmt/switch.h"
#include "ast/stmt/unreachable.h"
#include "ast/stmt/while_loop.h"

#include "backend/llvm/types/union.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

Value Codegen::generate([[maybe_unused]] const ast::Assignment& stmt) {
    auto var = stmt.variable->codegen(*this);

    if (!var.ok()) {
        return Value::poisoned();
    }

    auto value = stmt.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

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
            ast::OffsetValue<const Value&>{var, stmt.variable->offset},
            ast::OffsetValue<const Value&>{value, stmt.value->offset},
            ast::OffsetValue{without_equal(stmt.oper.value), stmt.oper.offset});

        if (!value.ok()) {
            return Value::poisoned();
        }
    }

    if (!var.is_deref && !llvm::isa<llvm::AllocaInst>(var.value) &&
        !llvm::isa<llvm::GlobalVariable>(var.value) &&
        !llvm::isa<llvm::LoadInst>(var.value) &&
        !llvm::isa<llvm::GetElementPtrInst>(var.value)) {
        error(stmt.variable->offset, "cannot assign to a value");

        return Value::poisoned();
    }

    if (!var.is_mutable) {
        error(stmt.value->offset, "cannot assign to an immutable value");

        return Value::poisoned();
    }

    m_current_result = var.value;

    if (!cast_to_result(var.type, value)) {
        type_mismatch(stmt.value->offset, var.type, value.type);
    }

    m_current_result = nullptr;

    return Value::poisoned();
}

Value Codegen::generate(const ast::BlockStmt& stmt) {
    auto current_scope = *m_current_scope;

    for (const auto& statement : stmt.body) {
        statement->codegen(*this);
    }

    *m_current_scope = std::move(current_scope);

    return Value::poisoned();
}

Value Codegen::generate(const ast::IfElse& stmt) {
    auto condition = stmt.condition->codegen(*this);

    if (!condition.ok()) {
        return Value::poisoned();
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* if_block = llvm::BasicBlock::Create(m_context, "", function);

    if (!stmt.else_block) {
        auto* end = llvm::BasicBlock::Create(m_context, "", function);

        m_builder.CreateCondBr(load_value(condition).value, if_block, end);

        m_builder.SetInsertPoint(if_block);
        stmt.if_block->codegen(*this);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(end);
        }

        m_builder.SetInsertPoint(end);

        return Value::poisoned();
    }

    auto* else_block = llvm::BasicBlock::Create(m_context, "", function);
    m_builder.CreateCondBr(load_value(condition).value, if_block, else_block);

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

        return Value::poisoned();
    }

    if (!end) {
        end = llvm::BasicBlock::Create(m_context, "", function);
    }

    m_builder.CreateBr(end);
    m_builder.SetInsertPoint(end);

    return Value::poisoned();
}

Value Codegen::generate(const ast::Switch& stmt) {
    auto value = stmt.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    if (auto* union_type = dyn_cast<types::Union>(value.type)) {
        if (!union_type->tag_type) {
            error(stmt.value->offset, "switch on an untagged union");
            return Value::poisoned();
        }

        auto* tag_member = load_struct_member(
            union_type->llvm_type, union_type->tag_type->llvm_type, value.value,
            union_member_tag);

        value = {union_type->tag_type, tag_member};
    } else {
        value = load_value(value);
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* end = llvm::BasicBlock::Create(m_context, "", function);

    auto* else_block = stmt.else_block
                           ? llvm::BasicBlock::Create(m_context, "", function)
                           : nullptr;

    auto* switch_inst = m_builder.CreateSwitch(
        value.value, else_block ? else_block : end, stmt.cases.size());

    bool all_terminate = stmt.else_block != nullptr;

    for (const auto& case_stmt : stmt.cases) {
        auto* block = llvm::BasicBlock::Create(m_context, "", function);

        m_builder.SetInsertPoint(block);
        case_stmt.block->codegen(*this);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(end);
            all_terminate = false;
        }

        for (const auto& case_value : case_stmt.values) {
            auto case_val = case_value->codegen(*this);
            auto val = cast(value.type, case_val);

            if (!val.ok()) {
                type_mismatch(stmt.offset, value.type, case_val.type);
                return Value::poisoned();
            }

            if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(val.value)) {
                switch_inst->addCase(constant, block);
                continue;
            }

            error(case_value->offset, "not a constant");
        }
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

    return Value::poisoned();
}

Value Codegen::generate(const ast::ReturnStmt& stmt) {
    auto* function = m_builder.GetInsertBlock()->getParent();

    if (!stmt.value) {
        if (!function->getReturnType()->isVoidTy()) {
            error(stmt.offset, "type mismatch");

            return Value::poisoned();
        }

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateRetVoid();
        }

        return Value::poisoned();
    }

    auto value = stmt.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    if (auto val = cast(m_current_function->return_type, value); val.ok()) {
        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateRet(load_value(val).value);
        }
    } else {
        type_mismatch(stmt.offset, m_current_function->return_type, value.type);
    }

    return Value::poisoned();
}

Value Codegen::generate(const ast::WhileLoop& stmt) {
    auto condition = stmt.condition->codegen(*this);

    if (!condition.ok()) {
        return Value::poisoned();
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* loop_body = m_loop_body;
    auto* loop_end = m_loop_end;

    m_loop_body = llvm::BasicBlock::Create(m_context, "", function);
    m_loop_end = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateCondBr(
        load_value(condition).value, m_loop_body, m_loop_end);

    m_builder.SetInsertPoint(m_loop_body);
    stmt.body->codegen(*this);

    condition = stmt.condition->codegen(*this);

    m_builder.CreateCondBr(
        load_value(condition).value, m_loop_body, m_loop_end);

    m_builder.SetInsertPoint(m_loop_end);

    m_loop_body = loop_body;
    m_loop_end = loop_end;

    return Value::poisoned();
}

Value Codegen::generate(const ast::ForLoop& stmt) {
    auto value = stmt.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* loop_body = m_loop_body;
    auto* loop_end = m_loop_end;

    m_loop_body = llvm::BasicBlock::Create(m_context, "", function);
    m_loop_end = llvm::BasicBlock::Create(m_context, "", function);

    if (auto* type = dyn_cast<types::Slice>(value.type)) {
        auto* llvm_contained_type = type->type->llvm_type;

        auto* usize = m_module->getDataLayout().getIntPtrType(m_context);
        auto* usize_null = llvm::ConstantInt::get(usize, 0);

        auto* index = create_alloca(usize);
        m_builder.CreateStore(usize_null, index);

        auto* length = load_struct_member(
            value.type->llvm_type, usize, value.value, slice_member_len);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(usize_null, length), m_loop_body,
            m_loop_end);

        m_builder.SetInsertPoint(m_loop_body);

        auto current_scope = *m_current_scope;

        auto* ptr_value = load_struct_member(
            type->llvm_type, llvm::PointerType::get(m_context, 0), value.value,
            slice_member_ptr);

        auto* index_value = m_builder.CreateLoad(usize, index);

        m_current_scope->names[stmt.name.value] = {
            type->type, m_builder.CreateLoad(
                            llvm_contained_type,
                            m_builder.CreateGEP(
                                llvm_contained_type, ptr_value, index_value))};

        stmt.body->codegen(*this);

        *m_current_scope = current_scope;

        auto* incremented =
            m_builder.CreateAdd(index_value, llvm::ConstantInt::get(usize, 1));

        m_builder.CreateStore(incremented, index);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(incremented, length), m_loop_body,
            m_loop_end);

        m_builder.SetInsertPoint(m_loop_end);

        m_loop_body = loop_body;
        m_loop_end = loop_end;

        return Value::poisoned();
    }

    if (auto* type = dyn_cast<types::Array>(value.type)) {
        auto* llvm_contained_type = type->type->llvm_type;

        auto* usize = m_module->getDataLayout().getIntPtrType(m_context);
        auto* usize_null = llvm::ConstantInt::get(usize, 0);

        auto* index = create_alloca(usize);
        m_builder.CreateStore(usize_null, index);

        auto* length = llvm::ConstantInt::get(usize, type->size);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(usize_null, length), m_loop_body,
            m_loop_end);

        m_builder.SetInsertPoint(m_loop_body);

        auto current_scope = *m_current_scope;

        auto* index_value = m_builder.CreateLoad(usize, index);

        m_current_scope->names[stmt.name.value] = {
            type->type,
            m_builder.CreateLoad(
                llvm_contained_type,
                m_builder.CreateGEP(
                    llvm_contained_type, value.value, index_value))};

        stmt.body->codegen(*this);

        *m_current_scope = current_scope;

        auto* incremented =
            m_builder.CreateAdd(index_value, llvm::ConstantInt::get(usize, 1));

        m_builder.CreateStore(incremented, index);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(incremented, length), m_loop_body,
            m_loop_end);

        m_builder.SetInsertPoint(m_loop_end);

        m_loop_body = loop_body;
        m_loop_end = loop_end;

        return Value::poisoned();
    }

    auto* type = dyn_cast<types::Range>(value.type);

    if (!type) {
        error(stmt.value->offset, "not iterable");

        return Value::poisoned();
    }

    auto* llvm_contained_type = type->type->llvm_type;

    llvm::Value* begin = load_struct_member(
        value.type->llvm_type, llvm_contained_type, value.value,
        range_member_begin);

    llvm::Value* end = load_struct_member(
        value.type->llvm_type, llvm_contained_type, value.value,
        range_member_end);

    if (!is_sint(type->type) && !is_uint(type->type)) {
        error(stmt.value->offset, "type mismatch");

        return Value::poisoned();
    }

    auto* variable = create_alloca(llvm_contained_type);
    m_builder.CreateStore(begin, variable);

    m_builder.CreateCondBr(
        is_sint(value.type) ? m_builder.CreateICmpSLT(begin, end)
                            : m_builder.CreateICmpULT(begin, end),
        m_loop_body, m_loop_end);

    m_builder.SetInsertPoint(m_loop_body);

    auto current_scope = *m_current_scope;

    m_current_scope->names[stmt.name.value] = {type->type, variable};
    stmt.body->codegen(*this);

    *m_current_scope = current_scope;

    auto* incremented = m_builder.CreateAdd(
        m_builder.CreateLoad(llvm_contained_type, variable),
        llvm::ConstantInt::get(llvm_contained_type, 1));

    m_builder.CreateStore(incremented, variable);

    m_builder.CreateCondBr(
        is_sint(value.type) ? m_builder.CreateICmpSLT(incremented, end)
                            : m_builder.CreateICmpULT(incremented, end),
        m_loop_body, m_loop_end);

    m_builder.SetInsertPoint(m_loop_end);

    m_loop_body = loop_body;
    m_loop_end = loop_end;

    return Value::poisoned();
}

Value Codegen::generate(const ast::BreakStmt& stmt) {
    if (!m_loop_end) {
        error(stmt.offset, "`break` not in loop");

        return Value::poisoned();
    }

    m_builder.CreateBr(m_loop_end);

    return Value::poisoned();
}

Value Codegen::generate(const ast::ContinueStmt& stmt) {
    if (!m_loop_body) {
        error(stmt.offset, "`continue` not in loop");

        return Value::poisoned();
    }

    m_builder.CreateBr(m_loop_body);

    return Value::poisoned();
}

Value Codegen::generate([[maybe_unused]] const ast::Unreachable& stmt) {
    m_builder.CreateUnreachable();

    return Value::poisoned();
}

Value Codegen::generate(const ast::AssertStmt& stmt) {
    auto condition = stmt.condition->codegen(*this);

    if (!condition.ok()) {
        return Value::poisoned();
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* success = llvm::BasicBlock::Create(m_context, "", function);
    auto* failure = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateCondBr(condition.value, success, failure);

    m_builder.SetInsertPoint(failure);

    auto src = read_file(m_filename);
    auto loc = cent::offset_to_loc(*src, stmt.condition->offset);

    m_builder.CreateCall(
        m_panic_fn, {m_builder.CreateGlobalString(fmt::format(
                        "Assertion failed at {}:{}:{}\n", m_filename, loc.line,
                        loc.column))});

    m_builder.CreateUnreachable();

    m_builder.SetInsertPoint(success);

    return Value::poisoned();
}

} // namespace cent::backend
