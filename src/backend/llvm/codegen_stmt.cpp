#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

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

    var = load_lvalue(var);

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
            OffsetValue<const Value&>{
                .value = var, .offset = stmt.variable->offset},
            OffsetValue<const Value&>{
                .value = value, .offset = stmt.value->offset},
            OffsetValue{
                .value = without_equal(stmt.oper.value),
                .offset = stmt.oper.offset});

        if (!value.ok()) {
            return Value::poisoned();
        }
    }

    if (var.ptr_depth < 1) {
        error(stmt.variable->offset, "cannot assign to an rvalue");

        return Value::poisoned();
    }

    if (!var.is_mutable) {
        error(stmt.value->offset, "cannot assign to an immutable value");

        return Value::poisoned();
    }

    m_current_result = var.value;

    cast_to_result_or_error(stmt.value->offset, var.type, value);

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

        m_builder.CreateCondBr(load_rvalue(condition).value, if_block, end);

        m_builder.SetInsertPoint(if_block);
        stmt.if_block->codegen(*this);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(end);
        }

        m_builder.SetInsertPoint(end);

        return Value::poisoned();
    }

    auto* else_block = llvm::BasicBlock::Create(m_context, "", function);
    m_builder.CreateCondBr(load_rvalue(condition).value, if_block, else_block);

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
            union_type->tag_type->llvm_type, value, union_member_tag);

        value = {.type = union_type->tag_type, .value = tag_member};
    } else {
        value = load_rvalue(value);
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
            auto val = cast_or_error(stmt.offset, value.type, case_val);

            if (!val.ok()) {
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

    if (auto val =
            cast_or_error(stmt.offset, m_current_function->return_type, value);
        val.ok()) {
        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateRet(load_rvalue(val).value);
        }
    }

    return Value::poisoned();
}

Value Codegen::generate(const ast::WhileLoop& stmt) {
    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* loop_continue = m_loop_continue;
    auto* loop_end = m_loop_end;

    m_loop_continue = llvm::BasicBlock::Create(m_context, "", function);
    m_loop_end = llvm::BasicBlock::Create(m_context, "", function);

    auto* loop_body = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateBr(m_loop_continue);
    m_builder.SetInsertPoint(m_loop_continue);

    auto condition = stmt.condition->codegen(*this);

    if (!condition.ok()) {
        return Value::poisoned();
    }

    m_builder.CreateCondBr(load_rvalue(condition).value, loop_body, m_loop_end);

    m_builder.SetInsertPoint(loop_body);

    stmt.body->codegen(*this);
    m_builder.CreateBr(m_loop_continue);

    m_builder.SetInsertPoint(m_loop_end);

    m_loop_continue = loop_continue;
    m_loop_end = loop_end;

    return Value::poisoned();
}

Value Codegen::generate(const ast::ForLoop& stmt) {
    auto value = stmt.value->codegen(*this);

    if (!value.ok()) {
        return Value::poisoned();
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* loop_continue = m_loop_continue;
    auto* loop_end = m_loop_end;

    auto* loop_body = llvm::BasicBlock::Create(m_context, "", function);

    m_loop_continue = llvm::BasicBlock::Create(m_context, "", function);
    m_loop_end = llvm::BasicBlock::Create(m_context, "", function);

    if (auto* type = dyn_cast<types::Slice>(value.type)) {
        if (stmt.is_mutable && !type->is_mutable) {
            error(stmt.value->offset, "slice is not mutable");
            return Value::poisoned();
        }

        auto* llvm_contained_type = type->type->llvm_type;

        auto* usize = m_module->getDataLayout().getIntPtrType(m_context);
        auto* usize_null = llvm::ConstantInt::get(usize, 0);

        auto* index = create_alloca(usize);
        m_builder.CreateStore(usize_null, index);

        auto* length = load_struct_member(usize, value, slice_member_len);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(usize_null, length), loop_body, m_loop_end);

        m_builder.SetInsertPoint(loop_body);

        auto current_scope = *m_current_scope;

        auto* ptr_value = load_struct_member(
            llvm::PointerType::get(m_context, 0), value, slice_member_ptr);

        auto* index_value = m_builder.CreateLoad(usize, index);

        m_current_scope->names[stmt.name.value] = {
            .element =
                {.type = type->type,
                 .value = m_builder.CreateGEP(
                     llvm_contained_type, ptr_value, index_value),
                 .ptr_depth = 1,
                 .is_mutable = stmt.is_mutable},
            .unit = m_current_unit};

        stmt.body->codegen(*this);

        m_builder.CreateBr(m_loop_continue);

        *m_current_scope = current_scope;

        m_builder.SetInsertPoint(m_loop_continue);

        auto* incremented =
            m_builder.CreateAdd(index_value, llvm::ConstantInt::get(usize, 1));

        m_builder.CreateStore(incremented, index);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(incremented, length), loop_body,
            m_loop_end);

        m_builder.SetInsertPoint(m_loop_end);

        m_loop_continue = loop_continue;
        m_loop_end = loop_end;

        return Value::poisoned();
    }

    if (auto* type = dyn_cast<types::Array>(value.type)) {
        if (stmt.is_mutable && !value.is_mutable) {
            error(stmt.value->offset, "array is not mutable");
            return Value::poisoned();
        }

        auto* llvm_contained_type = type->type->llvm_type;

        auto* usize = m_module->getDataLayout().getIntPtrType(m_context);
        auto* usize_null = llvm::ConstantInt::get(usize, 0);

        auto* index = create_alloca(usize);
        m_builder.CreateStore(usize_null, index);

        auto* length = llvm::ConstantInt::get(usize, type->size);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(usize_null, length), loop_body, m_loop_end);

        m_builder.SetInsertPoint(loop_body);

        auto current_scope = *m_current_scope;

        auto* index_value = m_builder.CreateLoad(usize, index);

        m_current_scope->names[stmt.name.value] = {
            .element =
                {.type = type->type,
                 .value = m_builder.CreateGEP(
                     llvm_contained_type, value.value, index_value),
                 .ptr_depth = 1,
                 .is_mutable = stmt.is_mutable},
            .unit = m_current_unit};

        stmt.body->codegen(*this);

        m_builder.CreateBr(m_loop_continue);

        *m_current_scope = current_scope;

        m_builder.SetInsertPoint(m_loop_continue);

        auto* incremented =
            m_builder.CreateAdd(index_value, llvm::ConstantInt::get(usize, 1));

        m_builder.CreateStore(incremented, index);

        m_builder.CreateCondBr(
            m_builder.CreateICmpULT(incremented, length), loop_body,
            m_loop_end);

        m_builder.SetInsertPoint(m_loop_end);

        m_loop_continue = loop_continue;
        m_loop_end = loop_end;

        return Value::poisoned();
    }

    auto* type = dyn_cast<types::Range>(value.type);

    if (!type) {
        error(stmt.value->offset, "not iterable");
        return Value::poisoned();
    }

    if (stmt.is_mutable) {
        error(stmt.value->offset, "ranges are not mutable");
        return Value::poisoned();
    }

    auto* llvm_contained_type = type->type->llvm_type;

    llvm::Value* begin =
        load_struct_member(llvm_contained_type, value, range_member_begin);

    llvm::Value* end =
        load_struct_member(llvm_contained_type, value, range_member_end);

    if (!is_sint(type->type) && !is_uint(type->type)) {
        error(stmt.value->offset, "type mismatch");
        return Value::poisoned();
    }

    auto* variable = create_alloca(llvm_contained_type);
    m_builder.CreateStore(begin, variable);

    auto create_compare = [&](llvm::Value* begin, llvm::Value* end) {
        if (type->inclusive) {
            return is_sint(type->type) ? m_builder.CreateICmpSLE(begin, end)
                                       : m_builder.CreateICmpULE(begin, end);
        }

        return is_sint(type->type) ? m_builder.CreateICmpSLT(begin, end)
                                   : m_builder.CreateICmpULT(begin, end);
    };

    m_builder.CreateCondBr(create_compare(begin, end), loop_body, m_loop_end);

    m_builder.SetInsertPoint(loop_body);

    auto current_scope = *m_current_scope;

    m_current_scope->names[stmt.name.value] = {
        .element = {.type = type->type, .value = variable, .ptr_depth = 1},
        .unit = m_current_unit};

    stmt.body->codegen(*this);

    m_builder.CreateBr(m_loop_continue);

    *m_current_scope = current_scope;

    m_builder.SetInsertPoint(m_loop_continue);

    auto* incremented = m_builder.CreateAdd(
        m_builder.CreateLoad(llvm_contained_type, variable),
        llvm::ConstantInt::get(llvm_contained_type, 1));

    m_builder.CreateStore(incremented, variable);

    m_builder.CreateCondBr(
        create_compare(incremented, end), loop_body, m_loop_end);

    m_builder.SetInsertPoint(m_loop_end);

    m_loop_continue = loop_continue;
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
    if (!m_loop_continue) {
        error(stmt.offset, "`continue` not in loop");

        return Value::poisoned();
    }

    m_builder.CreateBr(m_loop_continue);

    return Value::poisoned();
}

Value Codegen::generate([[maybe_unused]] const ast::Unreachable& stmt) {
    m_builder.CreateUnreachable();

    return Value::poisoned();
}

} // namespace cent::backend
