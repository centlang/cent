#include <charconv>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>

#include "cent/log.h"

#include "cent/ast/as_expr.h"
#include "cent/ast/assignment.h"
#include "cent/ast/binary_expr.h"
#include "cent/ast/block_stmt.h"
#include "cent/ast/call_expr.h"
#include "cent/ast/fn_decl.h"
#include "cent/ast/identifier.h"
#include "cent/ast/if_else.h"
#include "cent/ast/literals.h"
#include "cent/ast/member_expr.h"
#include "cent/ast/named_type.h"
#include "cent/ast/pointer.h"
#include "cent/ast/program.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/unary_expr.h"
#include "cent/ast/var_decl.h"
#include "cent/ast/while_loop.h"

#include "cent/backend/type.h"
#include "cent/backend/value.h"

#include "cent/backend/types/function.h"
#include "cent/backend/types/primitive.h"
#include "cent/backend/types/struct.h"

#include "cent/backend/codegen.h"

namespace cent::backend {

std::unique_ptr<llvm::Module> Codegen::generate() noexcept {
    m_types = {
        {"i8", std::make_shared<types::I8>()},
        {"i16", std::make_shared<types::I16>()},
        {"i32", std::make_shared<types::I32>()},
        {"i64", std::make_shared<types::I64>()},
        {"u8", std::make_shared<types::U8>()},
        {"u16", std::make_shared<types::U16>()},
        {"u32", std::make_shared<types::U32>()},
        {"u64", std::make_shared<types::U64>()},
        {"f32", std::make_shared<types::F32>()},
        {"f64", std::make_shared<types::F64>()},
        {"bool", std::make_shared<types::Bool>()},
        {"void", std::make_shared<types::Void>()}};

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
        if (function->block) {
            function->codegen(*this);
        }
    }

    return std::move(m_module);
}

bool Codegen::types_equal(Type& lhs, Type& rhs) noexcept {
    if (&lhs == &rhs) {
        return true;
    }

    if (lhs.is_pointer() && rhs.is_pointer()) {
        auto& lhs_pointer = static_cast<types::Pointer&>(lhs);
        auto& rhs_pointer = static_cast<types::Pointer&>(rhs);

        return types_equal(*lhs_pointer.type, *rhs_pointer.type);
    }

    return false;
}

std::shared_ptr<Type> Codegen::generate(ast::NamedType& type) noexcept {
    auto iterator = m_types.find(type.value);

    if (iterator == m_types.end()) {
        error(
            type.span.begin, m_filename,
            fmt::format("undeclared type: '{}'", type.value));

        return nullptr;
    }

    return iterator->second;
}

std::shared_ptr<Type> Codegen::generate(ast::Pointer& type) noexcept {
    auto points_to = type.type->codegen(*this);

    if (!points_to) {
        return nullptr;
    }

    return std::make_shared<types::Pointer>(points_to);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::I8& type) noexcept {
    return llvm::Type::getInt8Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::I16& type) noexcept {
    return llvm::Type::getInt16Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::I32& type) noexcept {
    return llvm::Type::getInt32Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::I64& type) noexcept {
    return llvm::Type::getInt64Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::U8& type) noexcept {
    return llvm::Type::getInt8Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::U16& type) noexcept {
    return llvm::Type::getInt16Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::U32& type) noexcept {
    return llvm::Type::getInt32Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::U64& type) noexcept {
    return llvm::Type::getInt64Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::F32& type) noexcept {
    return llvm::Type::getFloatTy(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::F64& type) noexcept {
    return llvm::Type::getDoubleTy(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::Bool& type) noexcept {
    return llvm::Type::getInt1Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::Void& type) noexcept {
    return llvm::Type::getVoidTy(m_context);
}

llvm::Type* Codegen::generate(types::Pointer& type) noexcept {
    auto* llvm_type = type.type->codegen(*this);

    return llvm_type->getPointerTo();
}

llvm::Type* Codegen::generate(types::Struct& type) noexcept {
    return type.type;
}

llvm::Type* Codegen::generate(types::Function& type) noexcept {
    return type.function;
}

std::optional<Value>
Codegen::generate([[maybe_unused]] ast::Assignment& stmt) noexcept {
    auto var = stmt.variable->codegen(*this);

    if (!var) {
        return std::nullopt;
    }

    auto value = generate(*stmt.value);

    if (!value) {
        return std::nullopt;
    }

    if (!types_equal(*var->type, *value->type)) {
        error(stmt.value->span.begin, m_filename, "type mismatch");

        return std::nullopt;
    }

    if (!var->is_mutable) {
        error(
            stmt.value->span.begin, m_filename,
            "cannot assign to an immutable value");

        return std::nullopt;
    }

    if (auto* variable = llvm::dyn_cast<llvm::AllocaInst>(var->value)) {
        m_builder.CreateStore(value->value, variable);

        return std::nullopt;
    }

    auto* variable = llvm::dyn_cast<llvm::GetElementPtrInst>(var->value);

    if (!variable) {
        error(
            stmt.variable->span.begin, m_filename, "cannot assign to a value");

        return std::nullopt;
    }

    m_builder.CreateStore(value->value, variable);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::BlockStmt& stmt) noexcept {
    auto locals = m_locals;

    for (auto& statement : stmt.body) {
        statement->codegen(*this);
    }

    m_locals = std::move(locals);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::IfElse& stmt) noexcept {
    auto condition = generate(*stmt.condition);

    if (!condition) {
        return std::nullopt;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* if_block = llvm::BasicBlock::Create(m_context, "", function);

    if (!stmt.else_block) {
        auto* end = llvm::BasicBlock::Create(m_context, "", function);

        m_builder.CreateCondBr(condition->value, if_block, end);

        m_builder.SetInsertPoint(if_block);
        stmt.if_block->codegen(*this);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(end);
        }

        m_builder.SetInsertPoint(end);

        return std::nullopt;
    }

    auto* else_block = llvm::BasicBlock::Create(m_context, "", function);
    m_builder.CreateCondBr(condition->value, if_block, else_block);

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

std::optional<Value> Codegen::generate(ast::ReturnStmt& stmt) noexcept {
    auto* function = m_builder.GetInsertBlock()->getParent();

    if (!stmt.value) {
        if (!function->getReturnType()->isVoidTy()) {
            error(stmt.span.begin, m_filename, "type mismatch");

            return std::nullopt;
        }

        m_builder.CreateRetVoid();

        return std::nullopt;
    }

    auto value = generate(*stmt.value);

    if (!value) {
        return std::nullopt;
    }

    if (!types_equal(*value->type, *m_functions[function]->return_type)) {
        error(stmt.value->span.begin, m_filename, "type mismatch");

        return std::nullopt;
    }

    m_builder.CreateRet(value->value);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::WhileLoop& stmt) noexcept {
    auto condition = generate(*stmt.condition);

    if (!condition) {
        return std::nullopt;
    }

    auto* function = m_builder.GetInsertBlock()->getParent();

    auto* body = llvm::BasicBlock::Create(m_context, "", function);
    auto* end = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.CreateCondBr(condition->value, body, end);

    m_builder.SetInsertPoint(body);
    stmt.body->codegen(*this);

    condition = generate(*stmt.condition);
    m_builder.CreateCondBr(condition->value, body, end);

    m_builder.SetInsertPoint(end);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::BinaryExpr& expr) noexcept {
    using enum frontend::Token::Type;

    auto lhs = generate(*expr.lhs);
    auto rhs = generate(*expr.rhs);

    if (!lhs || !rhs) {
        return std::nullopt;
    }

    if (!types_equal(*lhs->type, *rhs->type)) {
        error(expr.lhs->span.begin, m_filename, "type mismatch");

        return std::nullopt;
    }

    switch (expr.oper.value) {
    case Plus:
    case Minus:
    case Star:
    case Slash:
    case Less:
    case Greater:
    case GreaterEqual:
    case LessEqual: {
        auto type = lhs->type;

        if (!type->is_signed_int() && !type->is_unsigned_int() &&
            !type->is_float()) {
            error(expr.lhs->span.begin, m_filename, "type mismatch");
        }

        break;
    }
    case And:
    case Or:
        if (!lhs->type->is_bool()) {
            error(expr.lhs->span.begin, m_filename, "type mismatch");
        }

        break;
    default:
        break;
    };

    auto* value = [&]() -> llvm::Value* {
        switch (expr.oper.value) {
        case Plus:
            return m_builder.CreateAdd(lhs->value, rhs->value);
        case Minus:
            return m_builder.CreateSub(lhs->value, rhs->value);
        case Star:
            return m_builder.CreateMul(lhs->value, rhs->value);
        case Slash:
            return lhs->type->is_signed_int()
                       ? m_builder.CreateSDiv(lhs->value, rhs->value)
                       : m_builder.CreateUDiv(lhs->value, rhs->value);
        case And:
            return m_builder.CreateAnd(lhs->value, rhs->value);
        case Or:
            return m_builder.CreateOr(lhs->value, rhs->value);
        case Less:
            return lhs->type->is_signed_int()
                       ? m_builder.CreateICmpSLT(lhs->value, rhs->value)
                       : m_builder.CreateICmpULT(lhs->value, rhs->value);
        case Greater:
            return lhs->type->is_signed_int()
                       ? m_builder.CreateICmpSGT(lhs->value, rhs->value)
                       : m_builder.CreateICmpUGT(lhs->value, rhs->value);
        case EqualEqual:
            return m_builder.CreateICmpEQ(lhs->value, rhs->value);
        case BangEqual:
            return m_builder.CreateICmpNE(lhs->value, rhs->value);
        case GreaterEqual:
            return lhs->type->is_signed_int()
                       ? m_builder.CreateICmpSGE(lhs->value, rhs->value)
                       : m_builder.CreateICmpUGE(lhs->value, rhs->value);
        case LessEqual:
            return lhs->type->is_signed_int()
                       ? m_builder.CreateICmpSLE(lhs->value, rhs->value)
                       : m_builder.CreateICmpULE(lhs->value, rhs->value);
        default:
            return nullptr;
        }
    }();

    return Value{lhs->type, value};
}

std::optional<Value> Codegen::generate(ast::UnaryExpr& expr) noexcept {
    using enum frontend::Token::Type;

    auto value = generate(*expr.value);

    if (!value) {
        return std::nullopt;
    }

    switch (expr.oper.value) {
    case Minus:
        if (!value->type->is_signed_int() && !value->type->is_unsigned_int() &&
            !value->type->is_float()) {
            error(
                expr.span.begin, m_filename,
                "cannot apply '-' to a non-number type");

            return std::nullopt;
        }

        return Value{value->type, m_builder.CreateNeg(value->value)};
    case Bang:
        if (!value->type->is_bool()) {
            error(
                expr.span.begin, m_filename,
                "cannot apply '!' to a non-boolean type");

            return std::nullopt;
        }

        return Value{value->type, m_builder.CreateNot(value->value)};
    case Star: {
        if (!value->type->is_pointer()) {
            error(
                expr.span.begin, m_filename,
                "dereference of a non-pointer type");

            return std::nullopt;
        }

        auto& pointer = static_cast<types::Pointer&>(*value->type);

        return Value{pointer.type, value->value};
    }
    default:
        return std::nullopt;
    }
}

std::optional<Value> Codegen::generate(ast::IntLiteral& expr) noexcept {
    bool failed = false;

    auto with_type_suffix = [&]<typename Type>(
                                std::string_view suffix,
                                bool is_signed) -> std::optional<Value> {
        if (!expr.value.ends_with(suffix)) {
            return std::nullopt;
        }

        Type value{};

        auto [pointer, result] = std::from_chars(
            expr.value.begin(), expr.value.end() - suffix.size(), value);

        if (result == std::errc::result_out_of_range) {
            error(expr.span.begin, m_filename, "integer out of range");

            failed = true;
            return std::nullopt;
        }

        auto& type = m_types[suffix];
        auto* llvm_type = type->codegen(*this);

        return Value{type, llvm::ConstantInt::get(llvm_type, value, is_signed)};
    };

    if (auto value = with_type_suffix.operator()<std::int8_t>("i8", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::int16_t>("i16", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::int32_t>("i32", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::int64_t>("i64", true)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint8_t>("u8", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint16_t>("u16", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint32_t>("u32", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<std::uint64_t>("u64", false)) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    std::int32_t value{};

    auto [pointer, result] =
        std::from_chars(expr.value.begin(), expr.value.end(), value);

    if (result == std::errc::result_out_of_range) {
        error(expr.span.begin, m_filename, "integer out of range");

        return std::nullopt;
    }

    return Value{
        m_types["i32"],
        llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(m_context), value)};
}

std::optional<Value> Codegen::generate(ast::FloatLiteral& expr) noexcept {
    bool failed = false;

    auto with_type_suffix =
        [&]<typename Type>(std::string_view suffix) -> std::optional<Value> {
        if (!expr.value.ends_with(suffix)) {
            return std::nullopt;
        }

        Type value{};

        auto [pointer, result] = std::from_chars(
            expr.value.begin(), expr.value.end() - suffix.size(), value);

        if (result == std::errc::result_out_of_range) {
            error(expr.span.begin, m_filename, "float out of range");

            failed = true;
            return std::nullopt;
        }

        auto& type = m_types[suffix];
        auto* llvm_type = type->codegen(*this);

        return Value{type, llvm::ConstantFP::get(llvm_type, value)};
    };

    if (auto value = with_type_suffix.operator()<float>("f32")) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    if (auto value = with_type_suffix.operator()<double>("f64")) {
        return value;
    }

    if (failed) {
        return std::nullopt;
    }

    float value{};

    auto [pointer, result] =
        std::from_chars(expr.value.begin(), expr.value.end(), value);

    if (result == std::errc::result_out_of_range) {
        error(expr.span.begin, m_filename, "float out of range");

        return std::nullopt;
    }

    return Value{
        m_types["f32"],
        llvm::ConstantFP::get(llvm::Type::getFloatTy(m_context), value)};
}

std::optional<Value> Codegen::generate(ast::BoolLiteral& expr) noexcept {
    return Value{
        m_types["bool"],
        llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), expr.value)};
}

std::optional<Value> Codegen::generate(ast::Identifier& expr) noexcept {
    auto iterator = m_locals.find(expr.value);

    if (iterator == m_locals.end()) {
        error(
            expr.span.begin, m_filename,
            fmt::format("undeclared variable: '{}'", expr.value));

        return std::nullopt;
    }

    return iterator->second;
}

std::optional<Value> Codegen::generate(ast::CallExpr& expr) noexcept {
    auto* llvm_callee = m_module->getFunction(expr.identifier.value);
    auto& callee = m_functions[llvm_callee];

    if (!llvm_callee) {
        error(
            expr.identifier.span.begin, m_filename,
            fmt::format("undeclared function: '{}'", expr.identifier.value));

        return std::nullopt;
    }

    auto arg_size = llvm_callee->arg_size();

    if (arg_size != expr.arguments.size()) {
        error(
            expr.identifier.span.begin, m_filename,
            "incorrect number of arguments passed");

        return std::nullopt;
    }

    std::vector<llvm::Value*> arguments;
    arguments.reserve(arg_size);

    for (std::size_t i = 0; i < arg_size; ++i) {
        auto value = generate(*expr.arguments[i]);

        if (!value) {
            return std::nullopt;
        }

        if (!types_equal(*value->type, *callee->param_types[i])) {
            error(expr.arguments[i]->span.begin, m_filename, "type mismatch");

            return std::nullopt;
        }

        arguments.push_back(value->value);
    }

    return Value{
        callee->return_type, m_builder.CreateCall(llvm_callee, arguments)};
}

std::optional<Value> Codegen::generate(ast::MemberExpr& expr) noexcept {
    auto parent = expr.parent->codegen(*this);

    if (!parent) {
        return std::nullopt;
    }

    auto* type = (parent->type->is_pointer()
                      ? static_cast<types::Pointer&>(*parent->type).type
                      : parent->type)
                     ->codegen(*this);

    auto* value = parent->value;

    if (parent->type->is_pointer()) {
        if (auto* variable = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            value = m_builder.CreateLoad(variable->getAllocatedType(), value);
        }
    }

    auto* struct_type = llvm::dyn_cast<llvm::StructType>(type);

    if (!struct_type) {
        error(
            expr.member.span.begin, m_filename,
            "member access of a non-structure type");

        return std::nullopt;
    }

    auto iterator = m_members[struct_type].find(expr.member.value);

    if (iterator == m_members[struct_type].end()) {
        error(
            expr.member.span.begin, m_filename,
            fmt::format("no such member: '{}'", expr.member.value));

        return std::nullopt;
    }

    return Value{
        m_structs[struct_type]->fields[iterator->second],
        m_builder.CreateStructGEP(struct_type, value, iterator->second),
        parent->is_mutable};
}

std::optional<Value> Codegen::generate(ast::AsExpr& expr) noexcept {
    using enum llvm::Instruction::CastOps;

    auto value = generate(*expr.value);

    if (!value) {
        return std::nullopt;
    }

    auto type = expr.type->codegen(*this);

    if (!type) {
        return std::nullopt;
    }

    auto* llvm_type = type->codegen(*this);

    if (value->type->is_pointer() && type->is_pointer()) {
        return Value{type, value->value};
    }

    std::size_t from_size = value->value->getType()->getPrimitiveSizeInBits();
    std::size_t to_size = llvm_type->getPrimitiveSizeInBits();

    llvm::Instruction::CastOps cast_op = CastOpsEnd;

    if (value->type->is_float() && type->is_signed_int()) {
        cast_op = FPToSI;
    } else if (value->type->is_float() && type->is_unsigned_int()) {
        cast_op = FPToUI;
    } else if (value->type->is_signed_int() && type->is_float()) {
        cast_op = SIToFP;
    } else if (value->type->is_unsigned_int() && type->is_float()) {
        cast_op = UIToFP;
    } else if (to_size > from_size) {
        if (value->type->is_float() && type->is_float()) {
            cast_op = FPExt;
        } else {
            cast_op = value->type->is_unsigned_int() && type->is_signed_int()
                          ? ZExt
                          : SExt;
        }
    } else if (to_size < from_size) {
        cast_op = value->type->is_float() && type->is_float() ? FPTrunc : Trunc;
    } else {
        return value;
    }

    return Value{type, m_builder.CreateCast(cast_op, value->value, llvm_type)};
}

std::optional<Value> Codegen::generate(ast::FnDecl& decl) noexcept {
    auto* function = m_module->getFunction(decl.proto.name.value);
    auto* entry = llvm::BasicBlock::Create(m_context, "", function);

    m_builder.SetInsertPoint(entry);

    m_locals.clear();

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        auto* value = function->getArg(i);
        auto* variable = m_builder.CreateAlloca(value->getType());

        m_builder.CreateStore(value, variable);
        m_locals[decl.proto.params[i].name.value] = {
            m_functions[function]->param_types[i], variable};
    }

    decl.block->codegen(*this);

    if (m_builder.GetInsertBlock()->getTerminator()) {
        return std::nullopt;
    }

    if (!function->getReturnType()->isVoidTy()) {
        error(
            decl.proto.name.span.begin, m_filename,
            "non-void function does not return a value");

        return std::nullopt;
    }

    m_builder.CreateRetVoid();

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::Struct& decl) noexcept {
    auto* struct_type =
        llvm::StructType::getTypeByName(m_context, decl.name.value);

    std::vector<llvm::Type*> llvm_fields;
    std::vector<std::shared_ptr<Type>> fields;

    llvm_fields.reserve(decl.fields.size());
    fields.reserve(decl.fields.size());

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        auto& field = decl.fields[i];

        auto type = field.type->codegen(*this);

        if (!type) {
            return std::nullopt;
        }

        auto* llvm_type = type->codegen(*this);

        if (llvm_type->isVoidTy()) {
            error(
                field.name.span.begin, m_filename,
                fmt::format("'{}' cannot be of type 'void'", field.name.value));

            return std::nullopt;
        }

        llvm_fields.push_back(llvm_type);
        fields.push_back(type);

        m_members[struct_type][field.name.value] = i;
    }

    struct_type->setBody(llvm_fields);

    auto type = std::make_shared<types::Struct>(struct_type, std::move(fields));

    m_types[decl.name.value] = type;
    m_structs[struct_type] = type;

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::VarDecl& decl) noexcept {
    std::optional<Value> value = std::nullopt;
    std::shared_ptr<Type> type = nullptr;

    llvm::Type* llvm_type = nullptr;

    if (decl.type) {
        type = decl.type->codegen(*this);

        if (!type) {
            return std::nullopt;
        }

        llvm_type = type->codegen(*this);
    }

    if (decl.value) {
        value = generate(*decl.value);

        if (!value) {
            return std::nullopt;
        }

        if (type && !types_equal(*type, *value->type)) {
            error(decl.value->span.begin, m_filename, "type mismatch");

            return std::nullopt;
        }

        type = value->type;
        llvm_type = value->value->getType();
    }

    if (llvm_type->isVoidTy()) {
        error(
            decl.name.span.begin, m_filename,
            fmt::format("'{}' cannot be of type 'void'", decl.name.value));

        return std::nullopt;
    }

    auto* variable = m_builder.CreateAlloca(llvm_type);

    if (value) {
        m_builder.CreateStore(value->value, variable);
    } else {
        m_builder.CreateStore(
            llvm::Constant::getNullValue(llvm_type), variable);
    }

    m_locals[decl.name.value] = {type, variable, decl.is_mutable};

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::Expression& expr) noexcept {
    auto result = expr.codegen(*this);

    if (!result) {
        return std::nullopt;
    }

    if (auto* variable =
            llvm::dyn_cast_or_null<llvm::AllocaInst>(result->value)) {
        return Value{
            result->type,
            m_builder.CreateLoad(variable->getAllocatedType(), variable)};
    }

    if (auto* value = llvm::dyn_cast_or_null<llvm::LoadInst>(result->value)) {
        auto* type = result->type->codegen(*this);

        if (value->getType()->isPointerTy()) {
            return Value{result->type, m_builder.CreateLoad(type, value)};
        }

        return result;
    }

    if (auto* ptr =
            llvm::dyn_cast_or_null<llvm::GetElementPtrInst>(result->value)) {
        return Value{
            result->type,
            m_builder.CreateLoad(ptr->getResultElementType(), ptr)};
    }

    return result;
}

void Codegen::generate_fn_proto(ast::FnDecl& decl) noexcept {
    auto return_type = decl.proto.return_type
                           ? decl.proto.return_type->codegen(*this)
                           : m_types["void"];

    if (!return_type) {
        return;
    }

    auto* llvm_return_type = return_type->codegen(*this);

    std::vector<llvm::Type*> llvm_param_types;
    std::vector<std::shared_ptr<Type>> param_types;

    llvm_param_types.reserve(decl.proto.params.size());
    param_types.reserve(decl.proto.params.size());

    for (const auto& parameter : decl.proto.params) {
        auto type = parameter.type->codegen(*this);

        if (!type) {
            return;
        }

        auto* llvm_type = type->codegen(*this);

        if (llvm_type->isVoidTy()) {
            error(
                parameter.name.span.begin, m_filename,
                fmt::format(
                    "'{}' cannot be of type 'void'", parameter.name.value));

            return;
        }

        llvm_param_types.push_back(llvm_type);
        param_types.push_back(type);
    }

    auto* type =
        llvm::FunctionType::get(llvm_return_type, llvm_param_types, false);

    auto* function = llvm::Function::Create(
        type, llvm::Function::ExternalLinkage, decl.proto.name.value,
        *m_module);

    m_functions[function] = std::make_shared<types::Function>(
        type, return_type, std::move(param_types));
}

} // namespace cent::backend
