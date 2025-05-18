#include <array>
#include <charconv>
#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "cent/ast/array_type.h"
#include "cent/ast/as_expr.h"
#include "cent/ast/assignment.h"
#include "cent/ast/binary_expr.h"
#include "cent/ast/block_stmt.h"
#include "cent/ast/break_stmt.h"
#include "cent/ast/call_expr.h"
#include "cent/ast/continue_stmt.h"
#include "cent/ast/fn_decl.h"
#include "cent/ast/identifier.h"
#include "cent/ast/if_else.h"
#include "cent/ast/index_expr.h"
#include "cent/ast/literals.h"
#include "cent/ast/member_expr.h"
#include "cent/ast/method_expr.h"
#include "cent/ast/module.h"
#include "cent/ast/named_type.h"
#include "cent/ast/optional.h"
#include "cent/ast/pointer.h"
#include "cent/ast/return_stmt.h"
#include "cent/ast/tuple_type.h"
#include "cent/ast/unary_expr.h"
#include "cent/ast/var_decl.h"
#include "cent/ast/while_loop.h"

#include "cent/backend/type.h"
#include "cent/backend/value.h"

#include "cent/backend/types/enum.h"
#include "cent/backend/types/function.h"
#include "cent/backend/types/primitive.h"
#include "cent/backend/types/struct.h"

#include "cent/backend/codegen.h"

namespace cent::backend {

std::unique_ptr<llvm::Module> Codegen::generate() noexcept {
    m_primitive_types = {
        {"i8", std::make_shared<types::I8>()},
        {"i16", std::make_shared<types::I16>()},
        {"i32", std::make_shared<types::I32>()},
        {"i64", std::make_shared<types::I64>()},
        {"isize", std::make_shared<types::ISize>()},
        {"u8", std::make_shared<types::U8>()},
        {"u16", std::make_shared<types::U16>()},
        {"u32", std::make_shared<types::U32>()},
        {"u64", std::make_shared<types::U64>()},
        {"usize", std::make_shared<types::USize>()},
        {"f32", std::make_shared<types::F32>()},
        {"f64", std::make_shared<types::F64>()},
        {"str", std::make_shared<types::Str>()},
        {"bool", std::make_shared<types::Bool>()},
        {"void", std::make_shared<types::Void>()}};

    m_null_type = std::make_shared<types::Null>();

    generate(*m_program);

    return std::move(m_module);
}

std::shared_ptr<Type> Codegen::generate(ast::NamedType& type) noexcept {
    auto* scope = m_current_scope;
    std::size_t last_index = type.value.size() - 1;

    for (std::size_t i = 0; i < last_index; ++i) {
        scope = get_scope(type.value[i].offset, type.value[i].value, *scope);

        if (!scope) {
            return nullptr;
        }
    }

    return get_type(
        type.value[last_index].offset, type.value[last_index].value, *scope);
}

std::shared_ptr<Type> Codegen::generate(ast::Pointer& type) noexcept {
    auto points_to = type.type->codegen(*this);

    if (!points_to) {
        return nullptr;
    }

    return std::make_shared<types::Pointer>(points_to, type.is_mutable);
}

std::shared_ptr<Type> Codegen::generate(ast::Optional& type) noexcept {
    auto contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return std::make_shared<types::Optional>(contained);
}

std::shared_ptr<Type> Codegen::generate(ast::ArrayType& type) noexcept {
    auto contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    auto size = type.size->codegen(*this);

    if (!size) {
        return nullptr;
    }

    auto value = cast(m_primitive_types["usize"], *size);

    if (!value) {
        return nullptr;
    }

    if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value->value)) {
        return std::make_shared<types::Array>(
            contained, constant->getZExtValue());
    }

    error(type.offset, "not a constant");

    return nullptr;
}

std::shared_ptr<Type> Codegen::generate(ast::TupleType& type) noexcept {
    std::vector<std::shared_ptr<backend::Type>> types;
    std::vector<llvm::Type*> llvm_types;

    types.reserve(type.types.size());
    llvm_types.reserve(type.types.size());

    for (auto& element_type : type.types) {
        auto el_type = element_type->codegen(*this);

        if (!el_type) {
            return nullptr;
        }

        types.push_back(std::move(el_type));
    }

    for (auto& element_type : types) {
        llvm_types.push_back(element_type->codegen(*this));
    }

    return std::make_shared<types::Tuple>(
        llvm::StructType::create(llvm_types), std::move(types));
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

llvm::Type* Codegen::generate([[maybe_unused]] types::ISize& type) noexcept {
    return m_module->getDataLayout().getIntPtrType(m_context);
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

llvm::Type* Codegen::generate([[maybe_unused]] types::USize& type) noexcept {
    return m_module->getDataLayout().getIntPtrType(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::F32& type) noexcept {
    return llvm::Type::getFloatTy(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::F64& type) noexcept {
    return llvm::Type::getDoubleTy(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::Str& type) noexcept {
    return llvm::Type::getInt8Ty(m_context)->getPointerTo();
}

llvm::Type* Codegen::generate([[maybe_unused]] types::Bool& type) noexcept {
    return llvm::Type::getInt1Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] types::Null& type) noexcept {
    return nullptr;
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

llvm::Type* Codegen::generate(types::Enum& type) noexcept {
    return type.type->codegen(*this);
}

llvm::Type* Codegen::generate(types::Optional& type) noexcept {
    auto* contained = type.type->codegen(*this);

    if (type.type->is_pointer()) {
        return contained;
    }

    auto iterator = m_optional_types.find(contained);

    if (iterator != m_optional_types.end()) {
        return iterator->second;
    }

    std::array<llvm::Type*, 2> fields = {
        contained, llvm::Type::getInt1Ty(m_context)};

    auto* llvm_type = llvm::StructType::create(fields);

    m_optional_types[contained] = llvm_type;

    return llvm_type;
}

llvm::Type* Codegen::generate(types::Array& type) noexcept {
    auto* llvm_type = type.type->codegen(*this);

    if (!llvm_type) {
        return nullptr;
    }

    return llvm::ArrayType::get(llvm_type, type.size);
}

llvm::Type* Codegen::generate(types::Tuple& type) noexcept { return type.type; }

llvm::Type* Codegen::generate([[maybe_unused]] types::Function& type) noexcept {
    return nullptr;
}

std::optional<Value>
Codegen::generate([[maybe_unused]] ast::Assignment& stmt) noexcept {
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

std::optional<Value> Codegen::generate(ast::BlockStmt& stmt) noexcept {
    auto scope = m_scope;

    for (auto& statement : stmt.body) {
        statement->codegen(*this);
    }

    m_scope = std::move(scope);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::IfElse& stmt) noexcept {
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

std::optional<Value> Codegen::generate(ast::ReturnStmt& stmt) noexcept {
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

std::optional<Value> Codegen::generate(ast::WhileLoop& stmt) noexcept {
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

std::optional<Value> Codegen::generate(ast::BreakStmt& stmt) noexcept {
    if (!m_loop_end) {
        error(stmt.offset, "'break' not in loop");

        return std::nullopt;
    }

    m_builder.CreateBr(m_loop_end);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::ContinueStmt& stmt) noexcept {
    if (!m_loop_body) {
        error(stmt.offset, "'continue' not in loop");

        return std::nullopt;
    }

    m_builder.CreateBr(m_loop_body);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::BinaryExpr& expr) noexcept {
    using enum frontend::Token::Type;

    auto lhs = expr.lhs->codegen(*this);
    auto rhs = expr.rhs->codegen(*this);

    if (!lhs || !rhs) {
        return std::nullopt;
    }

    return generate_bin_expr(
        ast::OffsetValue<Value&>{*lhs, expr.lhs->offset},
        ast::OffsetValue<Value&>{*rhs, expr.rhs->offset}, expr.oper);
}

std::optional<Value> Codegen::generate(ast::UnaryExpr& expr) noexcept {
    using enum frontend::Token::Type;

    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    switch (expr.oper.value) {
    case Minus:
        if (value->type->is_float()) {
            return Value{
                value->type, m_builder.CreateFNeg(load_value(*value).value)};
        }

        if (!value->type->is_signed_int() && !value->type->is_unsigned_int()) {
            error(expr.offset, "cannot apply '-' to a non-number type");

            return std::nullopt;
        }

        return Value{
            value->type, m_builder.CreateNeg(load_value(*value).value)};
    case Bang:
        if (!value->type->is_bool()) {
            error(expr.offset, "cannot apply '!' to a non-boolean type");

            return std::nullopt;
        }

        return Value{
            value->type, m_builder.CreateNot(load_value(*value).value)};
    case Star: {
        if (!value->type->is_pointer()) {
            error(expr.offset, "dereference of a non-pointer type");

            return std::nullopt;
        }

        auto& pointer = static_cast<types::Pointer&>(*value->type);

        return Value{
            pointer.type, value->value, pointer.is_mutable, false, true};
    }
    case And:
        if (!llvm::isa<llvm::AllocaInst>(value->value) &&
            !llvm::isa<llvm::GetElementPtrInst>(value->value)) {
            error(expr.offset, "taking the reference of a non-variable value");

            return std::nullopt;
        }

        return Value{
            std::make_shared<types::Pointer>(value->type, value->is_mutable),
            value->value, false, true};
    case Not:
        if (!value->type->is_signed_int() && !value->type->is_unsigned_int()) {
            error(expr.offset, "cannot apply '~' to a non-integer type");

            return std::nullopt;
        }

        return Value{
            value->type, m_builder.CreateNot(load_value(*value).value)};
    default:
        return std::nullopt;
    }
}

std::optional<Value> Codegen::generate(ast::IntLiteral& expr) noexcept {
    bool failed = false;
    std::string_view literal = expr.value;

    std::uint8_t base = [&] {
        static constexpr auto hex = 16;
        static constexpr auto oct = 8;
        static constexpr auto bin = 2;
        static constexpr auto dec = 10;

        if (literal.starts_with("0x")) {
            literal = literal.substr(2);

            return hex;
        }

        if (literal.starts_with("0o")) {
            literal = literal.substr(2);

            return oct;
        }

        if (literal.starts_with("0b")) {
            literal = literal.substr(2);

            return bin;
        }

        return dec;
    }();

    auto with_type_suffix = [&]<typename Type>(
                                std::string_view suffix,
                                bool is_signed) -> std::optional<Value> {
        if (!literal.ends_with(suffix)) {
            return std::nullopt;
        }

        Type value{};
        literal = literal.substr(0, literal.size() - suffix.size());

        auto [pointer, result] =
            std::from_chars(literal.cbegin(), literal.cend(), value, base);

        if (result == std::errc::result_out_of_range) {
            error(expr.offset, "integer out of range");

            failed = true;
            return std::nullopt;
        }

        auto& type = m_primitive_types[suffix];
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
        std::from_chars(literal.cbegin(), literal.cend(), value, base);

    if (result == std::errc::result_out_of_range) {
        error(expr.offset, "integer out of range");

        return std::nullopt;
    }

    return Value{
        m_primitive_types["i32"],
        llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(m_context), value)};
}

std::optional<Value> Codegen::generate(ast::FloatLiteral& expr) noexcept {
    bool failed = false;

    std::string_view literal = expr.value;

    auto with_type_suffix =
        [&]<typename Type>(std::string_view suffix) -> std::optional<Value> {
        if (!expr.value.ends_with(suffix)) {
            return std::nullopt;
        }

        Type value{};

        literal = literal.substr(0, literal.size() - suffix.size());

        auto [pointer, result] =
            std::from_chars(literal.cbegin(), literal.cend(), value);

        if (result == std::errc::result_out_of_range) {
            error(expr.offset, "float out of range");

            failed = true;
            return std::nullopt;
        }

        auto& type = m_primitive_types[suffix];
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
        std::from_chars(literal.cbegin(), literal.cend(), value);

    if (result == std::errc::result_out_of_range) {
        error(expr.offset, "float out of range");

        return std::nullopt;
    }

    return Value{
        m_primitive_types["f32"],
        llvm::ConstantFP::get(llvm::Type::getFloatTy(m_context), value)};
}

std::optional<Value> Codegen::generate(ast::StrLiteral& expr) noexcept {
    return Value{
        m_primitive_types["str"], m_builder.CreateGlobalString(expr.value)};
}

std::optional<Value> Codegen::generate(ast::BoolLiteral& expr) noexcept {
    return Value{
        m_primitive_types["bool"],
        llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), expr.value)};
}

std::optional<Value>
Codegen::generate([[maybe_unused]] ast::NullLiteral& expr) noexcept {
    return Value{m_null_type, nullptr};
}

std::optional<Value> Codegen::generate(ast::StructLiteral& expr) noexcept {
    auto type = expr.type->codegen(*this);

    if (!type) {
        return std::nullopt;
    }

    if (!type->is_struct()) {
        error(expr.type->offset, "not a structure");

        return std::nullopt;
    }

    auto& struct_type = static_cast<types::Struct&>(*type);
    auto& members = m_members[struct_type.type];

    bool stack_allocated = m_current_result == nullptr;

    bool is_const = true;

    if (expr.fields.size() != struct_type.fields.size()) {
        error(expr.type->offset, "incorrect number of fields initialized");

        return std::nullopt;
    }

    std::vector<Value> values;
    values.reserve(expr.fields.size());

    for (auto& field : expr.fields) {
        auto value = field.value->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!llvm::isa<llvm::Constant>(value->value)) {
            is_const = false;
        }

        values.push_back(std::move(*value));
    }

    if (is_const) {
        std::vector<llvm::Constant*> llvm_values;
        llvm_values.reserve(expr.fields.size());

        for (auto& value : values) {
            llvm_values.push_back(static_cast<llvm::Constant*>(value.value));
        }

        return Value{
            type, llvm::ConstantStruct::get(struct_type.type, llvm_values)};
    }

    auto* variable = m_current_result
                         ? m_current_result
                         : m_builder.CreateAlloca(struct_type.type);

    for (std::size_t i = 0; i < expr.fields.size(); ++i) {
        auto& field = expr.fields[i];
        auto& value = values[i];

        auto iterator = members.find(field.name.value);

        if (iterator == members.end()) {
            error(
                field.name.offset,
                fmt::format("no such member: '{}'", field.name.value));

            return std::nullopt;
        }

        auto index = iterator->second;

        m_current_result =
            m_builder.CreateStructGEP(struct_type.type, variable, index);

        if (!cast_to_result(struct_type.fields[index], value)) {
            type_mismatch(
                expr.fields[index].value->offset, *struct_type.fields[index],
                *value.type);

            m_current_result = nullptr;

            return std::nullopt;
        }

        m_current_result = nullptr;
    }

    return Value{type, variable, false, false, false, stack_allocated};
}

std::optional<Value> Codegen::generate(ast::ArrayLiteral& expr) noexcept {
    auto type = expr.type->codegen(*this);

    if (!type) {
        return std::nullopt;
    }

    auto& array_type = static_cast<types::Array&>(*type);
    auto* llvm_type = static_cast<llvm::ArrayType*>(type->codegen(*this));

    bool stack_allocated = m_current_result == nullptr;

    bool is_const = true;

    if (expr.elements.size() != array_type.size) {
        error(expr.type->offset, "incorrect number of elements");

        return std::nullopt;
    }

    std::vector<Value> values;
    values.reserve(expr.elements.size());

    for (auto& element : expr.elements) {
        auto value = element->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!llvm::isa<llvm::Constant>(value->value)) {
            is_const = false;
        }

        values.push_back(std::move(*value));
    }

    if (is_const) {
        std::vector<llvm::Constant*> llvm_values;
        llvm_values.reserve(expr.elements.size());

        for (auto& value : values) {
            llvm_values.push_back(static_cast<llvm::Constant*>(value.value));
        }

        return Value{type, llvm::ConstantArray::get(llvm_type, llvm_values)};
    }

    auto* variable =
        m_current_result ? m_current_result : m_builder.CreateAlloca(llvm_type);

    for (std::size_t i = 0; i < expr.elements.size(); ++i) {
        auto& value = values[i];

        auto* intptr = m_module->getDataLayout().getIntPtrType(m_context);

        m_current_result = m_builder.CreateGEP(
            llvm_type, variable,
            {llvm::ConstantInt::get(intptr, 0),
             llvm::ConstantInt::get(intptr, i)});

        if (!cast_to_result(array_type.type, value)) {
            type_mismatch(
                expr.elements[i]->offset, *array_type.type, *value.type);

            m_current_result = nullptr;

            return std::nullopt;
        }

        m_current_result = nullptr;
    }

    return Value{type, variable, false, false, false, stack_allocated};
}

std::optional<Value> Codegen::generate(ast::TupleLiteral& expr) noexcept {
    bool is_const = true;

    std::vector<Value> values;
    values.reserve(expr.elements.size());

    for (auto& element : expr.elements) {
        auto value = element->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!llvm::isa<llvm::Constant>(value->value)) {
            is_const = false;
        }

        values.push_back(*value);
    }

    std::vector<std::shared_ptr<backend::Type>> types;
    std::vector<llvm::Type*> llvm_types;

    types.reserve(expr.elements.size());
    llvm_types.reserve(expr.elements.size());

    for (auto& value : values) {
        types.push_back(value.type);
    }

    for (auto& value : values) {
        llvm_types.push_back(value.type->codegen(*this));
    }

    auto* struct_type = llvm::StructType::create(llvm_types);

    bool stack_allocated = m_current_result == nullptr;

    if (is_const) {
        std::vector<llvm::Constant*> llvm_values;
        llvm_values.reserve(expr.elements.size());

        for (auto& value : values) {
            llvm_values.push_back(static_cast<llvm::Constant*>(value.value));
        }

        return Value{
            std::make_shared<types::Tuple>(struct_type, std::move(types)),
            llvm::ConstantStruct::get(struct_type, llvm_values)};
    }

    auto* variable = m_current_result ? m_current_result
                                      : m_builder.CreateAlloca(struct_type);

    for (std::size_t i = 0; i < values.size(); ++i) {
        auto* pointer = m_builder.CreateStructGEP(struct_type, variable, i);

        m_builder.CreateStore(load_value(values[i]).value, pointer);
    }

    return Value{
        std::make_shared<types::Tuple>(struct_type, std::move(types)),
        variable,
        false,
        false,
        false,
        stack_allocated};
}

std::optional<Value> Codegen::generate(ast::Identifier& expr) noexcept {
    auto* scope = m_current_scope;
    std::size_t last_index = expr.value.size() - 1;

    for (std::size_t i = 0; i < last_index; ++i) {
        scope = get_scope(expr.value[i].offset, expr.value[i].value, *scope);

        if (!scope) {
            return std::nullopt;
        }
    }

    return get_name(
        expr.value[last_index].offset, expr.value[last_index].value, *scope);
}

std::optional<Value> Codegen::generate(ast::CallExpr& expr) noexcept {
    auto value = expr.identifier->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    if (!value->type->is_function()) {
        error(expr.identifier->offset, "not a function");

        return std::nullopt;
    }

    auto& type = static_cast<types::Function&>(*value->type);
    auto* function = static_cast<llvm::Function*>(value->value);

    auto arg_size = function->arg_size();

    if (arg_size != expr.arguments.size()) {
        error(expr.identifier->offset, "incorrect number of arguments passed");

        return std::nullopt;
    }

    std::vector<llvm::Value*> arguments;
    arguments.reserve(arg_size);

    for (std::size_t i = 0; i < arg_size; ++i) {
        auto value = expr.arguments[i]->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (auto val = cast(type.param_types[i], *value)) {
            arguments.push_back(load_value(*val).value);
        } else {
            type_mismatch(
                expr.arguments[i]->offset, *type.param_types[i], *value->type);

            return std::nullopt;
        }
    }

    return Value{type.return_type, m_builder.CreateCall(function, arguments)};
}

std::optional<Value> Codegen::generate(ast::MethodExpr& expr) noexcept {
    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    auto iterator = m_methods[value->type].find(expr.name.value);

    if (iterator == m_methods[value->type].end()) {
        error(
            expr.name.offset,
            fmt::format("no such method: '{}'", expr.name.value));

        return std::nullopt;
    }

    auto arg_size = iterator->second.function->arg_size();

    if (arg_size - 1 != expr.arguments.size()) {
        error(expr.name.offset, "incorrect number of arguments passed");

        return std::nullopt;
    }

    std::vector<llvm::Value*> arguments;
    arguments.reserve(arg_size);

    if (iterator->second.type->param_types[0]->is_pointer()) {
        auto& self_type = static_cast<types::Pointer&>(
            *iterator->second.type->param_types[0]);

        if (!value->is_mutable && self_type.is_mutable) {
            error(
                expr.name.offset,
                fmt::format(
                    "cannot call method '{}' on an immutable value",
                    expr.name.value));

            return std::nullopt;
        }

        if (!value->value->getType()->isPointerTy()) {
            error(expr.value->offset, "type mismatch");

            return std::nullopt;
        }

        arguments.push_back(value->value);
    } else {
        arguments.push_back(load_value(*value).value);
    }

    for (std::size_t i = 0; i < expr.arguments.size(); ++i) {
        auto value = expr.arguments[i]->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (auto val =
                cast(iterator->second.type->param_types[i + 1], *value)) {
            arguments.push_back(load_value(*val).value);
        } else {
            type_mismatch(
                expr.arguments[i]->offset,
                *iterator->second.type->param_types[i + 1], *value->type);

            return std::nullopt;
        }
    }

    return Value{
        iterator->second.type->return_type,
        m_builder.CreateCall(iterator->second.function, arguments)};
}

std::optional<Value> Codegen::generate(ast::MemberExpr& expr) noexcept {
    auto parent = expr.parent->codegen(*this);

    if (!parent) {
        return std::nullopt;
    }

    auto no_such_member = [&] {
        error(
            expr.member.offset,
            fmt::format("no such member: '{}'", expr.member.value));
    };

    if (parent->type->is_tuple()) {
        if (expr.member.type != frontend::Token::Type::IntLiteral) {
            no_such_member();

            return std::nullopt;
        }

        std::size_t value{};
        std::string_view literal = expr.member.value;

        std::from_chars(literal.cbegin(), literal.cend(), value);

        auto& type = static_cast<types::Tuple&>(*parent->type);

        if (value >= type.types.size()) {
            no_such_member();

            return std::nullopt;
        }

        return Value{
            type.types[value],
            m_builder.CreateStructGEP(type.type, parent->value, value),
            parent->is_mutable};
    }

    auto not_a_struct = [&] {
        error(expr.member.offset, "member access of a non-structure type");
    };

    bool is_mutable = false;

    types::Struct* type = nullptr;

    if (parent->type->is_pointer()) {
        auto& pointer = static_cast<types::Pointer&>(*parent->type);

        if (!pointer.type->is_struct()) {
            not_a_struct();

            return std::nullopt;
        }

        is_mutable = pointer.is_mutable;
        type = static_cast<types::Struct*>(pointer.type.get());
    } else {
        if (!parent->type->is_struct()) {
            not_a_struct();

            return std::nullopt;
        }

        is_mutable = parent->is_mutable;
        type = static_cast<types::Struct*>(parent->type.get());
    }

    auto* value = parent->value;

    if (parent->type->is_pointer()) {
        if (auto* variable = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            value = m_builder.CreateLoad(variable->getAllocatedType(), value);
        }
    }

    auto iterator = m_members[type->type].find(expr.member.value);

    if (iterator == m_members[type->type].end()) {
        error(
            expr.member.offset,
            fmt::format("no such member: '{}'", expr.member.value));

        return std::nullopt;
    }

    return Value{
        type->fields[iterator->second],
        m_builder.CreateStructGEP(type->type, value, iterator->second),
        is_mutable};
}

std::optional<Value> Codegen::generate(ast::IndexExpr& expr) noexcept {
    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    if (!value->type->is_array()) {
        error(expr.value->offset, "index access of a non-array type");

        return std::nullopt;
    }

    auto index = expr.index->codegen(*this);

    if (!index) {
        return std::nullopt;
    }

    auto index_val = cast(m_primitive_types["usize"], *index);

    if (!index_val) {
        type_mismatch(
            expr.index->offset, *m_primitive_types["usize"], *index->type);

        return std::nullopt;
    }

    auto* type = static_cast<types::Array*>(value->type.get());

    return Value{
        type->type,
        m_builder.CreateGEP(
            type->codegen(*this), value->value,
            {llvm::ConstantInt::get(
                 m_module->getDataLayout().getIntPtrType(m_context), 0),
             index_val->value}),
        value->is_mutable};
}

std::optional<Value> Codegen::generate(ast::AsExpr& expr) noexcept {
    using enum llvm::Instruction::CastOps;

    auto value = expr.value->codegen(*this);

    if (!value) {
        return std::nullopt;
    }

    auto type = expr.type->codegen(*this);

    if (!type) {
        return std::nullopt;
    }

    if (value->type->is_pointer() && type->is_pointer()) {
        return Value{type, value->value};
    }

    if (auto val = cast(type, *value, false)) {
        return val;
    }

    type_mismatch(expr.type->offset, *type, *value->type);

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::FnDecl& decl) noexcept {
    auto type = decl.proto.type ? get_type(
                                      decl.proto.type->offset,
                                      decl.proto.type->value, *m_current_scope)
                                : nullptr;

    auto get_fn_type = [&]() -> std::shared_ptr<Type> {
        if (!decl.proto.type) {
            return m_scope.names[decl.proto.name.value].type;
        }

        auto method = m_methods[type].find(decl.proto.name.value);

        if (method != m_methods[type].end()) {
            return method->second.type;
        }

        return m_scope.scopes[decl.proto.type->value]
            .names[decl.proto.name.value]
            .type;
    };

    auto get_llvm_fn = [&]() {
        if (!decl.proto.type) {
            return static_cast<llvm::Function*>(
                m_scope.names[decl.proto.name.value].value);
        }

        auto method = m_methods[type].find(decl.proto.name.value);

        if (method != m_methods[type].end()) {
            return method->second.function;
        }

        return static_cast<llvm::Function*>(
            m_scope.scopes[decl.proto.type->value]
                .names[decl.proto.name.value]
                .value);
    };

    auto function_type = get_fn_type();
    auto* function = get_llvm_fn();

    auto* entry = llvm::BasicBlock::Create(
        m_context, "", static_cast<llvm::Function*>(function));

    m_builder.SetInsertPoint(entry);

    m_current_function = static_cast<types::Function*>(function_type.get());

    for (std::size_t i = 0; i < decl.proto.params.size(); ++i) {
        auto* value = function->getArg(i);
        auto* variable = m_builder.CreateAlloca(value->getType());

        m_builder.CreateStore(value, variable);

        m_scope.names[decl.proto.params[i].name.value] = {
            m_current_function->param_types[i], variable};
    }

    decl.block->codegen(*this);

    if (m_builder.GetInsertBlock()->getTerminator()) {
        return std::nullopt;
    }

    if (!function->getReturnType()->isVoidTy()) {
        error(
            decl.proto.name.offset,
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
                field.name.offset,
                fmt::format("'{}' cannot be of type 'void'", field.name.value));

            return std::nullopt;
        }

        llvm_fields.push_back(llvm_type);
        fields.push_back(type);

        m_members[struct_type][field.name.value] = i;
    }

    struct_type->setBody(llvm_fields);

    m_current_scope->types[decl.name.value] = std::make_shared<types::Struct>(
        m_current_scope_prefix + decl.name.value, struct_type,
        std::move(fields));

    return std::nullopt;
}

std::optional<Value> Codegen::generate(ast::EnumDecl& decl) noexcept {
    auto& type = m_current_scope->types[decl.name.value];
    auto& enum_type = static_cast<types::Enum&>(*type);

    auto* llvm_type = type->codegen(*this);

    std::uint64_t number = 0;

    for (std::size_t i = 0; i < decl.fields.size(); ++i) {
        auto& field = decl.fields[i];

        if (!field.value) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] =
                Value{
                    type, llvm::ConstantInt::get(
                              llvm_type, number++, type->is_signed_int())};

            continue;
        }

        auto value = field.value->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (auto val = cast(enum_type.type, *value)) {
            m_current_scope->scopes[decl.name.value].names[field.name.value] =
                *val;

            if (auto* constant =
                    llvm::dyn_cast<llvm::ConstantInt>(val->value)) {
                number = enum_type.is_signed_int() ? constant->getSExtValue()
                                                   : constant->getZExtValue();
            } else {
                error(field.value->offset, "not a constant");

                return std::nullopt;
            }
        } else {
            type_mismatch(field.value->offset, *enum_type.type, *value->type);

            return std::nullopt;
        }

        ++number;
    }

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

        if (llvm_type->isVoidTy()) {
            error(
                decl.name.offset,
                fmt::format("'{}' cannot be of type 'void'", decl.name.value));

            return std::nullopt;
        }
    }

    if (decl.value) {
        value = decl.value->codegen(*this);

        if (!value) {
            return std::nullopt;
        }

        if (!type) {
            type = value->type;
            llvm_type = value->type->codegen(*this);
        } else {
            m_current_result = m_builder.CreateAlloca(llvm_type);

            if (!cast_to_result(type, *value)) {
                type_mismatch(decl.value->offset, *type, *value->type);
            } else {
                m_scope.names[decl.name.value] = {
                    type, m_current_result, decl.is_mutable};
            }

            m_current_result = nullptr;

            return std::nullopt;
        }
    }

    if (value) {
        if (value->stack_allocated) {
            m_scope.names[decl.name.value] = {
                type, value->value, decl.is_mutable};

            return std::nullopt;
        }

        m_current_result = m_builder.CreateAlloca(llvm_type);

        m_builder.CreateStore(load_value(*value).value, m_current_result);

        m_scope.names[decl.name.value] = {
            type, m_current_result, decl.is_mutable};
    } else {
        m_current_result = m_builder.CreateAlloca(llvm_type);

        m_builder.CreateStore(
            llvm::Constant::getNullValue(llvm_type), m_current_result);

        m_scope.names[decl.name.value] = {
            type, m_current_result, decl.is_mutable};
    }

    m_current_result = nullptr;

    return std::nullopt;
}

void Codegen::generate(ast::Module& module, bool is_submodule) noexcept {
    if (module.path.file) {
        auto iterator = m_generated_modules.find(*module.path.file);

        if (iterator != m_generated_modules.end()) {
            *m_current_scope = iterator->second;
            return;
        }
    }

    auto* scope = m_current_scope;
    auto scope_prefix = m_current_scope_prefix;

    for (auto& submodule : module.submodules) {
        m_current_scope = &scope->scopes[submodule.first];
        m_current_scope_prefix += submodule.first + "::";

        generate(*submodule.second, true);
    }

    m_current_scope = scope;
    m_current_scope_prefix = scope_prefix;

    for (auto& enum_decl : module.enums) {
        if (m_current_scope->types.contains(enum_decl->name.value)) {
            error(
                enum_decl->name.offset,
                fmt::format("'{}' is already defined", enum_decl->name.value));

            continue;
        }

        auto type = enum_decl->type ? enum_decl->type->codegen(*this)
                                    : m_primitive_types["i32"];

        if (!type->is_signed_int() && !type->is_unsigned_int() &&
            !type->is_bool()) {
            error(enum_decl->type->offset, "type mismatch");
            continue;
        }

        m_current_scope->types[enum_decl->name.value] =
            std::make_shared<types::Enum>(
                m_current_scope_prefix + enum_decl->name.value, type);
    }

    for (auto& struct_decl : module.structs) {
        if (llvm::StructType::getTypeByName(
                m_context, struct_decl->name.value)) {
            error(
                struct_decl->name.offset,
                fmt::format(
                    "'{}' is already defined", struct_decl->name.value));

            continue;
        }

        llvm::StructType::create(m_context, struct_decl->name.value);
    }

    for (auto& enum_decl : module.enums) {
        enum_decl->codegen(*this);
    }

    for (auto& struct_decl : module.structs) {
        struct_decl->codegen(*this);
    }

    for (auto& function : module.functions) {
        generate_fn_proto(*function);
    }

    for (auto& function : module.functions) {
        if (function->block && !is_submodule) {
            function->codegen(*this);
        }
    }

    if (module.path.file) {
        m_generated_modules[*module.path.file] = *m_current_scope;
    }
}

bool Codegen::types_equal(Type& lhs, Type& rhs) noexcept {
    if (&lhs == &rhs) {
        return true;
    }

    if (lhs.is_pointer() && rhs.is_pointer()) {
        auto& lhs_pointer = static_cast<types::Pointer&>(lhs);
        auto& rhs_pointer = static_cast<types::Pointer&>(rhs);

        return lhs_pointer.is_mutable == rhs_pointer.is_mutable &&
               types_equal(*lhs_pointer.type, *rhs_pointer.type);
    }

    if (lhs.is_optional() && rhs.is_optional()) {
        auto& lhs_optional = static_cast<types::Optional&>(lhs);
        auto& rhs_optional = static_cast<types::Optional&>(rhs);

        return types_equal(*lhs_optional.type, *rhs_optional.type);
    }

    if (lhs.is_array() && rhs.is_array()) {
        auto& lhs_array = static_cast<types::Array&>(lhs);
        auto& rhs_array = static_cast<types::Array&>(rhs);

        return lhs_array.size == rhs_array.size &&
               types_equal(*lhs_array.type, *rhs_array.type);
    }

    return false;
}

std::optional<Value> Codegen::cast(
    std::shared_ptr<Type>& type, Value& value, bool implicit) noexcept {
    if (types_equal(*type, *value.type)) {
        return value;
    }

    auto* llvm_type = type->codegen(*this);

    if (type->is_optional()) {
        auto& contained = static_cast<types::Optional&>(*type).type;

        if (contained->is_pointer()) {
            if (!value.type->is_null()) {
                return Value{type, value.value};
            }

            auto* variable = m_builder.CreateAlloca(llvm_type);

            m_builder.CreateStore(
                llvm::Constant::getNullValue(llvm_type), variable);

            return Value{type, variable, false, false, false, true};
        }

        auto* variable = m_builder.CreateAlloca(llvm_type);
        auto* bool_ptr = m_builder.CreateStructGEP(
            llvm_type, variable, optional_member_bool);

        if (value.type->is_null()) {
            m_builder.CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), false),
                bool_ptr);

            return Value{type, variable, false, false, false, true};
        }

        if (!types_equal(*contained, *value.type)) {
            return std::nullopt;
        }

        auto* value_ptr = m_builder.CreateStructGEP(
            llvm_type, variable, optional_member_value);

        m_builder.CreateStore(load_value(value).value, value_ptr);

        m_builder.CreateStore(
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), true),
            bool_ptr);

        return Value{type, variable, false, false, false, true};
    }

    return primitive_cast(type, llvm_type, value, implicit);
}

std::optional<Value> Codegen::primitive_cast(
    std::shared_ptr<Type>& type, llvm::Type* llvm_type, Value& value,
    bool implicit) noexcept {
    using enum llvm::Instruction::CastOps;

    auto layout = m_module->getDataLayout();

    std::size_t from_size = layout.getTypeAllocSize(value.type->codegen(*this));
    std::size_t to_size = layout.getTypeAllocSize(llvm_type);

    bool value_is_float = value.type->is_float();
    bool value_is_sint = value.type->is_signed_int();
    bool value_is_uint = value.type->is_unsigned_int();
    bool value_is_ptr = value.type->is_pointer();
    bool value_is_enum = value.type->is_enum();

    if (!value_is_float && !value_is_sint && !value_is_uint && !value_is_ptr &&
        !value_is_enum) {
        return std::nullopt;
    }

    bool type_is_float = type->is_float();
    bool type_is_sint = type->is_signed_int();
    bool type_is_uint = type->is_unsigned_int();
    bool type_is_ptr = type->is_pointer();
    bool type_is_enum = type->is_enum();

    llvm::Instruction::CastOps cast_op = CastOpsEnd;

    if (value_is_float) {
        if (type_is_float) {
            if (to_size > from_size) {
                cast_op = FPExt;
            } else if (!implicit) {
                cast_op = FPTrunc;
            }
        } else if (!implicit) {
            if (type_is_sint) {
                cast_op = FPToSI;
            } else if (type_is_uint) {
                cast_op = FPToUI;
            }
        }
    } else if (
        value_is_sint ||
        (!implicit && value_is_enum &&
         static_cast<types::Enum&>(*value.type).type->is_signed_int())) {
        if (type_is_float) {
            cast_op = SIToFP;
        } else if (type_is_sint || type_is_uint || type_is_enum) {
            if (to_size > from_size) {
                cast_op = SExt;
            } else if (!implicit) {
                cast_op = Trunc;
            }
        } else if (!implicit && type_is_ptr) {
            cast_op = IntToPtr;
        }
    } else if (
        value_is_uint ||
        (!implicit && value_is_enum &&
         static_cast<types::Enum&>(*value.type).type->is_unsigned_int())) {
        if (type_is_float) {
            cast_op = UIToFP;
        } else if (type_is_sint || type_is_uint || type_is_enum) {
            if (to_size > from_size) {
                cast_op = ZExt;
            } else if (!implicit) {
                cast_op = Trunc;
            }
        } else if (!implicit && type_is_ptr) {
            cast_op = IntToPtr;
        }
    } else if (!implicit && value_is_ptr) {
        if (type_is_sint || type_is_uint) {
            cast_op = PtrToInt;
        } else if (type_is_ptr) {
            return Value{type, value.value};
        }
    }

    if (cast_op == CastOpsEnd) {
        return std::nullopt;
    }

    return Value{
        type,
        m_builder.CreateCast(cast_op, load_value(value).value, llvm_type)};
}

bool Codegen::cast_to_result(
    std::shared_ptr<Type>& type, Value& value, bool implicit) noexcept {
    if (types_equal(*type, *value.type)) {
        m_builder.CreateStore(load_value(value).value, m_current_result);

        return true;
    }

    auto* llvm_type = type->codegen(*this);

    if (type->is_optional()) {
        auto& contained = static_cast<types::Optional&>(*type).type;

        if (contained->is_pointer()) {
            if (!value.type->is_null()) {
                m_builder.CreateStore(value.value, m_current_result);

                return true;
            }

            m_builder.CreateStore(
                llvm::Constant::getNullValue(llvm_type), m_current_result);

            return true;
        }

        auto* bool_ptr = m_builder.CreateStructGEP(
            llvm_type, m_current_result, optional_member_bool);

        if (value.type->is_null()) {
            m_builder.CreateStore(
                llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), false),
                bool_ptr);

            return true;
        }

        if (!types_equal(*contained, *value.type)) {
            return false;
        }

        auto* value_ptr = m_builder.CreateStructGEP(
            llvm_type, m_current_result, optional_member_value);

        m_builder.CreateStore(load_value(value).value, value_ptr);

        m_builder.CreateStore(
            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), true),
            bool_ptr);

        return true;
    }

    if (auto val = primitive_cast(type, llvm_type, value, implicit)) {
        m_builder.CreateStore(val->value, m_current_result);
        return true;
    }

    return false;
}

std::optional<Value> Codegen::generate_bin_expr(
    ast::OffsetValue<Value&> lhs, ast::OffsetValue<Value&> rhs,
    ast::OffsetValue<frontend::Token::Type> oper) noexcept {
    using enum frontend::Token::Type;

    if (oper.value == EqualEqual || oper.value == BangEqual) {
        std::optional<Value> value = std::nullopt;

        if (lhs.value.type->is_optional() && rhs.value.type->is_null()) {
            value = lhs.value;
        } else if (rhs.value.type->is_optional() && lhs.value.type->is_null()) {
            value = rhs.value;
        }

        if (value) {
            auto& type = static_cast<types::Optional&>(*value->type);

            if (type.type->is_pointer()) {
                auto* null =
                    llvm::Constant::getNullValue(value->value->getType());

                auto* llvm_value = load_value(*value).value;

                auto* val = oper.value == EqualEqual
                                ? m_builder.CreateICmpEQ(llvm_value, null)
                                : m_builder.CreateICmpNE(llvm_value, null);

                return Value{m_primitive_types["bool"], val};
            }

            auto* bool_ptr = m_builder.CreateStructGEP(
                value->value->getType(), value->value, optional_member_bool);

            llvm::Value* val = m_builder.CreateLoad(
                llvm::Type::getInt1Ty(m_context), bool_ptr);

            if (oper.value == EqualEqual) {
                val = m_builder.CreateNot(val);
            }

            return Value{m_primitive_types["bool"], val};
        }
    }

    auto lhs_value = load_value(lhs.value);
    auto rhs_value = load_value(rhs.value);

    auto right = cast(lhs_value.type, rhs_value);

    if (!right) {
        type_mismatch(lhs.offset, *lhs_value.type, *rhs_value.type);

        return std::nullopt;
    }

    switch (oper.value) {
    case Plus:
    case Minus:
    case Star:
    case Slash:
    case Less:
    case Greater:
    case GreaterEqual:
    case LessEqual: {
        auto type = lhs_value.type;

        if (!type->is_signed_int() && !type->is_unsigned_int() &&
            !type->is_float()) {
            error(lhs.offset, "type mismatch");

            return std::nullopt;
        }

        break;
    }
    case Percent:
    case And:
    case Or:
    case Xor:
        if (!lhs_value.type->is_signed_int() &&
            !lhs_value.type->is_unsigned_int()) {
            error(lhs.offset, "type mismatch");
        }

        break;
    case AndAnd:
    case OrOr:
        if (!lhs_value.type->is_bool()) {
            error(lhs.offset, "type mismatch");

            return std::nullopt;
        }

        break;
    default:
        break;
    };

    switch (oper.value) {
    case Plus:
        return Value{
            lhs_value.type,
            lhs_value.type->is_float()
                ? m_builder.CreateFAdd(lhs_value.value, right->value)
                : m_builder.CreateAdd(lhs_value.value, right->value)};
    case Minus:
        return Value{
            lhs_value.type,
            lhs_value.type->is_float()
                ? m_builder.CreateFSub(lhs_value.value, right->value)
                : m_builder.CreateSub(lhs_value.value, right->value)};
    case Star:
        return Value{
            lhs_value.type,
            lhs_value.type->is_float()
                ? m_builder.CreateFMul(lhs_value.value, right->value)
                : m_builder.CreateMul(lhs_value.value, right->value)};
    case Slash:
        if (lhs_value.type->is_float()) {
            return Value{
                lhs_value.type,
                m_builder.CreateFDiv(lhs_value.value, right->value)};
        }

        return Value{
            lhs_value.type,
            lhs_value.type->is_signed_int()
                ? m_builder.CreateSDiv(lhs_value.value, right->value)
                : m_builder.CreateUDiv(lhs_value.value, right->value)};
    case Percent:
        return Value{
            lhs_value.type,
            lhs_value.type->is_signed_int()
                ? m_builder.CreateSRem(lhs_value.value, right->value)
                : m_builder.CreateURem(lhs_value.value, right->value)};
    case And:
        return Value{
            lhs_value.type, m_builder.CreateAnd(lhs_value.value, right->value)};
    case Or:
        return Value{
            lhs_value.type, m_builder.CreateOr(lhs_value.value, right->value)};
    case Xor:
        return Value{
            lhs_value.type, m_builder.CreateXor(lhs_value.value, right->value)};
    case AndAnd:
        return Value{
            m_primitive_types["bool"],
            m_builder.CreateLogicalAnd(lhs_value.value, right->value)};
    case OrOr:
        return Value{
            m_primitive_types["bool"],
            m_builder.CreateLogicalOr(lhs_value.value, right->value)};
    case Less:
        return Value{
            m_primitive_types["bool"],
            lhs_value.type->is_signed_int()
                ? m_builder.CreateICmpSLT(lhs_value.value, right->value)
                : m_builder.CreateICmpULT(lhs_value.value, right->value)};
    case Greater:
        return Value{
            m_primitive_types["bool"],
            lhs_value.type->is_signed_int()
                ? m_builder.CreateICmpSGT(lhs_value.value, right->value)
                : m_builder.CreateICmpUGT(lhs_value.value, right->value)};
    case EqualEqual:
        return Value{
            m_primitive_types["bool"],
            m_builder.CreateICmpEQ(lhs_value.value, right->value)};
    case BangEqual:
        return Value{
            m_primitive_types["bool"],
            m_builder.CreateICmpNE(lhs_value.value, right->value)};
    case GreaterEqual:
        return Value{
            m_primitive_types["bool"],
            lhs_value.type->is_signed_int()
                ? m_builder.CreateICmpSGE(lhs_value.value, right->value)
                : m_builder.CreateICmpUGE(lhs_value.value, right->value)};
    case LessEqual:
        return Value{
            m_primitive_types["bool"],
            lhs_value.type->is_signed_int()
                ? m_builder.CreateICmpSLE(lhs_value.value, right->value)
                : m_builder.CreateICmpULE(lhs_value.value, right->value)};
    default:
        return std::nullopt;
    }
}

Value Codegen::load_value(Value& value) noexcept {
    if (value.is_ref) {
        return value;
    }

    if (auto* variable =
            llvm::dyn_cast_or_null<llvm::AllocaInst>(value.value)) {
        return Value{
            value.type,
            m_builder.CreateLoad(variable->getAllocatedType(), variable),
            value.is_mutable};
    }

    if (auto* val = llvm::dyn_cast_or_null<llvm::LoadInst>(value.value)) {
        auto* type = value.type->codegen(*this);

        if (val->getType()->isPointerTy() && !value.type->is_pointer()) {
            return Value{
                value.type, m_builder.CreateLoad(type, val), value.is_mutable};
        }
    }

    if (auto* ptr =
            llvm::dyn_cast_or_null<llvm::GetElementPtrInst>(value.value)) {
        return Value{
            value.type, m_builder.CreateLoad(ptr->getResultElementType(), ptr),
            value.is_mutable};
    }

    return value;
}

std::shared_ptr<Type> Codegen::get_type(
    std::size_t offset, std::string_view name, Scope& parent) noexcept {
    auto primitive = m_primitive_types.find(name);

    if (primitive != m_primitive_types.end()) {
        return primitive->second;
    }

    auto user = parent.types.find(name);

    if (user == parent.types.end()) {
        error(offset, fmt::format("undeclared type: '{}'", name));

        return nullptr;
    }

    return user->second;
}

std::optional<Value> Codegen::get_name(
    std::size_t offset, std::string_view name, Scope& parent) noexcept {
    auto iterator = parent.names.find(name);

    if (iterator == parent.names.end()) {
        error(offset, fmt::format("undeclared identifier: '{}'", name));

        return std::nullopt;
    }

    return iterator->second;
}

Scope* Codegen::get_scope(
    std::size_t offset, std::string_view name, Scope& parent) noexcept {
    auto iterator = parent.scopes.find(name);

    if (iterator == parent.scopes.end()) {
        error(offset, fmt::format("could not find '{}'", name));

        return nullptr;
    }

    return &iterator->second;
}

void Codegen::generate_fn_proto(ast::FnDecl& decl) noexcept {
    if (!decl.is_extern && !decl.block) {
        error(
            decl.proto.name.offset,
            fmt::format("'{}' has no body", decl.proto.name.value));

        return;
    }

    auto& scope = decl.proto.type
                      ? m_current_scope->scopes[decl.proto.type->value]
                      : *m_current_scope;

    if (scope.names.contains(decl.proto.name.value)) {
        error(
            decl.proto.name.offset,
            fmt::format("'{}' is already defined", decl.proto.name.value));

        return;
    }

    auto return_type = decl.proto.return_type
                           ? decl.proto.return_type->codegen(*this)
                           : m_primitive_types["void"];

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
                parameter.name.offset,
                fmt::format(
                    "'{}' cannot be of type 'void'", parameter.name.value));

            return;
        }

        llvm_param_types.push_back(llvm_type);
        param_types.push_back(type);
    }

    auto* function_type =
        llvm::FunctionType::get(llvm_return_type, llvm_param_types, false);

    auto* function = llvm::Function::Create(
        function_type,
        (decl.is_public || decl.is_extern) ? llvm::Function::ExternalLinkage
                                           : llvm::Function::PrivateLinkage,
        decl.proto.name.value, *m_module);

    if (!decl.proto.type) {
        scope.names[decl.proto.name.value] = Value{
            std::make_shared<types::Function>(
                return_type, std::move(param_types)),
            function};

        return;
    }

    auto type = get_type(
        decl.proto.type->offset, decl.proto.type->value, *m_current_scope);

    auto func_type =
        std::make_shared<types::Function>(return_type, std::move(param_types));

    scope.names[decl.proto.name.value] = {func_type, function};

    if (!decl.proto.params.empty()) {
        auto param_type = decl.proto.params[0].type->codegen(*this);

        if (types_equal(*param_type, *type)) {
            m_methods[type][decl.proto.name.value] = {func_type, function};

            return;
        }

        if (param_type->is_pointer() &&
            types_equal(
                *static_cast<types::Pointer&>(*param_type).type, *type)) {
            m_methods[type][decl.proto.name.value] = {func_type, function};
        }
    }
}

void Codegen::type_mismatch(
    std::size_t offset, Type& expected, Type& got) noexcept {
    error(
        offset, fmt::format(
                    "expected '{}' but got '{}'", expected.to_string(),
                    got.to_string()));
}

} // namespace cent::backend
