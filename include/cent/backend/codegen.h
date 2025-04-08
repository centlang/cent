#ifndef CENT_BACKEND_CODEGEN_H
#define CENT_BACKEND_CODEGEN_H

#include <charconv>
#include <map>
#include <memory>
#include <utility>

#include <fmt/core.h>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "cent/span.h"

#include "cent/backend/variable.h"

namespace cent::ast {

struct Program;

struct Assignment;
struct BlockStmt;
struct IfElse;
struct ReturnStmt;
struct WhileLoop;

struct BinaryExpr;
struct UnaryExpr;
struct IntLiteral;
struct FloatLiteral;
struct BoolLiteral;
struct Identifier;
struct CallExpr;
struct MemberExpr;

struct FnDecl;
struct Struct;

struct VarDecl;

struct Expression;

} // namespace cent::ast

namespace cent::backend {

namespace types {

struct I8;
struct I16;
struct I32;
struct I64;

struct F32;
struct F64;

struct Bool;
struct Void;

struct Struct;

} // namespace types

struct Type;

class Codegen {
public:
    [[nodiscard]] Codegen(
        std::unique_ptr<ast::Program> program,
        std::string_view filename) noexcept
    : m_module{std::make_unique<llvm::Module>("", m_context)},
      m_builder{m_context}, m_program{std::move(program)},
      m_filename{filename} {}

    [[nodiscard]] std::unique_ptr<llvm::Module> generate() noexcept;

    llvm::Type* generate(types::I8& type) noexcept;
    llvm::Type* generate(types::I16& type) noexcept;
    llvm::Type* generate(types::I32& type) noexcept;
    llvm::Type* generate(types::I64& type) noexcept;

    llvm::Type* generate(types::F32& type) noexcept;
    llvm::Type* generate(types::F64& type) noexcept;

    llvm::Type* generate(types::Bool& type) noexcept;
    llvm::Type* generate(types::Void& type) noexcept;

    llvm::Type* generate(types::Struct& type) noexcept;

    llvm::Value* generate(ast::Assignment& stmt) noexcept;
    llvm::Value* generate(ast::BlockStmt& stmt) noexcept;
    llvm::Value* generate(ast::IfElse& stmt) noexcept;
    llvm::Value* generate(ast::ReturnStmt& stmt) noexcept;
    llvm::Value* generate(ast::WhileLoop& stmt) noexcept;

    llvm::Value* generate(ast::BinaryExpr& expr) noexcept;
    llvm::Value* generate(ast::UnaryExpr& expr) noexcept;
    llvm::Value* generate(ast::IntLiteral& expr) noexcept;
    llvm::Value* generate(ast::FloatLiteral& expr) noexcept;
    llvm::Value* generate(ast::BoolLiteral& expr) noexcept;
    llvm::Value* generate(ast::Identifier& expr) noexcept;
    llvm::Value* generate(ast::CallExpr& expr) noexcept;
    llvm::Value* generate(ast::MemberExpr& expr) noexcept;

    llvm::Value* generate(ast::FnDecl& decl) noexcept;
    llvm::Value* generate(ast::Struct& decl) noexcept;

    llvm::Value* generate(ast::VarDecl& decl) noexcept;

private:
    llvm::Value* generate(ast::Expression& expr) noexcept;

    void generate_fn_proto(ast::FnDecl& decl) noexcept;

    [[nodiscard]] llvm::FunctionType* get_fn_type(ast::FnDecl& decl) noexcept;

    [[nodiscard]] Type* get_type(Span span, std::string_view name) noexcept;

    template <typename ValueType> auto from_string(std::string_view value) {
        ValueType result;
        std::from_chars(value.data(), value.data() + value.size(), result);

        return result;
    }

    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    llvm::IRBuilder<> m_builder;

    std::map<std::string_view, std::shared_ptr<Type>> m_types;
    std::map<std::string_view, Variable> m_locals;

    std::map<llvm::StructType*, std::map<std::string_view, std::size_t>>
        m_members;

    std::unique_ptr<ast::Program> m_program;

    std::string_view m_filename;
};

} // namespace cent::backend

#endif
