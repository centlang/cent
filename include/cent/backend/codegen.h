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

#include "cent/log.h"
#include "cent/span.h"

namespace cent {

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

struct FnDecl;
struct VarDecl;

class Codegen {
public:
    [[nodiscard]] Codegen(
        std::unique_ptr<Program> program, std::string_view filename) noexcept
    : m_module{std::make_unique<llvm::Module>("", m_context)},
      m_builder{m_context}, m_program{std::move(program)},
      m_filename{filename} {}

    [[nodiscard]] std::unique_ptr<llvm::Module> generate() noexcept;

    llvm::Value* generate(Assignment& stmt) noexcept;
    llvm::Value* generate(BlockStmt& stmt) noexcept;
    llvm::Value* generate(IfElse& stmt) noexcept;
    llvm::Value* generate(ReturnStmt& stmt) noexcept;
    llvm::Value* generate(WhileLoop& stmt) noexcept;

    llvm::Value* generate(BinaryExpr& expr) noexcept;
    llvm::Value* generate(UnaryExpr& expr) noexcept;
    llvm::Value* generate(IntLiteral& expr) noexcept;
    llvm::Value* generate(FloatLiteral& expr) noexcept;
    llvm::Value* generate(BoolLiteral& expr) noexcept;
    llvm::Value* generate(Identifier& expr) noexcept;
    llvm::Value* generate(CallExpr& expr) noexcept;

    llvm::Value* generate(FnDecl& decl) noexcept;
    llvm::Value* generate(VarDecl& decl) noexcept;

private:
    struct Variable {
        llvm::Value* value;
        bool is_mutable;
    };

    void generate_fn_proto(FnDecl& decl) noexcept;

    [[nodiscard]] llvm::FunctionType* get_fn_type(FnDecl& decl) noexcept;

    [[nodiscard]] llvm::Type*
    get_type(Span span, std::string_view name) noexcept {
        if (name == "i32") {
            return get_i32_type();
        }

        if (name == "f32") {
            return get_f32_type();
        }

        if (name == "bool") {
            return get_bool_type();
        }

        if (name == "void") {
            return get_void_type();
        }

        error(span.begin, m_filename, fmt::format("undeclared type: {}", name));

        return nullptr;
    }

    [[nodiscard]] llvm::Type* get_i32_type() noexcept {
        return llvm::Type::getInt32Ty(m_context);
    }

    [[nodiscard]] llvm::Type* get_f32_type() noexcept {
        return llvm::Type::getFloatTy(m_context);
    }

    [[nodiscard]] llvm::Type* get_bool_type() noexcept {
        return llvm::Type::getInt1Ty(m_context);
    }

    [[nodiscard]] llvm::Type* get_void_type() noexcept {
        return llvm::Type::getVoidTy(m_context);
    }

    template <typename ValueType> auto from_string(std::string_view value) {
        ValueType result;
        std::from_chars(value.data(), value.data() + value.size(), result);

        return result;
    }

    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    llvm::IRBuilder<> m_builder;

    std::map<std::string_view, Variable> m_locals;

    std::unique_ptr<Program> m_program;

    std::string_view m_filename;
};

} // namespace cent

#endif
