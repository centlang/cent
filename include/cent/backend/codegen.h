#ifndef CENT_BACKEND_CODEGEN_H
#define CENT_BACKEND_CODEGEN_H

#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <utility>

#include <fmt/core.h>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "cent/ast/span_value.h"
#include "cent/frontend/token.h"

#include "cent/backend/scope.h"

namespace cent::ast {

struct NamedType;
struct Pointer;
struct Optional;

struct Module;

struct Assignment;
struct BlockStmt;
struct IfElse;
struct ReturnStmt;
struct WhileLoop;

struct BinaryExpr;
struct UnaryExpr;
struct IntLiteral;
struct FloatLiteral;
struct StrLiteral;
struct BoolLiteral;
struct StructLiteral;
struct Identifier;
struct CallExpr;
struct MethodExpr;
struct MemberExpr;
struct AsExpr;

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
struct ISize;

struct U8;
struct U16;
struct U32;
struct U64;
struct USize;

struct F32;
struct F64;

struct Str;

struct Bool;
struct Void;

struct Pointer;
struct Optional;

struct Struct;
struct Function;

} // namespace types

struct Value;
struct Type;

class Codegen {
public:
    [[nodiscard]] Codegen(
        std::unique_ptr<ast::Module> program, std::string_view filename,
        const llvm::DataLayout& layout, const std::string& triple) noexcept
    : m_module{std::make_unique<llvm::Module>("", m_context)},
      m_builder{m_context}, m_program{std::move(program)},
      m_filename{filename} {
        m_module->setDataLayout(layout);
        m_module->setTargetTriple(triple);
    }

    [[nodiscard]] std::unique_ptr<llvm::Module> generate() noexcept;

    [[nodiscard]] bool types_equal(Type& lhs, Type& rhs) noexcept;

    [[nodiscard]] std::optional<Value> cast(
        std::shared_ptr<Type>& type, Value& value,
        bool implicit = true) noexcept;

    std::shared_ptr<Type> generate(ast::NamedType& type) noexcept;
    std::shared_ptr<Type> generate(ast::Pointer& type) noexcept;
    std::shared_ptr<Type> generate(ast::Optional& type) noexcept;

    llvm::Type* generate(types::I8& type) noexcept;
    llvm::Type* generate(types::I16& type) noexcept;
    llvm::Type* generate(types::I32& type) noexcept;
    llvm::Type* generate(types::I64& type) noexcept;
    llvm::Type* generate(types::ISize& type) noexcept;

    llvm::Type* generate(types::U8& type) noexcept;
    llvm::Type* generate(types::U16& type) noexcept;
    llvm::Type* generate(types::U32& type) noexcept;
    llvm::Type* generate(types::U64& type) noexcept;
    llvm::Type* generate(types::USize& type) noexcept;

    llvm::Type* generate(types::F32& type) noexcept;
    llvm::Type* generate(types::F64& type) noexcept;

    llvm::Type* generate(types::Str& type) noexcept;

    llvm::Type* generate(types::Bool& type) noexcept;
    llvm::Type* generate(types::Void& type) noexcept;

    llvm::Type* generate(types::Pointer& type) noexcept;
    llvm::Type* generate(types::Optional& type) noexcept;

    llvm::Type* generate(types::Struct& type) noexcept;
    llvm::Type* generate(types::Function& type) noexcept;

    std::optional<Value> generate(ast::Assignment& stmt) noexcept;
    std::optional<Value> generate(ast::BlockStmt& stmt) noexcept;
    std::optional<Value> generate(ast::IfElse& stmt) noexcept;
    std::optional<Value> generate(ast::ReturnStmt& stmt) noexcept;
    std::optional<Value> generate(ast::WhileLoop& stmt) noexcept;

    std::optional<Value> generate(ast::BinaryExpr& expr) noexcept;
    std::optional<Value> generate(ast::UnaryExpr& expr) noexcept;
    std::optional<Value> generate(ast::IntLiteral& expr) noexcept;
    std::optional<Value> generate(ast::FloatLiteral& expr) noexcept;
    std::optional<Value> generate(ast::StrLiteral& expr) noexcept;
    std::optional<Value> generate(ast::BoolLiteral& expr) noexcept;
    std::optional<Value> generate(ast::StructLiteral& expr) noexcept;
    std::optional<Value> generate(ast::Identifier& expr) noexcept;
    std::optional<Value> generate(ast::CallExpr& expr) noexcept;
    std::optional<Value> generate(ast::MethodExpr& expr) noexcept;
    std::optional<Value> generate(ast::MemberExpr& expr) noexcept;
    std::optional<Value> generate(ast::AsExpr& expr) noexcept;

    std::optional<Value> generate(ast::FnDecl& decl) noexcept;
    std::optional<Value> generate(ast::Struct& decl) noexcept;

    std::optional<Value> generate(ast::VarDecl& decl) noexcept;

private:
    void generate(ast::Module& module, bool is_submodule = false) noexcept;

    std::optional<Value> generate(ast::Expression& expr) noexcept;

    std::optional<Value> generate_bin_expr(
        ast::SpanValue<Value&> lhs, ast::SpanValue<Value&> rhs,
        ast::SpanValue<frontend::Token::Type> oper) noexcept;

    Value load_value(Value& value) noexcept;

    std::shared_ptr<Type>
    get_type(Span span, std::string_view name, Scope& parent) noexcept;

    std::optional<Value>
    get_name(Span span, std::string_view name, Scope& parent) noexcept;

    Scope* get_scope(Span span, std::string_view name, Scope& parent) noexcept;

    void generate_fn_proto(ast::FnDecl& decl) noexcept;

    static constexpr auto optional_member_value = 0;
    static constexpr auto optional_member_bool = 1;

    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    llvm::IRBuilder<> m_builder;

    Scope m_scope;

    std::map<std::string_view, std::shared_ptr<Type>> m_primitive_types;

    Scope* m_current_scope{&m_scope};
    types::Function* m_current_function{nullptr};

    std::map<llvm::StructType*, std::map<std::string_view, std::size_t>>
        m_members;

    struct Method {
        std::shared_ptr<types::Function> type;
        llvm::Function* function;
    };

    std::map<std::shared_ptr<Type>, std::map<std::string_view, Method>>
        m_methods;

    std::map<std::filesystem::path, Scope> m_generated_modules;

    std::map<llvm::Type*, llvm::StructType*> m_optional_types;

    std::unique_ptr<ast::Module> m_program;

    std::string_view m_filename;
};

} // namespace cent::backend

#endif
