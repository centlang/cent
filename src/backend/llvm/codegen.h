#ifndef CENT_BACKEND_CODEGEN_H
#define CENT_BACKEND_CODEGEN_H

#include <filesystem>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <utility>

#include <fmt/core.h>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "frontend/token.h"
#include "offset_value.h"
#include "util.h"

#include "backend/llvm/scope.h"

namespace cent::ast {

struct NamedType;
struct Pointer;
struct Optional;
struct ArrayType;
struct SliceType;
struct TupleType;
struct RangeType;
struct FnPointer;

struct Module;

struct Assignment;
struct BlockStmt;
struct IfElse;
struct Switch;
struct ReturnStmt;
struct WhileLoop;
struct ForLoop;
struct BreakStmt;
struct ContinueStmt;
struct Unreachable;
struct AssertStmt;

struct BinaryExpr;
struct UnaryExpr;
struct IntLiteral;
struct FloatLiteral;
struct StrLiteral;
struct BoolLiteral;
struct NullLiteral;
struct Undefined;
struct RangeLiteral;
struct StructLiteral;
struct ArrayLiteral;
struct TupleLiteral;
struct Identifier;
struct CallExpr;
struct MethodExpr;
struct MemberExpr;
struct IndexExpr;
struct SliceExpr;
struct AsExpr;

struct FnProto;
struct FnDecl;
struct Struct;
struct Union;
struct EnumDecl;
struct TypeAlias;

struct VarDecl;

struct Expression;
struct Declaration;

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

struct Bool;
struct Null;
struct Undefined;
struct Void;

struct Pointer;
struct Optional;
struct Range;

struct Array;
struct Slice;
struct Tuple;

struct Struct;
struct Union;
struct Enum;
struct Alias;
struct Function;

} // namespace types

struct Value;
struct Type;

using Attributes = std::set<std::string_view>;

class Codegen {
public:
    [[nodiscard]] Codegen(
        std::unique_ptr<ast::Module> program, std::string_view filename,
        const llvm::DataLayout& layout, const std::string& triple)
    : m_module{std::make_unique<llvm::Module>("", m_context)},
      m_builder{m_context}, m_program{std::move(program)},
      m_filename{filename} {
        m_module->setDataLayout(layout);
        m_module->setTargetTriple(triple);
    }

    [[nodiscard]] std::unique_ptr<llvm::Module> generate();

    [[nodiscard]] bool had_error() const { return m_had_error; }

    [[nodiscard]] std::shared_ptr<Type> generate(const ast::NamedType& type);
    [[nodiscard]] std::shared_ptr<Type> generate(const ast::Pointer& type);
    [[nodiscard]] std::shared_ptr<Type> generate(const ast::Optional& type);
    [[nodiscard]] std::shared_ptr<Type> generate(const ast::ArrayType& type);
    [[nodiscard]] std::shared_ptr<Type> generate(const ast::SliceType& type);
    [[nodiscard]] std::shared_ptr<Type> generate(const ast::TupleType& type);
    [[nodiscard]] std::shared_ptr<Type> generate(const ast::RangeType& type);
    [[nodiscard]] std::shared_ptr<Type> generate(const ast::FnPointer& type);

    [[nodiscard]] llvm::Type* generate(const types::I8& type);
    [[nodiscard]] llvm::Type* generate(const types::I16& type);
    [[nodiscard]] llvm::Type* generate(const types::I32& type);
    [[nodiscard]] llvm::Type* generate(const types::I64& type);
    [[nodiscard]] llvm::Type* generate(const types::ISize& type);

    [[nodiscard]] llvm::Type* generate(const types::U8& type);
    [[nodiscard]] llvm::Type* generate(const types::U16& type);
    [[nodiscard]] llvm::Type* generate(const types::U32& type);
    [[nodiscard]] llvm::Type* generate(const types::U64& type);
    [[nodiscard]] llvm::Type* generate(const types::USize& type);

    [[nodiscard]] llvm::Type* generate(const types::F32& type);
    [[nodiscard]] llvm::Type* generate(const types::F64& type);

    [[nodiscard]] llvm::Type* generate(const types::Bool& type);
    [[nodiscard]] llvm::Type* generate(const types::Null& type);
    [[nodiscard]] llvm::Type* generate(const types::Undefined& type);
    [[nodiscard]] llvm::Type* generate(const types::Void& type);

    [[nodiscard]] llvm::Type* generate(const types::Pointer& type);
    [[nodiscard]] llvm::Type* generate(const types::Optional& type);
    [[nodiscard]] llvm::Type* generate(const types::Range& type);

    [[nodiscard]] llvm::Type* generate(const types::Array& type);
    [[nodiscard]] llvm::Type* generate(const types::Slice& type);
    [[nodiscard]] llvm::Type* generate(const types::Tuple& type);

    [[nodiscard]] llvm::Type* generate(const types::Struct& type);
    [[nodiscard]] llvm::Type* generate(const types::Union& type);
    [[nodiscard]] llvm::Type* generate(const types::Enum& type);
    [[nodiscard]] llvm::Type* generate(const types::Alias& type);
    [[nodiscard]] llvm::Type* generate(const types::Function& type);

    std::optional<Value> generate(const ast::Assignment& stmt);
    std::optional<Value> generate(const ast::BlockStmt& stmt);
    std::optional<Value> generate(const ast::IfElse& stmt);
    std::optional<Value> generate(const ast::Switch& stmt);
    std::optional<Value> generate(const ast::ReturnStmt& stmt);
    std::optional<Value> generate(const ast::WhileLoop& stmt);
    std::optional<Value> generate(const ast::ForLoop& stmt);
    std::optional<Value> generate(const ast::BreakStmt& stmt);
    std::optional<Value> generate(const ast::ContinueStmt& stmt);
    std::optional<Value> generate(const ast::Unreachable& stmt);
    std::optional<Value> generate(const ast::AssertStmt& stmt);

    [[nodiscard]] std::optional<Value> generate(const ast::BinaryExpr& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::UnaryExpr& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::IntLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::FloatLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::StrLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::BoolLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::NullLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::Undefined& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::RangeLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::StructLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::ArrayLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::TupleLiteral& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::Identifier& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::CallExpr& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::MethodExpr& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::MemberExpr& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::IndexExpr& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::SliceExpr& expr);
    [[nodiscard]] std::optional<Value> generate(const ast::AsExpr& expr);

    std::optional<Value> generate(const ast::FnDecl& decl);
    std::optional<Value> generate(const ast::Struct& decl);
    std::optional<Value> generate(const ast::Union& decl);
    std::optional<Value> generate(const ast::EnumDecl& decl);
    std::optional<Value> generate(const ast::TypeAlias& decl);

    std::optional<Value> generate(const ast::VarDecl& decl);

private:
    void generate(const ast::Module& module, bool is_submodule = false);

    [[nodiscard]] bool types_equal(const Type& lhs, const Type& rhs) const;

    [[nodiscard]] std::optional<Value>
    cast(std::shared_ptr<Type> type, const Value& value, bool implicit = true);

    [[nodiscard]] std::optional<Value> primitive_cast(
        std::shared_ptr<Type> type, llvm::Type* llvm_type, const Value& value,
        bool implicit = true);

    [[nodiscard]] bool cast_to_result(
        std::shared_ptr<Type> type, const Value& value, bool implicit = true);

    [[nodiscard]] std::optional<Value> generate_bin_expr(
        ast::OffsetValue<const Value&> lhs, ast::OffsetValue<const Value&> rhs,
        ast::OffsetValue<frontend::Token::Type> oper);

    [[nodiscard]] Value load_value(const Value& value);

    [[nodiscard]] llvm::Value* create_alloca(llvm::Type* type);

    [[nodiscard]] llvm::Value* load_struct_member(
        llvm::Type* struct_type, llvm::Type* member_type, llvm::Value* value,
        std::uint32_t index);

    [[nodiscard]] llvm::Value* create_gep_or_extract(
        llvm::Type* struct_type, llvm::Value* value, std::uint32_t index);

    [[nodiscard]] std::shared_ptr<Type>
    get_type(std::size_t offset, std::string_view name, Scope& parent);

    [[nodiscard]] Value*
    get_name(std::size_t offset, std::string_view name, Scope& parent);

    [[nodiscard]] Scope*
    get_scope(std::size_t offset, std::string_view name, Scope& parent);

    [[nodiscard]] std::shared_ptr<types::Function>
    generate_fn_type(const ast::FnProto& proto);

    void create_panic_fn();

    void generate_fn_proto(const ast::FnDecl& decl);
    void generate_struct(const ast::Struct& decl);
    void generate_union(const ast::Union& decl);
    void generate_enum(const ast::EnumDecl& decl);

    void
    type_mismatch(std::size_t offset, const Type& expected, const Type& got);

    void error(std::size_t offset, std::string_view message) {
        auto src = read_file(m_filename);
        auto [line, column] = cent::offset_to_pos(*src, offset);

        log::error(line, column, m_filename, message);
        m_had_error = true;
    }

    [[nodiscard]] Attributes parse_attrs(
        const ast::Declaration& decl,
        const std::set<std::string_view>& allowed);

    [[nodiscard]] static bool is_float(const Type& type);
    [[nodiscard]] static bool is_sint(const Type& type);
    [[nodiscard]] static bool is_uint(const Type& type);

    static constexpr auto optional_member_value = 0;
    static constexpr auto optional_member_bool = 1;

    static constexpr auto slice_member_ptr = 0;
    static constexpr auto slice_member_len = 1;

    static constexpr auto union_member_value = 0;
    static constexpr auto union_member_tag = 1;

    static constexpr auto range_member_begin = 0;
    static constexpr auto range_member_end = 1;

    llvm::LLVMContext m_context;
    std::unique_ptr<llvm::Module> m_module;
    llvm::IRBuilder<> m_builder;

    Scope m_scope;

    std::map<std::string_view, std::shared_ptr<Type>> m_primitive_types;

    Scope* m_current_scope{&m_scope};
    types::Function* m_current_function{nullptr};
    llvm::Value* m_current_result{nullptr};

    llvm::BasicBlock* m_loop_body{nullptr};
    llvm::BasicBlock* m_loop_end{nullptr};

    std::string m_current_scope_prefix;

    std::map<llvm::StructType*, std::map<std::string_view, std::size_t>>
        m_members;

    struct Method {
        std::shared_ptr<types::Function> type;
        llvm::Function* function;
    };

    std::map<Type*, std::map<std::string_view, Method>> m_methods;

    std::map<std::filesystem::path, Scope> m_generated_modules;

    std::shared_ptr<types::Null> m_null_type;
    std::shared_ptr<types::Undefined> m_undefined_type;
    std::shared_ptr<types::Void> m_void_type;

    llvm::StructType* m_slice_type{};
    llvm::Function* m_panic_fn{};

    std::map<llvm::Type*, llvm::StructType*> m_optional_types;
    std::map<llvm::Type*, llvm::StructType*> m_range_types;

    std::unique_ptr<ast::Module> m_program;
    std::string m_filename;

    bool m_had_error{false};
};

} // namespace cent::backend

#endif
