#ifndef CENT_BACKEND_CODEGEN_H
#define CENT_BACKEND_CODEGEN_H

#include <filesystem>
#include <initializer_list>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <tuple>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/TargetParser/Triple.h>

#include "frontend/token.h"
#include "offset_value.h"
#include "util.h"

#include "backend/llvm/types/function.h"
#include "backend/llvm/types/primitive.h"

#include "backend/llvm/scope.h"

namespace cent::ast {

struct NamedType;
struct Pointer;
struct Optional;
struct ArrayType;
struct SliceType;
struct TupleType;
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

struct BinaryExpr;
struct UnaryExpr;
struct UnwrapExpr;
struct IntLiteral;
struct FloatLiteral;
struct StrLiteral;
struct RuneLiteral;
struct BoolLiteral;
struct NullLiteral;
struct Undefined;
struct RangeLiteral;
struct StructLiteral;
struct ArrayLiteral;
struct TupleLiteral;
struct Identifier;
struct CallExpr;
struct CallExprGeneric;
struct MethodExpr;
struct MemberExpr;
struct IndexExpr;
struct SliceExpr;
struct AsExpr;
struct SizeofExpr;

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

struct Value;
struct Type;

class Codegen {
public:
    [[nodiscard]] Codegen(
        std::unique_ptr<ast::Module> program, std::string_view filename,
        const llvm::DataLayout& layout);

    [[nodiscard]] std::unique_ptr<llvm::Module> generate();

    [[nodiscard]] bool had_error() const { return m_had_error; }

    [[nodiscard]] Type* generate(const ast::NamedType& type);
    [[nodiscard]] Type* generate(const ast::Pointer& type);
    [[nodiscard]] Type* generate(const ast::Optional& type);
    [[nodiscard]] Type* generate(const ast::ArrayType& type);
    [[nodiscard]] Type* generate(const ast::ArrayType& type, std::size_t size);
    [[nodiscard]] Type* generate(const ast::SliceType& type);
    [[nodiscard]] Type* generate(const ast::TupleType& type);
    [[nodiscard]] Type* generate(const ast::FnPointer& type);

    Value generate(const ast::Assignment& stmt);
    Value generate(const ast::BlockStmt& stmt);
    Value generate(const ast::IfElse& stmt);
    Value generate(const ast::Switch& stmt);
    Value generate(const ast::ReturnStmt& stmt);
    Value generate(const ast::WhileLoop& stmt);
    Value generate(const ast::ForLoop& stmt);
    Value generate(const ast::BreakStmt& stmt);
    Value generate(const ast::ContinueStmt& stmt);
    Value generate(const ast::Unreachable& stmt);

    [[nodiscard]] Value generate(const ast::BinaryExpr& expr);
    [[nodiscard]] Value generate(const ast::UnaryExpr& expr);
    [[nodiscard]] Value generate(const ast::UnwrapExpr& expr);
    [[nodiscard]] Value generate(const ast::IntLiteral& expr);
    [[nodiscard]] Value generate(const ast::FloatLiteral& expr);
    [[nodiscard]] Value generate(const ast::StrLiteral& expr);
    [[nodiscard]] Value generate(const ast::RuneLiteral& expr);
    [[nodiscard]] Value generate(const ast::BoolLiteral& expr);
    [[nodiscard]] Value generate(const ast::NullLiteral& expr);
    [[nodiscard]] Value generate(const ast::Undefined& expr);
    [[nodiscard]] Value generate(const ast::RangeLiteral& expr);
    [[nodiscard]] Value generate(const ast::StructLiteral& expr);
    [[nodiscard]] Value generate(const ast::ArrayLiteral& expr);
    [[nodiscard]] Value generate(const ast::TupleLiteral& expr);
    [[nodiscard]] Value generate(const ast::Identifier& expr);
    [[nodiscard]] Value generate(const ast::CallExpr& expr);

    [[nodiscard]] Value generate(const ast::CallExprGeneric& expr);

    [[nodiscard]] Value generate(const ast::MethodExpr& expr);
    [[nodiscard]] Value generate(const ast::MemberExpr& expr);
    [[nodiscard]] Value generate(const ast::IndexExpr& expr);
    [[nodiscard]] Value generate(const ast::SliceExpr& expr);
    [[nodiscard]] Value generate(const ast::AsExpr& expr);
    [[nodiscard]] Value generate(const ast::SizeofExpr& expr);

    Value generate(const ast::FnDecl& decl);
    Value generate(const ast::Struct& decl);
    Value generate(const ast::Union& decl);
    Value generate(const ast::EnumDecl& decl);
    Value generate(const ast::TypeAlias& decl);

    Value generate(const ast::VarDecl& decl);

private:
    [[nodiscard]] Value create_core_panic();

    void create_core();
    void create_core_mem();

    void create_main();

    void generate(const ast::Module& module);

    [[nodiscard]] Value
    primitive_cast(Type* type, const Value& value, bool implicit = true);

    [[nodiscard]] Value
    cast(Type* type, const Value& value, bool implicit = true);

    [[nodiscard]] Value cast_or_error(
        std::size_t offset, Type* type, const Value& value,
        bool implicit = true);

    [[nodiscard]] Value generate_bin_logical_expr(
        const ast::Expression& lhs, const ast::Expression& rhs,
        OffsetValue<frontend::Token::Type> oper);

    [[nodiscard]] Value generate_bin_expr(
        OffsetValue<const Value&> lhs, OffsetValue<const Value&> rhs,
        OffsetValue<frontend::Token::Type> oper);

    [[nodiscard]] Value create_call(
        std::size_t offset, types::Function* type, llvm::Value* function,
        const std::vector<std::unique_ptr<ast::Expression>>& arguments);

    [[nodiscard]] Value load_rvalue(const Value& value);
    [[nodiscard]] Value load_lvalue(const Value& value);

    [[nodiscard]] llvm::Value*
    create_alloca(llvm::Type* type, llvm::Value* size = nullptr);

    [[nodiscard]] llvm::Value* create_alloca(Type* type);

    void create_out_of_bounds_check(
        llvm::Value* index, llvm::Value* len, bool inclusive = false);

    void create_panic(std::string_view message);

    void create_store(const Value& src, llvm::Value* dest);
    void zero_init(llvm::Value* value, Type* type);

    [[nodiscard]] llvm::Value* create_alloca_or_error(
        std::size_t offset, llvm::Type* type, llvm::Value* size = nullptr);

    [[nodiscard]] llvm::Value*
    create_alloca_or_error(std::size_t offset, Type* type);

    [[nodiscard]] llvm::Value* get_optional_bool(const Value& value);
    [[nodiscard]] llvm::Value* get_optional_value(const Value& value);

    [[nodiscard]] Value get_struct_member(
        Type* member_type, const Value& value, std::uint32_t index);

    [[nodiscard]] llvm::Value* load_struct_member(
        llvm::Type* member_type, const Value& value, std::uint32_t index);

    template <typename... Maps>
    [[nodiscard]] std::optional<std::string>
    did_you_mean_hint(std::string_view name, const Maps&... maps) {
        if (auto match = closest_match(name, maps...)) {
            return fmt::format("did you mean {}?", log::quoted(*match));
        }

        return std::nullopt;
    }

    [[nodiscard]] Type*
    get_type(std::size_t offset, std::string_view name, Scope& parent);

    [[nodiscard]] Value*
    get_name(std::size_t offset, std::string_view name, Scope& parent);

    [[nodiscard]] Scope*
    get_scope(std::size_t offset, std::string_view name, Scope& parent);

    [[nodiscard]] Scope*
    resolve_scope(const std::vector<OffsetValue<std::string>>& value);

    [[nodiscard]] types::Function* generate_fn_type(const ast::FnProto& proto);

    [[nodiscard]] types::Function* get_fn_type(
        Type* return_type, std::vector<Type*> param_types,
        std::vector<llvm::Constant*> default_args = {}, bool variadic = false);

    [[nodiscard]] types::Pointer* get_ptr_type(Type* type, bool is_mutable);
    [[nodiscard]] types::Slice* get_slice_type(Type* type, bool is_mutable);
    [[nodiscard]] types::Array* get_array_type(Type* type, std::size_t size);
    [[nodiscard]] types::Tuple* get_tuple_type(const std::vector<Type*>& types);
    [[nodiscard]] types::Optional* get_optional_type(Type* type);
    [[nodiscard]] types::Range* get_range_type(Type* type, bool inclusive);

    [[nodiscard]] types::VarLenArray*
    get_var_len_array_type(Type* type, llvm::Value* size);

    [[nodiscard]] Type* unwrap_type(Type* type, bool ignore_distinct = false);

    [[nodiscard]] TranslationUnit get_unit(const std::filesystem::path& path) {
        auto directory = std::filesystem::absolute(path);

        if (!std::filesystem::is_directory(path)) {
            directory = path.parent_path();
        }

        for (std::size_t i = 0; i < m_units.size(); ++i) {
            if (directory == m_units[i]) {
                return static_cast<TranslationUnit>(i);
            }
        }

        m_units.push_back(directory);
        return static_cast<TranslationUnit>(m_units.size()) - 1;
    }

    template <typename ElementType>
    [[nodiscard]] bool is_accessible(
        const Scope::Element<ElementType>& element, TranslationUnit unit) {
        return element.is_public || element.unit == unit;
    }

    void generate_fn_proto(const ast::FnDecl& decl);

    void
    type_mismatch(std::size_t offset, const Type* expected, const Type* got);

    template <typename... Args>
    void error_hint(
        std::size_t offset, std::string_view hint,
        fmt::format_string<Args...> message, Args&&... args) {
        auto src = read_file(m_filename);
        auto loc = offset_to_loc(*src, offset);

        log::error_hint(
            loc.line, loc.column, m_filename, loc.code, hint, message,
            std::forward<Args>(args)...);

        m_had_error = true;

        if (m_current_function) {
            m_current_fn_had_error = true;
        }
    }

    template <typename... Args>
    void error(
        std::size_t offset, fmt::format_string<Args...> message,
        Args&&... args) {
        auto src = read_file(m_filename);
        auto loc = offset_to_loc(*src, offset);

        log::error(
            loc.line, loc.column, m_filename, loc.code, message,
            std::forward<Args>(args)...);

        m_had_error = true;

        if (m_current_function) {
            m_current_fn_had_error = true;
        }
    }

    template <typename... Args>
    void warning(
        std::size_t offset, fmt::format_string<Args...> message,
        Args&&... args) {
        auto src = read_file(m_filename);
        auto loc = offset_to_loc(*src, offset);

        log::warning(
            loc.line, loc.column, m_filename, loc.code, message,
            std::forward<Args>(args)...);
    }

    template <typename... Args>
    void not_implemented(
        std::size_t offset, fmt::format_string<Args...> message,
        Args&&... args) {
        error(
            offset, "not implemented: {}",
            fmt::format(message, std::forward(args)...));
    }

    void report_invalid_attrs(
        const ast::Declaration& decl,
        std::initializer_list<std::string_view> allowed);

    template <typename... Attrs>
    [[nodiscard]] std::array<bool, sizeof...(Attrs)>
    parse_attrs_validate(const ast::Declaration& decl, Attrs... attrs) {
        report_invalid_attrs(decl, {attrs...});
        return parse_attrs(decl, attrs...);
    }

    template <typename... Attrs>
    [[nodiscard]] std::array<bool, sizeof...(Attrs)>
    parse_attrs(const ast::Declaration& decl, Attrs... attrs) {
        std::array<bool, sizeof...(Attrs)> result = {
            decl_get_attr(decl, attrs)...};

        return result;
    }

    [[nodiscard]] static bool matches_target(const ast::Declaration& decl);

    [[nodiscard]] static bool
    decl_get_attr(const ast::Declaration& decl, std::string_view attr);

    [[nodiscard]] static std::optional<llvm::Triple::OSType>
    attr_to_os_type(std::string_view attr) {
        if (attr == "linux") {
            return llvm::Triple::Linux;
        }

        if (attr == "windows") {
            return llvm::Triple::Win32;
        }

        return std::nullopt;
    }

    [[nodiscard]] static std::optional<llvm::Triple::ArchType>
    attr_to_arch_type(std::string_view attr) {
        if (attr == "x86_64") {
            return llvm::Triple::x86_64;
        }

        if (attr == "x86") {
            return llvm::Triple::x86;
        }

        if (attr == "aarch64") {
            return llvm::Triple::aarch64;
        }

        if (attr == "riscv64") {
            return llvm::Triple::riscv64;
        }

        return std::nullopt;
    }

    [[nodiscard]] static bool is_float(const Type* type);
    [[nodiscard]] static bool is_sint(const Type* type);
    [[nodiscard]] static bool is_uint(const Type* type);

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
    Scope m_core_module;

    std::map<std::string_view, std::unique_ptr<Type>> m_primitive_types;

    Scope* m_current_scope{&m_scope};
    types::Function* m_current_function{nullptr};
    TranslationUnit m_current_unit{0};
    bool m_current_fn_had_error{false};

    llvm::BasicBlock* m_loop_continue{nullptr};
    llvm::BasicBlock* m_loop_end{nullptr};

    std::string m_current_scope_prefix{"<main>::"};

    std::map<llvm::StructType*, std::map<std::string_view, std::size_t>>
        m_members;

    struct Method {
        types::Function* type;
        llvm::Function* function;
    };

    std::map<Type*, std::map<std::string_view, Method>> m_methods;

    std::map<std::filesystem::path, Scope> m_generated_modules;

    std::unique_ptr<types::Null> m_null_type;
    std::unique_ptr<types::Undefined> m_undefined_type;
    std::unique_ptr<types::Void> m_void_type;

    llvm::StructType* m_slice_type{};
    llvm::Type* m_size{};

    std::unique_ptr<ast::Module> m_program;
    std::string m_filename;

    std::map<std::pair<Type*, bool>, std::unique_ptr<types::Pointer>>
        m_ptr_types;

    std::map<std::pair<Type*, std::size_t>, std::unique_ptr<types::Array>>
        m_array_types;

    std::map<
        std::pair<Type*, llvm::Value*>, std::unique_ptr<types::VarLenArray>>
        m_var_len_array_types;

    std::map<std::pair<Type*, bool>, std::unique_ptr<types::Slice>>
        m_slice_types;

    std::map<std::vector<Type*>, std::unique_ptr<types::Tuple>> m_tuple_types;

    std::map<std::pair<Type*, bool>, std::unique_ptr<types::Range>>
        m_range_types;

    std::map<Type*, std::unique_ptr<types::Optional>> m_optional_types;

    using FnTypeKey = std::tuple<
        Type*, std::vector<Type*>, std::vector<llvm::Constant*>, bool>;

    std::map<FnTypeKey, std::unique_ptr<types::Function>> m_fn_types;

    std::vector<std::unique_ptr<Type>> m_named_types;

    std::vector<std::filesystem::path> m_units;

    bool m_had_error{false};
};

} // namespace cent::backend

#endif
