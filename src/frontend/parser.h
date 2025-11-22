#ifndef CENT_FRONTEND_PARSER_H
#define CENT_FRONTEND_PARSER_H

#include <array>
#include <concepts>
#include <cstdint>
#include <filesystem>
#include <memory>
#include <optional>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include "log.h"
#include "util.h"

#include "frontend/lexer.h"

#include "ast/attribute.h"
#include "ast/module.h"
#include "ast/node.h"

#include "ast/expr/binary_expr.h"
#include "ast/expr/literals.h"

#include "ast/stmt/block_stmt.h"
#include "ast/stmt/if_else.h"

#include "ast/decl/enum_decl.h"
#include "ast/decl/fn_decl.h"
#include "ast/decl/struct.h"
#include "ast/decl/type_alias.h"
#include "ast/decl/union.h"
#include "ast/decl/var_decl.h"

namespace cent::frontend {

class Parser {
public:
    [[nodiscard]] Parser(std::string_view source, std::string filename)
    : m_lexer{source, filename}, m_source{source},
      m_filename{std::move(filename)} {
        for (auto& token : m_buffer) {
            token = m_lexer.token();
            m_lexer.next_token();
        }
    }

    [[nodiscard]] std::unique_ptr<ast::Module> parse();

    [[nodiscard]] bool had_error() const {
        return m_had_error || m_lexer.had_error();
    };

private:
    [[nodiscard]] auto peek(std::uint8_t ahead = 0) const {
        return m_buffer[(m_buffer_index + ahead) % buffer_size];
    }

    [[nodiscard]] auto get() {
        auto result = peek();
        next();

        return result;
    }

    void next() {
        m_buffer[m_buffer_index] = m_lexer.token();
        m_lexer.next_token();

        ++m_buffer_index;
        m_buffer_index %= buffer_size;
    }

    [[nodiscard]] bool match(std::same_as<Token::Type> auto... types) const {
        return match(0, types...);
    }

    [[nodiscard]] bool
    match(std::uint8_t ahead, std::same_as<Token::Type> auto... types) const {
        auto type = peek(ahead).type;
        return ((type == types) || ...);
    }

    [[nodiscard]] bool match_next(std::same_as<Token::Type> auto... types) {
        if (match(types...)) {
            next();
            return true;
        }

        return false;
    }

    void expected(std::string_view what) { error("expected {}", what); }

    std::optional<Token> expect(std::string_view what, auto... types) {
        if (!match(types...)) {
            expected(what);
            return std::nullopt;
        }

        return get();
    }

    [[nodiscard]] std::unique_ptr<ast::BlockStmt> expect_block();

    [[nodiscard]] std::unique_ptr<ast::IfElse> parse_if_else();

    void expect_stmt(ast::BlockStmt& block);

    [[nodiscard]] std::vector<ast::Attribute> parse_attrs();

    [[nodiscard]] std::vector<std::unique_ptr<ast::Expression>> parse_args();

    [[nodiscard]] std::vector<ast::StructLiteral::Field> parse_field_values();

    [[nodiscard]] std::optional<ast::FnProto> parse_fn_proto();

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_prefix(bool is_condition);

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_access_or_call_expr(bool is_condition);

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_as_expr(bool is_condition);

    [[nodiscard]] std::unique_ptr<ast::BinaryExpr>
    expect_infix(std::unique_ptr<ast::Expression> lhs, bool is_condition);

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_bin_expr(bool is_condition, std::uint8_t precedence = 1);

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_range_expr(bool is_condition);

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_expr(bool is_condition);

    [[nodiscard]] std::unique_ptr<ast::Type> expect_var_type();

    [[nodiscard]] std::unique_ptr<ast::Type> parse_array_type();

    [[nodiscard]] std::unique_ptr<ast::Type> expect_type();

    void parse_switch(ast::BlockStmt& block);

    void parse_while(ast::BlockStmt& block);

    void parse_for(ast::BlockStmt& block);

    void parse_return(ast::BlockStmt& block);

    void parse_assignment(
        ast::BlockStmt& block, std::unique_ptr<ast::Expression> variable);

    [[nodiscard]] std::vector<ast::Struct::Field> parse_fields();

    [[nodiscard]] std::vector<std::unique_ptr<ast::Type>> parse_template_args();

    [[nodiscard]] std::vector<OffsetValue<std::string>> parse_template_params();

    [[nodiscard]] std::vector<ast::EnumDecl::Field> parse_enum_fields();

    [[nodiscard]] std::unique_ptr<ast::VarDecl>
    parse_var(std::vector<ast::Attribute> attrs, bool is_public = false);

    [[nodiscard]] std::unique_ptr<ast::FnDecl>
    parse_fn(std::vector<ast::Attribute> attrs, bool is_public = false);

    [[nodiscard]] std::unique_ptr<ast::Struct>
    parse_struct(std::vector<ast::Attribute> attrs, bool is_public = false);

    [[nodiscard]] std::unique_ptr<ast::Union>
    parse_union(std::vector<ast::Attribute> attrs, bool is_public = false);

    [[nodiscard]] std::unique_ptr<ast::TypeAlias>
    parse_type_alias(std::vector<ast::Attribute> attrs, bool is_public = false);

    [[nodiscard]] std::unique_ptr<ast::EnumDecl>
    parse_enum(std::vector<ast::Attribute> attrs, bool is_public = false);

    [[nodiscard]] std::unique_ptr<ast::Module>
    parse_submodule(const std::filesystem::path& path, std::string_view name);

    [[nodiscard]] std::unique_ptr<ast::Module> parse_submodule_dir(
        const std::filesystem::path& path, std::string_view name);

    [[nodiscard]] bool parse_with(ast::Module& module);

    [[nodiscard]] static std::uint8_t precedence_of(Token::Type type) {
        using enum Token::Type;

        enum : std::uint8_t {
            PNone = 0,
            PNull,
            PLogicalOr,
            PLogicalAnd,
            POr,
            PXor,
            PAnd,
            PCmp,
            PAdd,
            PMul
        };

        switch (type) {
        case QuestionQuestion:
            return PNull;
        case OrOr:
            return PLogicalOr;
        case AndAnd:
            return PLogicalAnd;
        case Or:
            return POr;
        case Xor:
            return PXor;
        case And:
            return PAnd;
        case Less:
        case Greater:
        case EqualEqual:
        case BangEqual:
        case GreaterEqual:
        case LessEqual:
            return PCmp;
        case Plus:
        case Minus:
            return PAdd;
        case Star:
        case Slash:
        case Percent:
            return PMul;
        default:
            return PNone;
        }
    }

    template <typename... Args>
    void error(fmt::format_string<Args...> message, Args&&... args) {
        error(peek().offset, message, std::forward<Args>(args)...);
    }

    template <typename... Args>
    void error(
        std::size_t offset, fmt::format_string<Args...> message,
        Args&&... args) {
        auto loc = offset_to_loc(m_source, offset);

        log::error(
            loc.line, loc.column, m_filename, loc.code, message,
            std::forward<Args>(args)...);

        m_had_error = true;
    }

    template <typename... Args>
    void warning(
        std::size_t offset, fmt::format_string<Args...> message,
        Args&&... args) {
        auto loc = offset_to_loc(m_source, offset);

        log::warning(
            loc.line, loc.column, m_filename, loc.code, message,
            std::forward<Args>(args)...);
    }

    static constexpr auto buffer_size = 2;

    Lexer m_lexer;

    std::array<Token, buffer_size> m_buffer;
    std::uint8_t m_buffer_index = 0;

    std::string_view m_source;
    std::string m_filename;

    std::map<std::filesystem::path, bool> m_submodules_visiting;

    bool m_had_error{false};
};

} // namespace cent::frontend

#endif
