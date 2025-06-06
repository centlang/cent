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

#include "cent/frontend/lexer.h"
#include "cent/log.h"
#include "cent/util.h"

#include "cent/ast/module.h"
#include "cent/ast/node.h"

#include "cent/ast/expr/binary_expr.h"
#include "cent/ast/expr/literals.h"

#include "cent/ast/stmt/block_stmt.h"
#include "cent/ast/stmt/if_else.h"

#include "cent/ast/decl/enum_decl.h"
#include "cent/ast/decl/fn_decl.h"
#include "cent/ast/decl/struct.h"
#include "cent/ast/decl/var_decl.h"

namespace cent::frontend {

class Parser {
public:
    [[nodiscard]] Parser(std::string_view source, std::string_view filename)
    : m_lexer{source}, m_source{source}, m_filename{filename} {
        for (auto& token : m_buffer) {
            token = m_lexer.token();
            m_lexer.next_token();
        }
    }

    [[nodiscard]] std::unique_ptr<ast::Module> parse();

    [[nodiscard]] bool had_error() const { return m_had_error; };

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

    void expected(std::string_view what) {
        error(fmt::format("expected {}", log::bold(what)));
    }

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

    [[nodiscard]] std::vector<std::unique_ptr<ast::Expression>> parse_args();

    [[nodiscard]] std::vector<ast::StructLiteral::Field> parse_field_values();

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
    expect_expr(bool is_condition);

    [[nodiscard]] std::unique_ptr<ast::Type> expect_var_type();

    [[nodiscard]] std::unique_ptr<ast::Type> parse_array_type();

    [[nodiscard]] std::unique_ptr<ast::Type> expect_type();

    [[nodiscard]] std::unique_ptr<ast::VarDecl> parse_var();

    void parse_while(ast::BlockStmt& block);

    void parse_return(ast::BlockStmt& block);

    void parse_assignment(
        ast::BlockStmt& block, std::unique_ptr<ast::Expression> variable);

    [[nodiscard]] std::vector<ast::Struct::Field> parse_fields();

    [[nodiscard]] std::vector<ast::EnumDecl::Field> parse_enum_fields();

    [[nodiscard]] std::unique_ptr<ast::FnDecl>
    parse_fn(bool is_public = false, bool is_extern = false);

    [[nodiscard]] std::unique_ptr<ast::Struct>
    parse_struct(bool is_public = false);

    [[nodiscard]] std::unique_ptr<ast::EnumDecl>
    parse_enum(bool is_public = false);

    [[nodiscard]] bool
    parse_submodule(ast::Module& module, const std::filesystem::path& path);

    [[nodiscard]] bool
    parse_submodule_dir(ast::Module& module, const std::filesystem::path& path);

    [[nodiscard]] bool parse_with(ast::Module& module);

    [[nodiscard]] static std::uint8_t precedence_of(Token::Type type) {
        using enum Token::Type;

        enum {
            PNone = 0,
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

    void error(std::string_view message) {
        auto [line, column] = cent::offset_to_pos(m_source, peek().offset);
        log::error(line, column, m_filename, message);

        m_had_error = true;
    }

    void error(std::size_t offset, std::string_view message) {
        auto [line, column] = cent::offset_to_pos(m_source, offset);
        log::error(line, column, m_filename, message);

        m_had_error = true;
    }

    static constexpr auto buffer_size = 2;

    Lexer m_lexer;

    std::array<Token, buffer_size> m_buffer;
    std::uint8_t m_buffer_index = 0;

    std::string_view m_source;
    std::string_view m_filename;

    bool m_had_error{false};
};

} // namespace cent::frontend

#endif
