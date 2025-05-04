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

#include "cent/ast/binary_expr.h"
#include "cent/ast/block_stmt.h"
#include "cent/ast/fn_decl.h"
#include "cent/ast/if_else.h"
#include "cent/ast/literals.h"
#include "cent/ast/module.h"
#include "cent/ast/node.h"
#include "cent/ast/struct.h"

namespace cent::frontend {

class Parser {
public:
    [[nodiscard]] Parser(
        std::string_view source, std::string_view filename) noexcept
    : m_lexer{source}, m_filename{filename} {
        for (auto& token : m_buffer) {
            token = m_lexer.token();
            m_lexer.next_token();
        }
    }

    [[nodiscard]] std::unique_ptr<ast::Module> parse() noexcept;

private:
    [[nodiscard]] auto peek(std::uint8_t ahead = 0) const noexcept {
        return m_buffer[(m_buffer_index + ahead) % buffer_size];
    }

    [[nodiscard]] auto get() noexcept {
        auto result = peek();
        next();

        return result;
    }

    void next() noexcept {
        m_buffer[m_buffer_index] = m_lexer.token();
        m_lexer.next_token();

        ++m_buffer_index;
        m_buffer_index %= buffer_size;
    }

    [[nodiscard]] bool
    match(std::same_as<Token::Type> auto... types) const noexcept {
        return match(0, types...);
    }

    [[nodiscard]] bool match(
        std::uint8_t ahead,
        std::same_as<Token::Type> auto... types) const noexcept {
        auto type = peek(ahead).type;
        return ((type == types) || ...);
    }

    std::optional<Token>
    expect(std::string_view expected, auto... types) noexcept {
        if (!match(types...)) {
            error(
                peek().span.begin, m_filename,
                fmt::format("expected {}", expected));

            return std::nullopt;
        }

        return get();
    }

    [[nodiscard]] std::unique_ptr<ast::BlockStmt> expect_block() noexcept;

    [[nodiscard]] std::unique_ptr<ast::IfElse> parse_if_else() noexcept;

    void expect_stmt(ast::BlockStmt& block) noexcept;

    [[nodiscard]] std::vector<std::unique_ptr<ast::Expression>>
    parse_args() noexcept;

    [[nodiscard]] std::vector<ast::StructLiteral::Field>
    parse_field_values() noexcept;

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_prefix(bool is_condition) noexcept;

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_member_expr(bool is_condition) noexcept;

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_as_expr(bool is_condition) noexcept;

    [[nodiscard]] std::unique_ptr<ast::BinaryExpr> expect_infix(
        std::unique_ptr<ast::Expression> lhs, bool is_condition) noexcept;

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_bin_expr(bool is_condition, std::uint8_t precedence = 1) noexcept;

    [[nodiscard]] std::unique_ptr<ast::Expression>
    expect_expr(bool is_condition) noexcept;

    [[nodiscard]] std::unique_ptr<ast::Type> expect_var_type() noexcept;

    [[nodiscard]] std::unique_ptr<ast::Type> expect_type() noexcept;

    void parse_var(ast::BlockStmt& block) noexcept;

    void parse_while(ast::BlockStmt& block) noexcept;

    void parse_return(ast::BlockStmt& block) noexcept;

    void parse_assignment(
        ast::BlockStmt& block,
        std::unique_ptr<ast::Expression> variable) noexcept;

    [[nodiscard]] std::vector<ast::FnDecl::Param> parse_params() noexcept;
    [[nodiscard]] std::vector<ast::Struct::Field> parse_fields() noexcept;

    [[nodiscard]] bool parse_fn(
        ast::Module& module, bool is_public = false,
        bool is_extern = false) noexcept;

    [[nodiscard]] bool
    parse_struct(ast::Module& module, bool is_public = false) noexcept;

    [[nodiscard]] bool parse_submodule(
        ast::Module& module, const std::filesystem::path& path) noexcept;

    [[nodiscard]] bool parse_submodule_dir(
        ast::Module& module, const std::filesystem::path& path) noexcept;

    [[nodiscard]] bool parse_with(ast::Module& module) noexcept;

    [[nodiscard]] static std::uint8_t precedence_of(Token::Type type) noexcept {
        using enum Token::Type;

        enum Precedence {
            None = 0,
            Or,
            And,
            Comparison,
            Additive,
            Multiplicative
        };

        switch (type) {
        case OrOr:
            return Precedence::Or;
        case AndAnd:
            return Precedence::And;
        case Less:
        case Greater:
        case EqualEqual:
        case BangEqual:
        case GreaterEqual:
        case LessEqual:
            return Comparison;
        case Plus:
        case Minus:
            return Additive;
        case Star:
        case Slash:
            return Multiplicative;
        default:
            return None;
        }
    }

    static constexpr auto buffer_size = 2;

    Lexer m_lexer;

    std::array<Token, buffer_size> m_buffer;
    std::uint8_t m_buffer_index = 0;

    std::string_view m_filename;
};

} // namespace cent::frontend

#endif
