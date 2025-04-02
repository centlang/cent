#ifndef CENT_FRONTEND_PARSER_H
#define CENT_FRONTEND_PARSER_H

#include <array>
#include <concepts>
#include <cstdint>
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
#include "cent/ast/node.h"
#include "cent/ast/program.h"
#include "cent/ast/struct.h"

namespace cent {

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

    [[nodiscard]] std::unique_ptr<Program> parse() noexcept;

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

    [[nodiscard]] std::unique_ptr<BlockStmt> expect_block() noexcept;

    [[nodiscard]] std::unique_ptr<IfElse> parse_if_else() noexcept;

    void expect_stmt(BlockStmt& block) noexcept;

    [[nodiscard]] std::vector<std::unique_ptr<Expression>>
    parse_args() noexcept;

    [[nodiscard]] std::unique_ptr<Expression> expect_prefix() noexcept;

    [[nodiscard]] std::unique_ptr<BinaryExpr>
    expect_infix(std::unique_ptr<Expression> lhs) noexcept;

    [[nodiscard]] std::unique_ptr<Expression>
    expect_bin_expr(std::uint8_t precedence = 1) noexcept;

    [[nodiscard]] std::unique_ptr<Expression> expect_expr() noexcept;

    [[nodiscard]] std::optional<SpanValue<std::string_view>>
    expect_var_type() noexcept;

    void parse_var(BlockStmt& block) noexcept;

    void parse_while(BlockStmt& block) noexcept;

    void parse_return(BlockStmt& block) noexcept;

    void parse_assignment(BlockStmt& block) noexcept;

    [[nodiscard]] std::vector<FnDecl::Param> parse_params() noexcept;
    [[nodiscard]] std::vector<Struct::Field> parse_fields() noexcept;

    void parse_fn(Program& program) noexcept;
    void parse_struct(Program& program) noexcept;

    [[nodiscard]] static std::uint8_t precedence_of(Token::Type type) noexcept {
        enum { None = 0, Or, And, Comparison, Additive, Multiplicative };

        switch (type) {
        case Token::Type::Or:
            return Or;
        case Token::Type::And:
            return And;
        case Token::Type::Less:
        case Token::Type::Greater:
        case Token::Type::EqualEqual:
        case Token::Type::BangEqual:
        case Token::Type::GreaterEqual:
        case Token::Type::LessEqual:
            return Comparison;
        case Token::Type::Plus:
        case Token::Type::Minus:
            return Additive;
        case Token::Type::Star:
        case Token::Type::Slash:
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

} // namespace cent

#endif
