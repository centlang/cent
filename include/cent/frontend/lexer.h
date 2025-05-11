#ifndef CENT_FRONTEND_LEXER_H
#define CENT_FRONTEND_LEXER_H

#include <cctype>
#include <string_view>

#include "cent/frontend/token.h"

namespace cent::frontend {

class Lexer {
public:
    [[nodiscard]] Lexer(std::string_view source) noexcept : m_source{source} {
        next_token();
    }

    [[nodiscard]] auto token() const noexcept { return m_token; }

    void next_token() noexcept;

private:
    [[nodiscard]] char peek() const noexcept { return m_source[m_offset]; }

    char get() noexcept { return m_source[m_offset++]; }

    [[nodiscard]] bool eof() const noexcept {
        return m_offset == m_source.size();
    }

    void skip_whitespaces() noexcept {
        while (!eof() && std::isspace(peek())) {
            get();
        }
    }

    void number() noexcept;
    void string() noexcept;
    void ident() noexcept;

    [[nodiscard]] static bool is_ident(char character) noexcept {
        return std::isalnum(character) || character == '_';
    }

    std::string_view m_source;
    std::size_t m_offset{0};

    Token m_token;
};

} // namespace cent::frontend

#endif
