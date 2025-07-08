#ifndef CENT_FRONTEND_LEXER_H
#define CENT_FRONTEND_LEXER_H

#include <cctype>
#include <string_view>

#include "frontend/token.h"

namespace cent::frontend {

class Lexer {
public:
    [[nodiscard]] Lexer(std::string_view source) : m_source{source} {
        next_token();
    }

    [[nodiscard]] auto token() const { return m_token; }

    void next_token();

private:
    [[nodiscard]] char peek() const { return m_source[m_offset]; }

    char get() { return m_source[m_offset++]; }

    [[nodiscard]] bool eof() const { return m_offset == m_source.size(); }

    void skip_whitespaces() {
        while (!eof() && std::isspace(peek())) {
            get();
        }
    }

    void number();
    void string();
    void ident();

    [[nodiscard]] static bool is_ident(char character) {
        return std::isalnum(character) || character == '_';
    }

    std::string_view m_source;
    std::size_t m_offset{0};

    Token m_token;
};

} // namespace cent::frontend

#endif
