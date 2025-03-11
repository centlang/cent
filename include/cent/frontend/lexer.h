#ifndef CENT_FRONTEND_LEXER_H
#define CENT_FRONTEND_LEXER_H

#include <cctype>
#include <string_view>

#include "cent/frontend/token.h"
#include "cent/position.h"

namespace cent {

class Lexer {
public:
    [[nodiscard]] Lexer(std::string_view source) noexcept
    : m_at{source.cbegin()}, m_end{source.cend()} {
        next_token();
    }

    [[nodiscard]] auto token() const noexcept { return m_token; }

    void next_token() noexcept;

private:
    [[nodiscard]] char peek() const noexcept { return *m_at; }

    char get() noexcept {
        ++m_position.column;
        return *m_at++;
    }

    [[nodiscard]] bool eof() const noexcept { return m_at == m_end; }

    void skip_whitespaces() noexcept {
        while (!eof() && std::isspace(peek())) {
            if (peek() == '\n') {
                ++m_position.line;
                m_position.column = 1;

                ++m_at;
            } else {
                get();
            }
        }
    }

    Position m_position;
    Token m_token;

    std::string_view::const_iterator m_at;
    std::string_view::const_iterator m_end;
};

} // namespace cent

#endif
