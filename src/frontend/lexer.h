#ifndef CENT_FRONTEND_LEXER_H
#define CENT_FRONTEND_LEXER_H

#include <cctype>
#include <string_view>

#include "log.h"
#include "util.h"

#include "frontend/token.h"

namespace cent::frontend {

class Lexer {
public:
    [[nodiscard]] Lexer(std::string_view source, std::string filename)
    : m_source{source}, m_filename{std::move(filename)} {
        next_token();
    }

    [[nodiscard]] auto token() const { return m_token; }

    void next_token();

private:
    [[nodiscard]] char peek() const { return m_source[m_offset]; }

    char get() { return m_source[m_offset++]; }

    void escape_seq() {
        get();

        if (eof()) {
            error("expected escape sequence");
            return;
        }

        switch (peek()) {
        case '\\':
            m_token.value += '\\';
            break;
        case 'n':
            m_token.value += '\n';
            break;
        case 'r':
            m_token.value += '\r';
            break;
        case 't':
            m_token.value += '\t';
            break;
        case '\'':
            m_token.value += '\'';
            break;
        case '"':
            m_token.value += '"';
            break;
        default:
            error("invalid escape sequence");
            break;
        }

        get();
    }

    [[nodiscard]] bool eof() const { return m_offset == m_source.size(); }

    void skip_whitespaces() {
        while (!eof() && std::isspace(peek())) {
            get();
        }
    }

    void number();
    void string();
    void ident();

    void error(std::size_t offset, std::string_view message) {
        auto loc = cent::offset_to_loc(m_source, offset);
        log::error(loc.line, loc.column, m_filename, message, loc.code);
    }

    void error(std::string_view message) { error(m_offset, message); }

    [[nodiscard]] static bool is_ident(char character) {
        return std::isalnum(character) || character == '_';
    }

    std::string_view m_source;
    std::string m_filename;

    std::size_t m_offset{0};
    Token m_token;
};

} // namespace cent::frontend

#endif
