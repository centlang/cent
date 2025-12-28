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

    [[nodiscard]] bool had_error() const { return m_had_error; };

private:
    [[nodiscard]] char peek() const { return m_source[m_offset]; }

    char get() { return m_source[m_offset++]; }

    void escape_seq();

    [[nodiscard]] bool eof() const { return m_offset == m_source.size(); }

    void skip_whitespaces() {
        while (!eof() && std::isspace(peek())) {
            get();
        }
    }

    void number();
    void string();
    void ident();

    template <typename... Args>
    void error(
        std::size_t offset, fmt::format_string<Args...> message,
        Args&&... args) {
        auto loc = cent::offset_to_loc(m_source, offset);

        log::error(
            loc.line, loc.column, m_filename, loc.code, message,
            std::forward<Args>(args)...);

        m_had_error = true;
    }

    template <typename... Args>
    void error(fmt::format_string<Args...> message, Args&&... args) {
        error(m_offset, message, std::forward<Args>(args)...);
    }

    template <typename... Args>
    void error_hint(
        std::size_t offset, std::string_view hint,
        fmt::format_string<Args...> message, Args&&... args) {
        auto loc = cent::offset_to_loc(m_source, offset);

        log::error_hint(
            loc.line, loc.column, m_filename, loc.code, hint, message,
            std::forward<Args>(args)...);

        m_had_error = true;
    }

    [[nodiscard]] static bool is_ident(char character) {
        return std::isalnum(character) || character == '_';
    }

    std::string_view m_source;
    std::string m_filename;

    std::size_t m_offset{0};
    Token m_token;

    bool m_had_error{false};
};

} // namespace cent::frontend

#endif
