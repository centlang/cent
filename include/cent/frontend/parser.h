#ifndef CENT_FRONTEND_PARSER_H
#define CENT_FRONTEND_PARSER_H

#include <memory>
#include <optional>
#include <string_view>

#include <fmt/core.h>

#include "cent/frontend/lexer.h"
#include "cent/log.h"

#include "cent/ast/program.h"

namespace cent {

class Parser {
public:
    [[nodiscard]] Parser(
        std::string_view source, std::string_view filename) noexcept
    : m_lexer{source}, m_filename{filename} {}

    [[nodiscard]] std::unique_ptr<Program> parse() noexcept;

private:
    [[nodiscard]] auto peek() const noexcept { return m_lexer.token(); }

    [[nodiscard]] auto get() noexcept {
        auto result = peek();
        next();

        return result;
    }

    void next() noexcept { m_lexer.next_token(); }

    [[nodiscard]] bool match(auto... types) const noexcept {
        auto type = peek().type;
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

    void parse_fn(Program& program) noexcept;

    Lexer m_lexer;
    std::string_view m_filename;
};

} // namespace cent

#endif
