#include "cent/frontend/lexer.h"

namespace cent {

void Lexer::next_token() noexcept {
    using enum Token::Type;

    skip_whitespaces();

    auto single_char = [&](Token::Type type) {
        m_token = {type, {}, Span::from(m_position)};
        get();
    };

    if (eof()) {
        m_token = {Eof, {}, Span::from(m_position)};
        return;
    }

    if (std::isdigit(peek())) {
        number();
        return;
    }

    switch (peek()) {
    case '(':
        single_char(LeftParen);
        break;
    case ')':
        single_char(RightParen);
        break;
    case '{':
        single_char(LeftBrace);
        break;
    case '}':
        single_char(RightBrace);
        break;
    case '=':
        single_char(Equal);
        break;
    case ',':
        single_char(Comma);
        break;
    case ':':
        single_char(Colon);
        break;
    case ';':
        single_char(Semicolon);
        break;
    case '+':
        single_char(Plus);
        break;
    case '-':
        single_char(Minus);
        break;
    case '*':
        single_char(Star);
        break;
    case '/':
        single_char(Slash);
        break;
    default:
        m_token = {Invalid, {m_at, m_at + 1}, Span::from(m_position)};
        get();

        break;
    }
}

void Lexer::number() noexcept {
    m_token = {Token::Type::IntLiteral, {m_at, m_at}, {m_position, {}}};

    while (!eof() && std::isdigit(peek())) {
        get();
        m_token.value = {m_token.value.cbegin(), m_at};
    }

    m_token.span.end = m_position;
}

} // namespace cent
