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

    if (is_ident(peek())) {
        ident();
        return;
    }

    auto logical_op = [&](Token::Type type) {
        const auto* at_before = m_at;
        auto begin = m_position;

        char oper = get();

        if (eof() || peek() != oper) {
            m_token = {Invalid, {at_before, m_at}, {begin, m_position}};
            return;
        }

        get();
        m_token = {type, {}, {begin, m_position}};
    };

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
    case '&':
        logical_op(And);
        break;
    case '|':
        logical_op(Or);
        break;
    default:
        m_token = {Invalid, {m_at, m_at + 1}, Span::from(m_position)};
        get();

        break;
    }
}

void Lexer::number() noexcept {
    m_token = {Token::Type::IntLiteral, {m_at, m_at}, {m_position, {}}};

    auto get_int = [&] {
        while (!eof() && std::isdigit(peek())) {
            get();
            m_token.value = {m_token.value.cbegin(), m_at};
        }
    };

    get_int();

    if (!eof() && peek() == '.') {
        get();

        m_token.value = {m_token.value.cbegin(), m_at};
        m_token.type = Token::Type::FloatLiteral;

        get_int();
    }

    m_token.span.end = m_position;
}

void Lexer::ident() noexcept {
    m_token = {Token::Type::Identifier, {m_at, m_at}, {m_position, {}}};

    while (!eof() && is_ident(peek())) {
        get();
        m_token.value = {m_token.value.cbegin(), m_at};
    }

    m_token.span.end = m_position;

    auto keyword = [&](Token::Type type) {
        m_token.type = type;
        m_token.value = {};
    };

    if (m_token.value == "true") {
        keyword(Token::Type::True);
        return;
    }

    if (m_token.value == "false") {
        keyword(Token::Type::False);
        return;
    }

    if (m_token.value == "fn") {
        keyword(Token::Type::Fn);
        return;
    }

    if (m_token.value == "if") {
        keyword(Token::Type::If);
        return;
    }

    if (m_token.value == "else") {
        keyword(Token::Type::Else);
        return;
    }
}

} // namespace cent
