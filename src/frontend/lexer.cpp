#include "cent/frontend/lexer.h"

namespace cent::frontend {

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

    auto twice = [&](Token::Type single, Token::Type type) {
        auto begin = m_position;
        char oper = get();

        if (eof() || peek() != oper) {
            m_token = {single, {}, {begin, m_position}};
            return;
        }

        get();
        m_token = {type, {}, {begin, m_position}};
    };

    auto with_equal = [&](Token::Type oper, Token::Type with_eq) {
        auto begin = m_position;
        get();

        if (eof() || peek() != '=') {
            m_token = {oper, {}, {begin, m_position}};
            return;
        }

        get();
        m_token = {with_eq, {}, {begin, m_position}};
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
    case ',':
        single_char(Comma);
        break;
    case '.':
        single_char(Dot);
        break;
    case ':':
        twice(Colon, ColonColon);
        break;
    case ';':
        single_char(Semicolon);
        break;
    case '+':
        with_equal(Plus, PlusEqual);
        break;
    case '-':
        with_equal(Minus, MinusEqual);
        break;
    case '*':
        with_equal(Star, StarEqual);
        break;
    case '?':
        single_char(QuestionMark);
        break;
    case '/': {
        auto begin = m_position;

        if (eof()) {
            m_token = {Slash, {}, {begin, m_position}};
            return;
        }

        if (peek() == '=') {
            get();
            m_token = {SlashEqual, {}, {begin, m_position}};

            return;
        }

        if (peek() != '/') {
            m_token = {Slash, {}, {begin, m_position}};
            return;
        }

        while (!eof() && peek() != '\n') {
            get();
        }

        next_token();
        break;
    }
    case '=':
        with_equal(Equal, EqualEqual);
        break;
    case '!':
        with_equal(Bang, BangEqual);
        break;
    case '>':
        with_equal(Greater, GreaterEqual);
        break;
    case '<':
        with_equal(Less, LessEqual);
        break;
    case '&':
        twice(And, AndAnd);
        break;
    case '|':
        twice(Or, OrOr);
        break;
    default:
        m_token = {Invalid, std::string{get()}, Span::from(m_position)};

        break;
    }
}

void Lexer::number() noexcept {
    m_token = {Token::Type::IntLiteral, {}, {m_position, {}}};

    auto get_int = [&] {
        while (!eof() && std::isdigit(peek())) {
            m_token.value += get();
        }
    };

    if (peek() == '0') {
        m_token.value += get();

        if (!eof() && (peek() == 'x' || peek() == 'o' || peek() == 'b')) {
            m_token.value += get();
        }
    }

    get_int();

    if (!eof() && (peek() == 'i' || peek() == 'u')) {
        m_token.value += get();
        get_int();

        m_token.span.end = m_position;
        return;
    }

    if (eof() || peek() != '.') {
        m_token.span.end = m_position;
        return;
    }

    m_token.value += get();
    m_token.type = Token::Type::FloatLiteral;

    get_int();

    if (!eof() && peek() == 'f') {
        m_token.value += get();
        get_int();
    }

    m_token.span.end = m_position;
}

void Lexer::ident() noexcept {
    using enum Token::Type;

    m_token = {Identifier, {}, {m_position, {}}};

    while (!eof() && is_ident(peek())) {
        m_token.value += get();
    }

    m_token.span.end = m_position;

    auto keyword = [&](Token::Type type) {
        m_token.type = type;
        m_token.value = {};
    };

    if (m_token.value == "true") {
        keyword(True);
        return;
    }

    if (m_token.value == "false") {
        keyword(False);
        return;
    }

    if (m_token.value == "pub") {
        keyword(Pub);
        return;
    }

    if (m_token.value == "fn") {
        keyword(Fn);
        return;
    }

    if (m_token.value == "struct") {
        keyword(Struct);
        return;
    }

    if (m_token.value == "with") {
        keyword(With);
        return;
    }

    if (m_token.value == "as") {
        keyword(As);
        return;
    }

    if (m_token.value == "if") {
        keyword(If);
        return;
    }

    if (m_token.value == "else") {
        keyword(Else);
        return;
    }

    if (m_token.value == "while") {
        keyword(While);
        return;
    }

    if (m_token.value == "let") {
        keyword(Let);
        return;
    }

    if (m_token.value == "mut") {
        keyword(Mut);
        return;
    }

    if (m_token.value == "return") {
        keyword(Return);
        return;
    }
}

} // namespace cent::frontend
