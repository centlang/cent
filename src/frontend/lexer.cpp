#include <cstdint>

#include "cent/frontend/lexer.h"

namespace cent::frontend {

void Lexer::next_token() {
    using enum Token::Type;

    skip_whitespaces();

    auto single_char = [&](Token::Type type) {
        m_token = {type, {}, m_offset};
        get();
    };

    if (eof()) {
        m_token = {Eof, {}, m_offset};
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
        m_token.offset = m_offset;
        m_token.value = {};

        char oper = get();

        if (eof() || peek() != oper) {
            m_token.type = single;
            return;
        }

        get();
        m_token.type = type;
    };

    auto twice_or_with_equal = [&](Token::Type single, Token::Type twice,
                                   Token::Type with_eq) {
        m_token.offset = m_offset;
        m_token.value = {};

        char oper = get();

        if (eof()) {
            m_token.type = single;
            return;
        }

        if (peek() == '=') {
            get();

            m_token.type = with_eq;
            return;
        }

        if (peek() == oper) {
            get();

            m_token.type = twice;
            return;
        }

        m_token.type = single;
    };

    auto with_equal = [&](Token::Type oper, Token::Type with_eq) {
        m_token.offset = m_offset;
        m_token.value = {};

        get();

        if (eof() || peek() != '=') {
            m_token.type = oper;
            return;
        }

        get();
        m_token.type = with_eq;
    };

    switch (peek()) {
    case '"':
        string();
        break;
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
    case '[':
        single_char(LeftBracket);
        break;
    case ']':
        single_char(RightBracket);
        break;
    case ',':
        single_char(Comma);
        break;
    case '.':
        m_token.offset = m_offset;
        m_token.value = {};

        get();

        if (eof() || peek() != '.') {
            m_token.type = Dot;
            return;
        }

        get();

        if (eof() || peek() != '.') {
            m_token.type = DotDot;
            return;
        }

        get();
        m_token.type = Ellipsis;

        break;
    case ':':
        twice(Colon, ColonColon);
        break;
    case ';':
        single_char(Semicolon);
        break;
    case '~':
        single_char(Not);
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
    case '%':
        with_equal(Percent, PercentEqual);
        break;
    case '?':
        single_char(QuestionMark);
        break;
    case '/': {
        m_token.offset = m_offset;
        m_token.value = {};

        get();

        if (eof()) {
            m_token.type = Slash;
            return;
        }

        if (peek() == '=') {
            get();

            m_token.type = SlashEqual;
            return;
        }

        if (peek() != '/') {
            m_token.type = Slash;
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
        twice_or_with_equal(And, AndAnd, AndEqual);
        break;
    case '|':
        twice_or_with_equal(Or, OrOr, OrEqual);
        break;
    case '^':
        with_equal(Xor, XorEqual);
        break;
    default:
        m_token = {Invalid, std::string{get()}, m_offset};
        break;
    }
}

void Lexer::number() {
    m_token = {Token::Type::IntLiteral, {}, m_offset};

    static constexpr auto hex = 16;
    static constexpr auto oct = 8;
    static constexpr auto bin = 2;
    static constexpr auto dec = 10;

    std::uint8_t base = dec;

    auto is_digit = [&](char character) -> bool {
        switch (base) {
        case hex:
            return std::isxdigit(character);
        case oct:
            return character >= '0' && character < '8';
        case bin:
            return character == '0' || character == '1';
        case dec:
            return std::isdigit(character);
        default:
            return false;
        }
    };

    auto get_int = [&] {
        while (!eof()) {
            if (peek() == '_') {
                get();
                continue;
            }

            if (!is_digit(peek())) {
                break;
            }

            m_token.value += get();
        }
    };

    if (peek() == '0') {
        m_token.value += get();

        if (eof()) {
            return;
        }

        if (peek() == 'x') {
            base = hex;
            m_token.value += get();
        } else if (peek() == 'o') {
            base = oct;
            m_token.value += get();
        } else if (peek() == 'b') {
            base = bin;
            m_token.value += get();
        }
    }

    get_int();

    if (!eof() && (peek() == 'i' || peek() == 'u')) {
        m_token.value += get();
        get_int();

        return;
    }

    if (eof() || peek() != '.') {
        return;
    }

    m_token.value += get();
    m_token.type = Token::Type::FloatLiteral;

    get_int();

    if (!eof() && peek() == 'f') {
        m_token.value += get();
        get_int();
    }
}

void Lexer::string() {
    get();
    m_token = {Token::Type::StrLiteral, {}, m_offset};

    while (true) {
        if (eof()) {
            m_token.type = Token::Type::Invalid;
            return;
        }

        if (peek() == '"') {
            get();
            return;
        }

        if (peek() != '\\') {
            m_token.value += get();
            continue;
        }

        get();

        if (eof()) {
            m_token.type = Token::Type::Invalid;
            return;
        }

        switch (get()) {
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
            m_token.type = Token::Type::Invalid;
            return;
        }
    }
}

void Lexer::ident() {
    using enum Token::Type;

    m_token = {Identifier, {}, m_offset};

    while (!eof() && is_ident(peek())) {
        m_token.value += get();
    }

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

    if (m_token.value == "null") {
        keyword(Null);
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

    if (m_token.value == "type") {
        keyword(Type);
        return;
    }

    if (m_token.value == "enum") {
        keyword(Enum);
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

    if (m_token.value == "const") {
        keyword(Const);
        return;
    }

    if (m_token.value == "return") {
        keyword(Return);
        return;
    }

    if (m_token.value == "break") {
        keyword(Break);
        return;
    }

    if (m_token.value == "continue") {
        keyword(Continue);
        return;
    }

    if (m_token.value == "unreachable") {
        keyword(Unreachable);
        return;
    }

    if (m_token.value == "undefined") {
        keyword(Undefined);
        return;
    }
}

} // namespace cent::frontend
