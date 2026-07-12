#include <array>
#include <charconv>
#include <cstdint>
#include <optional>

#include "unicode.h"

#include "frontend/lexer.h"

namespace cent::frontend {

void Lexer::next_token() {
    using enum Token::Type;

    skip_whitespaces();

    auto single_char = [&](Token::Type type) {
        m_token = {.type = type, .value = {}, .offset = m_offset};
        get();
    };

    if (eof()) {
        m_token = {.type = Eof, .value = {}, .offset = m_offset};
        return;
    }

    if (std::isdigit(static_cast<unsigned char>(peek()))) {
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

    auto unclosed_rune = [&] {
        error(m_token.offset, "unclosed rune literal");
    };

    switch (peek()) {
    case '"':
        string();
        break;
    case '\'':
        m_token = {
            .type = Token::Type::RuneLiteral, .value = {}, .offset = m_offset};

        get();

        if (eof()) {
            unclosed_rune();
            return;
        }

        if (peek() == '\\') {
            escape_seq();
        } else {
            utf8_char();
        }

        if (eof() || peek() != '\'') {
            unclosed_rune();
            return;
        }

        get();
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

        if (eof()) {
            m_token.type = DotDot;
            return;
        }

        if (peek() == '=') {
            get();
            m_token.type = DotDotEqual;
            return;
        }

        if (peek() != '.') {
            m_token.type = DotDot;
            return;
        }

        get();
        m_token.type = Ellipsis;

        break;
    case ':':
        twice(Colon, ColonColon);
        break;
    case '?':
        twice(Question, QuestionQuestion);
        break;
    case ';':
        single_char(Semicolon);
        break;
    case '~':
        single_char(Not);
        break;
    case '#':
        single_char(Hash);
        break;
    case '+': {
        m_token.offset = m_offset;
        m_token.value = {};

        get();

        if (eof()) {
            m_token.type = Plus;
            return;
        }

        if (peek() == '=') {
            get();

            m_token.type = PlusEqual;
            return;
        }

        m_token.type = Plus;

        if (peek() == '+') {
            error_hint(
                m_token.offset, "use `x += 1` instead",
                "increment operators are not allowed");

            get();
            return;
        }

        break;
    }
    case '-': {
        m_token.offset = m_offset;
        m_token.value = {};

        get();

        if (eof()) {
            m_token.type = Minus;
            return;
        }

        if (peek() == '=') {
            get();

            m_token.type = MinusEqual;
            return;
        }

        m_token.type = Minus;

        if (peek() == '-') {
            error_hint(
                m_token.offset, "use `x -= 1` instead",
                "decrement operators are not allowed");

            get();
            return;
        }

        break;
    }
    case '*':
        with_equal(Star, StarEqual);
        break;
    case '%':
        with_equal(Percent, PercentEqual);
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
        twice_or_with_equal(Greater, GreaterGreater, GreaterEqual);
        break;
    case '<':
        twice_or_with_equal(Less, LessLess, LessEqual);
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
        auto offset = m_offset;
        error(offset, "unexpected character");

        m_token = {
            .type = Identifier, .value = std::string{get()}, .offset = offset};

        break;
    }
}

void Lexer::escape_seq() {
    get();

    if (eof()) {
        error("expected escape sequence");
        return;
    }

    auto invalid_unicode = [&] { error("invalid unicode escape character"); };

    auto get_hex = [&]<typename ValueType>(
                       std::uint8_t count) -> std::optional<ValueType> {
        std::string hex;
        hex.reserve(count);

        for (std::uint8_t i = 0; i < count; ++i) {
            if (eof() || !std::isxdigit(static_cast<unsigned char>(peek()))) {
                invalid_unicode();
                return std::nullopt;
            }

            hex += get();
        }

        ValueType value{};
        std::from_chars(hex.data(), hex.data() + hex.size(), value, 16);

        return value;
    };

    if (peek() == 'u') {
        get();
        auto value = get_hex.template operator()<std::uint32_t>(4);

        if (!value) {
            return;
        }

        append_utf8(m_token.value, *value);
        return;
    }

    if (peek() == 'U') {
        get();
        auto value = get_hex.template operator()<std::uint32_t>(8);

        if (!value) {
            return;
        }

        append_utf8(m_token.value, *value);
        return;
    }

    if (peek() == 'x') {
        get();
        auto value = get_hex.template operator()<char>(2);

        if (!value) {
            return;
        }

        m_token.value += *value;
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
    case '0':
        m_token.value += '\0';
        break;
    default:
        error("invalid escape sequence");
        break;
    }

    get();
}

void Lexer::number() {
    m_token = {
        .type = Token::Type::IntLiteral, .value = {}, .offset = m_offset};

    std::uint8_t base = 10;

    auto get_int = [&] {
        while (!eof()) {
            if (peek() == '_') {
                get();
                continue;
            }

            if (!is_digit(peek(), base)) {
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
            base = 16;
            m_token.value += get();
        } else if (peek() == 'o') {
            base = 8;
            m_token.value += get();
        } else if (peek() == 'b') {
            base = 2;
            m_token.value += get();
        }
    }

    get_int();

    if (eof()) {
        return;
    }

    if (peek() != '.') {
        return;
    }

    std::size_t offset = m_offset;
    get();

    if (!is_digit(peek(), base)) {
        m_offset = offset;
        return;
    }

    m_token.value += '.';

    m_token.value += get();
    m_token.type = Token::Type::FloatLiteral;

    get_int();
}

void Lexer::string() {
    m_token = {
        .type = Token::Type::StrLiteral, .value = {}, .offset = m_offset};

    get();

    while (true) {
        if (eof()) {
            error(m_token.offset, "unclosed string literal");
            return;
        }

        if (peek() == '"') {
            get();
            return;
        }

        if (peek() == '\\') {
            escape_seq();
            continue;
        }

        utf8_char();
    }
}

void Lexer::ident() {
    using enum Token::Type;

    m_token = {.type = Identifier, .value = {}, .offset = m_offset};

    while (!eof() && is_ident(peek())) {
        m_token.value += get();
    }

    m_token.type = ident_type(m_token.value);
}

void Lexer::utf8_char() {
    m_token.value += peek();
    auto lead = static_cast<std::uint8_t>(get());

    std::uint8_t length{};

    if ((lead & 0x80) == 0x00) {
        length = 0;
    } else if ((lead & 0xe0) == 0xc0) {
        length = 1;
    } else if ((lead & 0xf0) == 0xe0) {
        length = 2;
    } else {
        length = 3;
    }

    for (std::uint8_t i = 0; i < length; ++i) {
        if (eof()) {
            error("invalid UTF-8 character");
            break;
        }

        m_token.value += get();
    }
}

Token::Type Lexer::ident_type(std::string_view ident) {
    struct Kw {
        std::string_view keyword;
        Token::Type type;
    };

    static constexpr std::array map{
        Kw{"true", Token::Type::True},
        Kw{"false", Token::Type::False},
        Kw{"null", Token::Type::Null},
        Kw{"pub", Token::Type::Pub},
        Kw{"fn", Token::Type::Fn},
        Kw{"type", Token::Type::Type},
        Kw{"union", Token::Type::Union},
        Kw{"enum", Token::Type::Enum},
        Kw{"with", Token::Type::With},
        Kw{"as", Token::Type::As},
        Kw{"in", Token::Type::In},
        Kw{"if", Token::Type::If},
        Kw{"else", Token::Type::Else},
        Kw{"switch", Token::Type::Switch},
        Kw{"while", Token::Type::While},
        Kw{"for", Token::Type::For},
        Kw{"let", Token::Type::Let},
        Kw{"mut", Token::Type::Mut},
        Kw{"const", Token::Type::Const},
        Kw{"return", Token::Type::Return},
        Kw{"params", Token::Type::Params},
        Kw{"defer", Token::Type::Defer},
        Kw{"break", Token::Type::Break},
        Kw{"continue", Token::Type::Continue},
        Kw{"unreachable", Token::Type::Unreachable},
        Kw{"undefined", Token::Type::Undefined},
        Kw{"nan", Token::Type::FloatLiteral},
        Kw{"inf", Token::Type::FloatLiteral}};

    for (const auto& [kw, type] : map) {
        if (kw == ident) {
            return type;
        }
    }

    return Token::Type::Identifier;
}

} // namespace cent::frontend
