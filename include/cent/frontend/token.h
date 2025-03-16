#ifndef CENT_FRONTEND_TOKEN_H
#define CENT_FRONTEND_TOKEN_H

#include <cstdint>
#include <string_view>

#include "cent/span.h"

namespace cent {

struct Token {
    enum struct Type : std::uint8_t {
        LeftParen,
        RightParen,

        LeftBrace,
        RightBrace,

        Equal,
        Comma,
        Colon,
        Semicolon,

        Plus,
        Minus,
        Star,
        Slash,

        And,
        Or,

        Invalid,

        Eof,

        IntLiteral,
        FloatLiteral,

        True,
        False,

        Fn,

        If,
        Else,

        Identifier
    };

    Type type = Type::Invalid;
    std::string_view value;

    Span span;
};

} // namespace cent

#endif
