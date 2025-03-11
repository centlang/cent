#ifndef CENT_FRONTEND_TOKEN_H
#define CENT_FRONTEND_TOKEN_H

#include <cstdint>

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

        Invalid,

        Eof
    };

    Type type = Type::Invalid;
    Span span;
};

} // namespace cent

#endif
