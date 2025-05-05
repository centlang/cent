#ifndef CENT_FRONTEND_TOKEN_H
#define CENT_FRONTEND_TOKEN_H

#include <cstdint>
#include <string>

#include "cent/span.h"

namespace cent::frontend {

struct Token {
    enum struct Type : std::uint8_t {
        LeftParen,
        RightParen,

        LeftBrace,
        RightBrace,

        Equal,
        Comma,
        Dot,
        Colon,
        Semicolon,
        ColonColon,

        Plus,
        Minus,
        Star,
        Slash,

        PlusEqual,
        MinusEqual,
        StarEqual,
        SlashEqual,

        Bang,
        QuestionMark,

        And,
        Or,

        AndAnd,
        OrOr,

        Less,
        Greater,

        EqualEqual,
        BangEqual,
        GreaterEqual,
        LessEqual,

        Invalid,

        Eof,

        IntLiteral,
        StrLiteral,
        FloatLiteral,

        True,
        False,

        Pub,
        Extern,

        Fn,
        Struct,
        With,

        As,

        If,
        Else,

        While,

        Let,
        Mut,

        Return,

        Break,
        Continue,

        Identifier
    };

    Type type = Type::Invalid;
    std::string value;

    Span span;
};

} // namespace cent::frontend

#endif
