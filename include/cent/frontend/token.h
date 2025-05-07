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

        LeftBracket,
        RightBracket,

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
        Percent,

        PlusEqual,
        MinusEqual,
        StarEqual,
        SlashEqual,
        PercentEqual,

        Bang,
        QuestionMark,

        And,
        Or,

        AndEqual,
        OrEqual,

        Xor,
        Not,

        XorEqual,

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

        Null,

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
