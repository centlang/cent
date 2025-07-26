#ifndef CENT_FRONTEND_TOKEN_H
#define CENT_FRONTEND_TOKEN_H

#include <cstdint>
#include <string>

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
        DotDot,
        DotDotEqual,
        Ellipsis,
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
        Question,
        QuestionQuestion,

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
        RuneLiteral,

        True,
        False,

        Null,

        Pub,

        Fn,
        Type,
        Union,
        Enum,
        With,

        As,
        In,

        If,
        Else,
        Switch,

        While,
        For,

        Let,
        Mut,
        Const,

        Return,

        Break,
        Continue,

        Unreachable,
        Undefined,
        Sizeof,

        Identifier
    };

    Type type = Type::Invalid;
    std::string value;

    std::size_t offset;
};

} // namespace cent::frontend

#endif
