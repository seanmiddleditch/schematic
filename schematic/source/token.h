// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

namespace potato::schematic::compiler
{
    class Source;

    enum class TokenType
    {
        Unknown,
        Integer,
        HexInteger,
        BinaryInteger,
        Real,
        Identifier,
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        LParen,
        RParen,
        Dot,
        Comma,
        Equals,
        SemiColon,
        Colon,
        Minus,
        Hash,
        At,
        Star,
        Question,
        End,
    };

    struct Token
    {
        TokenType type = TokenType::Unknown;
        unsigned offset = 0;
        unsigned length = 0;
    };

    const char* ToCStr(TokenType type) noexcept;
} // namespace potato::schematic
