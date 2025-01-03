// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cstdint>

namespace schematic::compiler
{
    class Source;

    enum class TokenType : std::uint8_t
    {
        Unknown,

        // Literals
        Integer,
        HexInteger,
        BinaryInteger,
        Float,

        // Identifier
        Identifier,

        // Strings
        String,
        MultilineString,

        // Pairs
        LBrace,
        RBrace,
        LBracket,
        RBracket,
        LParen,
        RParen,

        // Symbols
        Dot,
        Range,
        Comma,
        Equals,
        SemiColon,
        Colon,
        Minus,
        Hash,
        At,
        Star,
        Question,

        // End
        End,
    };

    struct Token
    {
        TokenType type = TokenType::Unknown;
        std::uint32_t line = 0;
        std::uint32_t offset = 0;
        std::uint32_t length = 0;
    };

    const char* ToCStr(TokenType type) noexcept;
} // namespace schematic::compiler
