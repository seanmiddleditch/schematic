// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cstdint>

namespace potato::schematic::compiler
{
    class Source;

    enum class TokenType
    {
        Unknown,
        Integer,
        HexInteger,
        BinaryInteger,
        Float,
        Identifier,
        String,
        MultilineString,
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
        std::uint32_t offset = 0;
        std::uint32_t length = 0;
    };

    const char* ToCStr(TokenType type) noexcept;
} // namespace potato::schematic
