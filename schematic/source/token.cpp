// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "token.h"

#include "schematic/source.h"

#include <fmt/core.h>

const char* potato::schematic::compiler::ToCStr(TokenType type) noexcept
{
    switch (type)
    {
        using enum TokenType;
        case Unknown: return "???";
        case Integer: return "integer";
        case Float: return "float";
        case Identifier: return "identifier";
        case LBrace: return "{";
        case RBrace: return "}";
        case LBracket: return "[";
        case RBracket: return "]";
        case LParen: return "(";
        case RParen: return ")";
        case Dot: return ".";
        case Comma: return ",";
        case Equals: return "=";
        case SemiColon: return ";";
        case Colon: return ":";
        case Minus: return "-";
        case Hash: return "#";
        case At: return "@";
        case Star: return "*";
        case Question: return "?";
        case End: return "End of File";
    }
    return "<unknown>";
}
