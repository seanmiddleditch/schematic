// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "lexer.h"

#include "schematic/arena.h"
#include "schematic/logger.h"
#include "schematic/source.h"

#include <fmt/core.h>

#include <cstdint>

static bool Match(const char* c, std::string_view match) noexcept;

static bool IsWhitespace(char c) noexcept;
static bool IsDigit(char c) noexcept;
static bool IsHexDigit(char c) noexcept;
static bool IsBinaryDigit(char c) noexcept;
static bool IsAlpha(char c) noexcept;
static bool IsIdentHead(char c) noexcept;
static bool IsIdentBody(char c) noexcept;

// FIXME: this assumes that input text is NUL-terminated, but the interface relies on std::string_view which
//  provides no such guarantee. Either the interface needs to change, or the code here needs to be updated
//  and deeply audited.

bool potato::schematic::compiler::Tokenize(Logger& logger, ArenaAllocator& alloc, const Source* source, Array<Token>& tokens)
{
    if (source == nullptr)
        return false;

    tokens = Array<Token>();

    const std::string_view data = source->Data();
    const char* const text = data.data();
    const char* c = text;
    bool result = true;

    auto Pos = [&c, text]() noexcept
    {
        return static_cast<std::uint32_t>(c - text);
    };

    auto Error = [&logger, source, &Pos, &result]<typename... Args>(fmt::format_string<Args...> format, const Args&... args)
    {
        logger.Error({ .source = source, .offset = Pos(), .length = 1 },
            fmt::vformat(format, fmt::make_format_args(args...)));
        result = false;
    };

    auto Push = [&alloc, &tokens, &Pos](TokenType type)
    {
        tokens.PushBack(alloc, Token{ .type = type, .offset = Pos(), .length = 1 });
    };

    while (*c != '\0')
    {
        // skip whitespace
        if (IsWhitespace(*c))
        {
            while (IsWhitespace(*c))
                ++c;
            continue;
        }

        // skip comments
        if (Match(c, "//"))
        {
            c += 2;
            while (*c != '\0' && *c != '\n')
                ++c;
            continue;
        }

        // special characters
        // note: dot (.) is special, because it _could_ start a number
        switch (*c)
        {
            case ',':
                Push(TokenType::Comma);
                ++c;
                continue;
            case '=':
                Push(TokenType::Equals);
                ++c;
                continue;
            case ';':
                Push(TokenType::SemiColon);
                ++c;
                continue;
            case ':':
                Push(TokenType::Colon);
                ++c;
                continue;
            case '-':
                Push(TokenType::Minus);
                ++c;
                continue;
            case '#':
                Push(TokenType::Hash);
                ++c;
                continue;
            case '@':
                Push(TokenType::At);
                ++c;
                continue;
            case '*':
                Push(TokenType::Star);
                ++c;
                continue;
            case '?':
                Push(TokenType::Question);
                ++c;
                continue;
            case '{':
                Push(TokenType::LBrace);
                ++c;
                continue;
            case '}':
                Push(TokenType::RBrace);
                ++c;
                continue;
            case '[':
                Push(TokenType::LBracket);
                ++c;
                continue;
            case ']':
                Push(TokenType::RBracket);
                ++c;
                continue;
            case '(':
                Push(TokenType::LParen);
                ++c;
                continue;
            case ')':
                Push(TokenType::RParen);
                ++c;
                continue;
            default: break;
        }

        const bool isDot = *c == '.';

        // parse numbers, or just a plain dot
        if (IsDigit(*c) || isDot)
        {
            TokenType type = isDot ? TokenType::Float : TokenType::Integer;
            const std::uint32_t start = Pos();

            const bool isZero = *c == '0';
            ++c;

            if (isZero)
            {
                // hexadecimal 0x123
                if (*c == 'x')
                {
                    ++c;
                    if (!IsHexDigit(*c))
                        Error("Expected digits after 0x prefix");
                    while (IsHexDigit(*c))
                        ++c;
                    tokens.PushBack(alloc, Token{ .type = TokenType::HexInteger, .offset = start, .length = Pos() - start });
                    continue;
                }

                // binary 0b010
                if (*c == 'b')
                {
                    ++c;
                    if (!IsBinaryDigit(*c))
                        Error("Expected digits after 0b prefix");
                    while (IsBinaryDigit(*c))
                        ++c;
                    tokens.PushBack(alloc, Token{ .type = TokenType::BinaryInteger, .offset = start, .length = Pos() - start });
                    continue;
                }

                // zero may not be followed by any other digits
                if (IsDigit(*c))
                    Error("Leading zeroes are not permitted");
            }

            if (isDot && !IsDigit(*c))
            {
                tokens.PushBack(alloc, Token{ .type = TokenType::Dot, .offset = start, .length = 1 });
                continue;
            }

            while (IsDigit(*c))
                ++c;

            // decimal
            if (!isDot && *c == '.')
            {
                type = TokenType::Float;
                ++c;
                while (IsDigit(*c))
                    ++c;
            }

            // exponent
            if (*c == 'e' || *c == 'E')
            {
                type = TokenType::Float;
                ++c;

                if (*c == '-' || *c == '+')
                    ++c;

                if (!IsDigit(*c))
                    Error("Digits expected after exponent");

                while (IsDigit(*c))
                    ++c;
            }

            tokens.PushBack(alloc, Token{ .type = type, .offset = start, .length = Pos() - start });
            continue;
        }

        // identifiers
        if (IsIdentHead(*c))
        {
            const std::uint32_t start = Pos();
            ++c;
            while (IsIdentBody(*c))
                ++c;

            tokens.PushBack(alloc, Token{ .type = TokenType::Identifier, .offset = start, .length = Pos() - start });
            continue;
        }

        // strings
        if (*c == '"')
        {
            const std::uint32_t start = Pos();

            // multi-line strings
            if (c[1] == '"' && c[2] == '"')
            {
                c += 3;
                while (*c != '\0')
                {
                    if (c[0] == '"' && c[1] == '"' && c[2] == '"')
                        break;

                    ++c;
                }

                if (*c == '\0')
                {
                    Error("Unterminated long string");
                    break;
                }

                c += 3; // eat final delim
                tokens.PushBack(alloc, Token{ .type = TokenType::MultilineString, .offset = start, .length = Pos() - start });
                continue;
            }
            else
            {
                const char delim = *c;
                ++c;
                while (*c != '\0' && *c != delim)
                {
                    if (*c == '\\')
                    {
                        ++c;

                        if (*c == '\0')
                            break; // will trigger the unterminated string error

                        if (*c == '\n')
                            break; // will trigger the unterminated string error

                        if (*c != '\\' && *c != '\n')
                            Error("Unexpected string escape \\%c", *c);
                    }
                    ++c;
                }

                if (*c != delim)
                {
                    Error("Unterminated string");
                    break;
                }

                ++c; // eat final delim
                tokens.PushBack(alloc, Token{ .type = TokenType::String, .offset = start, .length = Pos() - start });
                continue;
            }
        }

        // unknown token
        Error("Unexpected input `{}`", *c);
        ++c;
    }

    tokens.PushBack(alloc, Token{ .type = TokenType::End, .offset = Pos() });
    return result;
}

bool Match(const char* c, std::string_view match) noexcept
{
    return std::string_view(c).starts_with(match);
}

bool IsWhitespace(char c) noexcept
{
    return c == ' ' || c == '\r' || c == '\n' || c == '\t';
}

bool IsDigit(char c) noexcept
{
    return c >= '0' && c <= '9';
}

bool IsHexDigit(char c) noexcept
{
    return (c >= '0' && c <= '9') ||
        (c >= 'a' && c <= 'f') ||
        (c >= 'A' && c <= 'F');
}

bool IsBinaryDigit(char c) noexcept
{
    return c == '0' || c == '1';
}

bool IsAlpha(char c) noexcept
{
    return (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z');
}

bool IsIdentHead(char c) noexcept
{
    return c == '_' || c == '$' || IsAlpha(c);
}

bool IsIdentBody(char c) noexcept
{
    return c == '_' || IsAlpha(c) || IsDigit(c);
}
