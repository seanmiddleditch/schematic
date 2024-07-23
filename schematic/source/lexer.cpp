// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "lexer.h"

#include "schematic/arena.h"
#include "schematic/logger.h"
#include "schematic/source.h"

#include <fmt/core.h>

#include <cstdint>

static bool IsWhitespace(char c) noexcept;
static bool IsDigit(char c) noexcept;
static bool IsHexDigit(char c) noexcept;
static bool IsBinaryDigit(char c) noexcept;
static bool IsAlpha(char c) noexcept;
static bool IsIdentHead(char c) noexcept;
static bool IsIdentBody(char c) noexcept;

namespace
{
    struct Input
    {
        Input(const char* text, std::size_t size) noexcept
            : cur(text)
            , start(text)
            , sentinel(text + size)
        {
        }

        bool Match(char) noexcept;
        bool Match(const char* text) noexcept;
        bool Match(bool (*func)(char) noexcept) noexcept;

        bool MatchAll(bool (*func)(char) noexcept) noexcept;

        bool IsEof() const noexcept;

        char Peek() const noexcept;

        std::uint32_t Pos() const noexcept;

        void Advance() noexcept;

    private:
        const char* cur = nullptr;
        const char* start = nullptr;
        const char* sentinel = nullptr;
    };
} // namespace

// FIXME: this assumes that input text is NUL-terminated, but the interface relies on std::string_view which
//  provides no such guarantee. Either the interface needs to change, or the code here needs to be updated
//  and deeply audited.

bool potato::schematic::compiler::Tokenize(Logger& logger, ArenaAllocator& alloc, const Source* source, Array<Token>& tokens)
{
    if (source == nullptr)
        return false;

    tokens = Array<Token>();

    const std::string_view data = source->Data();
    Input in(data.data(), data.size());
    bool result = true;

    auto Error = [&logger, source, &in, &result]<typename... Args>(fmt::format_string<Args...> format, const Args&... args)
    {
        logger.Error({ .source = source, .offset = in.Pos(), .length = 1 },
            fmt::vformat(format, fmt::make_format_args(args...)));
        result = false;
    };

    auto Push = [&alloc, &tokens, &in](TokenType type)
    {
        tokens.PushBack(alloc, Token{ .type = type, .offset = in.Pos(), .length = 1 });
    };

    while (!in.IsEof())
    {
        // skip whitespace
        if (in.Match(IsWhitespace))
        {
            while (!in.IsEof() && in.Match(IsWhitespace))
                ;
            continue;
        }

        // skip comments
        if (in.Match("//"))
        {
            while (!in.IsEof() && !in.Match('\n'))
                in.Advance();
            continue;
        }

        // special characters
        // note: dot (.) is special, because it _could_ start a number
        switch (in.Peek())
        {
            case ',':
                Push(TokenType::Comma);
                in.Advance();
                continue;
            case '=':
                Push(TokenType::Equals);
                in.Advance();
                continue;
            case ';':
                Push(TokenType::SemiColon);
                in.Advance();
                continue;
            case ':':
                Push(TokenType::Colon);
                in.Advance();
                continue;
            case '-':
                Push(TokenType::Minus);
                in.Advance();
                continue;
            case '#':
                Push(TokenType::Hash);
                in.Advance();
                continue;
            case '@':
                Push(TokenType::At);
                in.Advance();
                continue;
            case '*':
                Push(TokenType::Star);
                in.Advance();
                continue;
            case '?':
                Push(TokenType::Question);
                in.Advance();
                continue;
            case '{':
                Push(TokenType::LBrace);
                in.Advance();
                continue;
            case '}':
                Push(TokenType::RBrace);
                in.Advance();
                continue;
            case '[':
                Push(TokenType::LBracket);
                in.Advance();
                continue;
            case ']':
                Push(TokenType::RBracket);
                in.Advance();
                continue;
            case '(':
                Push(TokenType::LParen);
                in.Advance();
                continue;
            case ')':
                Push(TokenType::RParen);
                in.Advance();
                continue;
            default: break;
        }

        const std::uint32_t start = in.Pos();

        const bool isZero = in.Peek() == '0';
        const bool isDot = in.Match('.');

        // parse numbers, or just a plain dot
        if (isDot || in.Match(IsDigit))
        {
            TokenType type = isDot ? TokenType::Float : TokenType::Integer;

            if (isZero)
            {
                // hexadecimal 0x123
                if (in.Match('x'))
                {
                    if (!in.Match(IsHexDigit))
                        Error("Expected digits after 0x prefix");
                    while (in.Match(IsHexDigit))
                        ;
                    tokens.PushBack(alloc, Token{ .type = TokenType::HexInteger, .offset = start, .length = in.Pos() - start });
                    continue;
                }

                // binary 0b010
                if (in.Match('b'))
                {
                    if (!in.Match(IsBinaryDigit))
                        Error("Expected digits after 0b prefix");
                    while (in.Match(IsBinaryDigit))
                        ;
                    tokens.PushBack(alloc, Token{ .type = TokenType::BinaryInteger, .offset = start, .length = in.Pos() - start });
                    continue;
                }

                // zero may not be followed by any other digits
                if (in.Match(IsDigit))
                    Error("Leading zeroes are not permitted");
            }

            if (isDot && !in.Match(IsDigit))
            {
                tokens.PushBack(alloc, Token{ .type = TokenType::Dot, .offset = start, .length = 1 });
                continue;
            }

            while (in.Match(IsDigit))
                ;

            // decimal
            if (!isDot && in.Match('.'))
            {
                type = TokenType::Float;
                while (in.Match(IsDigit))
                    ;
            }

            // exponent
            if (in.Match('e') || in.Match('E'))
            {
                type = TokenType::Float;

                if (in.Match('-') || in.Match('+'))
                    ;

                if (!in.Match(IsDigit))
                    Error("Digits expected after exponent");

                while (in.Match(IsDigit))
                    ;
            }

            tokens.PushBack(alloc, Token{ .type = type, .offset = start, .length = in.Pos() - start });
            continue;
        }

        // identifiers
        if (in.Match(IsIdentHead))
        {
            while (in.Match(IsIdentBody))
                ;

            tokens.PushBack(alloc, Token{ .type = TokenType::Identifier, .offset = start, .length = in.Pos() - start });
            continue;
        }

        // strings
        if (in.Match("\"\"\""))
        {
            while (!in.Match("\"\"\""))
            {
                if (in.IsEof())
                {
                    Error("Unterminated long string");
                    break;
                }

                in.Advance();
            }

            tokens.PushBack(alloc, Token{ .type = TokenType::MultilineString, .offset = start, .length = in.Pos() - start });
            continue;
        }
        else if (in.Match('"'))
        {
            while (!in.IsEof() && in.Peek() != '"')
            {
                if (in.Match('\\'))
                {
                    if (in.IsEof())
                        break; // will trigger the unterminated string error

                    if (!in.Match('\\') && !in.Match('n'))
                        Error("Unexpected string escape \\%c", in.Peek());

                    continue;
                }

                if (in.Peek() == '\n')
                {
                    break; // will trigger the unterminated string error
                }

                in.Advance();
            }

            if (!in.Match('"'))
            {
                Error("Unterminated string");
                break;
            }

            tokens.PushBack(alloc, Token{ .type = TokenType::String, .offset = start, .length = in.Pos() - start });
            continue;
        }

        // unknown token
        Error("Unexpected input `{}`", in.Peek());
        in.Advance();
    }

    tokens.PushBack(alloc, Token{ .type = TokenType::End, .offset = in.Pos() });
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

bool Input::Match(char c) noexcept
{
    if (cur == sentinel)
        return false;
    if (*cur != c)
        return false;
    ++cur;
    return true;
}

bool Input::Match(const char* text) noexcept
{
    const std::size_t tlen = std::strlen(text);
    if (tlen > (sentinel - cur))
        return false;
    if (std::memcmp(cur, text, tlen) != 0)
        return false;
    cur += tlen;
    return true;
}

bool Input::Match(bool (*func)(char) noexcept) noexcept
{
    if (cur == sentinel)
        return false;
    if (!func(*cur))
        return false;
    ++cur;
    return true;
}

bool Input::MatchAll(bool (*func)(char) noexcept) noexcept
{
    const char* const original = cur;
    while (cur != sentinel && func(*cur))
        ++cur;
    return cur != original;
}

bool Input::IsEof() const noexcept
{
    return cur == sentinel;
}

char Input::Peek() const noexcept
{
    if (cur == sentinel)
        return '\0';
    return *cur;
}

std::uint32_t Input::Pos() const noexcept
{
    return cur - start;
}

void Input::Advance() noexcept
{
    if (cur != sentinel)
        ++cur;
}
