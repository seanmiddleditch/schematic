// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "lexer.h"

#include "location.h"

#include "schematic/allocator.h"

#include <fmt/core.h>

#include <cstdint>
#include <utility>

using namespace schematic;
using namespace schematic::compiler;

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
            : cur_(text)
            , start_(text)
            , sentinel_(text + size)
        {
        }

        bool Match(char) noexcept;
        bool Match(const char* text) noexcept;
        bool Match(bool (*func)(char) noexcept) noexcept;

        bool IsEof() const noexcept;

        char Peek() const noexcept;

        std::uint32_t Pos() const noexcept;
        std::uint16_t Line() const noexcept { return line_; }

        void Advance() noexcept;
        void Advance(std::size_t length) noexcept;

    private:
        std::uint16_t line_ = 1;
        const char* cur_ = nullptr;
        const char* start_ = nullptr;
        const char* sentinel_ = nullptr;
    };
} // namespace

Array<Token> schematic::compiler::Lexer::Tokenize()
{
    tokens_.Clear();

    Input in(source_.data(), source_.size());
    bool result = true;

    auto Error = [this, &in, &result]<typename... Args>(std::uint32_t pos, fmt::format_string<Args...> format, Args&&... args)
    {
        char buffer[1024];
        auto rs = fmt::format_to_n(buffer, sizeof buffer, format, std::forward<Args>(args)...);
        context_.LogMessage(LogLevel::Error, FindRange(filename_, source_, in.Line(), pos, in.Pos() - pos), { buffer, rs.out });
        result = false;
    };

    auto Push = [this, &in](TokenType type)
    {
        tokens_.PushBack(arena_, Token{ .type = type, .line = in.Line(), .offset = in.Pos(), .length = 1 });
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

        // .. range symbol
        if (isDot && in.Match('.'))
        {
            Push(TokenType::Dot);
            continue;
        }

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
                        Error(start, "Expected digits after 0x prefix");
                    while (in.Match(IsHexDigit))
                        ;
                    tokens_.PushBack(arena_, Token{ .type = TokenType::HexInteger, .line = in.Line(), .offset = start, .length = in.Pos() - start });
                    continue;
                }

                // binary 0b010
                if (in.Match('b'))
                {
                    if (!in.Match(IsBinaryDigit))
                        Error(start, "Expected digits after 0b prefix");
                    while (in.Match(IsBinaryDigit))
                        ;
                    tokens_.PushBack(arena_, Token{ .type = TokenType::BinaryInteger, .line = in.Line(), .offset = start, .length = in.Pos() - start });
                    continue;
                }

                // zero may not be followed by any other digits
                if (in.Match(IsDigit))
                    Error(start, "Leading zeroes are not permitted");
            }

            if (isDot && !in.Match(IsDigit))
            {
                tokens_.PushBack(arena_, Token{ .type = TokenType::Dot, .line = in.Line(), .offset = start, .length = 1 });
                continue;
            }

            while (in.Match(IsDigit))
                ;

            // decimal, or integer followed by ..
            const uint32_t intPartEnd = in.Pos();
            if (!isDot && in.Match('.'))
            {
                // integer followed by ..
                if (in.Match('.'))
                {
                    type = TokenType::Integer;
                    tokens_.PushBack(arena_, Token{ .type = type, .line = in.Line(), .offset = start, .length = intPartEnd - start });
                    tokens_.PushBack(arena_, Token{ .type = TokenType::Range, .line = in.Line(), .offset = intPartEnd, .length = in.Pos() - intPartEnd });
                    continue;
                }

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
                    Error(start, "Digits expected after exponent");

                while (in.Match(IsDigit))
                    ;
            }

            tokens_.PushBack(arena_, Token{ .type = type, .line = in.Line(), .offset = start, .length = in.Pos() - start });
            continue;
        }

        // identifiers
        if (in.Match(IsIdentHead))
        {
            while (in.Match(IsIdentBody))
                ;

            tokens_.PushBack(arena_, Token{ .type = TokenType::Identifier, .line = in.Line(), .offset = start, .length = in.Pos() - start });
            continue;
        }

        // strings
        if (in.Match("\"\"\""))
        {
            const std::uint32_t startLine = in.Line();
            while (!in.Match("\"\"\""))
            {
                if (in.IsEof())
                {
                    context_.LogMessage(LogLevel::Error, FindRange(filename_, source_, startLine, start, in.Pos() - start), "Unterminated long string");
                    result = false;
                    break;
                }

                in.Advance();
            }

            tokens_.PushBack(arena_, Token{ .type = TokenType::MultilineString, .line = in.Line(), .offset = start, .length = in.Pos() - start });
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
                        Error(in.Pos(), "Unexpected string escape \\%c", in.Peek());

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
                Error(start, "Unterminated string");
                break;
            }

            tokens_.PushBack(arena_, Token{ .type = TokenType::String, .line = in.Line(), .offset = start, .length = in.Pos() - start });
            continue;
        }

        // unknown token
        Error(in.Pos(), "Unexpected input `{}`", in.Peek());
        in.Advance();
    }

    tokens_.PushBack(arena_, Token{ .type = TokenType::End, .line = in.Line(), .offset = in.Pos() });
    return tokens_;
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
    return c == '_' || IsAlpha(c);
}

bool IsIdentBody(char c) noexcept
{
    return c == '_' || IsAlpha(c) || IsDigit(c);
}

bool Input::Match(char c) noexcept
{
    if (cur_ == sentinel_)
        return false;
    if (*cur_ != c)
        return false;
    Advance();
    return true;
}

bool Input::Match(const char* text) noexcept
{
    const std::size_t tlen = std::strlen(text);
    if (tlen > (sentinel_ - cur_))
        return false;
    if (std::memcmp(cur_, text, tlen) != 0)
        return false;
    Advance(tlen);
    return true;
}

bool Input::Match(bool (*func)(char) noexcept) noexcept
{
    if (cur_ == sentinel_)
        return false;
    if (!func(*cur_))
        return false;
    Advance();
    return true;
}

bool Input::IsEof() const noexcept
{
    return cur_ == sentinel_;
}

char Input::Peek() const noexcept
{
    if (cur_ == sentinel_)
        return '\0';
    return *cur_;
}

std::uint32_t Input::Pos() const noexcept
{
    return cur_ - start_;
}

void Input::Advance() noexcept
{
    if (cur_ == sentinel_)
        return;

    if (*cur_ == '\n')
        ++line_;

    ++cur_;
}

void Input::Advance(std::size_t length) noexcept
{
    const std::size_t avail = sentinel_ - cur_;
    if (length > avail)
        length = avail;
    for (const char* c = cur_; c != cur_ + length; ++c)
    {
        if (*c == '\n')
            ++line_;
    }
    cur_ += length;
}
