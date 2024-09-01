// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "token.h"

#include "schematic/compiler.h"

namespace potato::schematic::compiler
{
    constexpr Range FindRange(std::string_view source, const Token& token) noexcept
    {
        Range result;
        result.start.line = token.line;

        const char* const text = source.data();
        std::size_t col = 0;
        for (col = token.offset; col != 0; --col)
        {
            if (text[col - 1] == '\n')
                break;
        }
        result.start.column = token.offset - col + 1;

        result.end = result.start;
        for (std::size_t i = 0; i != token.length; ++i)
        {
            if (text[i + token.offset] == '\n')
            {
                ++result.start.line;
                result.end.column = 1;
            }
        }

        return result;
    }

    constexpr std::string_view ExtractLine(std::string_view source, std::uint32_t line) noexcept
    {
        const char* pos = source.data();
        const char* lineStart = pos;
        const char* const end = pos + source.size();

        while (pos != end && line > 1)
        {
            if (*pos++ == '\n')
            {
                lineStart = pos;
                --line;
            }
        }

        while (pos != end && *pos != '\n')
            ++pos;

        return std::string_view(lineStart, pos - lineStart);
    }
} // namespace potato::schematic::compiler
