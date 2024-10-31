// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "token.h"

#include "schematic/compiler.h"

namespace schematic::compiler
{
    constexpr LogLocation FindRange(std::string_view filename, std::string_view source, const Token& token) noexcept
    {
        LogLocation result;
        result.file = filename;
        result.line = token.line;

        const char* const text = source.data();
        std::size_t col = 0;
        for (col = token.offset; col != 0; --col)
        {
            if (text[col - 1] == '\n')
                break;
        }
        result.column = token.offset - col + 1;

        result.source = source.substr(token.offset - col);

        std::uint32_t end = result.column;
        for (std::size_t i = 0; i != token.length; ++i)
        {
            ++end;
            if (text[i + token.offset] == '\n')
                break;
        }

        result.source = result.source.substr(0, end);

        result.length = end - result.column;
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
} // namespace schematic::compiler
