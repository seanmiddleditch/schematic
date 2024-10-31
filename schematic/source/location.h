// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "token.h"

#include "schematic/compiler.h"

namespace schematic::compiler
{
    constexpr LogLocation FindRange(std::string_view filename, std::string_view source, std::uint32_t line, std::uint32_t offset, std::uint32_t length) noexcept
    {
        LogLocation result;
        result.file = filename;
        result.line = line;

        if (length == 0)
            length = 1;

        std::size_t start = offset;
        for (; start != 0; --start)
        {
            if (source[start - 1] == '\n')
                break;
        }
        result.column = offset - start + 1;

        std::uint32_t newline = offset;
        for (; newline != source.size(); ++newline)
        {
            if (source[newline] == '\n')
                break;
        }
        const std::uint32_t lineLength = static_cast<std::uint32_t>(newline - start);

        result.source = source.substr(start, lineLength);
        result.length = lineLength > length ? length : lineLength;
        return result;
    }

    constexpr LogLocation FindRange(std::string_view filename, std::string_view source, const Token& token) noexcept
    {
        return FindRange(filename, source, token.line, token.offset, token.length);
    }
} // namespace schematic::compiler
