// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/compiler.h"

namespace potato::schematic::compiler
{
    constexpr Location FindLocation(std::string_view source, std::size_t offset) noexcept
    {
        Location result;

        const char* pos = source.data();
        const char* lineStart = pos;
        const char* const end = pos + source.size();

        while (pos != end && offset-- != 0)
        {
            if (*pos++ == '\n')
            {
                ++result.line;
                lineStart = pos;
            }
        }

        result.column = static_cast<std::uint32_t>(pos - lineStart) + 1;

        return result;
    }

    constexpr Range FindRange(std::string_view source, std::size_t offset, std::size_t length) noexcept
    {
        Range result;

        const char* pos = source.data();
        const char* lineStart = pos;
        const char* const end = pos + source.size();

        while (pos != end && offset-- != 0)
        {
            if (*pos++ == '\n')
            {
                ++result.start.line;
                lineStart = pos;
            }
        }

        result.start.column = static_cast<std::uint32_t>(pos - lineStart) + 1;

        result.end.line = result.start.line;

        while (pos != end && length-- != 0)
        {
            if (*pos++ == '\n')
            {
                ++result.end.line;
                lineStart = pos;
            }
        }

        result.end.column = static_cast<std::uint32_t>(pos - lineStart) + 1;

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
