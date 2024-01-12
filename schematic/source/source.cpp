// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/source.h"

using namespace potato::schematic::compiler;

SourceLocation potato::schematic::compiler::Source::OffsetToLocation(unsigned offset) const noexcept
{
    const std::string_view data = Data();

    SourceLocation location;

    const char* pos = data.data();
    const char* lineStart = pos;
    const char* const end = pos + data.size();

    while (pos != end && offset-- != 0)
    {
        if (*pos++ == '\n')
        {
            ++location.line;
            lineStart = pos;
        }
    }

    location.column = static_cast<unsigned>(pos - lineStart) + 1;

    return location;
}

std::string_view potato::schematic::compiler::Source::Line(unsigned line) const noexcept
{
    const std::string_view data = Data();

    const char* pos = data.data();
    const char* lineStart = pos;
    const char* const end = pos + data.size();

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
