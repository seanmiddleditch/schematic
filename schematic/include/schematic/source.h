// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cassert>
#include <cstdint>
#include <string_view>

namespace potato::schematic::compiler
{
    struct SourceLocation
    {
        unsigned line = 1;
        unsigned column = 1;
    };

    class Source
    {
    public:
        virtual std::string_view Name() const noexcept = 0;
        virtual std::string_view Data() const noexcept = 0;

        SourceLocation OffsetToLocation(unsigned offset) const noexcept;
        std::string_view Line(unsigned line) const noexcept;

    protected:
        ~Source() = default;
    };
} // namespace potato::schematic::compiler
