// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cassert>
#include <cstdint>
#include <string_view>

namespace potato::schematic::compiler
{
    struct SourceLocation
    {
        std::uint32_t line = 1;
        std::uint32_t column = 1;
    };

    class Source
    {
    public:
        virtual std::string_view Name() const noexcept = 0;
        virtual std::string_view Data() const noexcept = 0;

        SourceLocation OffsetToLocation(std::uint32_t offset) const noexcept;
        std::string_view Line(std::uint32_t line) const noexcept;

    protected:
        ~Source() = default;
    };
} // namespace potato::schematic::compiler
