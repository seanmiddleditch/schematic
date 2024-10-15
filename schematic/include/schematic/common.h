// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_COMMON_H
#define SCHEMATIC_COMMON_H 1
#pragma once

#include <cstdint>
#include <span>

namespace potato::schematic
{
    static constexpr std::uint32_t InvalidIndex = std::uint32_t(-1);

    template <typename T, typename I = std::uint32_t>
    using ReadOnlySpan = std::span<const T>;

    template <typename T>
    struct IndexRange
    {
        T start = InvalidIndex;
        std::uint32_t count = 0;
    };
} // namespace potato::schematic

#endif // SCHEMATIC_COMMON_H
