// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_COMMON_H
#define SCHEMATIC_COMMON_H 1
#pragma once

#include <cassert>
#include <cstdint>
#include <span>

namespace potato::schematic
{
    static constexpr struct
    {
    } InvalidIndex = {};

    template <typename T, typename I = std::uint32_t>
    struct ReadOnlySpan
    {
        constexpr const T* begin() const noexcept { return data; }
        constexpr const T* end() const noexcept { return data + size; }

        constexpr const T& operator[](const I& index) const noexcept
        {
            assert(static_cast<std::uint32_t>(index) < size);
            return data[static_cast<std::uint32_t>(index)];
        }

        constexpr std::uint32_t Size() const noexcept
        {
            return size;
        }

        constexpr ReadOnlySpan<T> SubSpan(I start, std::uint32_t count) const noexcept
        {
            assert(static_cast<std::uint32_t>(start) < size);
            assert(count <= size - static_cast<std::uint32_t>(start));
            return ReadOnlySpan<T>{ .data = data + static_cast<std::uint32_t>(start), .size = count };
        }

        const T* data = nullptr;
        std::uint32_t size = 0;
    };

    template <typename Tag>
    struct Index
    {
        Index() = default;
        constexpr Index(decltype(InvalidIndex)) noexcept { }
        explicit constexpr Index(std::uint32_t init) noexcept
            : index(init) { }

        explicit constexpr operator std::uint32_t() const noexcept { return index; }

        bool operator==(const Index&) const = default;
        auto operator<=>(const Index&) const = default;

        constexpr bool operator==(decltype(InvalidIndex)) noexcept
        {
            return index == std::uint32_t(-1);
        }

        constexpr auto operator<=>(std::uint32_t rhs) const noexcept
        {
            return index <=> rhs;
        }

        constexpr auto& operator++() noexcept
        {
            ++index;
            return *this;
        }
        constexpr Index operator++(int) noexcept
        {
            Index result = *this;
            ++index;
            return result;
        }

        constexpr Index operator+(std::uint32_t offset) const noexcept
        {
            return Index(index + offset);
        }
        constexpr std::uint32_t operator-(const Index& rhs) const noexcept
        {
            return index - rhs.index;
        }

        std::uint32_t index = std::uint32_t(-1);
    };

    template <typename T>
    struct IndexRange
    {
        T start = InvalidIndex;
        std::uint32_t count = 0;

        struct Iterator
        {
            constexpr auto& operator++() noexcept
            {
                ++value;
                return *this;
            }

            constexpr const T& operator*() const noexcept
            {
                return value;
            }

            auto operator<=>(const Iterator&) const = default;

            T value = InvalidIndex;
        };

        constexpr Iterator begin() const noexcept { return { start }; }
        constexpr Iterator end() const noexcept { return { start + count }; }
    };

    using AnnotationIndex = Index<struct AnnotationIndex_>;
    using EnumItemIndex = Index<struct EnumItemIndex_>;
    using FieldIndex = Index<struct FieldIndex_>;
    using ModuleIndex = Index<struct ModuleIndex_>;
    using TypeIndex = Index<struct TypeIndex_>;
    using ValueIndex = Index<struct ValueIndex_>;
} // namespace potato::schematic

#endif // SCHEMATIC_COMMON_H
