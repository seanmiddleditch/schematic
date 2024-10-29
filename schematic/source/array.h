// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/allocator.h"
#include "schematic/common.h"

#include <cassert>
#include <span>

namespace schematic
{
    // Array is a non-owning growable contiguous container of
    // Trivial elements.
    //
    // Operations that mutate capacity require an ArenaAllocator
    // from which new capacity is allocated. Previous memory is
    // left untouched, as elements must be trivially copyable.
    template <Trivial T, typename I>
    class Array
    {
    public:
        Array() = default;
        explicit Array(T* first, size_t capacity) noexcept
            : first_(first)
            , last_(first)
            , sentinel_(first + capacity)
        {
        }

        explicit Array(T* first, size_t size, size_t capacity) noexcept
            : first_(first)
            , last_(first + size)
            , sentinel_(first + capacity)
        {
        }

        [[nodiscard]] const T* begin() const noexcept { return first_; }
        [[nodiscard]] const T* end() const noexcept { return last_; }

        [[nodiscard]] T* begin() noexcept { return first_; }
        [[nodiscard]] T* end() noexcept { return last_; }

        [[nodiscard]] bool IsEmpty() const noexcept { return first_ == last_; }
        [[nodiscard]] explicit operator bool() const noexcept { return first_ != last_; }

        [[nodiscard]] size_t Capacity() const noexcept { return sentinel_ - first_; }
        [[nodiscard]] size_t Size() const noexcept { return last_ - first_; }
        [[nodiscard]] const T* Data() const noexcept { return first_; }
        [[nodiscard]] T* Data() noexcept { return first_; }

        void Clear() noexcept { last_ = first_; }

        [[nodiscard]] operator std::span<T>() const noexcept
        {
            return { first_, last_ };
        }

        template <typename U, typename J>
            requires std::is_convertible_v<T, U>
        [[nodiscard]] operator ReadOnlySpan<U, J>() const noexcept
        {
            return { first_, static_cast<std::uint32_t>(last_ - first_) };
        }

        inline void Reserve(ArenaAllocator& arena, size_t capacity);

        template <typename U>
            requires std::is_trivially_constructible_v<T, U>
        T& PushBack(ArenaAllocator& arena, const U& value);

        template <typename... Args>
            requires std::is_constructible_v<T, Args...>
        T& EmplaceBack(ArenaAllocator& arena, Args&&... args);

        void PopBack() noexcept
        {
            assert(first_ != last_);
            --last_;
        }

        [[nodiscard]] T& Front() noexcept
        {
            assert(first_ != last_);
            return *first_;
        }

        [[nodiscard]] const T& Front() const noexcept
        {
            assert(first_ != last_);
            return *first_;
        }

        [[nodiscard]] T& Back() noexcept
        {
            assert(first_ != last_);
            return *(last_ - 1);
        }

        [[nodiscard]] const T& Back() const noexcept
        {
            assert(first_ != last_);
            return *(last_ - 1);
        }

        [[nodiscard]] const T& operator[](I index) const noexcept
        {
            assert(static_cast<std::uint32_t>(index) < (last_ - first_));
            return first_[static_cast<std::uint32_t>(index)];
        }

        [[nodiscard]] T& operator[](I index) noexcept
        {
            assert(static_cast<std::uint32_t>(index) < (last_ - first_));
            return first_[static_cast<std::uint32_t>(index)];
        }

    private:
        T* first_ = nullptr;
        T* last_ = nullptr;
        const T* sentinel_ = nullptr;
    };

    template <Trivial T, typename I>
    void Array<T, I>::Reserve(ArenaAllocator& arena, size_t capacity)
    {
        if ((sentinel_ - first_) >= capacity)
            return;

        const Array<T, I> previous = *this;

        *this = arena.NewArrayCapacity<T, I>(capacity);
        for (const T* item = previous.first_; item != previous.last_; ++item)
            new (static_cast<void*>(last_++)) T(*item); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
    }

    template <Trivial T, typename I>
    template <typename U>
        requires std::is_trivially_constructible_v<T, U>
    T& Array<T, I>::PushBack(ArenaAllocator& arena, const U& value)
    {
        if (last_ == sentinel_)
            Reserve(arena, first_ == sentinel_ ? 32 : (sentinel_ - first_) * 2);

        new (static_cast<void*>(last_)) T(value); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
        return *last_++;
    }

    template <Trivial T, typename I>
    template <typename... Args>
        requires std::is_constructible_v<T, Args...>
    T& Array<T, I>::EmplaceBack(ArenaAllocator& arena, Args&&... args)
    {
        if (last_ == sentinel_)
            Reserve(arena, first_ == sentinel_ ? 32 : (sentinel_ - first_) * 2);

        new (static_cast<void*>(last_)) T(std::forward<Args>(args)...); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
        return *last_++;
    }
} // namespace schematic
