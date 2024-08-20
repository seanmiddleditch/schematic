// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/allocator.h"

#include <span>

namespace potato::schematic
{
    // Array is a non-owning growable contiguous container of
    // Trivial elements.
    //
    // Operations that mutate capacity require an ArenaAllocator
    // from which new capacity is allocated. Previous memory is
    // left untouched, as elements must be trivially copyable.
    template <Trivial T>
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

        [[nodiscard]] const T* begin() const noexcept { return first_; }
        [[nodiscard]] const T* end() const noexcept { return last_; }

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

        const T& operator[](size_t index) const noexcept
        {
            assert(index < (last_ - first_));
            return first_[index];
        }

    private:
        T* first_ = nullptr;
        T* last_ = nullptr;
        const T* sentinel_ = nullptr;
    };

    template <Trivial T>
    void Array<T>::Reserve(ArenaAllocator& arena, size_t capacity)
    {
        if ((sentinel_ - first_) >= capacity)
            return;

        const Array<T> previous = *this;

        *this = arena.NewArray<T>(capacity);
        for (const T* item = previous.first_; item != previous.last_; ++item)
            new (static_cast<void*>(last_++)) T(*item); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
    }

    template <Trivial T>
    template <typename U>
        requires std::is_trivially_constructible_v<T, U>
    T& Array<T>::PushBack(ArenaAllocator& arena, const U& value)
    {
        if (last_ == sentinel_)
            Reserve(arena, first_ == sentinel_ ? 32 : (sentinel_ - first_) * 2);

        new (static_cast<void*>(last_)) T(value); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
        return *last_++;
    }

    template <Trivial T>
    template <typename... Args>
        requires std::is_constructible_v<T, Args...>
    T& Array<T>::EmplaceBack(ArenaAllocator& arena, Args&&... args)
    {
        if (last_ == sentinel_)
            Reserve(arena, first_ == sentinel_ ? 32 : (sentinel_ - first_) * 2);

        new (static_cast<void*>(last_)) T(std::forward<Args>(args)...); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
        return *last_++;
    }
} // namespace potato::schematic
