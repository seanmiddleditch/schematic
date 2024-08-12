// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cassert>
#include <cstring>
#include <string_view>

namespace potato::schematic
{
    class ArenaAllocator;

    // String is a non-owning string reference that has both
    // O(1) size calculation and guarantees NUL termination.
    class CStringView
    {
    public:
        CStringView() = default;
        explicit CStringView(const char* str, size_t length) noexcept
            : str_(str)
            , len_(length)
        {
            assert(str_[len_] == '\0');
        }

        [[nodiscard]] const char* CStr() const noexcept { return str_; }
        [[nodiscard]] const char* Data() const noexcept { return str_; }
        [[nodiscard]] size_t Size() const noexcept { return len_; }

        [[nodiscard]] explicit operator bool() const noexcept { return len_ != 0; }

        [[nodiscard]] operator std::string_view() const noexcept { return { str_, len_ }; }

        [[nodiscard]] bool operator==(const CStringView& rhs) const noexcept
        {
            return len_ == rhs.len_ && std::strncmp(str_, rhs.str_, len_) == 0;
        }
        [[nodiscard]] bool operator==(std::string_view rhs) const noexcept
        {
            return len_ == rhs.size() && std::strncmp(str_, rhs.data(), len_) == 0;
        }
        [[nodiscard]] bool operator==(const char* rhs) const noexcept
        {
            size_t n = 0;
            for (n = 0; n != len_ && rhs[n] != '\0'; ++n)
                if (str_[n] != rhs[n])
                    return false;
            return rhs[n] == '\0';
        }

    private:
        const char* str_ = "";
        size_t len_ = 0;
    };

    // Array is a non-owning growable contiguous container of
    // Trivial elements.
    //
    // Operations that mutate capacity require an ArenaAllocator
    // from which new capacity is allocated. Previous memory is
    // left untouched, as elements must be trivially copyable.
    template <typename T>
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

        inline void Reserve(ArenaAllocator& alloc, size_t capacity);

        template <typename U>
            requires std::is_trivially_constructible_v<T, U>
        T& PushBack(ArenaAllocator& alloc, const U& value);

        template <typename... Args>
            requires std::is_constructible_v<T, Args...>
        T& EmplaceBack(ArenaAllocator& alloc, Args&&... args);

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
} // namespace potato::schematic
