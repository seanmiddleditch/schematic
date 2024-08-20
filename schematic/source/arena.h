// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/allocator.h"

#include <cassert>
#include <cstring>
#include <new>
#include <span>
#include <string_view>
#include <type_traits>

namespace potato::schematic
{
    class ArenaAllocator;

    // Trivial types must be trivially copyable and destructible.
    //
    // Unlike the std::is_trivial type trait, the Trivial concept
    // does not require a default constructor (trivial or otherwise).
    template <typename T>
    concept Trivial =
        std::is_trivially_destructible_v<T> &&
        std::is_trivially_copy_constructible_v<T> &&
        std::is_trivially_copy_assignable_v<T>;

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

        void Clear() noexcept { last_ = first_; }

        [[nodiscard]] operator std::span<T>() const noexcept
        {
            return { first_, last_ };
        }

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

    // ArenaAllocator provides efficient single-threaded allocation for
    // Trivial objects, arrays of Trivial objects, and strings.
    //
    // ArenaAllocator uses the provided allocator to allocate larger
    // blocks of memory, and will free them on destruction.
    class ArenaAllocator
    {
    public:
        explicit ArenaAllocator(Allocator& alloc) noexcept
            : alloc_(&alloc)
        {
        }
        inline ~ArenaAllocator();

        ArenaAllocator(const ArenaAllocator&) = delete;
        ArenaAllocator& operator=(const ArenaAllocator&) = delete;

        [[nodiscard]] inline void* Allocate(size_t size, size_t align);

        [[nodiscard]] inline const char* NewString(std::string_view string);

        template <Trivial T>
        [[nodiscard]] inline Array<T> NewArray(size_t capacity)
            requires std::is_trivially_destructible_v<T>;

        template <Trivial T, typename... Args>
        [[nodiscard]] inline T* Create(Args&&... args)
            requires std::is_constructible_v<T, Args...>;

        inline void Clear();

    private:
        [[nodiscard]] static constexpr size_t AlignTo(size_t value, size_t align) noexcept;

        inline void EnsureBlock(size_t minimum);

        struct BlockHeader
        {
            BlockHeader* previous = nullptr;
            BlockHeader* next = nullptr;
            size_t size = 0;
        };

        Allocator* alloc_ = nullptr;
        void* block_ = nullptr;
        size_t head_ = 0;
        size_t capacity_ = 0;
        BlockHeader* first_ = nullptr;
    };

    template <typename T>
    void Array<T>::Reserve(ArenaAllocator& alloc, size_t capacity)
    {
        if ((sentinel_ - first_) >= capacity)
            return;

        const Array<T> previous = *this;

        *this = alloc.NewArray<T>(capacity);
        for (const T* item = previous.first_; item != previous.last_; ++item)
            new (static_cast<void*>(last_++)) T(*item); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
    }

    template <typename T>
    template <typename U>
        requires std::is_trivially_constructible_v<T, U>
    T& Array<T>::PushBack(ArenaAllocator& alloc, const U& value)
    {
        if (last_ == sentinel_)
            Reserve(alloc, first_ == sentinel_ ? 32 : (sentinel_ - first_) * 2);

        new (static_cast<void*>(last_)) T(value); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
        return *last_++;
    }

    template <typename T>
    template <typename... Args>
        requires std::is_constructible_v<T, Args...>
    T& Array<T>::EmplaceBack(ArenaAllocator& alloc, Args&&... args)
    {
        if (last_ == sentinel_)
            Reserve(alloc, first_ == sentinel_ ? 32 : (sentinel_ - first_) * 2);

        new (static_cast<void*>(last_)) T(std::forward<Args>(args)...); // NOLINT(bugprone-multi-level-implicit-pointer-conversion)
        return *last_++;
    }

    ArenaAllocator::~ArenaAllocator()
    {
        BlockHeader* block = static_cast<BlockHeader*>(block_);
        while (block != nullptr)
        {
            BlockHeader* const previous = block->previous;
            alloc_->Deallocate(block, block->size);
            block = previous;
        }
    }

    // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
    void* ArenaAllocator::Allocate(size_t size, size_t align)
    {
        head_ = AlignTo(head_, align);

        if (capacity_ < size || head_ >= capacity_ - size)
        {
            EnsureBlock(size);
            head_ = AlignTo(head_, align);
        }

        void* const address = static_cast<char*>(block_) + head_;
        head_ += size;
        return address;
    }

    const char* ArenaAllocator::NewString(std::string_view string)
    {
        if (string.empty())
            return {};

        char* const memory = static_cast<char*>(Allocate(string.size() + 1 /*NUL*/, 1));
        std::memcpy(memory, string.data(), string.size());
        memory[string.size()] = '\0';
        return memory;
    }

    template <Trivial T>
    Array<T> ArenaAllocator::NewArray(size_t capacity)
        requires std::is_trivially_destructible_v<T>
    {
        void* const memory = Allocate(sizeof(T) * capacity, alignof(T));
        return Array(static_cast<T*>(memory), capacity);
    }

    template <Trivial T, typename... Args>
    T* ArenaAllocator::Create(Args&&... args)
        requires std::is_constructible_v<T, Args...>
    {
        return new (Allocate(sizeof(T), alignof(T))) T(std::forward<Args>(args)...);
    }

    void ArenaAllocator::Clear()
    {
        block_ = first_;
        head_ = 0;
        capacity_ = block_ != nullptr ? static_cast<BlockHeader*>(block_)->size : 0;
    }

    constexpr size_t ArenaAllocator::AlignTo(size_t value, size_t align) noexcept
    {
        size_t mask = align - 1;

        const size_t unaligned = value & mask;
        if (unaligned != 0)
            value += align - unaligned;

        return value;
    }

    void ArenaAllocator::EnsureBlock(size_t minimum)
    {
        constexpr size_t block_size = 16ull * 1024ull;
        constexpr size_t block_capacity = block_size - sizeof(BlockHeader);

        const bool is_large = minimum > block_capacity;

        void* block = nullptr;
        if (block_ != nullptr && !is_large && static_cast<BlockHeader*>(block_)->next != nullptr)
        {
            block = static_cast<BlockHeader*>(block_)->next;
        }
        else
        {
            capacity_ = block_capacity >= minimum ? block_capacity : minimum;
            block = alloc_->Allocate(sizeof(BlockHeader) + capacity_);
            head_ = sizeof(BlockHeader);
        }

        BlockHeader* const header = new (block) BlockHeader{ .previous = static_cast<BlockHeader*>(block_), .size = capacity_ };

        if (block_ != nullptr)
            static_cast<BlockHeader*>(block_)->next = header;
        if (first_ == nullptr)
            first_ = header;

        block_ = block;
    }
} // namespace potato::schematic
