// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <new>
#include <span>
#include <string_view>
#include <type_traits>

namespace potato::schematic
{
    class Allocator;
    class ArenaAllocator;
    class NewDeleteAllocator;

    class Allocator
    {
    public:
        virtual void* Allocate(std::size_t size) = 0;
        virtual void Deallocate(void* memory, std::size_t size) = 0;

        Allocator(const Allocator&) = delete;
        Allocator& operator=(const Allocator&) = delete;

        template <typename T, typename... Args>
        [[nodiscard]] inline T* New(Args&&... args)
            requires std::is_constructible_v<T, Args...>
        {
            return new (Allocate(sizeof(T))) T(std::forward<Args>(args)...);
        }

        [[nodiscard]] const char* NewString(std::string_view string)
        {
            const std::size_t size = string.size();
            char* const result = static_cast<char*>(Allocate(size + 1));
            std::memcpy(result, string.data(), size);
            result[size] = '\0';
            return result;
        }

        template <typename T>
        [[nodiscard]] std::span<T> NewArray(const std::size_t size, const T& value = {})
        {
            T* const array = static_cast<T*>(Allocate(size * sizeof(T)));
            for (std::size_t index = 0; index != size; ++index)
                new (&array[index]) T(value);
            return std::span<T>(array, size);
        }

    protected:
        Allocator() = default;
        ~Allocator() = default;
    };

    class NewDeleteAllocator final : public Allocator
    {
    public:
        void* Allocate(std::size_t size) override { return ::operator new(size); }
        void Deallocate(void* memory, std::size_t) override { ::operator delete(memory); }
    };

    // Trivial types must be trivially copyable and destructible.
    //
    // Unlike the std::is_trivial type trait, the Trivial concept
    // does not require a default constructor (trivial or otherwise).
    template <typename T>
    concept Trivial =
        std::is_trivially_destructible_v<T> &&
        std::is_trivially_copy_constructible_v<T> &&
        std::is_trivially_copy_assignable_v<T>;

    template <Trivial T> class Array;

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
        ~ArenaAllocator();

        ArenaAllocator(const ArenaAllocator&) = delete;
        ArenaAllocator& operator=(const ArenaAllocator&) = delete;

        [[nodiscard]] void* Allocate(size_t size, size_t align);

        [[nodiscard]] const char* NewString(std::string_view string);

        template <Trivial T, template <typename> class A = Array>
        [[nodiscard]] A<T> NewArray(size_t capacity)
            requires std::is_trivially_destructible_v<T>;

        template <Trivial T, typename... Args>
        [[nodiscard]] T* New(Args&&... args)
            requires std::is_constructible_v<T, Args...>;

        void Clear();

    private:
        void EnsureBlock(size_t minimum);

        struct BlockHeader;

        Allocator* alloc_ = nullptr;
        void* block_ = nullptr;
        size_t head_ = 0;
        size_t capacity_ = 0;
        BlockHeader* first_ = nullptr;
    };

    template <Trivial T, template <typename> class A>
    A<T> ArenaAllocator::NewArray(size_t capacity)
        requires std::is_trivially_destructible_v<T>
    {
        void* const memory = Allocate(sizeof(T) * capacity, alignof(T));
        return A(static_cast<T*>(memory), capacity);
    }

    template <Trivial T, typename... Args>
    T* ArenaAllocator::New(Args&&... args)
        requires std::is_constructible_v<T, Args...>
    {
        return new (Allocate(sizeof(T), alignof(T))) T(std::forward<Args>(args)...);
    }
} // namespace potato::schematic
