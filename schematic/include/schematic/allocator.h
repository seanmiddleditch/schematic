// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_ALLOCATOR_H
#define SCHEMATIC_ALLOCATOR_H 1
#pragma once

#include <cstdlib>
#include <cstring>
#include <new>
#include <string_view>
#include <type_traits>

namespace schematic
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

    // Arena array types must be trivially copyable and destructible.
    //
    // Unlike the std::is_trivial type trait, the Trivial concept
    // does not require a default constructor (trivial or otherwise).
    template <typename T>
    concept Trivial =
        std::is_trivially_destructible_v<T> &&
        std::is_trivially_copy_constructible_v<T> &&
        std::is_trivially_copy_assignable_v<T>;

    template <Trivial T, typename I = std::uint32_t>
    class Array;

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
        ArenaAllocator() noexcept;
        ~ArenaAllocator();

        ArenaAllocator(const ArenaAllocator&) = delete;
        ArenaAllocator& operator=(const ArenaAllocator&) = delete;

        [[nodiscard]] void* Allocate(size_t size, size_t align);

        [[nodiscard]] const char* NewString(std::string_view string);

        template <Trivial T, typename I = std::uint32_t, template <typename, typename> class A = Array>
        [[nodiscard]] A<T, I> NewArrayCapacity(size_t capacity);

        template <Trivial T, typename I = std::uint32_t, template <typename, typename> class A = Array>
        [[nodiscard]] A<T, I> NewArray(size_t size);

        template <Trivial T, typename I = std::uint32_t, template <typename, typename> class A = Array>
        [[nodiscard]] A<T, I> NewArray(size_t size, const T& value);

        template <typename T, typename... Args>
        [[nodiscard]] T* New(Args&&... args)
            requires std::is_trivially_destructible_v<T> && std::is_constructible_v<T, Args...>;

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

    template <Trivial T, typename I, template <typename, typename> class A>
    A<T, I> ArenaAllocator::NewArrayCapacity(size_t capacity)
    {
        void* const memory = Allocate(sizeof(T) * capacity, alignof(T));
        return A<T, I>(static_cast<T*>(memory), capacity);
    }

    template <Trivial T, typename I, template <typename, typename> class A>
    [[nodiscard]] A<T, I> ArenaAllocator::NewArray(size_t size)
    {
        T* const elements = static_cast<T*>(Allocate(sizeof(T) * size, alignof(T)));

        if constexpr (!std::is_trivially_default_constructible_v<T>)
        {
            for (T* element = elements; element != elements + size; ++element)
                new (element) T();
        }

        return A<T, I>(elements, size, size);
    }

    template <Trivial T, typename I, template <typename, typename> class A>
    [[nodiscard]] A<T, I> ArenaAllocator::NewArray(size_t size, const T& value)
    {
        T* const elements = static_cast<T*>(Allocate(sizeof(T) * size, alignof(T)));

        for (T* element = elements; element != elements + size; ++element)
            new (element) T(value);

        return A<T, I>(elements, size, size);
    }

    template <typename T, typename... Args>
    T* ArenaAllocator::New(Args&&... args)
        requires std::is_trivially_destructible_v<T> && std::is_constructible_v<T, Args...>
    {
        return new (Allocate(sizeof(T), alignof(T))) T(std::forward<Args>(args)...);
    }
} // namespace schematic

#endif // SCHEMATIC_ALLOCATOR_H
