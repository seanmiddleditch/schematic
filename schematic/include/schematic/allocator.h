// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cstdlib>
#include <cstring>
#include <new>
#include <span>
#include <string_view>
#include <type_traits>

namespace potato::schematic
{
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
} // namespace potato::schematic
