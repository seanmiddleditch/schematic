// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "array.h"

#include <type_traits>

namespace schematic::compiler
{
    template <typename T, typename Predicate>
    T* Find(const Array<T*>& container, Predicate&& pred) noexcept
        requires std::is_invocable_r_v<bool, Predicate, decltype(*container.begin())>
    {
        for (T* const* it = container.begin(); it != container.end(); ++it)
        {
            if (*it != nullptr && std::forward<Predicate>(pred)(*it))
                return *it;
        }
        return nullptr;
    }

    template <typename T, typename Predicate>
    auto FindIndex(const Array<T*>& container, Predicate&& pred) noexcept
        requires std::is_invocable_r_v<bool, Predicate, decltype(*container.begin())>
    {
        for (T* const* it = container.begin(); it != container.end(); ++it)
        {
            if (*it != nullptr && std::forward<Predicate>(pred)(*it))
                return it - container.begin();
        }
        return container.end() - container.begin();
    }
} // namespace schematic::compiler
