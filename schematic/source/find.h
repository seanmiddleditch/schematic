// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <type_traits>

namespace potato::schematic::compiler
{
    template <typename Container, typename T>
    auto* Find(Container& container, const T& value) noexcept
    {
        for (auto* it = container.begin(); it != container.end(); ++it)
        {
            if (*it == value)
                return it;
        }
        return nullptr;
    }

    template <typename Container, typename Predicate>
    auto* Find(Container& container, Predicate&& pred) noexcept
        requires std::is_invocable_r_v<bool, Predicate, decltype(*container.begin())>
    {
        for (auto* it = container.begin(); it != container.end(); ++it)
        {
            if (std::forward<Predicate>(pred)(*it))
                return it;
        }
        return decltype(container.begin()){};
    }
} // namespace potato::schematic::compiler
