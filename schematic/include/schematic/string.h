// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cassert>
#include <cstring>
#include <string_view>

namespace potato::schematic
{
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

        [[nodiscard]] operator std::string_view() const noexcept { return std::string_view(str_, len_); }

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
} // namespace potato::schematic
