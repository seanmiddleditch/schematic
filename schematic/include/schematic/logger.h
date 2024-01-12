// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <string_view>

namespace potato::schematic::compiler
{
    class Source;

    struct LogLocation
    {
        const Source* source = nullptr;
        unsigned offset = 0;
        unsigned length = 1;
    };

    class Logger
    {
    public:
        virtual ~Logger() = default;

        virtual void Error(const LogLocation& location, std::string_view message) = 0;
    };
} // namespace potato::schematic::compiler
