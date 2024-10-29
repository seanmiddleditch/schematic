// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_LOGGER_H
#define SCHEMATIC_LOGGER_H 1
#pragma once

#include "schematic/allocator.h"

#include <string_view>

namespace schematic
{
    struct Range
    {
        struct
        {
            std::uint16_t line = 1;
            std::uint16_t column = 1;
        } start, end;
    };

    class Logger
    {
    public:
        virtual void Error(std::string_view filename, const Range& range, std::string_view message) = 0;

        Logger(const Logger&) = delete;
        Logger& operator=(const Logger&) = delete;

    protected:
        Logger() = default;
        ~Logger() = default;
    };
} // namespace schematic

#endif // SCHEMATIC_LOGGER_H
