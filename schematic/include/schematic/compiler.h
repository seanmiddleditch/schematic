// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_COMPILER_H
#define SCHEMATIC_COMPILER_H 1
#pragma once

#include "schematic/allocator.h"

#include <string_view>

namespace potato::schematic
{
    struct Range;
    struct Schema;

    class Logger
    {
    public:
        virtual void Error(std::string_view filename, const Range& range, std::string_view message) = 0;

        static Logger& Default() noexcept;

        Logger(const Logger&) = delete;
        Logger& operator=(const Logger&) = delete;

    protected:
        Logger() = default;
        ~Logger() = default;
    };

    class CompileContext
    {
    public:
        virtual std::string_view ReadFileContents(ArenaAllocator& arena, std::string_view filename) = 0;
        virtual std::string_view ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer) = 0;

        CompileContext(const CompileContext&) = delete;
        CompileContext& operator=(const CompileContext&) = delete;

    protected:
        CompileContext() = default;
        ~CompileContext() = default;
    };

    struct Range
    {
        struct
        {
            std::uint16_t line = 1;
            std::uint16_t column = 1;
        } start, end;
    };

    const Schema* Compile(ArenaAllocator& arena, Logger& logger, CompileContext& ctx, std::string_view filename, std::string_view source);
} // namespace potato::schematic

#endif // SCHEMATIC_COMPILER_H
