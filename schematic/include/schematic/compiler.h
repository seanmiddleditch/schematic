// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_COMPILER_H
#define SCHEMATIC_COMPILER_H 1
#pragma once

#include "schematic/allocator.h"

#include <string_view>

namespace potato::schematic
{
    class Logger;
    struct Schema;

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

    const Schema* Compile(ArenaAllocator& arena, Logger& logger, CompileContext& ctx, std::string_view filename, std::string_view source);
} // namespace potato::schematic

#endif // SCHEMATIC_COMPILER_H
