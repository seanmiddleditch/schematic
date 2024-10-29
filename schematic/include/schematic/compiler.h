// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_COMPILER_H
#define SCHEMATIC_COMPILER_H 1
#pragma once

#include "schematic/allocator.h"

#include <string_view>

namespace schematic
{
    struct Schema;

    struct LogLocation
    {
        std::string_view file;
        std::uint32_t line = 0;
        std::uint32_t column = 0;
        std::uint32_t length = 0;
    };

    class CompileContext
    {
    public:
        virtual std::string_view ReadFileContents(ArenaAllocator& arena, std::string_view filename) = 0;
        virtual std::string_view ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer) = 0;
        virtual void LogMessage(const LogLocation& location, std::string_view message) = 0;

    protected:
        ~CompileContext() = default;
    };

    class Compiler
    {
    public:
        ~Compiler() = default; // intentionally trivial; Compiler must not hold onto external resources

        virtual void AddStandardPreamble() = 0;
        virtual void AddPreamble(std::string_view filename) = 0;
        virtual const Schema* Compile(std::string_view filename) = 0;

    protected:
        Compiler() = default;
    };

    Compiler* NewCompiler(ArenaAllocator& arena, CompileContext& context);
} // namespace schematic

#endif // SCHEMATIC_COMPILER_H
