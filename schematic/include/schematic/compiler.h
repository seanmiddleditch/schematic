// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_COMPILER_H
#define SCHEMATIC_COMPILER_H 1
#pragma once

#include "schematic/allocator.h"

#include <string_view>

namespace potato::schematic::compiler
{
    class Generator;
}

namespace potato::schematic
{
    struct Range;
    struct Schema;

    class CompileContext
    {
    public:
        virtual void Error(std::string_view filename, const Range& range, std::string_view message) = 0;

        virtual std::string_view ReadFileContents(ArenaAllocator& arena, std::string_view filename) = 0;
        virtual std::string_view ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer) = 0;

        CompileContext(const CompileContext&) = delete;
        CompileContext& operator=(const CompileContext&) = delete;

    protected:
        CompileContext() = default;
        ~CompileContext() = default;
    };

    class Compiler final
    {
    public:
        explicit Compiler(CompileContext& ctx, ArenaAllocator& arena);

        Compiler(const Compiler&) = delete;
        Compiler& operator=(const Compiler&) = delete;

        void SetUseBuiltins(bool useBuiltins = true);

        const Schema* Compile(std::string_view filename);

    private:
        CompileContext& ctx_;
        ArenaAllocator& arena_;
        compiler::Generator* generator_ = nullptr;
        bool useBuiltins_ = false;
    };

    struct Location
    {
        std::uint16_t line = 1;
        std::uint16_t column = 1;
    };

    struct Range
    {
        Location start;
        Location end;
    };
} // namespace potato::schematic

#endif // SCHEMATIC_COMPILER_H
