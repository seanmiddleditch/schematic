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

    struct ModuleId
    {
        static constexpr std::size_t InvalidValue = ~0;
        std::size_t value = InvalidValue;
    };

    class CompileContext
    {
    public:
        virtual void Error(ModuleId moduleId, const Range& range, std::string_view message) = 0;

        virtual std::string_view ReadFileContents(ModuleId id) = 0;
        virtual std::string_view GetFileName(ModuleId id) = 0;
        virtual ModuleId ResolveModule(std::string_view name, ModuleId referrer) = 0;

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

        const Schema* Compile(ModuleId moduleId);

    private:
        CompileContext& ctx_;
        ArenaAllocator& arena_;
        compiler::Generator* generator_ = nullptr;
        bool useBuiltins_ = false;
    };

    struct Location
    {
        std::uint32_t line = 1;
        std::uint32_t column = 1;
    };

    struct Range
    {
        Location start;
        Location end;
    };
} // namespace potato::schematic

#endif // SCHEMATIC_COMPILER_H
