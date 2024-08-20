// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/allocator.h"

#include <string_view>

namespace potato::schematic
{
    struct Range;
    struct Schema;

    struct FileId
    {
        static constexpr std::size_t InvalidValue = ~0;
        std::size_t value = InvalidValue;
    };

    class CompileContext
    {
    public:
        virtual void Error(FileId file, const Range& range, std::string_view message) = 0;

        virtual std::string_view ReadFileContents(FileId id) = 0;
        virtual std::string_view GetFileName(FileId id) = 0;
        virtual FileId ResolveModule(std::string_view name, FileId referrer) = 0;

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
        virtual ~Compiler();

        Compiler(const Compiler&) = delete;
        Compiler& operator=(const Compiler&) = delete;

        void SetUseBuiltins(bool useBuiltins = true);

        bool Compile(FileId file);

        const Schema* GetSchema(); // returns nullptr if Compile has not previously returned true

    private:
        struct Impl;
        Impl* impl_ = nullptr;
        ArenaAllocator& arena_;
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
