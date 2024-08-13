// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/schema.h"

namespace potato::schematic
{
    struct FileId
    {
        static constexpr std::size_t InvalidValue = ~0;
        std::size_t value = InvalidValue;
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

    class CompileContext
    {
    public:
        virtual void Error(FileId file, const Range& range, std::string_view message) = 0;

        virtual std::string_view ReadFileContents(FileId id) = 0;
        virtual std::string_view GetFileName(FileId id) = 0;
        virtual FileId ResolveModule(std::string_view name, FileId referrer) = 0;

    protected:
        ~CompileContext() = default;
    };

    class Compiler
    {
    public:
        explicit Compiler(CompileContext& ctx);
        virtual ~Compiler();

        Compiler(const Compiler&) = delete;
        Compiler& operator=(const Compiler&) = delete;

        void AddBuiltins();

        const Schema* Compile(FileId file = FileId{0});

    private:
        struct Impl;
        Impl* impl_ = nullptr;
    };
} // namespace potato::schematic
