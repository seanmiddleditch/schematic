// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

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

    class Allocator
    {
    public:
        virtual void* Allocate(std::size_t size) = 0;
        virtual void Deallocate(void* memory, std::size_t size) = 0;

    protected:
        ~Allocator() = default;
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

    class Compiler final
    {
    public:
        explicit Compiler(CompileContext& ctx, Allocator& allocator);
        explicit Compiler(CompileContext& ctx);
        virtual ~Compiler();

        Compiler(const Compiler&) = delete;
        Compiler& operator=(const Compiler&) = delete;

        void AddBuiltins();

        const Schema* Compile(FileId file);

    private:
        struct Impl;
        Impl* impl_ = nullptr;
        Allocator* allocator_ = nullptr;
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
