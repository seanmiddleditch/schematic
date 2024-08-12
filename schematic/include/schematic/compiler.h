// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/schema.h"
#include "schematic/source.h"

#include <filesystem>

namespace potato::schematic
{
    struct LogLocation
    {
        const compiler::Source* source = nullptr;
        std::uint32_t offset = 0;
        std::uint32_t length = 1;
    };

    class CompileContext
    {
    public:
        virtual void Error(const LogLocation& location, std::string_view message) = 0;
        virtual const compiler::Source* LoadModule(const std::filesystem::path& filename) = 0;
        virtual const compiler::Source* ResolveModule(std::string_view name, const compiler::Source* referrer) = 0;

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

        const Schema* Compile(const std::filesystem::path& filename);

    private:
        struct Impl;
        Impl* impl_ = nullptr;
    };
} // namespace potato::schematic
