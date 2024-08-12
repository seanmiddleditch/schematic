// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/logger.h"
#include "schematic/schema.h"

#include <filesystem>

namespace potato::schematic
{
    class CompileContext : public potato::schematic::compiler::Logger
    {
    public:
        virtual void Error(const compiler::LogLocation& location, std::string_view message) = 0;
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
