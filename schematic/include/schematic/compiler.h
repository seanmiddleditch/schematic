// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/arena.h"
#include "schematic/logger.h"
#include "schematic/schema.h"

#include <filesystem>

namespace potato::schematic
{
    class Compiler
    {
    public:
        Compiler();
        virtual ~Compiler();

        Compiler(const Compiler&) = delete;
        Compiler& operator=(const Compiler&) = delete;

        void AddBuiltins();

        const Schema* Compile(const std::filesystem::path& filename);

    protected:
        virtual void Error(const compiler::LogLocation& location, std::string_view message) = 0;
        virtual const compiler::Source* LoadModule(const std::filesystem::path& filename) = 0;
        virtual const compiler::Source* ResolveModule(std::string_view name, const compiler::Source* referrer) = 0;

    private:
        struct Impl;
        Impl* impl_ = nullptr;
    };
} // namespace potato::schematic::compiler
