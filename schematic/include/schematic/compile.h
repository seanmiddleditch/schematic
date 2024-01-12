// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/arena.h"
#include "schematic/logger.h"
#include "schematic/schema.h"

namespace potato::schematic::compiler
{
    class Resolver;
    class Source;

    struct CompileOptions
    {
        bool builtins = true;
    };

    const Module* Compile(Logger& logger, Resolver& resolver, ArenaAllocator& alloc, const Source* source, const CompileOptions& options);
} // namespace potato::schematic
