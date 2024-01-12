// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "token.h"

#include "schematic/arena.h"
#include "schematic/source.h"

#include <span>

namespace potato::schematic::compiler
{
    struct AstNodeModule;
    struct AstNodeImport;
    class Logger;

    class ParseContext
    {
    public:
        virtual ~ParseContext() = default;

        virtual const AstNodeModule* LoadImport(const AstNodeImport& imp) = 0;
    };

    const AstNodeModule* Parse(ParseContext& ctx, Logger& logger, ArenaAllocator& alloc, const Source* source, std::span<const Token> tokens);
} // namespace potato::schematic::compiler
