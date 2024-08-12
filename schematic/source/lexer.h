// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "arena.h"
#include "token.h"

namespace potato::schematic
{
    class CompileContext;
}

namespace potato::schematic::compiler
{
    class Source;

    bool Tokenize(CompileContext& ctx, ArenaAllocator& alloc, const Source* source, Array<Token>& tokens);
} // namespace potato::schematic::compiler
