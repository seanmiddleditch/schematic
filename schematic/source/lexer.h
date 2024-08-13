// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "arena.h"
#include "token.h"

namespace potato::schematic
{
    class CompileContext;
    struct FileId;
}

namespace potato::schematic::compiler
{
    class Source;

    bool Tokenize(CompileContext& ctx, ArenaAllocator& alloc, FileId file, Array<Token>& tokens);
} // namespace potato::schematic::compiler
