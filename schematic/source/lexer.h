// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "token.h"

#include "schematic/arena.h"

namespace potato::schematic::compiler
{
    class Logger;
    class Source;

    bool Tokenize(Logger& logger, ArenaAllocator& alloc, const Source* source, Array<Token>& tokens);
} // namespace potato::schematic::compiler
