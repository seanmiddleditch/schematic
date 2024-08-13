// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "arena.h"
#include "token.h"

#include "schematic/compiler.h"

namespace potato::schematic::compiler
{
    class Source;

    class Lexer final
    {
    public:
        Lexer(CompileContext& ctx, ArenaAllocator& alloc, FileId file) noexcept
            : ctx_(ctx)
            , alloc_(alloc)
            , file_(file)
        {
        }

        Array<Token> Tokenize();

    private:
        CompileContext& ctx_;
        ArenaAllocator& alloc_;
        FileId file_;
        Array<Token> tokens_;
    };
} // namespace potato::schematic::compiler
