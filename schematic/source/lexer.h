// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "array.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/compiler.h"

namespace potato::schematic::compiler
{
    class Source;

    class Lexer final
    {
    public:
        Lexer(CompileContext& ctx, ArenaAllocator& arena, ModuleId moduleId) noexcept
            : ctx_(ctx)
            , arena_(arena)
            , moduleId_(moduleId)
        {
        }

        Array<Token> Tokenize();

    private:
        CompileContext& ctx_;
        ArenaAllocator& arena_;
        ModuleId moduleId_;
        Array<Token> tokens_;
    };
} // namespace potato::schematic::compiler
