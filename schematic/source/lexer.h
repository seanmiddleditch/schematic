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
        Lexer(CompileContext& ctx, ArenaAllocator& arena, ModuleId moduleId, std::string_view source) noexcept
            : ctx_(ctx)
            , arena_(arena)
            , moduleId_(moduleId)
            , source_(source)
        {
        }

        Array<Token> Tokenize();

    private:
        CompileContext& ctx_;
        ArenaAllocator& arena_;
        ModuleId moduleId_;
        std::string_view source_;
        Array<Token> tokens_;
    };
} // namespace potato::schematic::compiler
