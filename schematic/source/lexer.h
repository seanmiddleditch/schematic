// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "array.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/compiler.h"

namespace schematic::compiler
{
    class Source;

    class Lexer final
    {
    public:
        Lexer(ArenaAllocator& arena, Logger& logger, std::string_view filename, std::string_view source) noexcept
            : arena_(arena)
            , logger_(logger)
            , filename_(filename)
            , source_(source)
        {
        }

        Array<Token> Tokenize();

    private:
        ArenaAllocator& arena_;
        Logger& logger_;
        std::string_view filename_;
        std::string_view source_;
        Array<Token> tokens_;
    };
} // namespace schematic::compiler
