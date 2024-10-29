// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "lexer.h"
#include "test_strings.h"
#include "token.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

#include <span>

namespace schematic::test
{
    struct IsTokenType : Catch::Matchers::MatcherGenericBase
    {
        IsTokenType(schematic::compiler::TokenType type)
            : type_(type)
        {
        }

        bool match(const char* text) const
        {
            using namespace schematic::compiler;

            TestContext ctx;
            ArenaAllocator arena;

            Lexer lexer(arena, ctx, "<test>", text);
            Array<Token> tokens = lexer.Tokenize();

            if (tokens.IsEmpty())
                UNSCOPED_INFO("Tokenize failed");

            const TokenType actual = tokens.IsEmpty() ? TokenType::Unknown : tokens.Front().type;
            if (actual != type_)
                UNSCOPED_INFO("Expected " << ToCStr(type_) << " got " << ToCStr(actual));

            return !tokens.IsEmpty() &&
                tokens.Size() == 2 &&
                tokens.Front().offset == 0 &&
                tokens.Front().length == std::strlen(text) &&
                tokens.Front().type == type_;
        }

        std::string describe() const override
        {
            return fmt::format("== {}", schematic::compiler::ToCStr(type_));
        }

    private:
        schematic::compiler::TokenType type_;
    };

    struct IsLexError : Catch::Matchers::MatcherGenericBase
    {
        bool match(const char* text) const
        {
            using namespace schematic::compiler;

            TestContext ctx;
            ctx.reportErrors = false;
            ArenaAllocator arena;
            Array<Token> tokens;

            Lexer lexer(arena, ctx, "<test>", text);
            tokens = lexer.Tokenize();
            return !tokens.IsEmpty();
        }

        std::string describe() const override
        {
            return "IsLexError";
        }
    };

    struct IsExactSequence : Catch::Matchers::MatcherGenericBase
    {
        explicit IsExactSequence(std::initializer_list<const uint8_t> expected)
            : expected_(expected)
        {
        }

        bool match(Buffer value) const
        {
            using namespace schematic::compiler;

            return value.size() == expected_.size() && std::memcmp(value.data(), expected_.data(), value.size()) == 0;
        }

        std::string describe() const override
        {
            return "== " + Catch::StringMaker<Buffer>{}.convert(Buffer{ expected_ });
        }

    private:
        std::span<const uint8_t> expected_;
    };
} // namespace schematic::test
