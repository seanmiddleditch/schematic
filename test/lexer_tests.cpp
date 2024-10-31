// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "lexer.h"
#include "test_matchers.h"
#include "test_strings.h"

#include "schematic/compiler.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

using namespace schematic;
using namespace schematic::compiler;
using namespace schematic::test;

TEST_CASE("Lexer", "[schematic]")
{
    SECTION("Numbers")
    {
        CHECK_THAT("123", IsTokenType(TokenType::Integer));

        CHECK_THAT("0x1", IsTokenType(TokenType::HexInteger));
        CHECK_THAT("0xf", IsTokenType(TokenType::HexInteger));

        CHECK_THAT("0b101", IsTokenType(TokenType::BinaryInteger));

        CHECK_THAT("0.", IsTokenType(TokenType::Float));
        CHECK_THAT(".0", IsTokenType(TokenType::Float));
        CHECK_THAT("0.0", IsTokenType(TokenType::Float));
        CHECK_THAT("0.0e1", IsTokenType(TokenType::Float));
        CHECK_THAT("0.0e-1", IsTokenType(TokenType::Float));

        CHECK_THAT("00", IsLexError("<test>(1): Leading zeroes are not permitted"));
        CHECK_THAT("01234", IsLexError("<test>(1): Leading zeroes are not permitted"));
    }

    SECTION("Symbols")
    {
        CHECK_THAT("-", IsTokenType(TokenType::Minus));
        CHECK_THAT(".", IsTokenType(TokenType::Dot));
        CHECK_THAT(";", IsTokenType(TokenType::SemiColon));
        CHECK_THAT("*", IsTokenType(TokenType::Star));
        CHECK_THAT("?", IsTokenType(TokenType::Question));
        CHECK_THAT("=", IsTokenType(TokenType::Equals));
    }

    SECTION("Strings")
    {
        CHECK_THAT(R"("Hello World!")", IsTokenType(TokenType::String));
        CHECK_THAT(R"("Hello World!)", IsLexError("<test>(1): Unterminated string"));
        CHECK_THAT("\"Hello World!\n\"", IsLexError("<test>(1): Unterminated string"));
        CHECK_THAT("\\L", IsLexError("<test>(1): Unexpected input `\\`"));

        CHECK_THAT(R"("""Hello
World!""")",
            IsTokenType(TokenType::MultilineString));
        CHECK_THAT(R"("""
Hello World!)",
            IsLexError("<test>(1): Unterminated long string"));
    }

    SECTION("Comments")
    {
        ArenaAllocator arena;
        TestContext ctx;

        Lexer lexer(arena, ctx, "<test>", R"--(
        // this is a comment

        // and another comment
)--");
        Array<Token> tokens;
        REQUIRE(!lexer.Tokenize().IsEmpty());

        CHECK_THAT("/", IsLexError("<test>(1): Unexpected input `/`"));
    }
}
