// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "lexer.h"
#include "test_context.h"
#include "test_matchers.h"
#include "test_strings.h"

#include "schematic/compiler.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

using namespace potato::schematic;
using namespace potato::schematic::compiler;
using namespace potato::schematic::test;

TEST_CASE("Lexer", "[potato][schematic]")
{
    TestContext ctx;
    ArenaAllocator arena;

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

        CHECK_THAT("00", IsLexError());
        CHECK_THAT("01234", IsLexError());
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
        CHECK_THAT(R"("Hello World!)", IsLexError());
        CHECK_THAT("\"Hello World!\n\"", IsLexError());
        CHECK_THAT("\\L", IsLexError());

        CHECK_THAT(R"("""Hello
World!""")",
            IsTokenType(TokenType::MultilineString));
        CHECK_THAT(R"("""Hello World!)", IsLexError());
    }

    SECTION("Comments")
    {
        Lexer lexer(ctx, arena, "<test>", R"--(
        // this is a comment

        // and another comment
)--");
        Array<Token> tokens;
        REQUIRE(!lexer.Tokenize().IsEmpty());

        CHECK_THAT("/", IsLexError());
    }
}
