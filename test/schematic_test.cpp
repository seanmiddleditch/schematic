// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic_test.h"

#include "schematic/compile.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

#include "../source/lexer.h"

using namespace potato::schematic;
using namespace potato::schematic::compiler;
using namespace potato::schematic::test;

TEST_CASE("Compiler", "[potato][schematic]")
{
    ArenaAllocator alloc;
    TestResolver resolver;
    TestLogger logger;

    auto CompileTest = [&alloc, &logger, &resolver](std::string_view name) -> const Schema&
    {
        const Source* const source = resolver.ResolveModule(name, nullptr);
        REQUIRE(source != nullptr);
        const Schema* const schema = Compile(logger, resolver, alloc, source, CompileOptions{});
        REQUIRE(schema != nullptr);
        REQUIRE(schema->root != nullptr);
        return *schema;
    };

    SECTION("Lexer")
    {
        Array<Token> tokens;
        const TestSource source("<test>", R"--(
        // this is a comment

        // and another comment
)--");

        REQUIRE(Tokenize(logger, alloc, &source, tokens));

        CHECK_THAT("123", IsTokenType(TokenType::Integer));

        CHECK_THAT("0.", IsTokenType(TokenType::Float));
        CHECK_THAT(".0", IsTokenType(TokenType::Float));
        CHECK_THAT("0.0", IsTokenType(TokenType::Float));
        CHECK_THAT("0.0e1", IsTokenType(TokenType::Float));
        CHECK_THAT("0.0e-1", IsTokenType(TokenType::Float));

        CHECK_THAT("00", IsLexError());
        CHECK_THAT("01234", IsLexError());

        CHECK_THAT(".", IsTokenType(TokenType::Dot));
        CHECK_THAT(";", IsTokenType(TokenType::SemiColon));

        CHECK_THAT(R"("Hello World!")", IsTokenType(TokenType::String));
        CHECK_THAT(R"("Hello World!)", IsLexError());
        CHECK_THAT("\"Hello World!\n\"", IsLexError());
        CHECK_THAT("\\L", IsLexError());

        CHECK_THAT(R"("""Hello
World!""")",
            IsTokenType(TokenType::MultilineString));
        CHECK_THAT(R"("""Hello World!)", IsLexError());

        CHECK_THAT("/", IsLexError());
    }

    SECTION("Enum")
    {
        resolver.AddFile("enum", R"--(
        enum color
        {
            red,
            green = 0xff,
            blue,
        }

        struct test
        {
            color c = color.green;
        }
)--");
        const Schema& schema = CompileTest("enum");

        const TypeEnum* const color = CastTo<TypeEnum>(FindType(&schema, "color"));
        REQUIRE(color != nullptr);
        CHECK(color->kind == TypeKind::Enum);
        CHECK(color->items.Size() == 3);

        const EnumItem* const green = FindItem(color, "green");

        CHECK_THAT(FindItem(color, "red"), IsValue<ValueInt>(0));
        CHECK_THAT(green, IsValue<ValueInt>(255));
        CHECK_THAT(FindItem(color, "blue"), IsValue<ValueInt>(256));

        const TypeAggregate* const test = CastTo<TypeAggregate>(FindType(&schema, "test"));
        CHECK_THAT(FindField(test, "c"), IsEnumValue(color, green));
    }

    SECTION("Struct")
    {
        resolver.AddFile("main", R"--(
        import imported;

        struct test : base
        {
            int32 num = 42;
            bool b = true;
            float zero = .0;
            float thousand = 1.0e3;
        }
)--");
        resolver.AddFile("imported", R"--(
        struct unused {}
        struct base {}
)--");
        const Schema& schema = CompileTest("main");

        CHECK(FindType(&schema, "unused") == nullptr);

        const Type* const base = FindType(&schema, "base");
        CHECK(base != nullptr);

        const TypeAggregate* const test = CastTo<TypeAggregate>(FindType(&schema, "test"));
        REQUIRE(test != nullptr);
        CHECK(test->kind == TypeKind::Aggregate);
        CHECK(test->base == base);
        CHECK(test->fields.Size() == 4);

        CHECK_THAT(FindField(test, "num"), IsValue<ValueInt>(42));
        CHECK_THAT(FindField(test, "b"), IsValue<ValueBool>(true));
        CHECK_THAT(FindField(test, "zero"), IsValue<ValueFloat>(0.));
        CHECK_THAT(FindField(test, "thousand"), IsValue<ValueFloat>(1'000.0));
    }

    SECTION("Type Modifiers")
    {
        resolver.AddFile("type_modifiers", R"--(
        struct test
        {
            int32[] num = { 1, 2, 3 };
            string? optional = null;
            string* required = "abc";
        }
)--");
        const Schema& schema = CompileTest("type_modifiers");
    }

    SECTION("Initializers")
    {
        resolver.AddFile("initializer", R"--(
        struct embed
        {
            int32 a;
            int32 b;
            int32 c;
        }

        struct test
        {
            embed first = embed{ 1, 2, c = -0x3, };
            embed second = { 4, 5, 6 };
        }
)--");
        const Schema& schema = CompileTest("initializer");

        const TypeAggregate* const embed = CastTo<TypeAggregate>(FindType(&schema, "embed"));
        REQUIRE(embed != nullptr);

        const TypeAggregate* const test = CastTo<TypeAggregate>(FindType(&schema, "test"));
        CHECK(test != nullptr);

        {
            const Field* const field = FindField(test, "first");
            REQUIRE(field != nullptr);
            CHECK(field->type == embed);

            const ValueObject* const value = CastTo<ValueObject>(field->value);
            REQUIRE(value != nullptr);
            REQUIRE(value->fields.Size() == embed->fields.Size());

            CHECK(value->fields[0].field == &embed->fields[0]);
            CHECK_THAT(value->fields[0].value, IsValue<ValueInt>(1));

            CHECK(value->fields[2].field == &embed->fields[2]);
            CHECK_THAT(value->fields[2].value, IsValue<ValueInt>(-3));
        }

        {
            const Field* const field = FindField(test, "second");
            REQUIRE(field != nullptr);
            CHECK(field->type == embed);

            const ValueObject* const value = CastTo<ValueObject>(field->value);
            REQUIRE(value != nullptr);
            REQUIRE(value->fields.Size() == embed->fields.Size());

            CHECK(value->fields[0].field == &embed->fields[0]);
            CHECK_THAT(value->fields[0].value, IsValue<ValueInt>(4));

            CHECK(value->fields[1].field == &embed->fields[1]);
            CHECK_THAT(value->fields[1].value, IsValue<ValueInt>(5));
        }
    }

    SECTION("Attributes")
    {
        resolver.AddFile("annotations", R"--(
        attribute Ignore;
        attribute Name { string first; int32 second; int32 third = 7; }
        attribute More {}
        attribute Reference { $type type; }

        [Ignore, Name("Toby", second = -2) ]
        [More()]
        struct test
        {
            [Ignore] int32 field;
        }

        [Reference(test)]
        enum enum
        {
            [Ignore] item
        }
)--");
        const Schema& schema = CompileTest("annotations");

        CHECK(CastTo<TypeAttribute>(FindType(&schema, "Ignore")) != nullptr);
        CHECK(CastTo<TypeAttribute>(FindType(&schema, "Name")) != nullptr);
        CHECK(CastTo<TypeAttribute>(FindType(&schema, "More")) != nullptr);
        CHECK(CastTo<TypeAttribute>(FindType(&schema, "Reference")) != nullptr);

        const TypeAggregate* const test = CastTo<TypeAggregate>(FindType(&schema, "test"));
        CHECK(HasAttribute(test, "More"));

        const Field* const field = FindField(test, "field");
        CHECK(HasAttribute(field, "Ignore"));

        const Annotation* const name = FindAnnotation(test, "Name");
        CHECK(name != nullptr);

        const Value* const first = FindArgument(name, "first");
        CHECK_THAT(first, IsStringValue("Toby"));

        const Value* const second = FindArgument(name, "second");
        CHECK_THAT(second, IsValue<ValueInt>(-2));

        const Value* const third = FindArgument(name, "third");
        CHECK_THAT(third, IsValue<ValueInt>(7));
    }
}
