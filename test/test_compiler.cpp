// Schematic. Copyright (C) Sean Middleditch and contributors.

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

#define CompileTest(NAME) \
    ([&](auto name) { \
        const ModuleId moduleId = ctx.ResolveModule(name, ModuleId{}); \
        if (moduleId.value == ModuleId::InvalidValue) \
        { \
            INFO("File not found: " << name); \
            REQUIRE(false); \
        } \
        const Schema* const schema = compiler.Compile(moduleId); \
        REQUIRE(schema != nullptr); \
        REQUIRE(schema->root != nullptr); \
        return *schema; \
    }((NAME)));

TEST_CASE("Compiler", "[potato][schematic]")
{
    TestContext ctx;
    ArenaAllocator arena;
    Compiler compiler(ctx, arena);
    compiler.SetUseBuiltins(true);

    SECTION("Enum")
    {
        const Schema& schema = CompileTest("schemas/enum.sat");

        const TypeEnum* const color = CastTo<TypeEnum>(FindType(&schema, "color"));
        REQUIRE(color != nullptr);
        CHECK(color->kind == TypeKind::Enum);
        CHECK(color->items.size() == 3);

        const EnumItem* const green = FindItem(color, "green");

        CHECK_THAT(FindItem(color, "red"), IsValue<ValueInt>(0));
        CHECK_THAT(green, IsValue<ValueInt>(255));
        CHECK_THAT(FindItem(color, "blue"), IsValue<ValueInt>(256));

        const TypeStruct* const test = CastTo<TypeStruct>(FindType(&schema, "test"));
        CHECK_THAT(FindField(test, "c"), IsEnumValue(color, green));
    }

    SECTION("Struct")
    {
        const Schema& schema = CompileTest("schemas/struct.sat");

        CHECK(FindType(&schema, "unused") == nullptr);

        const Type* const base = FindType(&schema, "base");
        CHECK(base != nullptr);

        const TypeStruct* const test = CastTo<TypeStruct>(FindType(&schema, "test"));
        REQUIRE(test != nullptr);
        CHECK(test->kind == TypeKind::Struct);
        CHECK(test->base == base);
        CHECK(test->fields.size() == 4);

        CHECK_THAT(FindField(test, "num"), IsValue<ValueInt>(42));
        CHECK_THAT(FindField(test, "b"), IsValue<ValueBool>(true));
        CHECK_THAT(FindField(test, "zero"), IsValue<ValueFloat>(0.));
        CHECK_THAT(FindField(test, "thousand"), IsValue<ValueFloat>(1'000.0));
    }

    SECTION("Type Modifiers")
    {
        const Schema& schema = CompileTest("schemas/modifiers.sat");
    }

    SECTION("Initializers")
    {
        const Schema& schema = CompileTest("schemas/initializers.sat");

        const TypeStruct* const embed = CastTo<TypeStruct>(FindType(&schema, "embed"));
        REQUIRE(embed != nullptr);

        const TypeStruct* const test = CastTo<TypeStruct>(FindType(&schema, "test"));
        CHECK(test != nullptr);

        {
            const Field* const field = FindField(test, "first");
            REQUIRE(field != nullptr);
            CHECK(field->type == embed);

            const ValueObject* const value = CastTo<ValueObject>(field->value);
            REQUIRE(value != nullptr);
            REQUIRE(value->fields.size() == embed->fields.size());

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
            REQUIRE(value->fields.size() == embed->fields.size());

            CHECK(value->fields[0].field == &embed->fields[0]);
            CHECK_THAT(value->fields[0].value, IsValue<ValueInt>(4));

            CHECK(value->fields[1].field == &embed->fields[1]);
            CHECK_THAT(value->fields[1].value, IsValue<ValueInt>(5));
        }
    }

    SECTION("Attributes")
    {
        const Schema& schema = CompileTest("schemas/annotations.sat");

        CHECK(CastTo<TypeAttribute>(FindType(&schema, "Ignore")) != nullptr);
        CHECK(CastTo<TypeAttribute>(FindType(&schema, "Name")) != nullptr);
        CHECK(CastTo<TypeAttribute>(FindType(&schema, "More")) != nullptr);
        CHECK(CastTo<TypeAttribute>(FindType(&schema, "Reference")) != nullptr);

        const TypeStruct* const test = CastTo<TypeStruct>(FindType(&schema, "test"));
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

    SECTION("Messages")
    {
        const Schema& schema = CompileTest("schemas/message.sat");
    }
}
