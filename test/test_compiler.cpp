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
}
