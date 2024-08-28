// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "embed_tests.h"
#include "test_context.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"

using namespace potato::schematic;
using namespace potato::schematic::compiler;
using namespace potato::schematic::test;

TEST_CASE("Schemas", "[potato][schematic]")
{
    TestContext ctx;
    ctx.AddEmbeds();
    ArenaAllocator arena;
    Compiler compiler(ctx, arena);
    compiler.SetUseBuiltins(true);

    for (std::size_t i = 0; i != test_embeds_count; ++i)
    {
        DYNAMIC_SECTION("schema: " << test_embeds[i].name)
        {
            const ModuleId moduleId = ctx.ResolveModule(test_embeds[i].name, ModuleId{});
            if (moduleId.value == ModuleId::InvalidValue)
            {
                INFO("File not found: " << test_embeds[i].name);
                REQUIRE(false);
            }
            const Schema* const schema = compiler.Compile(moduleId);
            REQUIRE(schema != nullptr);
            REQUIRE(schema->root != nullptr);
        }
    }
}
