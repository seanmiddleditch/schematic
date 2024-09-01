// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "test_context.h"
#include "test_logger.h"

#include "schematic/compiler.h"
#include "schematic/protobuf.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

using namespace potato::schematic;
using namespace potato::schematic::test;

TEST_CASE("Serialize", "[potato][schematic]")
{
    TestContext ctx;
    TestLogger logger;
    ArenaAllocator arena;
    google::protobuf::Arena pb_arena;

    for (std::size_t i = 0; i != test_embeds_count; ++i)
    {
        const EmbeddedTest& test = test_embeds[i];

        // can't serialize tests that won't compile
        if (std::strstr(test.source, "ERROR: ") != nullptr)
            continue;

        DYNAMIC_SECTION(test.name)
        {
            const Schema* const original = Compile(arena, logger, ctx, test.name, test.source);
            REQUIRE(original != nullptr);

            const proto::Schema* const proto = SerializeSchemaProto(pb_arena, original);
            REQUIRE(proto != nullptr);

            const Schema* const deserialized = ParseSchemaProto(arena, logger, proto);
            REQUIRE(deserialized != nullptr);

            const proto::Schema* const proto2 = SerializeSchemaProto(pb_arena, deserialized);
            REQUIRE(proto2 != nullptr);

            CHECK(proto->ShortDebugString() == proto2->ShortDebugString());
        }
    }
}

TEST_CASE("ParseSchemaProto Error", "[potato][schematic]")
{
    TestLogger logger;
    ArenaAllocator arena;
    google::protobuf::Arena pb_arena;

    TypeInt i32;
    i32.name = "i32";
    i32.width = 32;
    i32.isSigned = true;
    TypeEnum bad;
    bad.name = "bad";
    bad.base = &i32;
    Module root;
    root.filename = "<root>";
    const Type* types[] = { &i32, &bad };
    root.types = types;
    const Module* modules[] = { &root };
    Schema schema;
    schema.root = &root;
    schema.modules = modules;
    schema.types = types;

    const proto::Schema* const proto = SerializeSchemaProto(pb_arena, &schema);
    REQUIRE(proto != nullptr);

    CHECK(ParseSchemaProto(arena, logger, proto) == nullptr);
}
