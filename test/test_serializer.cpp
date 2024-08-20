// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "test_context.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"
#include "schematic/serialize.h"
#include "schematic/utility.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

using namespace potato::schematic;
using namespace potato::schematic::test;

static constexpr char imported_source[] = R"(
    attribute Attr {
        string name;
    }

    struct Base {
        int32 x = 0;
    }
)";

static constexpr char main_source[] = R"(
    import imported;

    [Attr("Derived")]
    struct Derived : Base {
        int16 y = 1;
    }
)";

TEST_CASE("Serialize", "[potato][schematic]")
{
    TestContext ctx;
    NewDeleteAllocator alloc;
    ArenaAllocator arena(alloc);
    google::protobuf::Arena pb_arena;
    Compiler compiler(ctx, alloc);
    compiler.SetUseBuiltins(true);

    ctx.AddFile("<main>", main_source);
    ctx.AddFile("imported", imported_source);
    REQUIRE(compiler.Compile(FileId{ 0 }));

    const Schema* const original = compiler.GetSchema();
    REQUIRE(original != nullptr);

    
    const proto::Schema* const proto = Serialize(pb_arena, original);
    REQUIRE(proto != nullptr);

    const Schema* const deserialized = Deserialize(arena, proto);
    REQUIRE(deserialized != nullptr);

    const proto::Schema* const proto2 = Serialize(pb_arena, deserialized);
    REQUIRE(proto2 != nullptr);

    CHECK(proto->ShortDebugString() == proto2->ShortDebugString());
}
