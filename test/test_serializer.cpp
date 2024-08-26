// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "test_context.h"

#include "schematic/compiler.h"
#include "schematic/protobuf.h"
#include "schematic/schema.h"
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

    message Message {
        float f @3;
    }
)";

TEST_CASE("Serialize", "[potato][schematic]")
{
    TestContext ctx;
    ArenaAllocator arena;
    google::protobuf::Arena pb_arena;
    Compiler compiler(ctx, arena);
    compiler.SetUseBuiltins(true);

    ctx.AddFile("<main>", main_source);
    ctx.AddFile("imported", imported_source);
    const Schema* const original = compiler.Compile(ModuleId{ 0 });
    REQUIRE(original != nullptr);

    const proto::Schema* const proto = SerializeSchemaProto(pb_arena, original);
    REQUIRE(proto != nullptr);

    const Schema* const deserialized = ParseSchemaProto(arena, proto);
    REQUIRE(deserialized != nullptr);

    const proto::Schema* const proto2 = SerializeSchemaProto(pb_arena, deserialized);
    REQUIRE(proto2 != nullptr);

    CHECK(proto->ShortDebugString() == proto2->ShortDebugString());
}

TEST_CASE("ParseError", "[potato][schematic]")
{
    TestContext ctx;
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

    CHECK(ParseSchemaProto(arena, proto) == nullptr);
}
