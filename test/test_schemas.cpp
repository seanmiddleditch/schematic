// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "embed_tests.h"
#include "test_context.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"

#include <string>
#include <vector>

using namespace potato::schematic;
using namespace potato::schematic::compiler;
using namespace potato::schematic::test;

TEST_CASE("Schemas", "[potato][schematic]")
{
    TestContext ctx;
    ArenaAllocator arena;
    Compiler compiler(ctx, arena);
    compiler.SetUseBuiltins(true);

    for (std::size_t i = 0; i != test_embeds_count; ++i)
    {
        const EmbeddedTest& test = test_embeds[i];
        DYNAMIC_SECTION(test.name)
        {
            std::vector<std::string> expected_errors;

            std::istringstream source(test.source);
            std::string line;
            while (std::getline(source, line))
            {
                constexpr char prefix[] = "ERROR: ";
                const auto error_pos = line.find(prefix);
                if (error_pos != std::string::npos)
                    expected_errors.push_back(line.substr(error_pos + std::strlen(prefix)));
            }

            const Schema* const schema = compiler.Compile(ModuleId{ i });

            if (expected_errors.empty())
            {
                REQUIRE(schema != nullptr);
                REQUIRE(schema->root != nullptr);
            }
            else
            {
                CHECK(schema == nullptr);

                std::size_t next = 0;
                for (const std::string& error : ctx.errors)
                {
                    if (next < expected_errors.size())
                    {
                        const std::string& expected = expected_errors[next];
                        if (expected == error)
                            ++next;
                        else
                            FAIL_CHECK(error);
                    }
                    else
                    {
                        FAIL_CHECK(error);
                    }
                }
                CHECK(next == expected_errors.size());
            }
        }
    }
}
