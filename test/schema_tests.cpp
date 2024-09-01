// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "embed_tests.h"
#include "evaluator.h"
#include "test_context.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"

#include <sstream>
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
            std::vector<CheckEvaluator> checks;

            {
                std::istringstream source(test.source);
                std::string line;
                std::size_t number = 0;
                while (std::getline(source, line))
                {
                    ++number;

                    constexpr char error_prefix[] = "ERROR: ";
                    const auto error_pos = line.find(error_prefix);
                    if (error_pos != std::string::npos)
                        expected_errors.push_back(line.substr(error_pos + std::strlen(error_prefix)));

                    constexpr char check_prefix[] = "CHECK: ";
                    const auto check_pos = line.find(check_prefix);
                    if (check_pos != std::string::npos)
                        checks.emplace_back(line.substr(check_pos + std::strlen(check_prefix)), test.name, number);
                }
            }

            if (!expected_errors.empty())
                ctx.reportErrors = false;

            const Schema* const schema = compiler.Compile(test.name);

            if (!checks.empty())
            {
                REQUIRE(schema != nullptr);
                REQUIRE(schema->root != nullptr);

                for (CheckEvaluator& check : checks)
                    check.Check(schema);
            }

            if (!expected_errors.empty())
            {
                std::size_t next = 0;
                for (const std::string& error : ctx.errors)
                {
                    if (next < expected_errors.size())
                    {
                        const std::string& expected = expected_errors[next];
                        if (expected == error)
                        {
                            ++next;
                            continue;
                        }
                    }
                    FAIL_CHECK(error);
                }

                if (next != expected_errors.size())
                {
                    std::ostringstream buffer;
                    while (next != expected_errors.size())
                        buffer << expected_errors[next++] << '\n';
                    FAIL_CHECK("Expected errors were NOT encountered:\n"
                        << buffer.str());
                }
            }
        }
    }
}
