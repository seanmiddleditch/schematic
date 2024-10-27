// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "embed_tests.h"
#include "evaluator.h"
#include "test_context.h"
#include "test_logger.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"

#include <sstream>
#include <string>
#include <vector>

using namespace potato::schematic;
using namespace potato::schematic::test;

void TestSchema(const char* filename)
{
    TestContext ctx;
    TestLogger logger;
    ArenaAllocator arena;

    const EmbeddedTest* test = nullptr;
    for (std::size_t i = 0; i != test_embeds_count; ++i)
    {
        if (std::strcmp(test_embeds[i].name, filename) == 0)
        {
            test = &test_embeds[i];
            break;
        }
    }
    REQUIRE(test != nullptr);

    std::vector<CheckEvaluator> checks;

    {
        std::istringstream source(test->source);
        std::string line;
        std::size_t number = 0;
        while (std::getline(source, line))
        {
            ++number;

            constexpr char error_prefix[] = "ERROR: ";
            const auto error_pos = line.find(error_prefix);
            if (error_pos != std::string::npos)
                logger.expectedErrors.push_back({ line.substr(error_pos + std::strlen(error_prefix)) });

            constexpr char check_prefix[] = "CHECK: ";
            const auto check_pos = line.find(check_prefix);
            if (check_pos != std::string::npos)
                checks.emplace_back(line.substr(check_pos + std::strlen(check_prefix)), test->name, number);
        }
    }

    Compiler* compiler = NewCompiler(arena, logger, ctx);
    compiler->AddStandardPreamble();
    compiler->AddPreamble("schemas/include/preamble_impl.sat");
    const Schema* const schema = compiler->Compile(test->name);

    if (!checks.empty())
    {
        REQUIRE(schema != nullptr);
        REQUIRE(schema->root < schema->modules.Size());

        for (CheckEvaluator& check : checks)
            check.Check(schema);
    }

    for (const auto& expected : logger.expectedErrors)
    {
        if (expected.encounted)
            continue;

        FAIL_CHECK("Expected error NOT encountered:\n"
            << expected.message);
    }
}
