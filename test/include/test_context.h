// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "embed_tests.h"

#include "schematic/compiler.h"

#include <span>
#include <string>

namespace potato::schematic::test
{
    struct TestContext final : potato::schematic::CompileContext
    {
        inline std::string_view ReadFileContents(ArenaAllocator& arena, std::string_view filename) override;
        inline std::string_view ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer) override;

        bool reportErrors = true;
    };

    std::string_view TestContext::ReadFileContents(ArenaAllocator& arena, std::string_view filename)
    {
        for (const EmbeddedTest& test : std::span{ test_embeds, test_embeds_count })
        {
            if (test.name == filename)
                return test.source;
        }
        return {};
    }

    std::string_view TestContext::ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer)
    {
        if (!referrer.empty())
        {
            std::string relative{ referrer };
            if (const auto sep = relative.rfind('/'); sep != std::string::npos)
            {
                relative.resize(sep + 1);
                relative += name;
            }

            for (std::size_t i = 0; i != test_embeds_count; ++i)
                if (test_embeds[i].name == relative)
                    return test_embeds[i].name;
        }

        for (std::size_t i = 0; i != test_embeds_count; ++i)
            if (test_embeds[i].name == name)
                return test_embeds[i].name;

        return {};
    }
} // namespace potato::schematic::test
