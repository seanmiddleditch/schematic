// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "embed_tests.h"
#include "location.h"

#include "schematic/compiler.h"

#include <catch2/catch_test_macros.hpp>
#include <fmt/core.h>

#include <span>
#include <string>
#include <vector>

namespace schematic::test
{
    struct TestContext final : schematic::CompileContext
    {
        struct ExpectedError
        {
            std::string message;
            bool encounted = false;
        };

        inline std::string_view ReadFileContents(ArenaAllocator& arena, std::string_view filename) override;
        inline std::string_view ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer) override;
        inline void LogMessage(const LogLocation& location, std::string_view message) override;

        std::vector<ExpectedError> expectedErrors;
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

    void TestContext::LogMessage(const LogLocation& location, std::string_view message)
    {
        // line prefix
        std::string prefix;
        if (!location.file.empty())
            fmt::format_to(std::back_inserter(prefix), "{}", location.file);
        if (location.line != 0)
            fmt::format_to(std::back_inserter(prefix), "({})", location.line);
        prefix.append(": ");

        std::string buffer;
        buffer.append(prefix);
        buffer.append(message);

        for (auto& expected : expectedErrors)
        {
            if (expected.encounted)
                continue;

            if (expected.message == buffer)
            {
                expected.encounted = true;
                return;
            }
        }

        if (!reportErrors)
            return;

        // find source for error, so we can pretty-print source
        std::string_view source;
        for (const EmbeddedTest& test : std::span{ test_embeds, test_embeds_count })
        {
            if (test.name == location.file)
            {
                source = test.source;
                break;
            }
        }

        std::string_view line = schematic::compiler::ExtractLine(source, location.line);
        if (!line.empty())
        {
            buffer.push_back('\n');
            buffer.append(prefix);
            buffer.append(line);

            buffer.push_back('\n');
            buffer.append(prefix.size(), ' ');
            unsigned col = 0;
            for (const char c : line)
            {
                ++col;
                if (col >= location.column)
                {
                    if (col >= location.column + location.length)
                        break;
                    buffer.push_back('^');
                }
                else if (c == '\n')
                {
                    break;
                }
                else if (std::isspace(c))
                {
                    buffer.push_back(c);
                }
                else
                {
                    buffer.push_back(' ');
                }
            }
        }

        FAIL_CHECK(buffer);
    }
} // namespace schematic::test
