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

namespace potato::schematic::test
{
    struct TestContext final : potato::schematic::CompileContext
    {
        inline void Error(std::string_view filename, const Range& range, std::string_view message) override;

        inline std::string_view ReadFileContents(ArenaAllocator& arena, std::string_view filename) override;
        inline std::string_view ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer) override;

        std::vector<std::string> errors;
        bool reportErrors = true;
    };

    void TestContext::Error(std::string_view filename, const Range& range, std::string_view message)
    {
        if (filename.empty())
        {
            errors.emplace_back(message);
            if (reportErrors)
                UNSCOPED_INFO(message);
            return;
        }

        std::string_view source;
        for (const EmbeddedTest& test : std::span{ test_embeds, test_embeds_count })
        {
            if (test.name == filename)
            {
                source = test.source;
                break;
            }
        }

        std::string buffer;
        fmt::format_to(std::back_inserter(buffer), "{}({}): ", filename, range.start.line);
        const auto prefix = buffer.size();
        buffer.append(message);
        errors.push_back(buffer);

        // everything after this is for logging full error information
        if (!reportErrors)
            return;

        UNSCOPED_INFO(buffer);

        std::string_view line = potato::schematic::compiler::ExtractLine(source, range.start.line);
        if (line.empty())
            return;

        buffer.resize(prefix);
        buffer.append(line);
        UNSCOPED_INFO(buffer);

        buffer.clear();
        buffer.append(prefix, ' ');
        unsigned col = 0;
        for (const char c : line)
        {
            ++col;
            if (col >= range.start.column)
            {
                if (range.start.line == range.end.line && col >= range.end.column)
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
        UNSCOPED_INFO(buffer);
    }

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
