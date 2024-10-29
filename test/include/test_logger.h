// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "embed_tests.h"
#include "location.h"

#include "schematic/logger.h"

#include <catch2/catch_test_macros.hpp>
#include <fmt/core.h>

#include <span>
#include <string>
#include <vector>

namespace schematic::test
{
    struct TestLogger final : schematic::Logger
    {
        struct ExpectedError
        {
            std::string message;
            bool encounted = false;
        };

        inline void Error(std::string_view filename, const Range& range, std::string_view message) override;

        std::vector<ExpectedError> expectedErrors;
        std::vector<std::string> errors;
        bool reportErrors = true;
    };

    void TestLogger::Error(std::string_view filename, const Range& range, std::string_view message)
    {
        // line prefix
        std::string prefix;
        if (!filename.empty())
            fmt::format_to(std::back_inserter(prefix), "{}({}): ", filename, range.start.line);

        std::string buffer;
        buffer.append(prefix);
        buffer.append(message);
        errors.push_back(buffer);

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
            if (test.name == filename)
            {
                source = test.source;
                break;
            }
        }

        std::string_view line = schematic::compiler::ExtractLine(source, range.start.line);
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
        }

        FAIL_CHECK(buffer);
    }
} // namespace schematic::test
