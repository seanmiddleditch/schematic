// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "embed_tests.h"
#include "location.h"

#include "schematic/compiler.h"

#include <catch2/catch_test_macros.hpp>
#include <fmt/core.h>

#include <string>
#include <vector>

namespace potato::schematic::test
{
    struct TestContext final : potato::schematic::CompileContext
    {
        inline void Error(ModuleId moduleId, const Range& range, std::string_view message) override;

        inline std::string_view ReadFileContents(ModuleId id) override;
        inline std::string_view GetFileName(ModuleId id) override;
        inline ModuleId ResolveModule(std::string_view name, ModuleId referrer) override;

        std::vector<std::string> errors;
        bool reportErrors = true;
    };

    void TestContext::Error(ModuleId moduleId, const Range& range, std::string_view message)
    {
        if (moduleId.value == ModuleId::InvalidValue)
        {
            errors.emplace_back(message);
            if (reportErrors)
                UNSCOPED_INFO(message);
            return;
        }

        std::string_view source = ReadFileContents(moduleId);
        std::string buffer;
        fmt::format_to(std::back_inserter(buffer), "{}({}): ", GetFileName(moduleId), range.start.line);
        const auto prefix = buffer.size();
        buffer.append(message);
        errors.push_back(buffer);

        // everything after this is for logging full error information
        if (!reportErrors)
            return;

        UNSCOPED_INFO(buffer);

        std::string_view line = potato::schematic::compiler::ExtractLine(source, range.start.line);
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

    std::string_view TestContext::ReadFileContents(ModuleId id)
    {
        if (id.value >= test_embeds_count)
            return {};

        return test_embeds[id.value].source;
    }

    std::string_view TestContext::GetFileName(ModuleId id)
    {
        if (id.value >= test_embeds_count)
            return {};

        return test_embeds[id.value].name;
    }

    ModuleId TestContext::ResolveModule(std::string_view name, ModuleId referrer)
    {
        const EmbeddedTest& refTest = test_embeds[referrer.value];

        if (referrer.value != ModuleId::InvalidValue)
        {
            std::string relative = refTest.name;
            if (const auto sep = relative.rfind('/'); sep != std::string::npos)
            {
                relative.resize(sep + 1);
                relative += name;
            }

            for (std::size_t i = 0; i != test_embeds_count; ++i)
                if (test_embeds[i].name == relative)
                    return ModuleId{ i };
        }

        for (std::size_t i = 0; i != test_embeds_count; ++i)
            if (test_embeds[i].name == name)
                return ModuleId{ i };

        return ModuleId{};
    }
} // namespace potato::schematic::test
