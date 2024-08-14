// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/compiler.h"

#include <catch2/catch_test_macros.hpp>
#include <fmt/core.h>

#include "location.h"

namespace potato::schematic::test
{
    struct TestSource final
    {
        TestSource(std::string name, std::string data) noexcept
            : name(std::move(name))
            , data(std::move(data))
        {
        }

        std::string name;
        std::string data;
    };

    struct TestContext final : potato::schematic::CompileContext
    {
        inline void Error(FileId file, const Range& range, std::string_view message) override;

        inline std::string_view ReadFileContents(FileId id) override;
        inline std::string_view GetFileName(FileId id) override;
        inline FileId ResolveModule(std::string_view name, FileId referrer) override;

        inline void AddFile(std::string name, std::string source);

        std::vector<TestSource> files;
    };

    void TestContext::Error(FileId file, const Range& range, std::string_view message)
    {
        UNSCOPED_INFO(message);
        if (file.value == FileId::InvalidValue)
            return;

        std::string_view source = ReadFileContents(file);
        std::string_view line = potato::schematic::compiler::ExtractLine(source, range.start.line);
        std::string buffer;
        fmt::format_to(std::back_inserter(buffer), "{}({}): ", GetFileName(file), range.start.line);
        const auto prefix = buffer.size();
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

    std::string_view TestContext::ReadFileContents(FileId id)
    {
        if (id.value >= files.size())
            return {};

        return files[id.value].data;
    }

    std::string_view TestContext::GetFileName(FileId id)
    {
        if (id.value >= files.size())
            return {};

        return files[id.value].name;
    }

    FileId TestContext::ResolveModule(std::string_view name, FileId referrer)
    {
        for (std::size_t i = 0; i != files.size(); ++i)
            if (files[i].name == name)
                return FileId{ i };
        return FileId{};
    }

    void TestContext::AddFile(std::string name, std::string source)
    {
        files.emplace_back(std::move(name), std::move(source));
    }
} // namespace potato::schematic::test
