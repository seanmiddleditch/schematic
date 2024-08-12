// Schematic. Copyright (C) Sean Middleditch and contributors.

#define _SILENCE_STDEXT_ARR_ITERS_DEPRECATION_WARNING

#include "schematic/compiler.h"
#include "schematic/serialize.h"
#include "schematic/source.h"

#include <fmt/core.h>
#include <fmt/std.h>

#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <vector>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

// TODO:
//
//  - Abstract types? With validation
//
//  - Lexer FIXME
//

namespace
{
    enum class Command
    {
        Compile,
        Help
    };

    struct File final : Source
    {
        std::filesystem::path filename;
        std::string name;
        std::string source;

        std::string_view Name() const noexcept override { return name; }
        std::string_view Data() const noexcept override { return source; }
    };

    struct MainContext final : potato::schematic::CompileContext
    {
        void Error(const LogLocation& location, std::string_view message) override;
        const File* LoadModule(const std::filesystem::path& filename) override;
        const File* ResolveModule(std::string_view name, const Source* referrer) override;

        const File* ResolveModuleFile(std::string_view name, const std::filesystem::path& dir);

        std::filesystem::path input;
        std::filesystem::path output;
        std::filesystem::path deps;
        std::vector<std::filesystem::path> search;
        Command command = Command::Compile;
        bool writeJson = false;

        std::vector<std::unique_ptr<File>> files;
    };
} // namespace

static bool ParseArguments(MainContext& ctx, std::span<char*> args);

int main(int argc, char** argv)
{
    MainContext ctx;

    if (!ParseArguments(ctx, std::span{ &argv[1], &argv[argc] }))
        return 1;

    Compiler compiler(ctx);
    const Schema* const schema = compiler.Compile(ctx.input);
    if (schema == nullptr)
        return 1;

    {
        std::ostream* out = &std::cout;

        std::ofstream out_file;
        if (!ctx.output.empty())
        {
            auto ios_flags = std::ios_base::out;
            if (!ctx.writeJson)
                ios_flags |= std::ios_base::binary;

            out_file.open(ctx.output, ios_flags);
            if (!out_file)
            {
                fmt::println(stderr, "Cannot open output file: {}", ctx.output);
                return 1;
            }

            out = &out_file;
        }

        if (ctx.writeJson)
        {
            const std::string serialized = SerializeJson(*schema);
            out->write(serialized.data(), serialized.size());
        }
        else
        {
            const std::vector<char> serialized = SerializeBinary(*schema);
            out->write(serialized.data(), serialized.size());
        }

        out_file.close();
    }

    if (!ctx.deps.empty())
    {
        std::ofstream deps(compiler.deps);
        if (!deps)
        {
            fmt::println(stderr, "Cannot open deps file: {}", ctx.deps);
            return 1;
        }

        const std::filesystem::path cwd = std::filesystem::current_path();
        deps << ctx.output.lexically_proximate(cwd) << ": ";

        for (size_t index = 0; index != ctx.files.size(); ++index)
        {
            if (index != 0)
                deps << "  ";

            deps << ctx.files[index]->filename.lexically_proximate(cwd);

            if (index != ctx.files.size() - 1)
                deps << " \\";
            deps << '\n';
        }
    }

    return 0;
}

static bool StartsWithOption(std::string_view arg, std::string option)
{
    return arg.starts_with(option) &&
        arg.size() > option.size() &&
        arg[option.size()] == '=';
}

bool ParseArguments(MainContext& ctx, std::span<char*> args)
{
    enum class NextArg
    {
        Unknown,
        Search,
        Output,
        Dependency,
    } next = NextArg::Unknown;
    bool allowFlags = true;

    for (const std::string_view arg : args)
    {
        switch (next)
        {
            using enum NextArg;
            case Unknown:
                break;
            case Search:
                compiler.search.push_back(arg);
                next = NextArg::Unknown;
                continue;
            case Output:
                compiler.output = arg;
                next = NextArg::Unknown;
                continue;
            case Dependency:
                compiler.deps = arg;
                next = NextArg::Unknown;
                continue;
        }

        if (allowFlags && arg.starts_with("-"))
        {
            if (arg == "--")
            {
                allowFlags = false;
                continue;
            }

            if (arg == "-I")
            {
                next = NextArg::Search;
                continue;
            }
            if (arg.starts_with("-I"))
            {
                compiler.search.emplace_back(arg.substr(2));
                continue;
            }

            if (arg == "-o")
            {
                next = NextArg::Output;
                continue;
            }
            if (arg.starts_with("-o"))
            {
                compiler.output = arg.substr(2);
                continue;
            }

            if (arg == "-MF")
            {
                next = NextArg::Dependency;
                continue;
            }
            if (arg.starts_with("-MF"))
            {
                compiler.deps = arg.substr(3);
                continue;
            }

            if (arg == "-Ojson")
            {
                compiler.writeJson = true;
                continue;
            }
            if (arg == "-Obin")
            {
                compiler.writeJson = false;
                continue;
            }

            fmt::println(stderr, "Unknown option: {}", arg);
            return false;
        }

        if (!compiler.input.empty())
        {
            fmt::println(stderr, "Too many input files: {}", arg);
            return false;
        }
        compiler.input = arg;
    }

    switch (next)
    {
        using enum NextArg;
        case Unknown:
            break;
        case Search:
            fmt::println(stderr, "Expected path after -I");
            return false;
        case Output:
            fmt::println(stderr, "Expected path after -o");
            return false;
        case Dependency:
            fmt::println(stderr, "Expected path after -MF");
            return false;
    }

    if (compiler.input.empty())
    {
        fmt::println(stderr, "No input file provided");
        return false;
    }

    return true;
}

void MainContext::Error(const LogLocation& location, std::string_view message)
{
    if (location.source == nullptr)
    {
        fmt::println(stderr, "<unknown>: {}", message);
        return;
    }

    const SourceLocation loc = location.source->OffsetToLocation(location.offset);
    fmt::println(stderr, "{}({},{}): {}", location.source->Name(), loc.line, loc.column, message);
}

const File* MainContext::ResolveModuleFile(std::string_view name, const std::filesystem::path& dir)
{
    std::filesystem::path filename;

    filename = dir / name;
    filename.replace_extension("sat");

    for (const std::unique_ptr<File>& file : files)
        if (file->filename == filename)
            return file.get();

    if (const File* const file = LoadModule(filename); file != nullptr)
        return file;

    for (const std::filesystem::path& s : search)
    {
        filename = s / name;
        filename.replace_extension("sat");
        if (const File* const file = LoadModule(filename); file != nullptr)
            return file;
    }

    return nullptr;
}

const File* MainContext::LoadModule(const std::filesystem::path& filename)
{
    std::ifstream input(filename);
    if (!input)
        return nullptr;

    File* const file = files.emplace_back(std::make_unique<File>()).get();
    file->filename = filename;
    file->name = file->filename.generic_string();

    file->source = std::string(std::istreambuf_iterator<char>(input), {});

    return file;
}

const File* MainContext::ResolveModule(std::string_view name, const Source* referrer)
{
    const File* const file = static_cast<const File*>(referrer);
    if (file == nullptr)
        return {};

    const std::filesystem::path dir = file->filename.parent_path();

    return ResolveModuleFile(name, dir);
}
