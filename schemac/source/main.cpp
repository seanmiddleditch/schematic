// Schematic. Copyright (C) Sean Middleditch and contributors.

#define _SILENCE_STDEXT_ARR_ITERS_DEPRECATION_WARNING

#include "schematic/compile.h"
#include "schematic/logger.h"
#include "schematic/resolver.h"
#include "schematic/serialize.h"
#include "schematic/source.h"

#include <fmt/core.h>
#include <fmt/std.h>

#include <filesystem>
#include <fstream>
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
//  - Serialization of schema
//

namespace
{
    enum class Command
    {
        Compile,
        Help
    };

    enum class OutputType
    {
        Binary,
        Json
    };

    struct File final : Source
    {
        std::filesystem::path filename;
        std::string name;
        std::string source;

        std::string_view Name() const noexcept override { return name; }
        std::string_view Data() const noexcept override { return source; }
    };

    struct State
    {
        const File* ResolveModuleFile(std::string_view name, const std::filesystem::path& dir);
        const File* LoadFile(const std::filesystem::path& filename);

        std::filesystem::path input;
        std::filesystem::path output;
        std::filesystem::path deps;
        std::vector<std::filesystem::path> search;
        Command command = Command::Compile;
        OutputType outputType = OutputType::Json;

        std::vector<std::unique_ptr<File>> files;
    };

    struct MainLogger final : potato::schematic::compiler::Logger
    {
        explicit MainLogger(State& state)
            : state(state)
        {
        }

        void Error(const LogLocation& location, std::string_view message) override;

        State& state;
    };

    struct MainResolver final : potato::schematic::compiler::Resolver
    {
        explicit MainResolver(State& state)
            : state(state)
        {
        }

        const Source* ResolveModule(std::string_view name, const Source* referrer) override;

        State& state;
    };
} // namespace

static bool ParseArguments(State& state, std::span<char*> args);

int main(int argc, char** argv)
{
    State state;

    if (!ParseArguments(state, std::span{ &argv[1], &argv[argc] }))
        return 1;

    fmt::println("Input={}", state.input);
    fmt::println("Output={}", state.output);

    state.search = std::move(state.search);

    MainLogger logger(state);
    MainResolver resolver(state);

    const Source* const source = state.LoadFile(state.input);
    if (source == nullptr)
    {
        fmt::println(stderr, "Cannot open input file: {}", state.input);
        return 1;
    }

    ArenaAllocator alloc;
    CompileOptions options;
    const Module* const mod = Compile(logger, resolver, alloc, source, options);
    if (mod == nullptr)
        return 1;

    switch (state.outputType)
    {
        case OutputType::Binary:
        {
            const std::vector<std::byte> serialized = SerializeBinary(*mod);

            std::ofstream out(state.output, std::ios_base::out | std::ios_base::binary);
            if (!out)
            {
                fmt::println(stderr, "Cannot open output file: {}", state.output);
                return 1;
            }

            out.write(reinterpret_cast<const char*>(serialized.data()), serialized.size());
            break;
        }
        case OutputType::Json:
        {
            const std::string serialized = SerializeJson(*mod);

            std::ofstream out(state.output, std::ios_base::out);
            if (!out)
            {
                fmt::println(stderr, "Cannot open output file: {}", state.output);
                return 1;
            }

            out.write(serialized.data(), serialized.size());
            break;
        }
    }

    if (!state.deps.empty())
    {
        std::ofstream deps(state.deps);
        if (!deps)
        {
            fmt::println(stderr, "Cannot open deps file: {}", state.deps);
            return 1;
        }

        deps << state.output.relative_path() << ": ";

        for (size_t index = 0; index != state.files.size(); ++index)
        {
            if (index != 0)
                deps << "  ";

            deps << state.files[index]->filename.relative_path();

            if (index != state.files.size() - 1)
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

bool ParseArguments(State& state, std::span<char*> args)
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
                state.search.push_back(arg);
                next = NextArg::Unknown;
                continue;
            case Output:
                state.output = arg;
                next = NextArg::Unknown;
                continue;
            case Dependency:
                state.deps = arg;
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
                state.search.emplace_back(arg.substr(2));
                continue;
            }

            if (arg == "-o")
            {
                next = NextArg::Output;
                continue;
            }
            if (arg.starts_with("-o"))
            {
                state.output = arg.substr(2);
                continue;
            }

            if (arg == "-MF")
            {
                next = NextArg::Dependency;
                continue;
            }
            if (arg.starts_with("-MF"))
            {
                state.deps = arg.substr(3);
                continue;
            }

            if (arg == "-Ojson")
            {
                state.outputType = OutputType::Json;
                continue;
            }
            if (arg == "-Obin")
            {
                state.outputType = OutputType::Binary;
                continue;
            }

            fmt::println(stderr, "Unknown option: {}", arg);
            return false;
        }

        if (!state.input.empty())
        {
            fmt::println(stderr, "Too many input files: {}", arg);
            return false;
        }
        state.input = arg;
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

    if (state.input.empty())
    {
        fmt::println(stderr, "No input file provided");
        return false;
    }

    if (state.output.empty())
    {
        fmt::println(stderr, "No output file provided");
        return false;
    }

    return true;
}

void MainLogger::Error(const LogLocation& location, std::string_view message)
{
    if (location.source == nullptr)
    {
        fmt::println(stderr, "<unknown>: {}", message);
        return;
    }

    const SourceLocation loc = location.source->OffsetToLocation(location.offset);
    fmt::println(stderr, "{}({},{}): {}", location.source->Name(), loc.line, loc.column, message);
}

const File* State::ResolveModuleFile(std::string_view name, const std::filesystem::path& dir)
{
    std::filesystem::path filename;

    filename = dir / name;
    filename.replace_extension("sat");

    for (const std::unique_ptr<File>& file : files)
        if (file->filename == filename)
            return file.get();

    if (const File* const file = LoadFile(filename); file != nullptr)
        return file;

    for (const std::filesystem::path& s : search)
    {
        filename = s / name;
        filename.replace_extension("sat");
        if (const File* const file = LoadFile(filename); file != nullptr)
            return file;
    }

    return nullptr;
}

const File* State::LoadFile(const std::filesystem::path& filename)
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

const Source* MainResolver::ResolveModule(std::string_view name, const Source* referrer)
{
    const File* const file = static_cast<const File*>(referrer);
    if (file == nullptr)
        return {};

    const std::filesystem::path dir = file->filename.parent_path();

    return state.ResolveModuleFile(name, dir);
}
