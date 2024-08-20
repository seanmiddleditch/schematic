// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"
#include "schematic/serialize.h"

#include <fmt/core.h>
#include <fmt/std.h>

#include <filesystem>
#include <fstream>
#include <iostream>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "google/protobuf/util/json_util.h"

using namespace potato::schematic;

namespace
{
    enum class Command : unsigned char
    {
        Compile,
        Help
    };

    struct File final
    {
        std::filesystem::path filename;
        std::string name;
        std::string source;
    };

    struct MainContext final : potato::schematic::CompileContext
    {
        void Error(FileId file, const Range& range, std::string_view message) override;

        std::string_view ReadFileContents(FileId id) override;
        std::string_view GetFileName(FileId id) override;
        FileId ResolveModule(std::string_view name, FileId referrer) override;

        FileId TryLoadFile(const std::filesystem::path& filename);

        std::filesystem::path input;
        std::filesystem::path output;
        std::filesystem::path deps;
        std::vector<std::filesystem::path> search;
        Command command = Command::Compile;
        bool writeJson = false;

        std::vector<File> files;
    };
} // namespace

static bool ParseArguments(MainContext& ctx, std::span<char*> args);

int main(int argc, char** argv)
{
    MainContext ctx;

    if (!ParseArguments(ctx, std::span{ &argv[1], &argv[argc] }))
        return 1;

    const FileId root = ctx.TryLoadFile(ctx.input);

    NewDeleteAllocator alloc;
    Compiler compiler(ctx, alloc);
    compiler.SetUseBuiltins(true);
    if (!compiler.Compile(root))
        return 1;

    const Schema* const schema = compiler.GetSchema();
    if (schema == nullptr)
    {
        fmt::println(stderr, "Internal error: schema not created");
        return 1;
    }

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

        google::protobuf::Arena arena;
        const proto::Schema* const proto = Serialize(arena, schema);
        if (proto == nullptr)
        {
            fmt::println(stderr, "Internal error: serialization to protobuf failed");
            return 1;
        }

        if (ctx.writeJson)
        {
            google::protobuf::util::JsonPrintOptions options;
            options.add_whitespace = true;
            options.preserve_proto_field_names = true;
            std::string json;
            google::protobuf::util::MessageToJsonString(*proto, &json, options);
            out->write(json.data(), static_cast<std::streamsize>(json.size()));
        }
        else
        {
            if (!proto->SerializeToOstream(out))
            {
                fmt::println(stderr, "Internal error: serializing to output stream failed");
                return 1;
            }
        }

        out_file.close();
    }

    if (!ctx.deps.empty())
    {
        std::ofstream deps(ctx.deps);
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

            deps << ctx.files[index].filename.lexically_proximate(cwd);

            if (index != ctx.files.size() - 1)
                deps << " \\";
            deps << '\n';
        }
    }

    return 0;
}

static bool StartsWithOption(std::string_view arg, std::string_view option)
{
    return arg.starts_with(option) &&
        arg.size() > option.size() &&
        arg[option.size()] == '=';
}

bool ParseArguments(MainContext& ctx, std::span<char*> args)
{
    enum class NextArg : unsigned char
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
                ctx.search.emplace_back(arg);
                next = NextArg::Unknown;
                continue;
            case Output:
                ctx.output = arg;
                next = NextArg::Unknown;
                continue;
            case Dependency:
                ctx.deps = arg;
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
                ctx.search.emplace_back(arg.substr(2));
                continue;
            }

            if (arg == "-o")
            {
                next = NextArg::Output;
                continue;
            }
            if (arg.starts_with("-o"))
            {
                ctx.output = arg.substr(2);
                continue;
            }

            if (arg == "-MF")
            {
                next = NextArg::Dependency;
                continue;
            }
            if (arg.starts_with("-MF"))
            {
                ctx.deps = arg.substr(3);
                continue;
            }

            if (arg == "-Ojson")
            {
                ctx.writeJson = true;
                continue;
            }
            if (arg == "-Obin")
            {
                ctx.writeJson = false;
                continue;
            }

            fmt::println(stderr, "Unknown option: {}", arg);
            return false;
        }

        if (!ctx.input.empty())
        {
            fmt::println(stderr, "Too many input files: {}", arg);
            return false;
        }
        ctx.input = arg;
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

    if (ctx.input.empty())
    {
        fmt::println(stderr, "No input file provided");
        return false;
    }

    return true;
}

void MainContext::Error(FileId file, const Range& range, std::string_view message)
{
    if (file.value == FileId::InvalidValue)
    {
        fmt::println(stderr, "<unknown>: {}", message);
        return;
    }

    if (range.start.line == 0)
    {
        fmt::println(stderr, "{}: {}", files[file.value].filename, message);
        return;
    }

    fmt::println(stderr, "{}({},{}): {}", files[file.value].filename, range.start.line, range.start.column, message);
}

std::string_view MainContext::ReadFileContents(FileId id)
{
    if (id.value >= files.size())
        return {};

    return files[id.value].source;
}

std::string_view MainContext::GetFileName(FileId id)
{
    if (id.value >= files.size())
        return {};

    return files[id.value].name;
}

FileId MainContext::ResolveModule(std::string_view name, FileId referrer)
{
    std::filesystem::path filename;

    if (referrer.value < files.size())
        filename = files[referrer.value].filename.parent_path() / name;
    else
        filename = name;

    filename.replace_extension("sat");

    for (std::size_t i = 0; i != files.size(); ++i)
        if (files[i].filename == filename)
            return FileId{ i };

    if (const FileId file = TryLoadFile(filename); file.value != FileId::InvalidValue)
        return file;

    for (const std::filesystem::path& s : search)
    {
        filename = s / name;
        filename.replace_extension("sat");
        if (const FileId file = TryLoadFile(filename); file.value != FileId::InvalidValue)
            return file;
    }

    return {};
}

FileId MainContext::TryLoadFile(const std::filesystem::path& filename)
{
    std::ifstream input(filename);
    if (!input)
        return FileId{};

    File& file = files.emplace_back();
    file.filename = filename;
    file.name = file.filename.generic_string();
    file.source = std::string(std::istreambuf_iterator<char>(input), {});

    return FileId{ files.size() - 1 };
}
