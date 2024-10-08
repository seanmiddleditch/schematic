// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"
#include "schematic/logger.h"
#include "schematic/protobuf.h"
#include "schematic/schema.h"

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
        std::string_view ReadFileContents(ArenaAllocator& arena, std::string_view filename) override;
        std::string_view ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer) override;

        const File* TryLoadFile(const std::filesystem::path& filename);

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
        return 3;

    const File* const root = ctx.TryLoadFile(ctx.input);
    if (root == nullptr)
    {
        fmt::println(stderr, "Cannot open input file: {}", ctx.input);
        return 1;
    }

    NewDeleteAllocator alloc;
    ArenaAllocator arena(alloc);
    const Schema* const schema = Compile(arena, Logger::Default(), ctx, root->name, root->source);
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
                return 3;
            }

            out = &out_file;
        }

        google::protobuf::Arena arena;
        const proto::Schema* const proto = SerializeSchemaProto(arena, schema);
        if (proto == nullptr)
        {
            fmt::println(stderr, "Internal error: serialization to protobuf failed");
            return 2;
        }

        if (ctx.writeJson)
        {
            google::protobuf::util::JsonPrintOptions options;
            options.add_whitespace = true;
            options.always_print_primitive_fields = true;
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
                return 2;
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
            return 3;
        }

        const std::filesystem::path cwd = std::filesystem::current_path();
        deps << ctx.output.lexically_proximate(cwd).generic_string() << ": ";

        for (size_t index = 0; index != ctx.files.size(); ++index)
        {
            if (index != 0)
                deps << "  ";

            deps << ctx.files[index].filename.lexically_proximate(cwd).generic_string();

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

std::string_view MainContext::ReadFileContents(ArenaAllocator& arena, std::string_view filename)
{
    for (const File& file : files)
    {
        if (file.filename == filename)
            return file.source;
    }
    return {};
}

std::string_view MainContext::ResolveModule(ArenaAllocator& arena, std::string_view name, std::string_view referrer)
{
    std::filesystem::path filename = name;

    if (!referrer.empty())
        filename = std::filesystem::path{ referrer }.parent_path() / filename;

    for (std::size_t i = 0; i != files.size(); ++i)
        if (files[i].filename == filename)
            return arena.NewString(files[i].name);

    if (const File* const file = TryLoadFile(filename); file != nullptr)
        return file->name;

    for (const std::filesystem::path& s : search)
    {
        filename = s / name;
        if (const File* const file = TryLoadFile(filename); file != nullptr)
            return arena.NewString(file->name);
    }

    return {};
}

const File* MainContext::TryLoadFile(const std::filesystem::path& filename)
{
    std::ifstream input(filename);
    if (!input)
        return nullptr;

    File& file = files.emplace_back();
    file.filename = filename;
    file.name = file.filename.generic_string();
    file.source = std::string(std::istreambuf_iterator<char>(input), {});

    return &file;
}
