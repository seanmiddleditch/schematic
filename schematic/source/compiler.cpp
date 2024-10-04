// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"

#include "ast.h"
#include "ir_gen.h"
#include "lexer.h"
#include "location.h"
#include "parser.h"
#include "schema_gen.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <fmt/core.h>

#include <charconv>
#include <climits>
#include <cstdint>
#include <utility>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

namespace
{
    struct DefaultLogger final : Logger
    {
        void Error(std::string_view filename, const Range& range, std::string_view message) override
        {
            if (filename.empty())
            {
                fmt::println(stderr, "{}", message);
                return;
            }

            if (range.start.line == 0)
            {
                fmt::println(stderr, "{}: {}", filename, message);
                return;
            }

            if (range.start.column == 0)
            {
                fmt::println(stderr, "{}({}): {}", filename, range.start.line, message);
                return;
            }

            fmt::println(stderr, "{}({},{}): {}", filename, range.start.line, range.start.column, message);
        }
    };
} // namespace

Logger& Logger::Default() noexcept
{
    static DefaultLogger logger;
    return logger;
}

const Schema* potato::schematic::Compile(ArenaAllocator& arena, Logger& logger, CompileContext& ctx, std::string_view filename, std::string_view source)
{
    IRState state;
    IRGenerator irGen(arena, logger, ctx, state, filename, source);

    IRModule* const irRoot = irGen.Compile();
    if (irRoot == nullptr)
        return nullptr;

    SchemaGenerator schemaGen(arena, logger);
    const Schema* const schema = schemaGen.Compile(irRoot);

    return schema;
}
