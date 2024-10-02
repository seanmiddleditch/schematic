// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"

#include "ast.h"
#include "generator.h"
#include "ir_gen.h"
#include "lexer.h"
#include "location.h"
#include "parser.h"
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
    struct SchemaBuilder final
    {
        // CompileContext& ctx;
        ArenaAllocator& arena;

        void VisitTypes(const Module* mod, Array<const Type*>& visited);
        void VisitTypes(const Type* type, Array<const Type*>& visited);
        void VisitTypes(const Annotation* annotation, Array<const Type*>& visited);
        void VisitTypes(const Value* value, Array<const Type*>& visited);
        void VisitModules(const Module* mod, Array<const Module*>& visited);
    };

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
    CompilerState state;
    Generator generator(arena, logger, ctx, state);

    const Module* const root = generator.Compile(filename, source);
    if (root == nullptr)
        return nullptr;

    IRState state2;
    IRGenerator irGenerator(arena, logger, ctx, state2, filename, source);

    const IRModule* const irRoot = irGenerator.Compile();
    if (irRoot == nullptr)
        return nullptr;

    Schema* const schema = arena.New<Schema>();
    schema->root = root;

    SchemaBuilder builder{ .arena = arena };

    {
        Array<const Type*> visited;
        builder.VisitTypes(schema->root, visited);
        schema->types = visited;
    }

    {
        Array<const Module*> visited;
        builder.VisitModules(schema->root, visited);
        schema->modules = visited;
    }

    return schema;
}

void SchemaBuilder::VisitTypes(const Module* mod, Array<const Type*>& visited)
{
    if (mod == nullptr)
        return;

    for (const Type* const type : mod->types)
        VisitTypes(type, visited);
}

void SchemaBuilder::VisitTypes(const Type* type, Array<const Type*>& visited)
{
    if (type == nullptr)
        return;

    for (const Type* const exists : visited)
        if (exists == type)
            return;

    for (const Annotation* const annotation : type->annotations)
        VisitTypes(annotation, visited);

    if (const TypeStruct* const struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
    {
        VisitTypes(struct_->base, visited);

        for (const Field& field : struct_->fields)
        {
            VisitTypes(field.type, visited);
            VisitTypes(field.value, visited);

            for (const Annotation* const annotation : field.annotations)
                VisitTypes(annotation, visited);
        }
    }
    else if (const TypeMessage* const message = CastTo<TypeMessage>(type); message != nullptr)
    {
        for (const Field& field : message->fields)
        {
            VisitTypes(field.type, visited);
            VisitTypes(field.value, visited);

            for (const Annotation* const annotation : field.annotations)
                VisitTypes(annotation, visited);
        }
    }
    else if (const TypeAttribute* const attr = CastTo<TypeAttribute>(type); attr != nullptr)
    {
        for (const Field& field : attr->fields)
        {
            VisitTypes(field.type, visited);
            VisitTypes(field.value, visited);

            for (const Annotation* const annotation : field.annotations)
                VisitTypes(annotation, visited);
        }
    }
    else if (const TypeEnum* const enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
    {
        VisitTypes(enum_->base, visited);

        for (const EnumItem& item : enum_->items)
        {
            for (const Annotation* const annotation : item.annotations)
                VisitTypes(annotation, visited);
        }
    }
    else if (const TypePointer* const pointer = CastTo<TypePointer>(type); pointer != nullptr)
    {
        VisitTypes(pointer->type, visited);
    }
    else if (const TypeArray* const array = CastTo<TypeArray>(type); array != nullptr)
    {
        VisitTypes(array->type, visited);
    }

    visited.PushBack(arena, type);
}

void SchemaBuilder::VisitTypes(const Annotation* annotation, Array<const Type*>& visited)
{
    if (annotation == nullptr)
        return;

    VisitTypes(annotation->attribute, visited);

    for (const Argument& arg : annotation->arguments)
        VisitTypes(arg.value, visited);
}

void SchemaBuilder::VisitTypes(const Value* value, Array<const Type*>& visited)
{
    if (value == nullptr)
        return;

    if (const ValueType* const type = CastTo<ValueType>(value); type != nullptr)
        VisitTypes(type->type, visited);
}

void SchemaBuilder::VisitModules(const Module* mod, Array<const Module*>& visited)
{
    if (mod == nullptr)
        return;

    for (const Module* const visitedMod : visited)
        if (mod == visitedMod)
            return;

    visited.PushBack(arena, mod);

    for (const Module* const imp : mod->imports)
        VisitModules(imp, visited);
}
