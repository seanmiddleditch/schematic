// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"

#include "ast.h"
#include "generator.h"
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

namespace potato::schematic::compiler
{
    struct SchemaBuilder
    {
        CompileContext& ctx;
        ArenaAllocator& arena;

        void VisitTypes(const Module* mod, Array<const Type*>& visited);
        void VisitTypes(const Type* type, Array<const Type*>& visited);
        void VisitTypes(const Annotation* annotation, Array<const Type*>& visited);
        void VisitTypes(const Value* value, Array<const Type*>& visited);
        void VisitModules(const Module* mod, Array<const Module*>& visited);
    };
} // namespace potato::schematic::compiler

potato::schematic::Compiler::Compiler(CompileContext& ctx, ArenaAllocator& arena)
    : ctx_(ctx)
    , arena_(arena)
{
}

void potato::schematic::Compiler::SetUseBuiltins(bool useBuiltins)
{
    useBuiltins_ = useBuiltins;
}

const Schema* potato::schematic::Compiler::Compile(ModuleId moduleId)
{
    if (generator_ == nullptr)
        generator_ = arena_.New<Generator>(ctx_, arena_);

    const Module* const root = generator_->Compile(moduleId, useBuiltins_);
    if (root == nullptr)
        return nullptr;

    Schema* const schema = arena_.New<Schema>();
    schema->root = root;

    SchemaBuilder builder{ .ctx = ctx_, .arena = arena_ };

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

void potato::schematic::compiler::SchemaBuilder::VisitTypes(const Module* mod, Array<const Type*>& visited)
{
    if (mod == nullptr)
        return;

    for (const Type* const type : mod->types)
        VisitTypes(type, visited);
}

void potato::schematic::compiler::SchemaBuilder::VisitTypes(const Type* type, Array<const Type*>& visited)
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

void potato::schematic::compiler::SchemaBuilder::VisitTypes(const Annotation* annotation, Array<const Type*>& visited)
{
    if (annotation == nullptr)
        return;

    VisitTypes(annotation->attribute, visited);

    for (const Argument& arg : annotation->arguments)
        VisitTypes(arg.value, visited);
}

void potato::schematic::compiler::SchemaBuilder::VisitTypes(const Value* value, Array<const Type*>& visited)
{
    if (value == nullptr)
        return;

    if (const ValueType* const type = CastTo<ValueType>(value); type != nullptr)
        VisitTypes(type->type, visited);
}

void potato::schematic::compiler::SchemaBuilder::VisitModules(const Module* mod, Array<const Module*>& visited)
{
    if (mod == nullptr)
        return;

    visited.PushBack(arena, mod);

    for (const Module* const imp : mod->imports)
        VisitModules(imp, visited);
}
