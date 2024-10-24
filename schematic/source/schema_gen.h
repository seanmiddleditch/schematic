// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ir.h"

#include "schematic/logger.h"
#include "schematic/schema.h"

namespace potato::schematic::compiler
{
    struct AstNode;

    class SchemaGenerator final
    {
    public:
        explicit SchemaGenerator(ArenaAllocator& arena, Logger& logger) noexcept
            : arena_(arena)
            , logger_(logger)
        {
        }

        const Schema* Compile(IRSchema* irSchema);

    private:
        const Type* CreateType(IRType* type);
        Annotations CreateAnnotations(Array<IRAnnotation*> irAnnotations);

        const Value* Resolve(IRValue* value);
        ValueIndex ResolveIndex(IRValue* value);

        ArenaAllocator& arena_;
        Logger& logger_;
        Schema* schema_ = nullptr;

        Array<Module, ModuleIndex> modules_;
        Array<Field, FieldIndex> fields_;
        Array<EnumItem, EnumItemIndex> enumItems_;
        Array<Annotation, AnnotationIndex> annotations_;
        Array<const Type*, TypeIndex> types_;
        Array<const Value*, ValueIndex> values_;
    };
} // namespace potato::schematic::compiler
