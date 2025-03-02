// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ir.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"

namespace schematic::compiler
{
    struct AstNode;

    class SchemaGenerator final
    {
    public:
        explicit SchemaGenerator(ArenaAllocator& arena, CompileContext& ctx) noexcept
            : arena_(arena)
            , ctx_(ctx)
        {
        }

        const Schema* Compile(IRSchema* irSchema);

    private:
        const Type* CreateType(IRType* type);
        Annotations CreateAnnotations(Array<IRAnnotation*> irAnnotations);
        const Value* CreateValue(IRValue* value);

        ArenaAllocator& arena_;
        CompileContext& ctx_;
        Schema* schema_ = nullptr;

        Array<Module, ModuleIndex> modules_;
        Array<Field, FieldIndex> fields_;
        Array<EnumItem, EnumItemIndex> enumItems_;
        Array<Annotation, AnnotationIndex> annotations_;
        Array<const Type*, TypeIndex> types_;
        Array<const Value*, ValueIndex> values_;
    };
} // namespace schematic::compiler
