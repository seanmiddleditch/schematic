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
        ModuleIndex CreateModule(IRModule* irModule);

        Type* Resolve(IRType* type);
        TypeIndex ResolveIndex(IRType* type);

        void CreateType(IRType* type);
        ReadOnlySpan<const Annotation*> CreateAnnotations(Array<IRAnnotation*> irAnnotations);

        Value* Resolve(IRValue* value);
        ValueIndex ResolveIndex(IRValue* value);

        ArenaAllocator& arena_;
        Logger& logger_;
        Schema* schema_ = nullptr;

        Array<Module, ModuleIndex> modules_;
        Array<Field, FieldIndex> fields_;
        Array<EnumItem, EnumItemIndex> enumItems_;
        Array<Type*, TypeIndex> types_;
        Array<Value*, ValueIndex> values_;
    };
} // namespace potato::schematic::compiler
