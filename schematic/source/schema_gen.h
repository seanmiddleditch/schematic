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
        Type* Resolve(IRType* type);
        void CreateType(IRType* type);
        Span<Annotation*> CreateAnnotations(Array<IRAnnotation*> irAnnotations);

        Value* Resolve(IRValue* value);

        std::uint32_t LineOf(const AstNode* node);

        ArenaAllocator& arena_;
        Logger& logger_;
        Schema* schema_ = nullptr;

        Array<Module*> modules_;
        Array<Type*> types_;
    };
} // namespace potato::schematic::compiler
