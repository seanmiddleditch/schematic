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

        const Schema* Compile(IRModule* module);

    private:
        Type* Resolve(IRType* type);
        std::uint32_t LineOf(const AstNode* node);

        ArenaAllocator& arena_;
        Logger& logger_;
        IRModule* module_ = nullptr;
        Schema* schema_ = nullptr;

        Array<Type*> types_;
    };
} // namespace potato::schematic::compiler
