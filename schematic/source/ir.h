// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "array.h"
#include "ast.h"

#include "schematic/allocator.h"
#include "schematic/schema.h"

#include <cstdint>

namespace potato::schematic::compiler
{
    enum class IRTypeKind : std::uint8_t
    {
        None,
        Alias,
        Attribute,
        Enum,
        Message,
        Struct,
        StructVersioned,

        Builtin,

        IndirectArray,
        IndirectIdentifier,
        IndirectNullable,
        IndirectPointer,
    };

    struct IRImport;
    struct IRModule;

    struct IRVersionRange;

    struct IRAttributeField;
    struct IREnumItem;
    struct IRMessageField;
    struct IRStructField;

    struct IRType;
    struct IRTypeAlias;
    struct IRTypeAttribute;
    struct IRTypeBuiltin;
    struct IRTypeEnum;
    struct IRTypeIndirect;
    struct IRTypeMessage;
    struct IRTypeStructVersioned;
    struct IRTypeStruct;

    struct IRImport;

    struct IRType
    {
        IRTypeKind kind = IRTypeKind::None;
        const AstNode* ast = nullptr;
        Type* type = nullptr;
        const char* name = nullptr;
        // FIXME: annotations

        template <typename T>
        friend T* CastTo(IRType* ir) noexcept
        {
            if (ir != nullptr && ir->kind == T::Kind)
                return static_cast<T*>(ir);
            return nullptr;
        }
    };

    struct IRTypeIndirect : IRType
    {
    };

    struct IRVersionRange
    {
        std::uint64_t min = 0;
        std::uint64_t max = 0;
    };

    struct IRAttributeField
    {
        const char* name = nullptr;
        IRType* type = nullptr;
        const AstNodeField* ast = nullptr;
        // FIXME: value
        // FIXME: annotations
    };

    struct IREnumItem
    {
        const char* name = nullptr;
        const AstNodeEnumItem* ast = nullptr;
        EnumItem* item = nullptr;
        // FIXME: value
        // FIXME: annotations
    };

    struct IRMessageField
    {
        const char* name = nullptr;
        IRType* type = nullptr;
        const AstNodeField* ast = nullptr;
        Field* item = nullptr;
        std::uint32_t proto = 0;
        // FIXME: default value
        // FIXME: annotations
    };

    struct IRStructField
    {
        const char* name = nullptr;
        IRType* type = nullptr;
        const AstNodeField* ast = nullptr;
        Field* item = nullptr;
        IRVersionRange version;
        // FIXME: default value
        // FIXME: annotations
    };

#define IR_TYPE(TYPE, KIND) \
    static constexpr IRTypeKind Kind = (KIND); \
    constexpr TYPE() noexcept { kind = Kind; }

    struct IRTypeAlias : IRType
    {
        IR_TYPE(IRTypeAlias, IRTypeKind::Alias);

        IRType* target = nullptr;
    };

    struct IRTypeBuiltin : IRType
    {
        IR_TYPE(IRTypeBuiltin, IRTypeKind::Builtin);

        // Must be Int, Float, Bool, Null, or String
        TypeKind typeKind = TypeKind::Int;

        // Used for Int and Float types only
        std::uint32_t width = 8;

        // Used for Int types only
        bool isSigned = false;
    };

    struct IRTypeAttribute : IRType
    {
        IR_TYPE(IRTypeAttribute, IRTypeKind::Attribute);

        Array<IRAttributeField*> fields;
    };

    struct IRTypeEnum : IRType
    {
        IR_TYPE(IRTypeEnum, IRTypeKind::Enum);

        IRType* base = nullptr;
        Array<IREnumItem*> items;
    };

    struct IRTypeMessage : IRType
    {
        IR_TYPE(IRTypeMessage, IRTypeKind::Message);

        Array<IRMessageField*> fields;
    };

    struct IRTypeStruct : IRType
    {
        IR_TYPE(IRTypeStruct, IRTypeKind::Struct);

        IRType* base = nullptr;
        Array<IRStructField*> fields;
        IRVersionRange version;
    };

    struct IRTypeStructVersioned : IRType
    {
        IR_TYPE(IRTypeStructVersioned, IRTypeKind::StructVersioned);

        IRTypeStruct* latest = nullptr;
        Array<IRTypeStruct*> versions;
    };

    struct IRTypeIndirectArray : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectArray, IRTypeKind::IndirectArray);

        const AstNodeTypeArray* ast = nullptr;
        IRType* target = nullptr;
        Type* type = nullptr;
        std::uint32_t size = 0; // zero means unsized
    };

    struct IRTypeIndirectIdentifier : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectIdentifier, IRTypeKind::IndirectIdentifier);

        const AstNodeIdentifier* ast = nullptr;
        const char* name = nullptr;
        Type* type = nullptr;
    };

    struct IRTypeIndirectNullable : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectNullable, IRTypeKind::IndirectNullable);

        const AstNodeTypeNullable* ast = nullptr;
        IRType* target = nullptr;
        Type* type = nullptr;
    };

    struct IRTypeIndirectPointer : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectPointer, IRTypeKind::IndirectPointer);

        const AstNodeTypePointer* ast = nullptr;
        IRType* target = nullptr;
        Type* type = nullptr;
    };

#undef IR_TYPE

    struct IRImport
    {
        const AstNodeImport* ast = nullptr;
        IRModule* resolved = nullptr;
    };

    struct IRModule
    {
        const char* filename = nullptr;
        const AstNodeModule* ast = nullptr;
        Array<IRImport*> imports;
        Array<IRType*> types;
    };

} // namespace potato::schematic::compiler
