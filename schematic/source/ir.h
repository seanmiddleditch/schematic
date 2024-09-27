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

        Builtin,

        IndirectArray,
        IndirectIdentifier,
        IndirectNullable,
        IndirectPointer,
    };

    struct IRImport;
    struct IRModule;

    struct IREnumItem;
    struct IRField;

    struct IRType;
    struct IRTypeAlias;
    struct IRTypeAttribute;
    struct IRTypeBuiltin;
    struct IRTypeEnum;
    struct IRTypeIndirect;
    struct IRTypeMessage;
    struct IRTypeStruct;

    struct IRImport;

    struct IRType
    {
        IRTypeKind kind = IRTypeKind::None;
        const AstNodeDecl* ast = nullptr;
        const char* name = nullptr;
        // FIXME: annotations
    };

    struct IRTypeIndirect : IRType
    {
    };

    struct IREnumItem
    {
        const char* name = nullptr;
        // FIXME: value
        // FIXME: annotations
    };

    struct IRField
    {
        const char* name = nullptr;
        IRType* type = nullptr;
        const AstNodeField* ast = nullptr;
        // FIXME: default value
        // FIXME: annotations
    };

#define IR_TYPE(TYPE, KIND) \
    static constexpr IRTypeKind Kind = (KIND); \
    constexpr TYPE() noexcept { kind = Kind; }

    struct IRTypeAlias : IRType
    {
        IR_TYPE(IRTypeAlias, IRTypeKind::Alias);
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

    struct IRTypeStruct : IRType
    {
        IR_TYPE(IRTypeStruct, IRTypeKind::Struct);

        IRType* base = nullptr;
        Array<IRField*> fields;
    };

    struct IRTypeIndirectArray : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectArray, IRTypeKind::IndirectArray);

        const AstNodeTypeArray* ast = nullptr;
        IRType* type = nullptr;
        std::uint32_t size = 0; // zero means unsized
    };

    struct IRTypeIndirectIdentifier : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectIdentifier, IRTypeKind::IndirectIdentifier);

        const AstNodeTypeName* ast = nullptr;
        const char* name = nullptr;
    };

    struct IRTypeIndirectNullable : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectNullable, IRTypeKind::IndirectNullable);

        const AstNodeTypeNullable* ast = nullptr;
        IRType* type = nullptr;
    };

    struct IRTypeIndirectPointer : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectPointer, IRTypeKind::IndirectPointer);

        const AstNodeTypePointer* ast = nullptr;
        IRType* type = nullptr;
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
