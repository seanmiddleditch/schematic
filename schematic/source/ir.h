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

    enum class IRValueKind : std::uint8_t
    {
        Literal,
        Identifier,
        Type,
        EnumItem,
        InitializerList,
    };

    struct IRImport;
    struct IRModule;
    struct IRSchema;

    struct IRVersionRange;

    struct IRAnnotation;

    struct IRAnnotationArgument;
    struct IREnumItem;
    struct IRField;
    struct IRInitializerNamedArgument;

    struct IRType;
    struct IRTypeAlias;
    struct IRTypeAttribute;
    struct IRTypeBuiltin;
    struct IRTypeEnum;
    struct IRTypeIndirect;
    struct IRTypeMessage;
    struct IRTypeStructVersioned;
    struct IRTypeStruct;
    struct IRTypeIndirectArray;
    struct IRTypeIndirectNullable;
    struct IRTypeIndirectPointer;

    struct IRValue;
    struct IRValueLiteral;
    struct IRValueIdentifier;
    struct IRValueType;
    struct IRValueEnumItem;
    struct IRValueInitializerList;

    struct IRImport;

    struct IRAnnotation
    {
        const AstNodeAnnotation* ast = nullptr;
        IRType* attribute;
        Array<IRAnnotationArgument*> arguments;
        Location location;
    };

    struct IRType
    {
        IRTypeKind kind = IRTypeKind::Alias;
        const AstNode* ast = nullptr;
        Type* type = nullptr;
        const char* name = nullptr;
        TypeIndex index = InvalidIndex;
        Array<IRAnnotation*> annotations;
        IRModule* parent = nullptr;
        Location location;

        Array<IRTypeIndirectArray*> arrayTypes;
        IRTypeIndirectNullable* nullableType = nullptr;
        IRTypeIndirectPointer* pointerType = nullptr;

        template <typename T>
        friend T* CastTo(IRType* ir) noexcept
        {
            if (ir != nullptr && ir->kind == T::Kind)
                return static_cast<T*>(ir);
            return nullptr;
        }
    };

    struct IRValue
    {
        IRValueKind kind = IRValueKind::Literal;
        const AstNode* ast = nullptr;
        ValueIndex index = InvalidIndex;
        Value* value = nullptr;
        Location location;

        template <typename T>
        friend T* CastTo(IRValue* ir) noexcept
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

    struct IRAnnotationArgument
    {
        const AstNode* ast = nullptr;
        IRField* field = nullptr;
        IRValue* value = nullptr;
        Location location;
    };

    struct IREnumItem
    {
        const char* name = nullptr;
        const AstNodeEnumItem* ast = nullptr;
        EnumItem* item = nullptr;
        std::int64_t value = 0;
        Array<IRAnnotation*> annotations;
        Location location;
    };

    struct IRField
    {
        const char* name = nullptr;
        FieldIndex index = InvalidIndex;
        IRType* type = nullptr;
        const AstNodeField* ast = nullptr;
        IRVersionRange version; // only for Struct
        std::uint32_t proto = 0; // only for Message
        IRValue* value = nullptr;
        Array<IRAnnotation*> annotations;
        Location location;
    };

    struct IRInitializerNamedArgument
    {
        const char* name = nullptr;
        const AstNodeNamedArgument* ast = nullptr;
        IRField* field = nullptr;
        IRValue* value = nullptr;
        Location location;
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

        Array<IRField*> fields;
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

        Array<IRField*> fields;
    };

    struct IRTypeStruct : IRType
    {
        IR_TYPE(IRTypeStruct, IRTypeKind::Struct);

        IRType* base = nullptr;
        Array<IRField*> fields;
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

        IRType* target = nullptr;
        std::uint32_t size = 0; // zero means unsized
    };

    struct IRTypeIndirectIdentifier : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectIdentifier, IRTypeKind::IndirectIdentifier);
    };

    struct IRTypeIndirectNullable : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectNullable, IRTypeKind::IndirectNullable);

        IRType* target = nullptr;
    };

    struct IRTypeIndirectPointer : IRTypeIndirect
    {
        IR_TYPE(IRTypeIndirectPointer, IRTypeKind::IndirectPointer);

        IRType* target = nullptr;
    };

#undef IR_TYPE

#define IR_VALUE(TYPE, KIND) \
    static constexpr IRValueKind Kind = (KIND); \
    constexpr TYPE() noexcept { kind = Kind; }

    struct IRValueLiteral : IRValue
    {
        IR_VALUE(IRValueLiteral, IRValueKind::Literal);
    };

    struct IRValueIdentifier : IRValue
    {
        IR_VALUE(IRValueIdentifier, IRValueKind::Identifier);

        const char* name = nullptr;
    };

    struct IRValueType : IRValue
    {
        IR_VALUE(IRValueType, IRValueKind::Type);

        IRType* target = nullptr;
    };

    struct IRValueEnumItem : IRValue
    {
        IR_VALUE(IRValueEnumItem, IRValueKind::EnumItem);

        IRTypeEnum* type = nullptr;
        IREnumItem* item = nullptr;
    };

    struct IRValueInitializerList : IRValue
    {
        IR_VALUE(IRValueInitializerList, IRValueKind::InitializerList);

        IRType* type = nullptr;
        Array<IRValue*> positional;
        Array<IRInitializerNamedArgument*> named;
    };

#undef IR_VALUE

    struct IRImport
    {
        const AstNodeImport* ast = nullptr;
        IRModule* resolved = nullptr;
    };

    struct IRModule
    {
        const char* filename = nullptr;
        const AstNodeModule* ast = nullptr;
        const Module* module = nullptr;
        ModuleIndex index = InvalidIndex;
        Array<IRImport*> imports;
        Array<IRType*> types;
    };

    struct IRSchema
    {
        Array<IRModule*> modules;
        Array<IRType*> types;
        IRModule* root = nullptr;
    };

} // namespace potato::schematic::compiler
