// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_SCHEMA_H
#define SCHEMATIC_SCHEMA_H 1
#pragma once

#include "common.h"

#include <cstdint>
#include <span>

namespace potato::schematic
{
    enum class TypeKind : std::uint8_t;
    enum class ValueKind : std::uint8_t;

    struct Argument;
    struct Annotation;
    struct EnumItem;
    struct Field;
    struct Location;
    struct Module;
    struct Schema;
    struct Type;
    struct Value;

    struct TypeAlias;
    struct TypeArray;
    struct TypeAttribute;
    struct TypeBool;
    struct TypeEnum;
    struct TypeFloat;
    struct TypeInt;
    struct TypeMessage;
    struct TypeNullable;
    struct TypePointer;
    struct TypeString;
    struct TypeStruct;
    struct TypeType;

    struct ValueArray;
    struct ValueBool;
    struct ValueEnum;
    struct ValueInt;
    struct ValueNull;
    struct ValueString;
    struct ValueObject;
    struct ValueFloat;
    struct ValueType;

    using Annotations = IndexRange<AnnotationIndex>;

    enum class TypeKind : std::uint8_t
    {
        Alias,
        Array,
        Attribute,
        Bool,
        Enum,
        Float,
        Int,
        Message,
        Nullable,
        Pointer,
        String,
        Struct,
        Type,
    };

    enum class ValueKind : std::uint8_t
    {
        Array,
        Bool,
        Enum,
        Float,
        Int,
        Null,
        Object,
        String,
        Type,
    };

    struct Location
    {
        std::uint32_t line = 0; // zero means no line information
        std::uint32_t column = 0; // zero means no column information
    };

    struct Argument
    {
        FieldIndex field = InvalidIndex;
        ValueIndex value = InvalidIndex;
        Location location;
    };

    struct Annotation
    {
        TypeIndex attribute = InvalidIndex;
        ReadOnlySpan<Argument> arguments;
        Location location;
    };

    struct EnumItem
    {
        const char* name = nullptr;
        TypeIndex parent = InvalidIndex;
        ValueIndex value = InvalidIndex;
        Annotations annotations;
        Location location;
    };

    struct Field
    {
        const char* name = nullptr;
        FieldIndex index = InvalidIndex;
        TypeIndex parent = InvalidIndex;
        TypeIndex type = InvalidIndex;
        ValueIndex value = InvalidIndex;
        std::uint32_t proto = 0;
        Annotations annotations;
        Location location;
    };

    struct Module
    {
        const char* filename = nullptr;
        TypeIndex index = InvalidIndex;
        ReadOnlySpan<ModuleIndex> imports;
    };

    struct Schema
    {
        ReadOnlySpan<Module, ModuleIndex> modules;
        ReadOnlySpan<Field, FieldIndex> fields;
        ReadOnlySpan<EnumItem, EnumItemIndex> enumItems;
        ReadOnlySpan<Annotation, AnnotationIndex> annotations;
        ReadOnlySpan<const Type*, TypeIndex> types;
        ReadOnlySpan<const Value*, ValueIndex> values;
        ModuleIndex root = InvalidIndex;
    };

    struct Type
    {
        const char* name = nullptr;
        TypeIndex index = InvalidIndex;
        ModuleIndex parent = InvalidIndex;
        TypeKind kind = TypeKind::Bool;
        Annotations annotations;
        Location location;
    };

    struct Value
    {
        ValueKind kind = ValueKind::Null;
        Location location;
    };

#define SCHEMATIC_TYPE(TYPE, KIND) \
    static constexpr TypeKind Kind = (KIND); \
    TYPE() noexcept { kind = Kind; }

    struct TypeAlias : Type
    {
        SCHEMATIC_TYPE(TypeAlias, TypeKind::Alias);

        TypeIndex type = InvalidIndex;
    };

    struct TypeArray : Type
    {
        SCHEMATIC_TYPE(TypeArray, TypeKind::Array);

        TypeIndex elements = InvalidIndex;
        std::uint32_t size = 0; // if non-zero, array if of a fixed size; otherwise dynamically-sized
    };

    struct TypeAttribute : Type
    {
        SCHEMATIC_TYPE(TypeAttribute, TypeKind::Attribute);

        IndexRange<FieldIndex> fields;
    };

    struct TypeBool : Type
    {
        SCHEMATIC_TYPE(TypeBool, TypeKind::Bool);
    };

    struct TypeEnum : Type
    {
        SCHEMATIC_TYPE(TypeEnum, TypeKind::Enum);

        TypeIndex base = InvalidIndex;
        IndexRange<EnumItemIndex> items;
    };

    struct TypeFloat : Type
    {
        SCHEMATIC_TYPE(TypeFloat, TypeKind::Float);

        std::uint32_t width = 32;
    };

    struct TypeInt : Type
    {
        SCHEMATIC_TYPE(TypeInt, TypeKind::Int);

        bool isSigned = true;
        std::uint32_t width = 32;
    };

    struct TypeMessage : Type
    {
        SCHEMATIC_TYPE(TypeMessage, TypeKind::Message);

        IndexRange<FieldIndex> fields;
    };

    struct TypeNullable : Type
    {
        SCHEMATIC_TYPE(TypeNullable, TypeKind::Nullable);

        TypeIndex target = InvalidIndex;
    };

    struct TypePointer : Type
    {
        SCHEMATIC_TYPE(TypePointer, TypeKind::Pointer);

        TypeIndex target = InvalidIndex;
    };

    struct TypeString : Type
    {
        SCHEMATIC_TYPE(TypeString, TypeKind::String);
    };

    struct TypeStruct : Type
    {
        SCHEMATIC_TYPE(TypeStruct, TypeKind::Struct);

        IndexRange<FieldIndex> fields;
        TypeIndex base = InvalidIndex;
        std::uint32_t version = 1;
    };

    struct TypeType : Type
    {
        SCHEMATIC_TYPE(TypeType, TypeKind::Type);
    };

#undef SCHEMATIC_TYPE

#define SCHEMATIC_VALUE(TYPE, KIND) \
    static constexpr ValueKind Kind = (KIND); \
    TYPE() noexcept { kind = Kind; }

    struct ValueArray : Value
    {
        SCHEMATIC_VALUE(ValueArray, ValueKind::Array);

        ReadOnlySpan<ValueIndex> elements;
        TypeIndex type = InvalidIndex;
    };

    struct ValueBool : Value
    {
        SCHEMATIC_VALUE(ValueBool, ValueKind::Bool);

        bool value = true;
    };

    struct ValueEnum : Value
    {
        SCHEMATIC_VALUE(ValueEnum, ValueKind::Enum);

        EnumItemIndex item;
    };

    struct ValueFloat : Value
    {
        SCHEMATIC_VALUE(ValueFloat, ValueKind::Float);

        double value = 0;
    };

    struct ValueInt : Value
    {
        SCHEMATIC_VALUE(ValueInt, ValueKind::Int);

        std::int64_t value = 0;
    };

    struct ValueNull : Value
    {
        SCHEMATIC_VALUE(ValueNull, ValueKind::Null);
    };

    struct ValueObject : Value
    {
        SCHEMATIC_VALUE(ValueObject, ValueKind::Object);

        ReadOnlySpan<Argument> fields;
        TypeIndex type = InvalidIndex;
    };

    struct ValueString : Value
    {
        SCHEMATIC_VALUE(ValueString, ValueKind::String);

        const char* value = nullptr;
    };

    struct ValueType : Value
    {
        SCHEMATIC_VALUE(ValueType, ValueKind::Type);

        TypeIndex type = InvalidIndex;
    };

#undef SCHEMATIC_VALUE

} // namespace potato::schematic

#endif // SCHEMATIC_SCHEMA_H
