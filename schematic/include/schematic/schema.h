// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_SCHEMA_H
#define SCHEMATIC_SCHEMA_H 1
#pragma once

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

    using ModuleIndex = std::uint32_t;
    using TypeIndex = std::uint32_t;

    static constexpr std::uint32_t InvalidIndex = std::uint32_t(-1);

    template <typename T, typename I = std::uint32_t>
    using ReadOnlySpan = std::span<const T>;

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
        const Field* field = nullptr;
        const Value* value = nullptr;
        Location location;
    };

    struct Annotation
    {
        const TypeAttribute* attribute = nullptr;
        ReadOnlySpan<Argument> arguments;
        Location location;
    };

    struct EnumItem
    {
        ReadOnlySpan<const Annotation*> annotations;
        const char* name = nullptr;
        const ValueInt* value = nullptr;
        const TypeEnum* owner = nullptr;
        Location location;
    };

    struct Field
    {
        ReadOnlySpan<const Annotation*> annotations;
        const char* name = nullptr;
        const Type* owner = nullptr;
        const Type* type = nullptr;
        const Value* value = nullptr;
        std::uint32_t proto = 0;
        Location location;
    };

    struct Module
    {
        ReadOnlySpan<const ModuleIndex> imports;
        const char* filename = nullptr;
    };

    struct Schema
    {
        ReadOnlySpan<const Module, ModuleIndex> modules;
        ReadOnlySpan<const Type*, TypeIndex> types;
        ModuleIndex root = 0;
    };

    struct Type
    {
        ReadOnlySpan<const Annotation*> annotations;
        const char* name = nullptr;
        ModuleIndex owner = 0;
        TypeKind kind = TypeKind::Bool;
        Location location;
    };

    struct Value
    {
        ValueKind kind = ValueKind::Null;
        ModuleIndex owner = 0;
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

        const Type* type = nullptr;
        std::uint32_t size = 0; // if non-zero, array if of a fixed size; otherwise dynamically-sized
    };

    struct TypeAttribute : Type
    {
        SCHEMATIC_TYPE(TypeAttribute, TypeKind::Attribute);

        ReadOnlySpan<Field> fields;
    };

    struct TypeBool : Type
    {
        SCHEMATIC_TYPE(TypeBool, TypeKind::Bool);
    };

    struct TypeEnum : Type
    {
        SCHEMATIC_TYPE(TypeEnum, TypeKind::Enum);

        const TypeInt* base = nullptr;
        ReadOnlySpan<EnumItem> items;
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

        ReadOnlySpan<Field> fields;
    };

    struct TypeNullable : Type
    {
        SCHEMATIC_TYPE(TypeNullable, TypeKind::Nullable);

        const Type* type = nullptr;
    };

    struct TypePointer : Type
    {
        SCHEMATIC_TYPE(TypePointer, TypeKind::Pointer);

        const Type* type = nullptr;
    };

    struct TypeString : Type
    {
        SCHEMATIC_TYPE(TypeString, TypeKind::String);
    };

    struct TypeStruct : Type
    {
        SCHEMATIC_TYPE(TypeStruct, TypeKind::Struct);

        ReadOnlySpan<Field> fields;
        const TypeStruct* base = nullptr;
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

        ReadOnlySpan<const Value*> elements;
        const Type* type = nullptr;
    };

    struct ValueBool : Value
    {
        SCHEMATIC_VALUE(ValueBool, ValueKind::Bool);

        bool value = true;
    };

    struct ValueEnum : Value
    {
        SCHEMATIC_VALUE(ValueEnum, ValueKind::Enum);

        const EnumItem* item = nullptr;
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
        const Type* type = nullptr;
    };

    struct ValueString : Value
    {
        SCHEMATIC_VALUE(ValueString, ValueKind::String);

        const char* value = nullptr;
    };

    struct ValueType : Value
    {
        SCHEMATIC_VALUE(ValueType, ValueKind::Type);

        const Type* type = nullptr;
    };

#undef SCHEMATIC_VALUE

} // namespace potato::schematic

#endif // SCHEMATIC_SCHEMA_H
