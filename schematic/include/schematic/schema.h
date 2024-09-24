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

    template <typename T>
    using Span = std::span<const T>;

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

    struct Argument
    {
        const Field* field = nullptr;
        const Value* value = nullptr;
        std::uint16_t line = 0; // if non-zero, the line within the owner module of the value
    };

    struct Annotation
    {
        const TypeAttribute* attribute = nullptr;
        Span<Argument> arguments;
        std::uint16_t line = 0; // if non-zero, the line within owner module of the owner type
    };

    struct EnumItem
    {
        Span<const Annotation*> annotations;
        const char* name = nullptr;
        const ValueInt* value = nullptr;
        const TypeEnum* owner = nullptr;
        std::uint16_t line = 0; // if non-zero, the line within the owner module of the owner enum
    };

    struct Field
    {
        Span<const Annotation*> annotations;
        const char* name = nullptr;
        const Type* owner = nullptr;
        const Type* type = nullptr;
        const Value* value = nullptr;
        std::uint32_t proto = 0;
        std::uint16_t line = 0; // if non-zero, the line within the owner module of the owner type
    };

    struct Module
    {
        Span<const Module*> imports;
        Span<const Type*> types;
        const char* filename = nullptr;
    };

    struct Schema
    {
        Span<const Module*> modules;
        Span<const Type*> types;
        const Module* root = nullptr;
    };

    struct Type
    {
        Span<const Annotation*> annotations;
        const char* name = nullptr;
        const Module* owner = nullptr;
        TypeKind kind = TypeKind::Bool;
        std::uint16_t line = 0; // if non-zero, the line within the owner module of the declaration
    };

    struct Value
    {
        ValueKind kind = ValueKind::Null;
        const Module* owner = nullptr;
        std::uint16_t line = 0; // if non-zero, the line within the owner module
    };

#define SCHEMATIC_TYPE(KIND) \
    static constexpr TypeKind Kind = (TypeKind::KIND); \
    Type##KIND() noexcept { kind = Kind; }

    struct TypeAlias : Type
    {
        SCHEMATIC_TYPE(Alias);

        const Type* type = nullptr;
    };

    struct TypeArray : Type
    {
        SCHEMATIC_TYPE(Array);

        const Type* type = nullptr;
        std::uint32_t size = 0; // if non-zero, array if of a fixed size; otherwise dynamically-sized
    };

    struct TypeAttribute : Type
    {
        SCHEMATIC_TYPE(Attribute);

        Span<Field> fields;
    };

    struct TypeBool : Type
    {
        SCHEMATIC_TYPE(Bool);
    };

    struct TypeEnum : Type
    {
        SCHEMATIC_TYPE(Enum);

        const TypeInt* base = nullptr;
        Span<EnumItem> items;
    };

    struct TypeFloat : Type
    {
        SCHEMATIC_TYPE(Float);

        std::uint32_t width = 32;
    };

    struct TypeInt : Type
    {
        SCHEMATIC_TYPE(Int);

        bool isSigned = true;
        std::uint32_t width = 32;
    };

    struct TypeMessage : Type
    {
        SCHEMATIC_TYPE(Message);

        Span<Field> fields;
    };

    struct TypeNullable : Type
    {
        SCHEMATIC_TYPE(Nullable);

        const Type* type = nullptr;
    };

    struct TypePointer : Type
    {
        SCHEMATIC_TYPE(Pointer);

        const Type* type = nullptr;
    };

    struct TypeString : Type
    {
        SCHEMATIC_TYPE(String);
    };

    struct TypeStruct : Type
    {
        SCHEMATIC_TYPE(Struct);

        Span<Field> fields;
        const TypeStruct* base = nullptr;
        std::uint32_t version = 1;
    };

    struct TypeType : Type
    {
        SCHEMATIC_TYPE(Type);
    };

#undef SCHEMATIC_TYPE

#define SCHEMATIC_VALUE(KIND) \
    static constexpr ValueKind Kind = (ValueKind::KIND); \
    Value##KIND() noexcept { kind = Kind; }

    struct ValueArray : Value
    {
        SCHEMATIC_VALUE(Array);

        Span<const Value*> elements;
        const Type* type = nullptr;
    };

    struct ValueBool : Value
    {
        SCHEMATIC_VALUE(Bool);

        bool value = true;
    };

    struct ValueEnum : Value
    {
        SCHEMATIC_VALUE(Enum);

        const EnumItem* item = nullptr;
    };

    struct ValueFloat : Value
    {
        SCHEMATIC_VALUE(Float);

        double value = 0;
    };

    struct ValueInt : Value
    {
        SCHEMATIC_VALUE(Int);

        std::int64_t value = 0;
    };

    struct ValueNull : Value
    {
        SCHEMATIC_VALUE(Null);
    };

    struct ValueObject : Value
    {
        SCHEMATIC_VALUE(Object);

        Span<Argument> fields;
        const Type* type = nullptr;
    };

    struct ValueString : Value
    {
        SCHEMATIC_VALUE(String);

        const char* value = nullptr;
    };

    struct ValueType : Value
    {
        SCHEMATIC_VALUE(Type);

        const Type* type = nullptr;
    };

#undef SCHEMATIC_VALUE

} // namespace potato::schematic

#endif // SCHEMATIC_SCHEMA_H
