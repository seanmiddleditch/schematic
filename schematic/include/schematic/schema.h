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

    struct TypeAggregate;
    struct TypeAttribute;
    struct TypeArray;
    struct TypeBool;
    struct TypeEnum;
    struct TypeFloat;
    struct TypeInt;
    struct TypeNullable;
    struct TypePointer;
    struct TypeString;
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
        Aggregate,
        Array,
        Attribute,
        Bool,
        Enum,
        Float,
        Int,
        Nullable,
        Pointer,
        String,
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
    };

    struct Annotation
    {
        const TypeAttribute* attribute = nullptr;
        Span<Argument> arguments;
    };

    struct EnumItem
    {
        const char* name = nullptr;
        const TypeEnum* owner = nullptr;
        const ValueInt* value = nullptr;
        Span<const Annotation*> annotations;
    };

    struct Field
    {
        const char* name = nullptr;
        const Type* owner = nullptr;
        const Type* type = nullptr;
        const Value* value = nullptr;
        Span<const Annotation*> annotations;
    };

    struct Module
    {
        const char* filename = nullptr;
        Span<const Module*> imports;
        Span<const Type*> types;
    };

    struct Schema
    {
        const Module* root = nullptr;
        Span<const Module*> modules;
        Span<const Type*> types;
    };

    struct Type
    {
        TypeKind kind = TypeKind::Aggregate;
        const char* name = nullptr;
        const Module* owner = nullptr;
        Span<const Annotation*> annotations;
    };

    struct Value
    {
        ValueKind kind = ValueKind::Null;
    };

#define SCHEMATIC_TYPE(KIND) \
    static constexpr TypeKind Kind = (TypeKind::KIND); \
    Type##KIND() noexcept { kind = Kind; }

    struct TypeAggregate : Type
    {
        SCHEMATIC_TYPE(Aggregate);

        const TypeAggregate* base = nullptr;
        Span<Field> fields;
    };

    struct TypeArray : Type
    {
        SCHEMATIC_TYPE(Array);

        const Type* type = nullptr;
        bool isFixed = false;
        std::uint32_t size = 0; // only non-zero if isFixed is true
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

        const Type* base = nullptr;
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

        const Type* type = nullptr;
        Span<const Value*> elements;
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

        const Type* type = nullptr;
        Span<Argument> fields;
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
