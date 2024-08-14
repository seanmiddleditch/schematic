// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cstdint>
#include <span>

namespace potato::schematic
{
    enum class TypeKind;
    enum class ValueKind;

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
    struct TypePolymorphic;
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

    enum class TypeKind
    {
        Bool,
        Int,
        Float,
        String,
        Aggregate,
        Attribute,
        Enum,
        Array,
        Polymorphic,
        Type,
    };

    enum class ValueKind
    {
        Bool,
        Null,
        Int,
        Float,
        Enum,
        String,
        Object,
        Array,
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
        std::span<const Argument> arguments;
    };

    struct EnumItem
    {
        const char* name = nullptr;
        const TypeEnum* owner = nullptr;
        const ValueInt* value = nullptr;
        std::span<const Annotation* const> annotations;
    };

    struct Field
    {
        const char* name = nullptr;
        const Type* owner = nullptr;
        const Type* type = nullptr;
        const Value* value = nullptr;
        std::span<const Annotation* const> annotations;
    };

    struct Module
    {
        const char* filename = nullptr;
        std::span<const Module* const> imports;
        std::span<const Type* const> types;
    };

    struct Schema
    {
        const Module* root = nullptr;
        std::span<const Module* const> modules;
        std::span<const Type* const> types;
    };

    struct Type
    {
        TypeKind kind = TypeKind::Aggregate;
        const char* name = nullptr;
        const Module* owner = nullptr;
        std::span<const Annotation* const> annotations;
    };

    struct Value
    {
        ValueKind kind = ValueKind::Int;
    };

#define SCHEMATIC_TYPE(KIND) \
    static constexpr TypeKind Kind = (TypeKind::KIND); \
    Type##KIND() noexcept { kind = Kind; }

    struct TypeString : Type
    {
        SCHEMATIC_TYPE(String);
    };

    struct TypeBool : Type
    {
        SCHEMATIC_TYPE(Bool);
    };

    struct TypeInt : Type
    {
        SCHEMATIC_TYPE(Int);

        bool isSigned = true;
        std::uint32_t bits = 32;
    };

    struct TypeFloat : Type
    {
        SCHEMATIC_TYPE(Float);

        std::uint32_t bits = 32;
    };

    struct TypeAggregate : Type
    {
        SCHEMATIC_TYPE(Aggregate);

        const TypeAggregate* base = nullptr;
        std::span<const Field> fields;
    };

    struct TypeAttribute : Type
    {
        SCHEMATIC_TYPE(Attribute);

        std::span<const Field> fields;
    };

    struct TypeEnum : Type
    {
        SCHEMATIC_TYPE(Enum);

        const Type* base = nullptr;
        std::span<const EnumItem> items;
    };

    struct TypeArray : Type
    {
        SCHEMATIC_TYPE(Array);

        const Type* type = nullptr;
        bool isFixed = false;
        std::uint32_t size = 0; // only non-zero if isFixed is true
    };

    struct TypePolymorphic : Type
    {
        SCHEMATIC_TYPE(Polymorphic);

        const Type* type = nullptr;
        bool isNullable = false;
    };

    struct TypeType : Type
    {
        SCHEMATIC_TYPE(Type);
    };

#undef SCHEMATIC_TYPE

#define SCHEMATIC_VALUE(KIND) \
    static constexpr ValueKind Kind = (ValueKind::KIND); \
    Value##KIND() noexcept { kind = Kind; }

    struct ValueBool : Value
    {
        SCHEMATIC_VALUE(Bool);

        bool value = true;
    };

    struct ValueNull : Value
    {
        SCHEMATIC_VALUE(Null);
    };

    struct ValueInt : Value
    {
        SCHEMATIC_VALUE(Int);

        std::int64_t value = 0;
    };

    struct ValueFloat : Value
    {
        SCHEMATIC_VALUE(Float);

        double value = 0;
    };

    struct ValueEnum : Value
    {
        SCHEMATIC_VALUE(Enum);

        const EnumItem* item = nullptr;
    };

    struct ValueString : Value
    {
        SCHEMATIC_VALUE(String);

        const char* value = nullptr;
    };

    struct ValueObject : Value
    {
        SCHEMATIC_VALUE(Object);

        const Type* type = nullptr;
        std::span<const Argument> fields;
    };

    struct ValueArray : Value
    {
        SCHEMATIC_VALUE(Array);

        const Type* type = nullptr;
        std::span<const Value* const> elements;
    };

    struct ValueType : Value
    {
        SCHEMATIC_VALUE(Type);

        const Type* type = nullptr;
    };

#undef SCHEMATIC_VALUE

} // namespace potato::schematic
