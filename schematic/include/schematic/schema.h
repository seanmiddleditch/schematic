// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/arena.h"

#include <cstdint>
#include <string_view>

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

    const Field* FindField(const TypeAggregate* aggregate, std::string_view name) noexcept;
    const Field* FindField(const TypeAttribute* attribute, std::string_view name) noexcept;

    const EnumItem* FindItem(const TypeEnum* enumeration, std::string_view name) noexcept;

    const Annotation* FindAnnotation(const Type* type, const TypeAttribute* attribute) noexcept;
    const Annotation* FindAnnotation(const Type* type, std::string_view name) noexcept;

    const Annotation* FindAnnotation(const Field* field, const TypeAttribute* attribute) noexcept;
    const Annotation* FindAnnotation(const Field* field, std::string_view name) noexcept;

    const Annotation* FindAnnotation(const EnumItem* item, const TypeAttribute* attribute) noexcept;
    const Annotation* FindAnnotation(const EnumItem* item, std::string_view name) noexcept;

    bool HasAttribute(const Type* type, const TypeAttribute* attribute) noexcept;
    bool HasAttribute(const Type* type, std::string_view name) noexcept;

    bool HasAttribute(const Field* field, const TypeAttribute* attribute) noexcept;
    bool HasAttribute(const Field* field, std::string_view name) noexcept;

    bool HasAttribute(const EnumItem* item, const TypeAttribute* attribute) noexcept;
    bool HasAttribute(const EnumItem* item, std::string_view name) noexcept;

    const Type* FindType(const Module* mod, std::string_view name) noexcept;
    const Type* FindType(const Schema* schema, std::string_view name) noexcept;

    const Value* FindArgument(const ValueObject* object, const Field* field) noexcept;
    const Value* FindArgument(const ValueObject* object, std::string_view name) noexcept;

    const Value* FindArgument(const Annotation* annotation, const Field* field) noexcept;
    const Value* FindArgument(const Annotation* annotation, std::string_view name) noexcept;

    bool IsA(const Type* type, const Type* parent) noexcept;

    bool IsKind(const Type* type, TypeKind kind) noexcept;
    bool IsKind(const Value* value, ValueKind kind) noexcept;

    template <typename T>
    const T* CastTo(const Type* type) noexcept
        requires std::is_base_of_v<Type, T>
    {
        if (IsKind(type, T::Kind))
            return static_cast<const T*>(type);
        return nullptr;
    }

    template <typename T>
    const T* CastTo(const Value* value) noexcept
        requires std::is_base_of_v<Value, T>
    {
        if (IsKind(value, T::Kind))
            return static_cast<const T*>(value);
        return nullptr;
    }

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
        Array<Argument> arguments;
    };

    struct EnumItem
    {
        CStringView name;
        const TypeEnum* owner = nullptr;
        const ValueInt* value = nullptr;
        Array<const Annotation*> annotations;
    };

    struct Field
    {
        CStringView name;
        const Type* owner = nullptr;
        const Type* type = nullptr;
        const Value* value = nullptr;
        Array<const Annotation*> annotations;
    };

    struct Module
    {
        CStringView filename;
        Array<const Module*> imports;
        Array<const Type*> types;
    };

    struct Schema
    {
        const Module* root = nullptr;
        Array<const Module*> modules;
        Array<const Type*> types;
    };

    struct Type
    {
        TypeKind kind = TypeKind::Aggregate;
        CStringView name;
        const Module* owner = nullptr;
        Array<const Annotation*> annotations;
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
        Array<Field> fields;
    };

    struct TypeAttribute : Type
    {
        SCHEMATIC_TYPE(Attribute);

        Array<Field> fields;
    };

    struct TypeEnum : Type
    {
        SCHEMATIC_TYPE(Enum);

        const Type* base = nullptr;
        Array<EnumItem> items;
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

        CStringView value;
    };

    struct ValueObject : Value
    {
        SCHEMATIC_VALUE(Object);

        const Type* type = nullptr;
        Array<Argument> fields;
    };

    struct ValueArray : Value
    {
        SCHEMATIC_VALUE(Array);

        const Type* type = nullptr;
        Array<const Value*> elements;
    };

    struct ValueType : Value
    {
        SCHEMATIC_VALUE(Type);

        const Type* type = nullptr;
    };

#undef SCHEMATIC_VALUE

} // namespace potato::schematic
