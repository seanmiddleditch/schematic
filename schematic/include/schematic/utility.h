// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_UTILITY_H
#define SCHEMATIC_UTILITY_H 1
#pragma once

#include "common.h"

#include <cstdint>
#include <span>
#include <string_view>

namespace potato::schematic
{
    enum class TypeKind : std::uint8_t;
    enum class ValueKind : std::uint8_t;

    struct Annotation;
    struct EnumItem;
    struct Field;
    struct Module;
    struct Schema;
    struct Type;
    struct Value;

    struct TypeAttribute;
    struct TypeEnum;
    struct TypeMessage;
    struct TypeStruct;

    struct ValueObject;

    using FieldIndex = std::uint32_t;
    using ModuleIndex = std::uint32_t;
    using TypeIndex = std::uint32_t;

    class Visitor;

    const Type* GetType(const Schema* schema, TypeIndex typeIndex) noexcept;
    ReadOnlySpan<Field> GetFields(const Schema* schema, IndexRange<FieldIndex> fields) noexcept;

    const Field* FindField(const Schema* schema, const TypeAttribute* type, std::string_view name) noexcept;
    const Field* FindField(const Schema* schema, const TypeMessage* type, std::string_view name) noexcept;
    const Field* FindField(const Schema* schema, const TypeStruct* type, std::string_view name) noexcept;
    const Field* FindField(const Schema* schema, const Type* type, std::string_view name) noexcept;
    const Field* FindField(const Schema* schema, TypeIndex typeIndex, std::string_view name) noexcept;

    const EnumItem* FindItem(const Schema* schema, const TypeEnum* type, std::string_view name) noexcept;

    const Annotation* FindAnnotation(const Schema* schema, const Type* type, const TypeAttribute* attribute) noexcept;
    const Annotation* FindAnnotation(const Schema* schema, const Type* type, std::string_view name) noexcept;
    const Annotation* FindAnnotation(const Schema* schema, TypeIndex typeIndex, const TypeAttribute* attribute) noexcept;
    const Annotation* FindAnnotation(const Schema* schema, TypeIndex typeIndex, std::string_view name) noexcept;

    const Annotation* FindAnnotation(const Schema* schema, const Field* field, const TypeAttribute* attribute) noexcept;
    const Annotation* FindAnnotation(const Schema* schema, const Field* field, std::string_view name) noexcept;

    const Annotation* FindAnnotation(const Schema* schema, const EnumItem* item, const TypeAttribute* attribute) noexcept;
    const Annotation* FindAnnotation(const Schema* schema, const EnumItem* item, std::string_view name) noexcept;

    bool HasAttribute(const Schema* schema, const Type* type, const TypeAttribute* attribute) noexcept;
    bool HasAttribute(const Schema* schema, const Type* type, std::string_view name) noexcept;

    bool HasAttribute(const Schema* schema, const Field* field, const TypeAttribute* attribute) noexcept;
    bool HasAttribute(const Schema* schema, const Field* field, std::string_view name) noexcept;

    bool HasAttribute(const Schema* schema, const EnumItem* item, const TypeAttribute* attribute) noexcept;
    bool HasAttribute(const Schema* schema, const EnumItem* item, std::string_view name) noexcept;

    const Type* FindType(const Schema* schema, std::string_view name) noexcept;
    const Type* FindType(const Schema* schema, ModuleIndex moduleIndex, std::string_view name) noexcept;

    const Value* FindArgument(const Schema* schema, const ValueObject* object, const Field* field) noexcept;
    const Value* FindArgument(const Schema* schema, const ValueObject* object, std::string_view name) noexcept;

    const Value* FindArgument(const Schema* schema, const Annotation* annotation, const Field* field) noexcept;
    const Value* FindArgument(const Schema* schema, const Annotation* annotation, std::string_view name) noexcept;

    bool IsA(const Schema* schema, const Type* type, const Type* parent) noexcept;

    bool IsKind(const Type* type, TypeKind kind) noexcept;
    bool IsKind(const Value* value, ValueKind kind) noexcept;

    template <typename T>
    const T* CastTo(const Type* type) noexcept
        requires std::is_base_of_v<Type, T>
    {
        if (type == nullptr)
            return nullptr;
        if (IsKind(type, T::Kind))
            return static_cast<const T*>(type);
        return nullptr;
    }

    template <typename T>
    const T* CastTo(const Value* value) noexcept
        requires std::is_base_of_v<Value, T>
    {
        if (value == nullptr)
            return nullptr;
        if (IsKind(value, T::Kind))
            return static_cast<const T*>(value);
        return nullptr;
    }

    template <typename T>
    const T* GetTypeAs(const Schema* schema, TypeIndex typeIndex) noexcept
        requires std::is_base_of_v<Type, T>
    {
        return CastTo<T>(GetType(schema, typeIndex));
    }

} // namespace potato::schematic

#endif // SCHEMATIC_UTILITY_H
