// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_UTILITY_H
#define SCHEMATIC_UTILITY_H 1
#pragma once

#include <cstdint>
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

    struct TypeStruct;
    struct TypeAttribute;
    struct TypeEnum;

    struct ValueObject;

    class Visitor;

    const Field* FindField(const TypeStruct* type, std::string_view name) noexcept;
    const Field* FindField(const TypeAttribute* type, std::string_view name) noexcept;

    const EnumItem* FindItem(const TypeEnum* type, std::string_view name) noexcept;

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

} // namespace potato::schematic

#endif // SCHEMATIC_UTILITY_H
