// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "lexer.h"
#include "location.h"
#include "test_context.h"
#include "token.h"

#include "schematic/schema.h"
#include "schematic/utility.h"

#include <catch2/catch_test_macros.hpp>
#include <fmt/core.h>

#include <span>
#include <utility>

namespace schematic::test
{
    struct Buffer : std::span<const uint8_t>
    {
    };

    template <typename T>
    struct NameOf
    {
        const Schema* schema = nullptr;
        T what = {};
    };

    struct ToStringHelper
    {
        inline static std::string ToString(const schematic::Schema* schema, const schematic::Type* type);
        inline static std::string ToString(const schematic::Schema* schema, const schematic::EnumItem* enumItem);
        inline static std::string ToString(const schematic::Schema* schema, const schematic::Value* value);

        inline static std::string ToString(const schematic::Schema* schema, const schematic::TypeIndex typeIndex);
        inline static std::string ToString(const schematic::Schema* schema, const schematic::ValueIndex valueIndex);
        inline static std::string ToString(const schematic::Schema* schema, const schematic::EnumItemIndex valueIndex);

        template <typename T>
        static std::string ToString(const schematic::Schema* schema, const T& what);
    };
} // namespace schematic::test

template <>
struct Catch::StringMaker<schematic::test::Buffer>
{
    inline static std::string convert(const schematic::test::Buffer& value);
};

template <>
struct Catch::StringMaker<const schematic::EnumItem*>
{
    inline static std::string convert(const schematic::EnumItem* item);
};

template <>
struct Catch::StringMaker<const schematic::Field*>
{
    inline static std::string convert(const schematic::Field* field);
};

template <>
struct Catch::StringMaker<schematic::TypeKind>
{
    inline static std::string convert(schematic::TypeKind kind);
};

template <>
struct Catch::StringMaker<schematic::ValueKind>
{
    inline static std::string convert(schematic::ValueKind kind);
};

template <typename T>
struct Catch::StringMaker<schematic::test::NameOf<T>>
{
    inline static std::string convert(schematic::test::NameOf<T> kind);
};

std::string Catch::StringMaker<schematic::test::Buffer>::convert(const schematic::test::Buffer& buffer)
{
    std::string result;
    result.append("{ ");
    for (const uint8_t octet : buffer)
        fmt::format_to(std::back_inserter(result), "{:02x} ", octet);
    result.push_back('}');
    return result;
}

std::string Catch::StringMaker<const schematic::EnumItem*>::convert(const schematic::EnumItem* item)
{
    if (item == nullptr)
        return "{null}";

    return fmt::format("{}.{}", item->parent.index, item->name);
}

std::string Catch::StringMaker<const schematic::Field*>::convert(const schematic::Field* field)
{
    if (field == nullptr)
        return "{null}";

    return fmt::format("{} {}.{}", field->type.index, field->parent.index, field->name);
}

std::string Catch::StringMaker<schematic::TypeKind>::convert(schematic::TypeKind kind)
{
    using namespace schematic;

    switch (kind)
    {
        case TypeKind::Alias: return "Alias";
        case TypeKind::Array: return "Array";
        case TypeKind::Attribute: return "Attribute";
        case TypeKind::Bool: return "Bool";
        case TypeKind::Enum: return "Enum";
        case TypeKind::Float: return "Float";
        case TypeKind::Int: return "Int";
        case TypeKind::Message: return "Message";
        case TypeKind::Nullable: return "Nullable";
        case TypeKind::Pointer: return "Pointer";
        case TypeKind::String: return "String";
        case TypeKind::Struct: return "Struct";
        case TypeKind::Type: return "Type";
    }

    return "<invalid>";
}

std::string Catch::StringMaker<schematic::ValueKind>::convert(schematic::ValueKind kind)
{
    using namespace schematic;

    switch (kind)
    {
        case ValueKind::Array: return "Array";
        case ValueKind::Bool: return "Bool";
        case ValueKind::Enum: return "Enum";
        case ValueKind::Float: return "Float";
        case ValueKind::Int: return "Int";
        case ValueKind::Null: return "Null";
        case ValueKind::Object: return "Object";
        case ValueKind::String: return "String";
        case ValueKind::Type: return "Type";
    }

    return "<invalid>";
}

template <typename T>
std::string Catch::StringMaker<schematic::test::NameOf<T>>::convert(schematic::test::NameOf<T> nameOf)
{
    return schematic::test::ToStringHelper::ToString(nameOf.schema, nameOf.what);
}

std::string schematic::test::ToStringHelper::ToString(const schematic::Schema*, const schematic::Type* type)
{
    using namespace schematic;
    if (type != nullptr)
        return type->name;
    return "<unknown type>";
}

std::string schematic::test::ToStringHelper::ToString(const schematic::Schema* schema, const schematic::EnumItem* enumItem)
{
    using namespace schematic;
    if (enumItem == nullptr)
        return "<null item>";

    const TypeEnum* const enum_ = GetTypeAs<TypeEnum>(schema, enumItem->parent);
    if (enum_ == nullptr)
        return fmt::format("<unknown type>.{}", enumItem->name);

    return fmt::format("{}.{}", enum_->name, enumItem->name);
}

std::string schematic::test::ToStringHelper::ToString(const schematic::Schema* schema, const schematic::Value* value)
{
    using namespace schematic;

    if (value == nullptr)
        return "{null}";

    switch (value->kind)
    {
        case ValueKind::Array: return ToString(schema, static_cast<const ValueArray*>(value)->elements);
        case ValueKind::Bool: return ToString(schema, static_cast<const ValueBool*>(value)->value);
        case ValueKind::Enum: return ToString(schema, static_cast<const ValueEnum*>(value)->item);
        case ValueKind::Float: return ToString(schema, static_cast<const ValueFloat*>(value)->value);
        case ValueKind::Int: return ToString(schema, static_cast<const ValueInt*>(value)->value);
        case ValueKind::Null: return "null";
        case ValueKind::Object: return fmt::format("Object(type={})", GetType(schema, static_cast<const ValueObject*>(value)->type)->name);
        case ValueKind::String: return ToString(schema, static_cast<const ValueString*>(value)->value);
        case ValueKind::Type: return ToString(schema, static_cast<const ValueType*>(value)->type);
    }

    return fmt::format("unknown(kind={})", std::to_underlying(value->kind));
}

std::string schematic::test::ToStringHelper::ToString(const schematic::Schema* schema, const schematic::TypeIndex typeIndex)
{
    using namespace schematic;
    return ToString(schema, GetType(schema, typeIndex));
}

std::string schematic::test::ToStringHelper::ToString(const schematic::Schema* schema, const schematic::ValueIndex valueIndex)
{
    using namespace schematic;
    return ToString(schema, GetValue(schema, valueIndex));
}

std::string schematic::test::ToStringHelper::ToString(const schematic::Schema* schema, const schematic::EnumItemIndex enumItemIndex)
{
    using namespace schematic;
    return ToString(schema, GetEnumItem(schema, enumItemIndex));
}

template <typename T>
std::string schematic::test::ToStringHelper::ToString(const schematic::Schema*, const T& what)
{
    return ::Catch::Detail::stringify(what);
}
