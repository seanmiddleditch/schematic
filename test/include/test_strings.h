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

namespace potato::schematic::test
{
    struct TypeIndexWrapper
    {
        TypeIndex index = InvalidIndex;
    };

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
        inline static std::string ToString(const potato::schematic::Schema* schema, const potato::schematic::Type* type);
        inline static std::string ToString(const potato::schematic::Schema* schema, const potato::schematic::test::TypeIndexWrapper typeIndex);
        inline static std::string ToString(const potato::schematic::Schema* schema, const potato::schematic::EnumItem* item);

        template <typename T>
        static std::string ToString(const potato::schematic::Schema* schema, const T& what);
    };
} // namespace potato::schematic::test

template <>
struct Catch::StringMaker<potato::schematic::test::Buffer>
{
    inline static std::string convert(const potato::schematic::test::Buffer& value);
};

template <>
struct Catch::StringMaker<const potato::schematic::EnumItem*>
{
    inline static std::string convert(const potato::schematic::EnumItem* item);
};

template <>
struct Catch::StringMaker<const potato::schematic::Field*>
{
    inline static std::string convert(const potato::schematic::Field* field);
};

template <>
struct Catch::StringMaker<const potato::schematic::Value*>
{
    inline static std::string convert(const potato::schematic::Value* value);
};

template <>
struct Catch::StringMaker<const potato::schematic::Type*>
{
    inline static std::string convert(const potato::schematic::Type* type);
};

template <>
struct Catch::StringMaker<potato::schematic::TypeKind>
{
    inline static std::string convert(potato::schematic::TypeKind kind);
};

template <>
struct Catch::StringMaker<potato::schematic::ValueKind>
{
    inline static std::string convert(potato::schematic::ValueKind kind);
};

template <typename T>
struct Catch::StringMaker<potato::schematic::test::NameOf<T>>
{
    inline static std::string convert(potato::schematic::test::NameOf<T> kind);
};

std::string Catch::StringMaker<potato::schematic::test::Buffer>::convert(const potato::schematic::test::Buffer& buffer)
{
    std::string result;
    result.append("{ ");
    for (const uint8_t octet : buffer)
        fmt::format_to(std::back_inserter(result), "{:02x} ", octet);
    result.push_back('}');
    return result;
}

std::string Catch::StringMaker<const potato::schematic::EnumItem*>::convert(const potato::schematic::EnumItem* item)
{
    if (item == nullptr)
        return "{null}";

    return fmt::format("{}.{}", item->parent, item->name);
}

std::string Catch::StringMaker<const potato::schematic::Field*>::convert(const potato::schematic::Field* field)
{
    if (field == nullptr)
        return "{null}";

    return fmt::format("{} {}.{}", field->type, field->parent, field->name);
}

std::string Catch::StringMaker<const potato::schematic::Value*>::convert(const potato::schematic::Value* value)
{
    using namespace potato::schematic;

    if (value == nullptr)
        return "{null}";

    switch (value->kind)
    {
        case ValueKind::Array: return Catch::Detail::stringify(static_cast<const ValueArray*>(value)->elements);
        case ValueKind::Bool: return Catch::Detail::stringify(static_cast<const ValueBool*>(value)->value);
        case ValueKind::Enum: return Catch::Detail::stringify(static_cast<const ValueEnum*>(value)->item);
        case ValueKind::Float: return Catch::Detail::stringify(static_cast<const ValueFloat*>(value)->value);
        case ValueKind::Int: return Catch::Detail::stringify(static_cast<const ValueInt*>(value)->value);
        case ValueKind::Null: return Catch::Detail::stringify(nullptr);
        case ValueKind::Object: return fmt::format("Object(type={})", static_cast<const ValueObject*>(value)->type);
        case ValueKind::String: return Catch::Detail::stringify(static_cast<const ValueString*>(value)->value);
        case ValueKind::Type: return Catch::Detail::stringify(static_cast<const ValueType*>(value)->type);
    }

    return fmt::format("unknown(kind={})", std::to_underlying(value->kind));
}

std::string Catch::StringMaker<const potato::schematic::Type*>::convert(const potato::schematic::Type* type)
{
    using namespace potato::schematic;

    if (type == nullptr)
        return "{null}";

    return type->name;
}

std::string Catch::StringMaker<potato::schematic::TypeKind>::convert(potato::schematic::TypeKind kind)
{
    using namespace potato::schematic;

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

std::string Catch::StringMaker<potato::schematic::ValueKind>::convert(potato::schematic::ValueKind kind)
{
    using namespace potato::schematic;

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
std::string Catch::StringMaker<potato::schematic::test::NameOf<T>>::convert(potato::schematic::test::NameOf<T> nameOf)
{
    return potato::schematic::test::ToStringHelper::ToString(nameOf.schema, nameOf.what);
}

std::string potato::schematic::test::ToStringHelper::ToString(const potato::schematic::Schema*, const potato::schematic::Type* type)
{
    using namespace potato::schematic;
    if (type != nullptr)
        return type->name;
    return "<unknown type>";
}

std::string potato::schematic::test::ToStringHelper::ToString(const potato::schematic::Schema* schema, const potato::schematic::test::TypeIndexWrapper typeIndex)
{
    using namespace potato::schematic;
    return ToString(schema, GetType(schema, typeIndex.index));
}

std::string potato::schematic::test::ToStringHelper::ToString(const potato::schematic::Schema* schema, const potato::schematic::EnumItem* item)
{
    using namespace potato::schematic;
    if (item == nullptr)
        return "<null item>";

    const TypeEnum* const enum_ = GetTypeAs<TypeEnum>(schema, item->parent);
    if (enum_ == nullptr)
        return fmt::format("<unknown type>.{}", item->name);

    return fmt::format("{}.{}", enum_->name, item->name);
}

template <typename T>
static std::string potato::schematic::test::ToStringHelper::ToString(const potato::schematic::Schema*, const T& what)
{
    return ::Catch::Detail::stringify(what);
}
