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
    struct Buffer : std::span<const uint8_t>
    {
    };
}

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

    return fmt::format("{}.{}", item->owner->name, item->name);
}

std::string Catch::StringMaker<const potato::schematic::Field*>::convert(const potato::schematic::Field* field)
{
    if (field == nullptr)
        return "{null}";

    return fmt::format("{} {}.{}", field->type->name, field->owner->name, field->name);
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
        case ValueKind::Object: return fmt::format("Object(type={})", static_cast<const ValueObject*>(value)->type->name);
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
