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
struct Catch::StringMaker<const potato::schematic::Value*>
{
    inline static std::string convert(const potato::schematic::Value* value);
};

template <>
struct Catch::StringMaker<const potato::schematic::Type*>
{
    inline static std::string convert(const potato::schematic::Type* type);
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

    return fmt::format("{}({})", item->name, item->value->value);
}

std::string Catch::StringMaker<const potato::schematic::Value*>::convert(const potato::schematic::Value* value)
{
    using namespace potato::schematic;

    if (value == nullptr)
        return "{null}";

    switch (value->kind)
    {
        case ValueKind::Array: return fmt::format("Array");
        case ValueKind::Bool: return fmt::format("{}", CastTo<ValueBool>(value)->value);
        case ValueKind::Enum: return Catch::StringMaker<const EnumItem*>::convert(CastTo<ValueEnum>(value)->item);
        case ValueKind::Float: return fmt::format("{}", CastTo<ValueFloat>(value)->value);
        case ValueKind::Int: return fmt::format("{}", CastTo<ValueInt>(value)->value);
        case ValueKind::Null: return fmt::format("Null");
        case ValueKind::Object: return fmt::format("Object");
        case ValueKind::String: return fmt::format("String");
        case ValueKind::Type: return fmt::format("Type");
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
