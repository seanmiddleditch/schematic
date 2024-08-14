// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "test_context.h"

#include "schematic/schema.h"

#include <catch2/catch_test_macros.hpp>
#include <fmt/core.h>

#include <span>

#include "lexer.h"
#include "location.h"
#include "token.h"

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
        return "invalid(nullptr)";

    return fmt::format("{}({})", item->name, item->value->value);
}

std::string Catch::StringMaker<const potato::schematic::Value*>::convert(const potato::schematic::Value* value)
{
    using namespace potato::schematic;

    if (value == nullptr)
        return "invalid";

    switch (value->kind)
    {
        using enum ValueKind;
        case Bool: return std::format("{}", CastTo<ValueBool>(value)->value);
        case Null: return std::format("nullptr");
        case Int: return std::format("{}", CastTo<ValueInt>(value)->value);
        case Enum: return Catch::StringMaker<const EnumItem*>::convert(CastTo<ValueEnum>(value)->item);
        case Object: return std::format("object");
        case Array: return std::format("array");
        case Type: return std::format("type");
    }

    return fmt::format("unknown(kind={})", std::to_underlying(value->kind));
}
