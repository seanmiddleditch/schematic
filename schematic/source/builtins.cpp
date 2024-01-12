// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "builtins.h"

#include "schematic/arena.h"
#include "schematic/schema.h"

#include <climits>

using namespace potato::schematic;

const Module* potato::schematic::compiler::CreateBuiltins(ArenaAllocator& alloc)
{
    Module* const builtins = alloc.Create<Module>();

    auto AddInt = [&alloc, builtins]<typename T>(const char* name, T)
    {
        TypeInt* const type = alloc.Create<TypeInt>();
        builtins->types.PushBack(alloc, type);
        type->name = alloc.NewString(name);
        type->isSigned = std::is_signed_v<T>;
        type->bits = CHAR_BIT * sizeof(T);
    };
    auto AddFloat = [&alloc, builtins]<typename T>(const char* name, T)
    {
        TypeFloat* const type = alloc.Create<TypeFloat>();
        builtins->types.PushBack(alloc, type);
        type->name = alloc.NewString(name);
        type->bits = CHAR_BIT * sizeof(T);
    };

    {
        TypeType* const type = alloc.Create<TypeType>();
        type->name = alloc.NewString("$type");
        builtins->types.PushBack(alloc, type);
    }

    {
        TypeBool* const type = alloc.Create<TypeBool>();
        type->name = alloc.NewString("bool");
        builtins->types.PushBack(alloc, type);
    }

    {
        TypeString* const type = alloc.Create<TypeString>();
        type->name = alloc.NewString("string");
        builtins->types.PushBack(alloc, type);
    }

    AddInt("int8", std::int8_t{});
    AddInt("uint8", std::uint8_t{});
    AddInt("int16", std::int16_t{});
    AddInt("uint16", std::uint16_t{});
    AddInt("int32", std::int32_t{});
    AddInt("uint32", std::uint32_t{});
    AddInt("int64", std::int64_t{});
    AddInt("uint64", std::uint64_t{});

    AddFloat("float", float{});
    AddFloat("double", double{});

    return builtins;
}
