// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/serialize.h"

#include "schematic_generated.h"

#include "schematic/schema.h"

#include <flatbuffers/minireflect.h>

using namespace potato::schematic;
using namespace potato::schematic::fbs;

std::vector<std::byte> potato::schematic::SerializeBinary(const Module& mod)
{
    flatbuffers::FlatBufferBuilder builder(4096);
    RootBuilder root(builder);
    FinishRootBuffer(builder, root.Finish());

    std::vector<std::byte> result;
    const std::span<const std::byte> as_bytes(reinterpret_cast<const std::byte*>(builder.GetBufferPointer()), builder.GetSize());
    result.insert_range(result.end(), as_bytes);
    return result;
}

int potato::schematic::DeserializeBinary(Module& mod, std::span<const std::byte> input)
{
    return 1;
}

std::string potato::schematic::SerializeJson(const Module& mod)
{
    flatbuffers::FlatBufferBuilder builder(4096);
    RootBuilder root(builder);
    FinishRootBuffer(builder, root.Finish());
    return flatbuffers::FlatBufferToString(builder.GetBufferPointer(), RootTypeTable());
}

int potato::schematic::DeserializeJson(Module& mod, std::string_view input)
{
    return 1;
}
