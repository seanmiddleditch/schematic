// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/allocator.h"
#include "schematic/schematic.pb.h"

namespace potato::schematic
{
    struct Schema;

    [[nodiscard]] const proto::Schema* SerializeSchemaProto(google::protobuf::Arena& arena, const Schema* schema);
    [[nodiscard]] const Schema* ParseSchemaProto(ArenaAllocator& arena, const proto::Schema* proto);
} // namespace potato::schematic
