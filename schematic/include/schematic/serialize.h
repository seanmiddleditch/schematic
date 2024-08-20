// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/allocator.h"
#include "schematic/schematic.pb.h"

#include <string>
#include <vector>

namespace potato::schematic
{
    struct Schema;

    const proto::Schema* Serialize(google::protobuf::Arena& arena, const Schema* schema);
    const Schema* Deserialize(Allocator& alloc, const proto::Schema* proto);
} // namespace potato::schematic
