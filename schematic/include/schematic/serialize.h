// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/schematic.pb.h"

#include <string>
#include <vector>

namespace potato::schematic
{
    struct Schema;

    const proto::Schema* SerializeBinary(google::protobuf::Arena& arena, const Schema& schema);
} // namespace potato::schematic
