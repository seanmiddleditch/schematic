// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_PROTOBUF_H
#define SCHEMATIC_PROTOBUF_H 1
#pragma once

#include "schematic/allocator.h"
#include "schematic/schematic.pb.h"

namespace potato::schematic
{
    class Logger;
    struct Schema;

    [[nodiscard]] const proto::Schema* SerializeSchemaProto(google::protobuf::Arena& arena, const Schema* schema);
    [[nodiscard]] const Schema* ParseSchemaProto(ArenaAllocator& arena, Logger& logger, const proto::Schema* proto);
} // namespace potato::schematic

#endif // SCHEMATIC_PROTOBUF_H
