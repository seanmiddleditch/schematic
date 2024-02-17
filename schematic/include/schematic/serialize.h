// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace potato::schematic
{
    struct Schema;

    std::vector<char> SerializeBinary(const Schema& schema);
    std::string SerializeJson(const Schema& schema);
} // namespace potato::schematic
