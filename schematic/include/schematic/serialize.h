// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace potato::schematic
{
    struct Module;

    std::vector<std::byte> SerializeBinary(const Module& mod);
    int DeserializeBinary(Module& mod, std::span<const std::byte> input);

    std::string SerializeJson(const Module& mod);
    int DeserializeJson(Module& mod, std::string_view input);
} // namespace potato::schematic
