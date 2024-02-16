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

    std::vector<char> SerializeBinary(const Module& mod);
    int DeserializeBinary(Module& mod, std::span<const char> input);

    std::string SerializeJson(const Module& mod);
    int DeserializeJson(Module& mod, std::string_view input);
} // namespace potato::schematic
