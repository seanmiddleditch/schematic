// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <cstdlib>

namespace schematic::test
{
    struct EmbeddedTest
    {
        const char* name = nullptr;
        const char* source = nullptr;
    };

    extern const EmbeddedTest test_embeds[];
    extern const std::size_t test_embeds_count;
} // namespace schematic::test
