// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

namespace potato::schematic
{
    struct ArenaAllocator;
    struct Module;
}

namespace potato::schematic::compiler
{
    const Module* CreateBuiltins(ArenaAllocator& alloc);
}
