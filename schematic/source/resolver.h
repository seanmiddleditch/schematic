// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <string_view>

namespace potato::schematic::compiler
{
    class Source;

    class Resolver
    {
    public:
        virtual ~Resolver() = default;

        virtual const Source* ResolveModule(std::string_view name, const Source* referrer) = 0;
    };
} // namespace potato::schematic::compiler
