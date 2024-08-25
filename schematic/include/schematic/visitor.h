// Schematic. Copyright (C) Sean Middleditch and contributors.

#ifndef SCHEMATIC_UTILITY_H
#define SCHEMATIC_UTILITY_H 1
#pragma once

#include "schematic/schema.h"

namespace potato::schematic
{
    class Visitor
    {
    public:
        enum class Action
        {
            Continue,
            Skip,
            Abort,
        };

        Visitor(const Visitor&) = delete;
        Visitor& operator=(const Visitor&) = delete;

        bool Accept(const Schema* schema);
        bool Accept(const Module* mod);
        bool Accept(const Type* type);
        bool Accept(const Field* field);
        bool Accept(const Value* value);

    protected:
        Visitor() = default;
        ~Visitor() = default;

        virtual Action EnterSchema(const Schema* schema) { return Action::Continue; }
        virtual void ExitSchema(const Schema* schema) { }

        virtual Action EnterModule(const Module* mod) { return Action::Continue; }
        virtual void ExitModule(const Module* mod) { }

        virtual Action EnterType(const Type* type) { return Action::Continue; }
        virtual void ExitType(const Type* type) { }

        virtual Action EnterField(const Field* field) { return Action::Continue; }
        virtual void ExitField(const Field* field) { }

        virtual Action EnterValue(const Value* value) { return Action::Continue; }
        virtual void ExitValue(const Value* value) { }
    };
} // namespace potato::schematic

#endif // SCHEMATIC_UTILITY_H
