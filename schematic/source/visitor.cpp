// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/visitor.h"

using namespace potato::schematic;

bool Visitor::Accept(const Schema* schema)
{
    if (schema == nullptr)
        return false;

    const auto action = EnterSchema(schema);
    if (action == Action::Abort)
        return false;

    if (action == Action::Skip)
        return true;

    for (const Module* const mod : schema->modules)
    {
        if (mod != nullptr && !Accept(mod))
            return false;
    }

    // we do not iterate types in the schema, but rather
    // only in the modules themselves, so that types
    // can be skipped

    ExitSchema(schema);
    return true;
}

bool Visitor::Accept(const Module* mod)
{
    if (mod == nullptr)
        return false;

    const auto action = EnterModule(mod);
    if (action == Action::Abort)
        return false;

    if (action == Action::Skip)
        return true;

    for (const Type* const type : mod->types)
    {
        if (type != nullptr && !Accept(type))
            return false;
    }

    ExitModule(mod);
    return true;
}

bool Visitor::Accept(const Type* type)
{
    if (type == nullptr)
        return false;

    const auto action = EnterType(type);
    if (action == Action::Abort)
        return false;

    if (action == Action::Skip)
        return true;

    switch (type->kind)
    {
        using enum TypeKind;
        case Bool:
        case Enum:
        case Float:
        case Int:
        case String:
        case Type:
            break; // nothing to do
        case Array:
            if (const potato::schematic::Type* const element = static_cast<const TypeArray*>(type)->type; element != nullptr && !Accept(element))
                return false;
            break;
        case Attribute:
            for (const potato::schematic::Field& field : static_cast<const TypeAttribute*>(type)->fields)
            {
                if (!Accept(&field))
                    return false;
            }
            break;
        case Message:
            for (const potato::schematic::Field& field : static_cast<const TypeMessage*>(type)->fields)
            {
                if (!Accept(&field))
                    return false;
            }
            break;
        case Nullable:
            if (const potato::schematic::Type* const ref = static_cast<const TypeNullable*>(type)->type; ref != nullptr && !Accept(ref))
                return false;
            break;
        case Pointer:
            if (const potato::schematic::Type* const ref = static_cast<const TypePointer*>(type)->type; ref != nullptr && !Accept(ref))
                return false;
            break;
        case Struct:
            if (const potato::schematic::Type* const base = static_cast<const TypeStruct*>(type)->base; base != nullptr && !Accept(base))
                return false;
            for (const potato::schematic::Field& field : static_cast<const TypeStruct*>(type)->fields)
            {
                if (!Accept(&field))
                    return false;
            }
            break;
    }

    ExitType(type);
    return true;
}

bool Visitor::Accept(const Field* field)
{
    if (field == nullptr)
        return false;

    const auto action = EnterField(field);
    if (action == Action::Abort)
        return false;

    if (action == Action::Skip)
        return true;

    if (field->type != nullptr && !Accept(field->type))
        return false;

    if (field->value != nullptr && !Accept(field->value))
        return false;

    ExitField(field);
    return true;
}

bool Visitor::Accept(const Value* value)
{
    if (value == nullptr)
        return false;

    const auto action = EnterValue(value);
    if (action == Action::Abort)
        return false;

    if (action == Action::Skip)
        return true;

    switch (value->kind)
    {
        using enum ValueKind;
        case Bool:
        case Enum:
        case Float:
        case Int:
        case Null:
        case String:
        case Type:
            break; // nothing else to do
        case Array:
            for (const Value* const element : static_cast<const ValueArray*>(value)->elements)
            {
                if (element != nullptr && !Accept(element))
                    return false;
            }
            break;
        case Object:
            for (const Argument& arg : static_cast<const ValueObject*>(value)->fields)
            {
                if (arg.value != nullptr && !Accept(arg.value))
                    return false;
            }
            break;
    }

    ExitValue(value);
    return true;
}
