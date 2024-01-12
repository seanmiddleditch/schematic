// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/schema.h"

using namespace potato::schematic;

const Field* potato::schematic::FindField(const TypeAggregate* aggregate, std::string_view name) noexcept
{
    if (aggregate == nullptr)
        return nullptr;

    for (auto& field : aggregate->fields)
    {
        if (field.name == name)
            return &field;
    }

    if (aggregate->base != nullptr)
        return FindField(aggregate->base, name);

    return nullptr;
}

const Field* potato::schematic::FindField(const TypeAttribute* attribute, std::string_view name) noexcept
{
    if (attribute == nullptr)
        return nullptr;

    for (auto& field : attribute->fields)
    {
        if (field.name == name)
            return &field;
    }

    return nullptr;
}

const EnumItem* potato::schematic::FindItem(const TypeEnum* enumeration, std::string_view name) noexcept
{
    if (enumeration == nullptr)
        return nullptr;

    for (auto& item : enumeration->items)
    {
        if (item.name == name)
            return &item;
    }

    return nullptr;
}

const Attribute* potato::schematic::FindAttribute(const Type* type, const TypeAttribute* attribute) noexcept
{
    if (type == nullptr)
        return nullptr;
    if (attribute == nullptr)
        return nullptr;

    for (const Attribute* const attr : type->attributes)
    {
        if (attr->attribute == attribute)
            return attr;
    }

    if (const TypeAggregate* const agg = CastTo<TypeAggregate>(type); agg != nullptr)
        return FindAttribute(agg->base, attribute);

    return nullptr;
}

const Attribute* potato::schematic::FindAttribute(const Type* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;
    if (name.empty())
        return nullptr;

    for (const Attribute* const attr : type->attributes)
    {
        if (attr->attribute->name == name)
            return attr;
    }

    if (const TypeAggregate* const agg = CastTo<TypeAggregate>(type); agg != nullptr)
        return FindAttribute(agg->base, name);

    return nullptr;
}

bool potato::schematic::HasAttribute(const Type* type, const TypeAttribute* attribute) noexcept
{
    return FindAttribute(type, attribute) != nullptr;
}

bool potato::schematic::HasAttribute(const Type* type, std::string_view name) noexcept
{
    return FindAttribute(type, name) != nullptr;
}

const Type* potato::schematic::FindType(const Module* mod, std::string_view name) noexcept
{
    if (mod == nullptr)
        return nullptr;

    for (const Type* type : mod->types)
    {
        if (type->name == name)
            return type;
    }
    return nullptr;
}

static const Value* FindArgument(const Array<Argument>& arguments, const Field* field) noexcept
{
    if (field == nullptr)
        return nullptr;

    for (const Argument& argument : arguments)
    {
        if (argument.field == field)
            return argument.value;
    }

    return field->value;
}

const Value* potato::schematic::FindArgument(const ValueObject* object, const Field* field) noexcept
{
    if (object == nullptr)
        return nullptr;

    return ::FindArgument(object->fields, field);
}

const Value* potato::schematic::FindArgument(const ValueObject* object, std::string_view name) noexcept
{
    if (object == nullptr)
        return nullptr;

    return FindArgument(object, FindField(CastTo<TypeAggregate>(object->type), name));
}

const Value* potato::schematic::FindArgument(const Attribute* attribute, const Field* field) noexcept
{
    if (attribute == nullptr)
        return nullptr;

        return ::FindArgument(attribute->arguments, field);
}

const Value* potato::schematic::FindArgument(const Attribute* attribute, std::string_view name) noexcept
{
    if (attribute == nullptr)
        return nullptr;

    return FindArgument(attribute, FindField(attribute->attribute, name));
}

bool potato::schematic::IsA(const Type* type, const Type* parent) noexcept
{
    if (parent == nullptr)
        return false;

    while (type != nullptr)
    {
        if (type == parent)
            return true;

        if (const TypeAggregate* const agg = CastTo<TypeAggregate>(type))
            type = agg->base;
        else
            break;
    }

    return false;
}

bool potato::schematic::IsKind(const Type* type, TypeKind kind) noexcept
{
    if (type == nullptr)
        return false;
    return type->kind == kind;
}

bool potato::schematic::IsKind(const Value* value, ValueKind kind) noexcept
{
    if (value == nullptr)
        return false;
    return value->kind == kind;
}
