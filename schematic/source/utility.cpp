// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/utility.h"

#include "schematic/schema.h"

using namespace potato::schematic;

const Field* potato::schematic::FindField(const TypeAttribute* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    for (auto& field : type->fields)
    {
        if (field.name == name)
            return &field;
    }

    return nullptr;
}

const Field* potato::schematic::FindField(const TypeMessage* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    for (auto& field : type->fields)
    {
        if (field.name == name)
            return &field;
    }

    return nullptr;
}

const Field* potato::schematic::FindField(const TypeStruct* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    for (auto& field : type->fields)
    {
        if (field.name == name)
            return &field;
    }

    if (type->base != nullptr)
        return FindField(type->base, name);

    return nullptr;
}

const Field* potato::schematic::FindField(const Type* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    if (const Field* const field = FindField(CastTo<TypeStruct>(type), name); field != nullptr)
        return field;
    if (const Field* const field = FindField(CastTo<TypeMessage>(type), name); field != nullptr)
        return field;
    if (const Field* const field = FindField(CastTo<TypeAttribute>(type), name); field != nullptr)
        return field;

    return nullptr;
}

const EnumItem* potato::schematic::FindItem(const TypeEnum* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    for (auto& item : type->items)
    {
        if (item.name == name)
            return &item;
    }

    return nullptr;
}

static const Annotation* FindAnnotation(std::span<const Annotation* const> annotations, const TypeAttribute* attribute) noexcept
{
    for (const Annotation* const annotation : annotations)
    {
        if (annotation->attribute == attribute)
            return annotation;
    }

    return nullptr;
}

static const Annotation* FindAnnotation(std::span<const Annotation* const> annotations, std::string_view name) noexcept
{
    for (const Annotation* const annotation : annotations)
    {
        if (annotation->attribute->name == name)
            return annotation;
    }

    return nullptr;
}

const Annotation* potato::schematic::FindAnnotation(const Type* type, const TypeAttribute* attribute) noexcept
{
    if (type == nullptr)
        return nullptr;
    if (attribute == nullptr)
        return nullptr;

    if (const Annotation* const annotation = ::FindAnnotation(type->annotations, attribute); annotation != nullptr)
        return annotation;

    if (const TypeStruct* const struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        return FindAnnotation(struct_->base, attribute);

    return nullptr;
}

const Annotation* potato::schematic::FindAnnotation(const Type* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;
    if (name.empty())
        return nullptr;

    if (const Annotation* const annotation = ::FindAnnotation(type->annotations, name); annotation != nullptr)
        return annotation;

    if (const TypeStruct* const struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        return FindAnnotation(struct_->base, name);

    return nullptr;
}

const Annotation* potato::schematic::FindAnnotation(const Field* field, const TypeAttribute* attribute) noexcept
{
    if (field == nullptr)
        return nullptr;
    if (attribute == nullptr)
        return nullptr;

    return ::FindAnnotation(field->annotations, attribute);
}

const Annotation* potato::schematic::FindAnnotation(const Field* field, std::string_view name) noexcept
{
    if (field == nullptr)
        return nullptr;
    if (name.empty())
        return nullptr;

    return ::FindAnnotation(field->annotations, name);
}

const Annotation* potato::schematic::FindAnnotation(const EnumItem* item, const TypeAttribute* attribute) noexcept
{
    if (item == nullptr)
        return nullptr;
    if (attribute == nullptr)
        return nullptr;

    return ::FindAnnotation(item->annotations, attribute);
}

const Annotation* potato::schematic::FindAnnotation(const EnumItem* item, std::string_view name) noexcept
{
    if (item == nullptr)
        return nullptr;
    if (name.empty())
        return nullptr;

    return ::FindAnnotation(item->annotations, name);
}

bool potato::schematic::HasAttribute(const Type* type, const TypeAttribute* attribute) noexcept
{
    return FindAnnotation(type, attribute) != nullptr;
}

bool potato::schematic::HasAttribute(const Type* type, std::string_view name) noexcept
{
    return FindAnnotation(type, name) != nullptr;
}

bool potato::schematic::HasAttribute(const Field* field, const TypeAttribute* attribute) noexcept
{
    return FindAnnotation(field, attribute) != nullptr;
}

bool potato::schematic::HasAttribute(const Field* field, std::string_view name) noexcept
{
    return FindAnnotation(field, name) != nullptr;
}

bool potato::schematic::HasAttribute(const EnumItem* item, const TypeAttribute* attribute) noexcept
{
    return FindAnnotation(item, attribute) != nullptr;
}

bool potato::schematic::HasAttribute(const EnumItem* item, std::string_view name) noexcept
{
    return FindAnnotation(item, name) != nullptr;
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

const Type* potato::schematic::FindType(const Schema* schema, std::string_view name) noexcept
{
    if (schema == nullptr)
        return nullptr;

    for (const Type* type : schema->types)
    {
        if (type->name == name)
            return type;
    }
    return nullptr;
}

static const Value* FindArgument(const std::span<const Argument>& arguments, const Field* field) noexcept
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

    return FindArgument(object, FindField(CastTo<TypeStruct>(object->type), name));
}

const Value* potato::schematic::FindArgument(const Annotation* annotation, const Field* field) noexcept
{
    if (annotation == nullptr)
        return nullptr;

    return ::FindArgument(annotation->arguments, field);
}

const Value* potato::schematic::FindArgument(const Annotation* annotation, std::string_view name) noexcept
{
    if (annotation == nullptr)
        return nullptr;

    return FindArgument(annotation, FindField(annotation->attribute, name));
}

bool potato::schematic::IsA(const Type* type, const Type* parent) noexcept
{
    if (parent == nullptr)
        return false;

    while (type != nullptr)
    {
        if (type == parent)
            return true;

        if (const TypeStruct* const agg = CastTo<TypeStruct>(type))
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
