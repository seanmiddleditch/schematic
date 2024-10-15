// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/utility.h"

#include "schematic/schema.h"

using namespace potato::schematic;

const Type* potato::schematic::GetType(const Schema* schema, TypeIndex typeIndex) noexcept
{
    if (schema == nullptr)
        return nullptr;

    if (typeIndex == InvalidIndex)
        return nullptr;

    if (typeIndex >= schema->types.size())
        return nullptr;

    return schema->types[typeIndex];
}

ReadOnlySpan<Field> potato::schematic::GetFields(const Schema* schema, IndexRange<FieldIndex> fields) noexcept
{
    if (schema == nullptr)
        return {};

    if (fields.start == InvalidIndex)
        return {};
    if (fields.count == 0)
        return {};

    if (fields.start >= schema->fields.size())
        return {};
    if (fields.count > schema->fields.size())
        return {};
    if (fields.start > schema->fields.size() - fields.count)
        return {};

    return schema->fields.subspan(fields.start, fields.count);
}

const Value* potato::schematic::GetValue(const Schema* schema, ValueIndex valueIndex) noexcept
{
    if (schema == nullptr)
        return nullptr;

    if (valueIndex == InvalidIndex)
        return nullptr;

    if (valueIndex >= schema->values.size())
        return nullptr;

    return schema->values[valueIndex];
}

const Field* potato::schematic::FindField(const Schema* schema, const TypeAttribute* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    for (auto& field : GetFields(schema, type->fields))
    {
        if (field.name == name)
            return &field;
    }

    return nullptr;
}

const Field* potato::schematic::FindField(const Schema* schema, const TypeMessage* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    for (auto& field : GetFields(schema, type->fields))
    {
        if (field.name == name)
            return &field;
    }

    return nullptr;
}

const Field* potato::schematic::FindField(const Schema* schema, const TypeStruct* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    for (FieldIndex index = type->fields.start; index != type->fields.start + type->fields.count; ++index)
    {
        const Field& field = schema->fields[index];
        if (field.name == name)
            return &field;
    }

    if (type->base != InvalidIndex)
        return FindField(schema, type->base, name);

    return nullptr;
}

const Field* potato::schematic::FindField(const Schema* schema, const Type* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;

    if (const Field* const field = FindField(schema, CastTo<TypeStruct>(type), name); field != nullptr)
        return field;
    if (const Field* const field = FindField(schema, CastTo<TypeMessage>(type), name); field != nullptr)
        return field;
    if (const Field* const field = FindField(schema, CastTo<TypeAttribute>(type), name); field != nullptr)
        return field;

    return nullptr;
}

const Field* potato::schematic::FindField(const Schema* schema, TypeIndex typeIndex, std::string_view name) noexcept
{
    return FindField(schema, GetType(schema, typeIndex), name);
}

const EnumItem* potato::schematic::FindItem(const Schema*, const TypeEnum* type, std::string_view name) noexcept
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

static const Annotation* FindAnnotation(std::span<const Annotation* const> annotations, TypeIndex attributeTypeIndex) noexcept
{
    for (const Annotation* const annotation : annotations)
    {
        if (annotation->attribute == attributeTypeIndex)
            return annotation;
    }

    return nullptr;
}

static const Annotation* FindAnnotation(const Schema* schema, std::span<const Annotation* const> annotations, std::string_view name) noexcept
{
    for (const Annotation* const annotation : annotations)
    {
        const TypeAttribute* const attribute = CastTo<TypeAttribute>(GetType(schema, annotation->attribute));
        if (attribute == nullptr)
            continue;
        if (attribute->name == name)
            return annotation;
    }

    return nullptr;
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, const Type* type, const TypeAttribute* attribute) noexcept
{
    if (type == nullptr)
        return nullptr;
    if (attribute == nullptr)
        return nullptr;

    if (const Annotation* const annotation = ::FindAnnotation(type->annotations, attribute->index); annotation != nullptr)
        return annotation;

    if (const TypeStruct* const struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        return FindAnnotation(schema, struct_->base, attribute);

    return nullptr;
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, const Type* type, std::string_view name) noexcept
{
    if (type == nullptr)
        return nullptr;
    if (schema == nullptr)
        return nullptr;
    if (name.empty())
        return nullptr;

    if (const Annotation* const annotation = ::FindAnnotation(schema, type->annotations, name); annotation != nullptr)
        return annotation;

    if (const TypeStruct* const struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        return FindAnnotation(schema, struct_->base, name);

    return nullptr;
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, TypeIndex typeIndex, const TypeAttribute* attribute) noexcept
{
    return FindAnnotation(schema, GetType(schema, typeIndex), attribute);
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, TypeIndex typeIndex, std::string_view name) noexcept
{
    return FindAnnotation(schema, GetType(schema, typeIndex), name);
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, const Field* field, const TypeAttribute* attribute) noexcept
{
    if (field == nullptr)
        return nullptr;
    if (attribute == nullptr)
        return nullptr;

    return ::FindAnnotation(field->annotations, attribute->index);
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, const Field* field, std::string_view name) noexcept
{
    if (field == nullptr)
        return nullptr;
    if (schema == nullptr)
        return nullptr;
    if (name.empty())
        return nullptr;

    return ::FindAnnotation(schema, field->annotations, name);
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, const EnumItem* item, const TypeAttribute* attribute) noexcept
{
    if (item == nullptr)
        return nullptr;
    if (attribute == nullptr)
        return nullptr;

    return ::FindAnnotation(item->annotations, attribute->index);
}

const Annotation* potato::schematic::FindAnnotation(const Schema* schema, const EnumItem* item, std::string_view name) noexcept
{
    if (item == nullptr)
        return nullptr;
    if (schema == nullptr)
        return nullptr;
    if (name.empty())
        return nullptr;

    return ::FindAnnotation(schema, item->annotations, name);
}

bool potato::schematic::HasAttribute(const Schema* schema, const Type* type, const TypeAttribute* attribute) noexcept
{
    return FindAnnotation(schema, type, attribute) != nullptr;
}

bool potato::schematic::HasAttribute(const Schema* schema, const Type* type, std::string_view name) noexcept
{
    return FindAnnotation(schema, type, name) != nullptr;
}

bool potato::schematic::HasAttribute(const Schema* schema, const Field* field, const TypeAttribute* attribute) noexcept
{
    return FindAnnotation(schema, field, attribute) != nullptr;
}

bool potato::schematic::HasAttribute(const Schema* schema, const Field* field, std::string_view name) noexcept
{
    return FindAnnotation(schema, field, name) != nullptr;
}

bool potato::schematic::HasAttribute(const Schema* schema, const EnumItem* item, const TypeAttribute* attribute) noexcept
{
    return FindAnnotation(schema, item, attribute) != nullptr;
}

bool potato::schematic::HasAttribute(const Schema* schema, const EnumItem* item, std::string_view name) noexcept
{
    return FindAnnotation(schema, item, name) != nullptr;
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

const Type* potato::schematic::FindType(const Schema* schema, ModuleIndex moduleIndex, std::string_view name) noexcept
{
    if (schema == nullptr)
        return nullptr;

    for (const Type* type : schema->types)
    {
        if (type->parent == moduleIndex && type->name == name)
            return type;
    }
    return nullptr;
}

static const Value* FindArgument(const Schema* schema, const std::span<const Argument>& arguments, const Field* field) noexcept
{
    if (field == nullptr)
        return nullptr;

    for (const Argument& argument : arguments)
    {
        if (argument.field == field->index)
            return GetValue(schema, argument.value);
    }

    return GetValue(schema, field->value);
}

const Value* potato::schematic::FindArgument(const Schema* schema, const ValueObject* object, const Field* field) noexcept
{
    if (object == nullptr)
        return nullptr;

    return ::FindArgument(schema, object->fields, field);
}

const Value* potato::schematic::FindArgument(const Schema* schema, const ValueObject* object, std::string_view name) noexcept
{
    if (object == nullptr)
        return nullptr;

    return FindArgument(schema, object, FindField(schema, object->type, name));
}

const Value* potato::schematic::FindArgument(const Schema* schema, const Annotation* annotation, const Field* field) noexcept
{
    if (annotation == nullptr)
        return nullptr;

    return ::FindArgument(schema, annotation->arguments, field);
}

const Value* potato::schematic::FindArgument(const Schema* schema, const Annotation* annotation, std::string_view name) noexcept
{
    if (annotation == nullptr)
        return nullptr;

    return FindArgument(schema, annotation, FindField(schema, annotation->attribute, name));
}

bool potato::schematic::IsA(const Schema* schema, const Type* type, const Type* parent) noexcept
{
    if (type == nullptr || parent == nullptr)
        return false;

    if (type == parent)
        return true;

    while (type != nullptr)
    {
        if (type == parent)
            return true;

        if (const TypeStruct* const struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        {
            if (struct_->base == InvalidIndex)
                break;
            type = GetType(schema, struct_->base);
        }
        else if (const TypeEnum* const enum_ = CastTo<TypeEnum>(type))
        {
            if (enum_->base == InvalidIndex)
                break;
            type = GetType(schema, enum_->base);
        }
        else
        {
            break;
        }
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
