// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schema_gen.h"

#include "schematic/utility.h"

#include <fmt/core.h>

#include <cassert>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

template <typename... Args>
static const char* NewStringFmt(ArenaAllocator& arena_, fmt::format_string<Args...> format, Args&&... args)
{
    const int length = fmt::formatted_size(format, args...);
    char* const buffer = static_cast<char*>(arena_.Allocate(length + 1, 1));
    fmt::format_to(buffer, format, std::forward<Args>(args)...);
    buffer[length] = '\0';
    return buffer;
}

const Schema* SchemaGenerator::Compile(IRSchema* irSchema)
{
    schema_ = arena_.New<Schema>();
    types_ = arena_.NewArray<Type*>(irSchema->types.Size());
    modules_ = arena_.NewArray<Module>(irSchema->modules.Size());

    schema_->root = CreateModule(irSchema->root);

    for (IRType* const irTypeIter : irSchema->root->types)
    {
        // the type may have already been resolved into a real type as a dependency
        if (irTypeIter->type != nullptr)
            continue;

        CreateType(irTypeIter);
    }

    schema_->types = types_;
    schema_->modules = modules_;
    schema_->fields = fields_;
    return schema_;
}

ModuleIndex SchemaGenerator::CreateModule(IRModule* irModule)
{
    irModule->index = static_cast<ModuleIndex>(modules_.Size());
    modules_.EmplaceBack(arena_);
    modules_[irModule->index].filename = arena_.NewString(irModule->filename);

    Array<ModuleIndex> imports = arena_.NewArray<ModuleIndex>(irModule->imports.Size());
    for (IRImport* const irImport : irModule->imports)
    {
        if (irImport->resolved->index == InvalidIndex)
            CreateModule(irImport->resolved);

        imports.PushBack(arena_, irImport->resolved->index);
    }
    modules_[irModule->index].imports = imports;

    return irModule->index;
}

Type* SchemaGenerator::Resolve(IRType* inIrType)
{
    if (inIrType == nullptr)
        return nullptr;

    if (inIrType->type != nullptr)
        return inIrType->type;

    CreateType(inIrType);
    assert(inIrType->type != nullptr);

    return inIrType->type;
}

TypeIndex SchemaGenerator::ResolveIndex(IRType* inIrType)
{
    if (Resolve(inIrType) == nullptr)
        return InvalidIndex;

    return inIrType->index;
}

void SchemaGenerator::CreateType(IRType* inIrType)
{
    if (IRTypeAlias* irType = CastTo<IRTypeAlias>(inIrType); irType != nullptr)
    {
        Type* const target = Resolve(irType->target);

        TypeAlias* const type = arena_.New<TypeAlias>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->type = irType->target->index;
        type->annotations = CreateAnnotations(irType->annotations);

        irType->index = static_cast<TypeIndex>(types_.Size());
        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeAttribute* irType = CastTo<IRTypeAttribute>(inIrType); irType != nullptr)
    {
        TypeAttribute* const type = arena_.New<TypeAttribute>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->annotations = CreateAnnotations(irType->annotations);

        for (IRField* const irField : irType->fields)
        {
            Resolve(irField->type);
            Resolve(irField->value);
        }

        irType->index = static_cast<TypeIndex>(types_.Size());

        type->fields.start = static_cast<FieldIndex>(fields_.Size());
        for (IRField* const irField : irType->fields)
        {
            irField->index = static_cast<std::uint32_t>(fields_.Size());

            Field& field = fields_.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.index = irField->index;
            field.type = ResolveIndex(irField->type);
            field.value = Resolve(irField->value);
            field.parent = inIrType->index;
            field.location = irField->location;
        }
        type->fields.count = static_cast<std::uint32_t>(fields_.Size() - type->fields.start);

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeEnum* irType = CastTo<IRTypeEnum>(inIrType); irType != nullptr)
    {
        const Type* const base = Resolve(irType->base);

        TypeEnum* const type = arena_.New<TypeEnum>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        if (irType->base != nullptr)
            type->base = irType->base->index;
        type->annotations = CreateAnnotations(irType->annotations);

        Array<EnumItem> items = arena_.NewArray<EnumItem>(irType->items.Size());
        for (IREnumItem* const irItem : irType->items)
        {
            EnumItem& item = items.EmplaceBack(arena_);
            irItem->item = &item;
            item.name = arena_.NewString(irItem->name);
            ValueInt* const value = arena_.New<ValueInt>();
            value->value = irItem->value;
            value->location = irItem->location;
            value->parent = schema_->root;
            item.value = value;
            item.location = irItem->location;
            item.annotations = CreateAnnotations(irItem->annotations);
        }
        type->items = items;

        irType->index = static_cast<TypeIndex>(types_.Size());

        for (EnumItem& item : items)
            item.parent = irType->index;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeMessage* irType = CastTo<IRTypeMessage>(inIrType); irType != nullptr)
    {
        TypeMessage* const type = arena_.New<TypeMessage>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->annotations = CreateAnnotations(irType->annotations);

        for (IRField* const irField : irType->fields)
        {
            Resolve(irField->type);
            Resolve(irField->value);
        }

        irType->index = static_cast<TypeIndex>(types_.Size());

        type->fields.start = static_cast<FieldIndex>(fields_.Size());
        for (IRField* const irField : irType->fields)
        {
            irField->index = static_cast<std::uint32_t>(fields_.Size());

            Field& field = fields_.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.index = irField->index;
            field.type = ResolveIndex(irField->type);
            field.value = Resolve(irField->value);
            field.proto = irField->proto;
            field.parent = irType->index;
            field.location = irField->location;
        }
        type->fields.count = static_cast<std::uint32_t>(fields_.Size() - type->fields.start);

        irType->index = static_cast<TypeIndex>(types_.Size());

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeStruct* irType = CastTo<IRTypeStruct>(inIrType); irType != nullptr)
    {
        const Type* const base = Resolve(irType->base);

        TypeStruct* const type = arena_.New<TypeStruct>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        if (irType->base != nullptr)
            type->base = irType->base->index;
        type->annotations = CreateAnnotations(irType->annotations);

        // Resolve types (and values, which reference types) before allocating fields;
        // otherwise a referenced struct might instantiate fields and make these ones
        // non-contiguous
        for (IRField* const irField : irType->fields)
        {
            Resolve(irField->type);
            Resolve(irField->value);
        }

        irType->index = static_cast<TypeIndex>(types_.Size());

        type->fields.start = static_cast<FieldIndex>(fields_.Size());
        for (IRField* const irField : irType->fields)
        {
            irField->index = static_cast<std::uint32_t>(fields_.Size());

            Field& field = fields_.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.index = irField->index;
            field.type = ResolveIndex(irField->type);
            field.value = Resolve(irField->value);
            field.parent = irType->index;
            field.location = irField->location;
        }
        type->fields.count = static_cast<std::uint32_t>(fields_.Size() - type->fields.start);

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeStructVersioned* irType = CastTo<IRTypeStructVersioned>(inIrType); irType != nullptr)
    {
        IRTypeStruct* maxVersion = 0;

        for (IRTypeStruct* irVersion : irType->versions)
        {
            const Type* const base = Resolve(irVersion->base);

            for (std::uint32_t version = irVersion->version.min; version <= irVersion->version.max; ++version)
            {
                TypeStruct* const type = arena_.New<TypeStruct>();
                type->name = NewStringFmt(arena_, "{}#{}", irVersion->name, version);
                type->parent = irType->parent->index;
                type->location = irType->location;
                if (irVersion->base != nullptr)
                    type->base = irVersion->base->index;
                type->version = version;
                type->annotations = CreateAnnotations(irVersion->annotations);

                for (IRField* const irField : irVersion->fields)
                {
                    if (irField->version.min != 0 &&
                        (irField->version.min > version ||
                            irField->version.max < version))
                    {
                        continue;
                    }

                    Resolve(irField->type);
                    Resolve(irField->value);
                }

                irVersion->index = static_cast<TypeIndex>(types_.Size());

                type->fields.start = static_cast<std::uint32_t>(fields_.Size());
                for (IRField* const irField : irVersion->fields)
                {
                    if (irField->version.min != 0 &&
                        (irField->version.min > version ||
                            irField->version.max < version))
                    {
                        continue;
                    }

                    irField->index = static_cast<std::uint32_t>(fields_.Size());

                    Field& field = fields_.EmplaceBack(arena_);
                    field.name = arena_.NewString(irField->name);
                    field.index = irField->index;
                    field.type = ResolveIndex(irField->type);
                    field.value = Resolve(irField->value);
                    field.parent = irType->index;
                    field.location = irField->location;
                }
                type->fields.count = static_cast<std::uint32_t>(fields_.Size() - type->fields.start);

                irVersion->type = type;

                if (maxVersion == nullptr || type->version > maxVersion->version.max)
                    maxVersion = irVersion;

                types_.PushBack(arena_, type);
            }
        }

        irType->type = maxVersion->type;

        TypeAlias* type = arena_.New<TypeAlias>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->type->parent;
        type->location = irType->location;
        type->type = maxVersion->index;

        irType->index = static_cast<TypeIndex>(types_.Size());
        types_.PushBack(arena_, type);
        return;
    }

    if (IRTypeBuiltin* irType = CastTo<IRTypeBuiltin>(inIrType); irType != nullptr)
    {
        if (irType->typeKind == TypeKind::Bool)
        {
            TypeBool* const type = arena_.New<TypeBool>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            irType->index = static_cast<TypeIndex>(types_.Size());
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Float)
        {
            TypeFloat* const type = arena_.New<TypeFloat>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            type->width = irType->width;
            irType->index = static_cast<TypeIndex>(types_.Size());
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Int)
        {
            TypeInt* const type = arena_.New<TypeInt>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            type->width = irType->width;
            type->isSigned = irType->isSigned;
            irType->index = static_cast<TypeIndex>(types_.Size());
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::String)
        {
            TypeString* const type = arena_.New<TypeString>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            irType->index = static_cast<TypeIndex>(types_.Size());
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Type)
        {
            TypeType* const type = arena_.New<TypeType>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            irType->index = static_cast<TypeIndex>(types_.Size());
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        assert(false);
        return;
    }

    if (IRTypeIndirectArray* irType = CastTo<IRTypeIndirectArray>(inIrType); irType != nullptr)
    {
        const Type* const target = Resolve(irType->target);
        if (target == nullptr)
            return;

        TypeArray* const type = arena_.New<TypeArray>();
        if (irType->size != 0)
            type->name = NewStringFmt(arena_, "{}[{}]", target->name, irType->size);
        else
            type->name = NewStringFmt(arena_, "{}[]", target->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->elements = irType->target->index;
        type->size = irType->size;

        irType->index = static_cast<TypeIndex>(types_.Size());
        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeIndirectNullable* irType = CastTo<IRTypeIndirectNullable>(inIrType); irType != nullptr)
    {
        const Type* const target = Resolve(irType->target);

        TypeNullable* const type = arena_.New<TypeNullable>();
        type->name = NewStringFmt(arena_, "{}?", target->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->target = irType->target->index;

        irType->index = static_cast<TypeIndex>(types_.Size());
        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeIndirectPointer* irType = CastTo<IRTypeIndirectPointer>(inIrType); irType != nullptr)
    {
        const Type* const target = Resolve(irType->target);

        TypeNullable* const type = arena_.New<TypeNullable>();
        type->name = NewStringFmt(arena_, "{}*", target->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->target = irType->target->index;

        irType->index = static_cast<TypeIndex>(types_.Size());
        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    assert(false);
}

ReadOnlySpan<Annotation*> SchemaGenerator::CreateAnnotations(Array<IRAnnotation*> irAnnotations)
{
    Array<Annotation*> annotations = arena_.NewArray<Annotation*>(irAnnotations.Size());
    for (IRAnnotation* irAnnotation : irAnnotations)
    {
        Annotation* const annotation = arena_.New<Annotation>();
        annotation->attribute = ResolveIndex(irAnnotation->attribute);
        annotation->location = irAnnotation->location;

        Array<Argument> arguments = arena_.NewArray<Argument>(irAnnotation->arguments.Size());
        for (IRAnnotationArgument* irArgument : irAnnotation->arguments)
        {
            Argument& arg = arguments.EmplaceBack(arena_);
            arg.field = irArgument->field->index;
            arg.value = Resolve(irArgument->value);
            arg.location = irArgument->location;
        }
        annotation->arguments = arguments;

        annotations.PushBack(arena_, annotation);
    }
    return annotations;
}

Value* SchemaGenerator::Resolve(IRValue* value)
{
    if (value == nullptr)
        return nullptr;

    if (value->value != nullptr)
        return value->value;

    if (IRValueLiteral* literal = CastTo<IRValueLiteral>(value))
    {
        if (const AstNodeLiteralBool* const node = CastTo<AstNodeLiteralBool>(value->ast); node != nullptr)
        {
            ValueBool* const result = arena_.New<ValueBool>();
            result->location = value->location;
            result->value = node->value;
            value->value = result;
            return result;
        }

        if (const AstNodeLiteralInt* const node = CastTo<AstNodeLiteralInt>(value->ast); node != nullptr)
        {
            ValueInt* const result = arena_.New<ValueInt>();
            result->location = value->location;
            result->value = node->value;
            value->value = result;
            return result;
        }

        if (const AstNodeLiteralFloat* const node = CastTo<AstNodeLiteralFloat>(value->ast); node != nullptr)
        {
            ValueFloat* const result = arena_.New<ValueFloat>();
            result->location = value->location;
            result->value = node->value;
            value->value = result;
            return result;
        }

        if (const AstNodeLiteralNull* const node = CastTo<AstNodeLiteralNull>(value->ast); node != nullptr)
        {
            ValueNull* const result = arena_.New<ValueNull>();
            result->location = value->location;
            value->value = result;
            return result;
        }

        if (const AstNodeLiteralString* const node = CastTo<AstNodeLiteralString>(value->ast); node != nullptr)
        {
            ValueString* const result = arena_.New<ValueString>();
            result->location = value->location;
            result->value = arena_.NewString(node->value);
            value->value = result;
            return result;
        }

        assert(false);
        return nullptr;
    }

    if (IRValueType* type = CastTo<IRValueType>(value))
    {
        ValueType* const result = arena_.New<ValueType>();
        result->location = value->location;
        result->type = ResolveIndex(type->target);
        value->value = result;
        return result;
    }

    if (IRValueEnumItem* item = CastTo<IRValueEnumItem>(value))
    {
        ValueEnum* const result = arena_.New<ValueEnum>();
        result->location = value->location;
        const TypeEnum* const type = CastTo<TypeEnum>(Resolve(item->type));
        result->item = FindItem(schema_, type, item->item->name);
        value->value = result;
        return result;
    }

    if (IRValueInitializerList* initializerList = CastTo<IRValueInitializerList>(value))
    {
        if (initializerList->type->kind == IRTypeKind::IndirectArray)
        {
            ValueArray* const result = arena_.New<ValueArray>();
            result->type = ResolveIndex(initializerList->type);
            result->location = value->location;

            Array<Value*> elements = arena_.NewArray<Value*>(initializerList->positional.Size());
            for (IRValue* const element : initializerList->positional)
                elements.PushBack(arena_, Resolve(element));

            result->elements = elements;
            value->value = result;
            return result;
        }

        if (IRTypeStruct* typeStruct = CastTo<IRTypeStruct>(initializerList->type); typeStruct != nullptr)
        {
            ValueObject* const result = arena_.New<ValueObject>();
            result->type = ResolveIndex(initializerList->type);
            result->location = value->location;

            Array<Argument> fields = arena_.NewArray<Argument>(initializerList->positional.Size() + initializerList->named.Size());

            std::uint32_t fieldIndex = 0;
            for (IRValue* const element : initializerList->positional)
            {
                Argument& field = fields.EmplaceBack(arena_);
                field.field = typeStruct->fields[fieldIndex]->index;
                field.value = Resolve(element);
                ++fieldIndex;
            }

            for (IRInitializerNamedArgument* const named : initializerList->named)
            {
                Argument& field = fields.EmplaceBack(arena_);
                field.field = named->field->index;
                field.value = Resolve(named->value);
                ++fieldIndex;
            }

            result->fields = fields;
            value->value = result;
            return result;
        }
    }

    assert(false);
    return nullptr;
}
