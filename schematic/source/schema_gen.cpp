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
        if (irImport->resolved->index == 0)
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

void SchemaGenerator::CreateType(IRType* inIrType)
{
    if (IRTypeAlias* irType = CastTo<IRTypeAlias>(inIrType); irType != nullptr)
    {
        Type* const target = Resolve(irType->target);

        TypeAlias* const type = arena_.New<TypeAlias>();
        type->name = arena_.NewString(irType->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->type = target;
        type->annotations = CreateAnnotations(irType->annotations);

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeAttribute* irType = CastTo<IRTypeAttribute>(inIrType); irType != nullptr)
    {
        TypeAttribute* const type = arena_.New<TypeAttribute>();
        type->name = arena_.NewString(irType->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->annotations = CreateAnnotations(irType->annotations);

        Array<Field> fields = arena_.NewArray<Field>(irType->fields.Size());
        for (IRAttributeField* const irField : irType->fields)
        {
            Field& field = fields.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.type = Resolve(irField->type);
            field.value = Resolve(irField->value);
            field.owner = type;
            field.location = irField->location;
        }
        type->fields = fields;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeEnum* irType = CastTo<IRTypeEnum>(inIrType); irType != nullptr)
    {
        const Type* const base = Resolve(irType->base);

        TypeEnum* const type = arena_.New<TypeEnum>();
        type->name = arena_.NewString(irType->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->base = CastTo<TypeInt>(base);
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
            value->owner = schema_->root;
            item.value = value;
            item.owner = type;
            item.location = irItem->location;
            item.annotations = CreateAnnotations(irItem->annotations);
        }
        type->items = items;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeMessage* irType = CastTo<IRTypeMessage>(inIrType); irType != nullptr)
    {
        TypeMessage* const type = arena_.New<TypeMessage>();
        type->name = arena_.NewString(irType->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->annotations = CreateAnnotations(irType->annotations);

        Array<Field> fields = arena_.NewArray<Field>(irType->fields.Size());
        for (IRMessageField* const irField : irType->fields)
        {
            Field& field = fields.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.type = Resolve(irField->type);
            field.value = Resolve(irField->value);
            field.proto = irField->proto;
            field.owner = type;
            field.location = irField->location;
        }
        type->fields = fields;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeStruct* irType = CastTo<IRTypeStruct>(inIrType); irType != nullptr)
    {
        const Type* const base = Resolve(irType->base);

        TypeStruct* const type = arena_.New<TypeStruct>();
        type->name = arena_.NewString(irType->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->base = CastTo<TypeStruct>(base);
        type->annotations = CreateAnnotations(irType->annotations);

        Array<Field> fields = arena_.NewArray<Field>(irType->fields.Size());
        for (IRStructField* const irField : irType->fields)
        {
            Field& field = fields.EmplaceBack(arena_);
            irField->field = &field;
            field.name = arena_.NewString(irField->name);
            field.type = Resolve(irField->type);
            field.value = Resolve(irField->value);
            field.owner = type;
            field.location = irField->location;
        }
        type->fields = fields;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeStructVersioned* irType = CastTo<IRTypeStructVersioned>(inIrType); irType != nullptr)
    {
        std::uint32_t maxVersion = 0;

        for (IRTypeStruct* irVersion : irType->versions)
        {
            const Type* const base = Resolve(irVersion->base);

            for (std::uint32_t version = irVersion->version.min; version <= irVersion->version.max; ++version)
            {
                TypeStruct* const type = arena_.New<TypeStruct>();
                type->name = NewStringFmt(arena_, "{}#{}", irVersion->name, version);
                type->owner = irType->owner->index;
                type->location = irType->location;
                type->base = CastTo<TypeStruct>(base);
                type->version = version;
                type->annotations = CreateAnnotations(irVersion->annotations);

                std::uint32_t fieldCount = 0; // FIXME: do this in IR
                for (IRStructField* const irField : irVersion->fields)
                {
                    if (irField->version.min == 0)
                        ++fieldCount;
                    else if (irField->version.min <= version && irField->version.max >= version)
                        ++fieldCount;
                }

                Array<Field> fields = arena_.NewArray<Field>(fieldCount);
                for (IRStructField* const irField : irVersion->fields)
                {
                    if (irField->version.min != 0 &&
                        (irField->version.min > version ||
                            irField->version.max < version))
                    {
                        continue;
                    }

                    Field& field = fields.EmplaceBack(arena_);
                    irField->field = &field;
                    field.name = arena_.NewString(irField->name);
                    field.type = Resolve(irField->type);
                    field.owner = type;
                    field.location = irField->location;
                }
                type->fields = fields;

                types_.PushBack(arena_, type);
                irVersion->type = type;

                if (type->version > maxVersion)
                {
                    maxVersion = type->version;
                    irType->type = type;
                }
            }
        }

        TypeAlias* type = arena_.New<TypeAlias>();
        type->name = arena_.NewString(irType->name);
        type->owner = irType->type->owner;
        type->location = irType->location;
        type->type = irType->type;
        types_.PushBack(arena_, type);
        return;
    }

    if (IRTypeBuiltin* irType = CastTo<IRTypeBuiltin>(inIrType); irType != nullptr)
    {
        if (irType->typeKind == TypeKind::Bool)
        {
            TypeBool* const type = arena_.New<TypeBool>();
            type->name = arena_.NewString(irType->name);
            type->owner = irType->owner->index;
            type->location = irType->location;
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Float)
        {
            TypeFloat* const type = arena_.New<TypeFloat>();
            type->name = arena_.NewString(irType->name);
            type->owner = irType->owner->index;
            type->location = irType->location;
            type->width = irType->width;
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Int)
        {
            TypeInt* const type = arena_.New<TypeInt>();
            type->name = arena_.NewString(irType->name);
            type->owner = irType->owner->index;
            type->location = irType->location;
            type->width = irType->width;
            type->isSigned = irType->isSigned;
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::String)
        {
            TypeString* const type = arena_.New<TypeString>();
            type->name = arena_.NewString(irType->name);
            type->owner = irType->owner->index;
            type->location = irType->location;
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Type)
        {
            TypeType* const type = arena_.New<TypeType>();
            type->name = arena_.NewString(irType->name);
            type->owner = irType->owner->index;
            type->location = irType->location;
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

        TypeArray* const type = arena_.New<TypeArray>();
        if (irType->size != 0)
            type->name = NewStringFmt(arena_, "{}[{}]", target->name, irType->size);
        else
            type->name = NewStringFmt(arena_, "{}[]", target->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->type = target;
        type->size = irType->size;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeIndirectNullable* irType = CastTo<IRTypeIndirectNullable>(inIrType); irType != nullptr)
    {
        const Type* const target = Resolve(irType->target);

        TypeNullable* const type = arena_.New<TypeNullable>();
        type->name = NewStringFmt(arena_, "{}?", target->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->type = target;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeIndirectPointer* irType = CastTo<IRTypeIndirectPointer>(inIrType); irType != nullptr)
    {
        const Type* const target = Resolve(irType->target);

        TypeNullable* const type = arena_.New<TypeNullable>();
        type->name = NewStringFmt(arena_, "{}*", target->name);
        type->owner = irType->owner->index;
        type->location = irType->location;
        type->type = target;

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
        annotation->attribute = CastTo<TypeAttribute>(Resolve(irAnnotation->attribute));
        annotation->location = irAnnotation->location;

        Array<Argument> arguments = arena_.NewArray<Argument>(irAnnotation->arguments.Size());
        for (IRAnnotationArgument* irArgument : irAnnotation->arguments)
        {
            Argument& arg = arguments.EmplaceBack(arena_);
            arg.field = FindField(annotation->attribute, irArgument->field->name);
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

    if (IRValueLiteral* literal = CastTo<IRValueLiteral>(value))
    {
        if (const AstNodeLiteralBool* const node = CastTo<AstNodeLiteralBool>(value->ast); node != nullptr)
        {
            ValueBool* const result = arena_.New<ValueBool>();
            result->location = value->location;
            result->value = node->value;
            return result;
        }

        if (const AstNodeLiteralInt* const node = CastTo<AstNodeLiteralInt>(value->ast); node != nullptr)
        {
            ValueInt* const result = arena_.New<ValueInt>();
            result->location = value->location;
            result->value = node->value;
            return result;
        }

        if (const AstNodeLiteralFloat* const node = CastTo<AstNodeLiteralFloat>(value->ast); node != nullptr)
        {
            ValueFloat* const result = arena_.New<ValueFloat>();
            result->location = value->location;
            result->value = node->value;
            return result;
        }

        if (const AstNodeLiteralNull* const node = CastTo<AstNodeLiteralNull>(value->ast); node != nullptr)
        {
            ValueNull* const result = arena_.New<ValueNull>();
            result->location = value->location;
            return result;
        }

        if (const AstNodeLiteralString* const node = CastTo<AstNodeLiteralString>(value->ast); node != nullptr)
        {
            ValueString* const result = arena_.New<ValueString>();
            result->location = value->location;
            result->value = arena_.NewString(node->value);
            return result;
        }

        assert(false);
        return nullptr;
    }

    if (IRValueType* type = CastTo<IRValueType>(value))
    {
        ValueType* const result = arena_.New<ValueType>();
        result->location = value->location;
        result->type = Resolve(type->target);
        return result;
    }

    if (IRValueEnumItem* item = CastTo<IRValueEnumItem>(value))
    {
        ValueEnum* const result = arena_.New<ValueEnum>();
        result->location = value->location;
        const TypeEnum* const type = CastTo<TypeEnum>(Resolve(item->type));
        result->item = FindItem(type, item->item->name);
        return result;
    }

    if (IRValueInitializerList* initializerList = CastTo<IRValueInitializerList>(value))
    {
        if (initializerList->type->kind == IRTypeKind::IndirectArray)
        {
            ValueArray* const result = arena_.New<ValueArray>();
            result->type = Resolve(initializerList->type);
            result->location = value->location;

            Array<Value*> elements = arena_.NewArray<Value*>(initializerList->positional.Size());
            for (IRValue* const element : initializerList->positional)
                elements.PushBack(arena_, Resolve(element));

            result->elements = elements;
            return result;
        }

        if (IRTypeStruct* typeStruct = CastTo<IRTypeStruct>(initializerList->type); typeStruct != nullptr)
        {
            ValueObject* const result = arena_.New<ValueObject>();
            result->type = Resolve(initializerList->type);
            result->location = value->location;

            Array<Argument> fields = arena_.NewArray<Argument>(initializerList->positional.Size() + initializerList->named.Size());

            std::uint32_t fieldIndex = 0;
            for (IRValue* const element : initializerList->positional)
            {
                Argument& field = fields.EmplaceBack(arena_);
                field.field = typeStruct->fields[fieldIndex]->field;
                field.value = Resolve(element);
                ++fieldIndex;
            }

            for (IRInitializerNamedArgument* const named : initializerList->named)
            {
                Argument& field = fields.EmplaceBack(arena_);
                field.field = named->field->field;
                field.value = Resolve(named->value);
                ++fieldIndex;
            }

            result->fields = fields;
            return result;
        }
    }

    assert(false);
    return nullptr;
}
