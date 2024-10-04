// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schema_gen.h"

#include "schematic/utility.h"

#include <fmt/core.h>

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

const Schema* SchemaGenerator::Compile(IRModule* module)
{
    module_ = module;
    schema_ = arena_.New<Schema>();
    types_ = arena_.NewArray<Type*>(module->types.Size());
    modules_ = arena_.NewArray<Module*>(2);

    Module* const root = arena_.New<Module>();
    schema_->root = root;
    root->filename = arena_.NewString(module->filename);

    modules_.PushBack(arena_, root);

    for (IRType* const irTypeIter : module->types)
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

Type* SchemaGenerator::Resolve(IRType* inIrType)
{
    if (inIrType == nullptr)
        return nullptr;

    if (inIrType->type != nullptr)
        return inIrType->type;

    CreateType(inIrType);
    return inIrType->type;
}

void SchemaGenerator::CreateType(IRType* inIrType)
{
    if (IRTypeAlias* irType = CastTo<IRTypeAlias>(inIrType); irType != nullptr)
    {
        Type* const target = Resolve(irType->target);

        TypeAlias* const type = arena_.New<TypeAlias>();
        types_.PushBack(arena_, type);
        type->name = arena_.NewString(irType->name);
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);
        type->type = target;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeAttribute* irType = CastTo<IRTypeAttribute>(inIrType); irType != nullptr)
    {
        TypeAttribute* const type = arena_.New<TypeAttribute>();
        types_.PushBack(arena_, type);
        type->name = arena_.NewString(irType->name);
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);

        Array<Field> fields = arena_.NewArray<Field>(irType->fields.Size());
        for (IRAttributeField* const irField : irType->fields)
        {
            Field& field = fields.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.type = Resolve(irField->type);
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
        types_.PushBack(arena_, type);
        type->name = arena_.NewString(irType->name);
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);
        type->base = CastTo<TypeInt>(base);

        Array<EnumItem> items = arena_.NewArray<EnumItem>(irType->items.Size());
        for (IREnumItem* const irItem : irType->items)
        {
            EnumItem& item = items.EmplaceBack(arena_);
            item.name = arena_.NewString(irItem->name);
        }
        type->items = items;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    if (IRTypeMessage* irType = CastTo<IRTypeMessage>(inIrType); irType != nullptr)
    {
        TypeMessage* const type = arena_.New<TypeMessage>();
        types_.PushBack(arena_, type);
        type->name = arena_.NewString(irType->name);
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);

        Array<Field> fields = arena_.NewArray<Field>(irType->fields.Size());
        for (IRMessageField* const irField : irType->fields)
        {
            Field& field = fields.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.type = Resolve(irField->type);
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
        types_.PushBack(arena_, type);
        type->name = arena_.NewString(irType->name);
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);
        type->base = CastTo<TypeStruct>(base);

        Array<Field> fields = arena_.NewArray<Field>(irType->fields.Size());
        for (IRStructField* const irField : irType->fields)
        {
            Field& field = fields.EmplaceBack(arena_);
            field.name = arena_.NewString(irField->name);
            field.type = Resolve(irField->type);
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

            TypeStruct* const type = arena_.New<TypeStruct>();
            types_.PushBack(arena_, type);
            type->name = arena_.NewString(irType->name);
            type->owner = schema_->root;
            type->line = LineOf(irVersion->ast);
            type->base = CastTo<TypeStruct>(base);

            Array<Field> fields = arena_.NewArray<Field>(irVersion->fields.Size());
            for (IRStructField* const irField : irVersion->fields)
            {
                Field& field = fields.EmplaceBack(arena_);
                field.name = arena_.NewString(irField->name);
                field.type = Resolve(irField->type);
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
        return;
    }

    if (IRTypeBuiltin* irType = CastTo<IRTypeBuiltin>(inIrType); irType != nullptr)
    {
        if (irType->typeKind == TypeKind::Bool)
        {
            TypeBool* const type = arena_.New<TypeBool>();
            type->name = arena_.NewString(irType->name);
            type->owner = schema_->root;
            type->line = LineOf(irType->ast);
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Float)
        {
            TypeFloat* const type = arena_.New<TypeFloat>();
            type->name = arena_.NewString(irType->name);
            type->owner = schema_->root;
            type->line = LineOf(irType->ast);
            type->width = irType->width;
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Int)
        {
            TypeInt* const type = arena_.New<TypeInt>();
            type->name = arena_.NewString(irType->name);
            type->owner = schema_->root;
            type->line = LineOf(irType->ast);
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
            type->owner = schema_->root;
            type->line = LineOf(irType->ast);
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        if (irType->typeKind == TypeKind::Type)
        {
            TypeType* const type = arena_.New<TypeType>();
            type->name = arena_.NewString(irType->name);
            type->owner = schema_->root;
            type->line = LineOf(irType->ast);
            types_.PushBack(arena_, type);
            inIrType->type = type;
            return;
        }

        // FIXME: Error, unexpected builtin type kind
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
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);
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
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);
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
        type->owner = schema_->root;
        type->line = LineOf(irType->ast);
        type->type = target;

        types_.PushBack(arena_, type);
        inIrType->type = type;
        return;
    }

    // FIXME: Error, unhandled IR Type
}

std::uint32_t SchemaGenerator::LineOf(const AstNode* node)
{
    return 0;
}
