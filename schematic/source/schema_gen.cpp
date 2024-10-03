// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schema_gen.h"

#include "schematic/utility.h"

using namespace potato::schematic;
using namespace potato::schematic::compiler;

const Schema* SchemaGenerator::Compile(IRModule* module)
{
    module_ = module;
    schema_ = arena_.New<Schema>();
    types_ = arena_.NewArray<Type*>(module->types.Size());

    Module* const root = arena_.New<Module>();
    root->filename = arena_.NewString(module->filename);

    for (IRType* const irTypeIter : module->types)
    {
        if (IRTypeAlias* irType = CastTo<IRTypeAlias>(irTypeIter); irType != nullptr)
        {
            Type* const target = Resolve(irType->target);

            TypeAlias* const type = arena_.New<TypeAlias>();
            types_.PushBack(arena_, type);
            type->name = arena_.NewString(irType->name);
            type->owner = root;
            type->line = LineOf(irType->ast);
            type->type = target;
            continue;
        }

        if (IRTypeAttribute* irType = CastTo<IRTypeAttribute>(irTypeIter); irType != nullptr)
        {
            TypeAttribute* const type = arena_.New<TypeAttribute>();
            types_.PushBack(arena_, type);
            type->name = arena_.NewString(irType->name);
            type->owner = root;
            type->line = LineOf(irType->ast);
            continue;
        }

        if (IRTypeEnum* irType = CastTo<IRTypeEnum>(irTypeIter); irType != nullptr)
        {
            const Type* const base = Resolve(irType->base);

            TypeEnum* const type = arena_.New<TypeEnum>();
            types_.PushBack(arena_, type);
            type->name = arena_.NewString(irType->name);
            type->owner = root;
            type->line = LineOf(irType->ast);
            type->base = CastTo<TypeInt>(base);
            continue;
        }

        if (IRTypeMessage* irType = CastTo<IRTypeMessage>(irTypeIter); irType != nullptr)
        {
            TypeMessage* const type = arena_.New<TypeMessage>();
            types_.PushBack(arena_, type);
            type->name = arena_.NewString(irType->name);
            type->owner = root;
            type->line = LineOf(irType->ast);
            continue;
        }

        if (IRTypeStruct* irType = CastTo<IRTypeStruct>(irTypeIter); irType != nullptr)
        {
            const Type* const base = Resolve(irType->base);

            TypeStruct* const type = arena_.New<TypeStruct>();
            types_.PushBack(arena_, type);
            type->name = arena_.NewString(irType->name);
            type->owner = root;
            type->line = LineOf(irType->ast);
            type->base = CastTo<TypeStruct>(base);
            continue;
        }

        if (IRTypeStructVersioned* irType = CastTo<IRTypeStructVersioned>(irTypeIter); irType != nullptr)
        {
            for (IRTypeStruct* irVersion : irType->versions)
            {
                const Type* const base = Resolve(irVersion->base);

                TypeStruct* const type = arena_.New<TypeStruct>();
                types_.PushBack(arena_, type);
                type->name = arena_.NewString(irType->name);
                type->owner = root;
                type->line = LineOf(irVersion->ast);
                type->base = CastTo<TypeStruct>(base);
            }
            continue;
        }

        if (IRTypeBuiltin* irType = CastTo<IRTypeBuiltin>(irTypeIter); irType != nullptr)
        {
            if (irType->typeKind == TypeKind::Bool)
            {
                TypeBool* const type = arena_.New<TypeBool>();
                type->name = arena_.NewString(irType->name);
                type->owner = root;
                type->line = LineOf(irType->ast);
            }
            else if (irType->typeKind == TypeKind::Float)
            {
                TypeFloat* const type = arena_.New<TypeFloat>();
                type->name = arena_.NewString(irType->name);
                type->owner = root;
                type->line = LineOf(irType->ast);
                type->width = irType->width;
            }
            else if (irType->typeKind == TypeKind::Int)
            {
                TypeInt* const type = arena_.New<TypeInt>();
                type->name = arena_.NewString(irType->name);
                type->owner = root;
                type->line = LineOf(irType->ast);
                type->width = irType->width;
                type->isSigned = irType->isSigned;
            }
            else if (irType->typeKind == TypeKind::String)
            {
                TypeString* const type = arena_.New<TypeString>();
                type->name = arena_.NewString(irType->name);
                type->owner = root;
                type->line = LineOf(irType->ast);
            }
            else if (irType->typeKind == TypeKind::Type)
            {
                TypeType* const type = arena_.New<TypeType>();
                type->name = arena_.NewString(irType->name);
                type->owner = root;
                type->line = LineOf(irType->ast);
            }
            else
            {
                // FIXME: error, invalid builtin type
            }
            continue;
        }

        // FIXME: error, invalid top-level IR type
    }

    return schema_;
}

Type* SchemaGenerator::Resolve(IRType* type)
{
    if (type == nullptr)
        return nullptr;

    if (type->type != nullptr)
        return type->type;

    switch (type->kind)
    {
        case IRTypeKind::IndirectArray:
            break;
        case IRTypeKind::IndirectIdentifier:
            // FIXME: error, not lowered
            break;
        case IRTypeKind::IndirectNullable:
            break;
        case IRTypeKind::IndirectPointer:
            break;
        case IRTypeKind::None:
        case IRTypeKind::Alias:
        case IRTypeKind::Attribute:
        case IRTypeKind::Enum:
        case IRTypeKind::Message:
        case IRTypeKind::Struct:
        case IRTypeKind::StructVersioned:
            // FIXME: error, not lowered
            break;
    }

    return nullptr;
}

std::uint32_t SchemaGenerator::LineOf(const AstNode* node)
{
    return 0;
}
