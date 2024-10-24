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
    schema_->root = irSchema->root->index;

    modules_ = arena_.NewArray<Module, ModuleIndex>(irSchema->maxModuleIndex.index, {});
    types_ = arena_.NewArray<const Type*, TypeIndex>(irSchema->maxTypeIndex.index, {});
    fields_ = arena_.NewArray<Field, FieldIndex>(irSchema->maxFieldIndex.index, {});
    enumItems_ = arena_.NewArray<EnumItem, EnumItemIndex>(irSchema->maxEnumItemIndex.index, {});
    annotations_ = arena_.NewArray<Annotation, AnnotationIndex>(irSchema->maxAnnotationIndex.index, {});
    values_ = arena_.NewArray<const Value*, ValueIndex>(irSchema->maxValueIndex.index, {});

    for (IRModule* const irModule : irSchema->modules)
    {
        if (irModule->index == InvalidIndex)
            continue;

        Module& module = modules_[irModule->index];
        module.filename = arena_.NewString(irModule->filename);

        Array<Import> imports = arena_.NewArrayCapacity<Import>(irModule->imports.Size());
        for (IRImport* const irImport : irModule->imports)
        {
            Import& import = imports.EmplaceBack(arena_);
            if (irImport->ast != nullptr)
                import.import = arena_.NewString(irImport->ast->target->value);

            import.module = irImport->resolved->index;
        }
        module.imports = imports;
    }

    for (IRType* const irType : irSchema->types)
    {
        assert(irType->index != InvalidIndex);

        types_[irType->index] = CreateType(irType);
    }

    for (IRValue* const irValue : irSchema->values)
    {
        assert(irValue->index != InvalidIndex);

        values_[irValue->index] = CreateValue(irValue);
    }

    schema_->types = types_;
    schema_->modules = modules_;
    schema_->fields = fields_;
    schema_->values = values_;
    schema_->enumItems = enumItems_;
    schema_->annotations = annotations_;
    return schema_;
}

const Type* SchemaGenerator::CreateType(IRType* inIrType)
{
    if (IRTypeAlias* irType = CastTo<IRTypeAlias>(inIrType); irType != nullptr)
    {
        TypeAlias* const type = arena_.New<TypeAlias>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->type = irType->target->index;
        type->annotations = CreateAnnotations(irType->annotations);
        return type;
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
            Field& field = fields_[irField->index];
            field.name = arena_.NewString(irField->name);
            field.index = irField->index;
            field.type = irField->type->index;
            field.value = ResolveIndex(irField->value);
            field.parent = irType->index;
            field.location = irField->location;
            field.annotations = CreateAnnotations(irField->annotations);
        }

        if (!irType->fields.IsEmpty())
        {
            type->fields.start = irType->fields.Front()->index;
            type->fields.count = static_cast<std::uint32_t>(irType->fields.Size());
        }

        return type;
    }

    if (IRTypeEnum* irType = CastTo<IRTypeEnum>(inIrType); irType != nullptr)
    {
        TypeEnum* const type = arena_.New<TypeEnum>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        if (irType->base != nullptr)
            type->base = irType->base->index;
        type->annotations = CreateAnnotations(irType->annotations);

        for (IREnumItem* const irItem : irType->items)
        {
            EnumItem& item = enumItems_[irItem->index];
            item.name = arena_.NewString(irItem->name);
            item.value = irItem->value;
            item.location = irItem->location;
            item.parent = irType->index;
            item.annotations = CreateAnnotations(irItem->annotations);
        }

        if (!irType->items.IsEmpty())
        {
            type->items.start = irType->items.Front()->index;
            type->items.count = static_cast<std::uint32_t>(irType->items.Size());
        }

        return type;
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
            Field& field = fields_[irField->index];
            field.name = arena_.NewString(irField->name);
            field.index = irField->index;
            field.type = irField->type->index;
            field.value = ResolveIndex(irField->value);
            field.proto = irField->proto;
            field.parent = irType->index;
            field.location = irField->location;
            field.annotations = CreateAnnotations(irField->annotations);
        }

        if (!irType->fields.IsEmpty())
        {
            type->fields.start = irType->fields.Front()->index;
            type->fields.count = static_cast<std::uint32_t>(irType->fields.Size());
        }

        return type;
    }

    if (IRTypeStruct* irType = CastTo<IRTypeStruct>(inIrType); irType != nullptr)
    {
        TypeStruct* const type = arena_.New<TypeStruct>();
        type->name = arena_.NewString(irType->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        if (irType->base != nullptr)
            type->base = irType->base->index;
        type->annotations = CreateAnnotations(irType->annotations);

        for (IRField* const irField : irType->fields)
        {
            Field& field = fields_[irField->index];
            field.name = arena_.NewString(irField->name);
            field.index = irField->index;
            field.type = irField->type->index;
            field.value = ResolveIndex(irField->value);
            field.parent = irType->index;
            field.location = irField->location;
            field.annotations = CreateAnnotations(irField->annotations);
        }

        if (!irType->fields.IsEmpty())
        {
            type->fields.start = irType->fields.Front()->index;
            type->fields.count = static_cast<std::uint32_t>(irType->fields.Size());
        }

        return type;
    }

    if (IRTypeStructVersioned* irType = CastTo<IRTypeStructVersioned>(inIrType); irType != nullptr)
    {
        TypeStruct* const type = arena_.New<TypeStruct>();
        type->name = NewStringFmt(arena_, "{}#{}", irType->name, irType->version);
        type->parent = irType->parent->index;
        type->location = irType->location;
        if (irType->base != nullptr)
            type->base = irType->base->index;
        type->version = irType->version;
        type->annotations = CreateAnnotations(irType->annotations);

        for (IRField* const irField : irType->fields)
        {
            Field& field = fields_[irField->index];
            field.name = arena_.NewString(irField->name);
            field.index = irField->index;
            field.type = irField->type->index;
            if (irField->value != nullptr)
                field.value = irField->value->index;
            field.parent = irType->index;
            field.location = irField->location;
            field.annotations = CreateAnnotations(irField->annotations);
        }

        if (!irType->fields.IsEmpty())
        {
            type->fields.start = irType->fields.Front()->index;
            type->fields.count = static_cast<std::uint32_t>(irType->fields.Size());
        }

        return type;
    }

    if (IRTypeBuiltin* irType = CastTo<IRTypeBuiltin>(inIrType); irType != nullptr)
    {
        if (irType->typeKind == TypeKind::Bool)
        {
            TypeBool* const type = arena_.New<TypeBool>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            return type;
        }

        if (irType->typeKind == TypeKind::Float)
        {
            TypeFloat* const type = arena_.New<TypeFloat>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            type->width = irType->width;
            return type;
        }

        if (irType->typeKind == TypeKind::Int)
        {
            TypeInt* const type = arena_.New<TypeInt>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            type->width = irType->width;
            type->isSigned = irType->isSigned;
            return type;
        }

        if (irType->typeKind == TypeKind::String)
        {
            TypeString* const type = arena_.New<TypeString>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            return type;
        }

        if (irType->typeKind == TypeKind::Type)
        {
            TypeType* const type = arena_.New<TypeType>();
            type->name = arena_.NewString(irType->name);
            type->parent = irType->parent->index;
            type->location = irType->location;
            return type;
        }

        assert(false);
        return nullptr;
    }

    if (IRTypeIndirectArray* irType = CastTo<IRTypeIndirectArray>(inIrType); irType != nullptr)
    {
        TypeArray* const type = arena_.New<TypeArray>();
        if (irType->size != 0)
            type->name = NewStringFmt(arena_, "{}[{}]", irType->target->name, irType->size);
        else
            type->name = NewStringFmt(arena_, "{}[]", irType->target->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->elements = irType->target->index;
        type->size = irType->size;
        return type;
    }

    if (IRTypeIndirectNullable* irType = CastTo<IRTypeIndirectNullable>(inIrType); irType != nullptr)
    {
        TypeNullable* const type = arena_.New<TypeNullable>();
        type->name = NewStringFmt(arena_, "{}?", irType->target->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->target = irType->target->index;
        return type;
    }

    if (IRTypeIndirectPointer* irType = CastTo<IRTypeIndirectPointer>(inIrType); irType != nullptr)
    {
        TypeNullable* const type = arena_.New<TypeNullable>();
        type->name = NewStringFmt(arena_, "{}*", irType->target->name);
        type->parent = irType->parent->index;
        type->location = irType->location;
        type->target = irType->target->index;
        return type;
    }

    assert(false);
    return nullptr;
}

Annotations SchemaGenerator::CreateAnnotations(Array<IRAnnotation*> irAnnotations)
{
    if (irAnnotations.IsEmpty())
        return {};

    for (IRAnnotation* irAnnotation : irAnnotations)
    {
        Annotation& annotation = annotations_[irAnnotation->index];
        annotation.attribute = irAnnotation->attribute->index;
        annotation.location = irAnnotation->location;

        Array<Argument> arguments = arena_.NewArrayCapacity<Argument>(irAnnotation->arguments.Size());
        for (IRAnnotationArgument* irArgument : irAnnotation->arguments)
        {
            Argument& arg = arguments.EmplaceBack(arena_);
            arg.field = irArgument->field->index;
            arg.value = ResolveIndex(irArgument->value);
            arg.location = irArgument->location;
        }
        annotation.arguments = arguments;
    }

    return Annotations{ irAnnotations.Front()->index, static_cast<std::uint32_t>(irAnnotations.Size()) };
}

const Value* SchemaGenerator::CreateValue(IRValue* value)
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
        result->type = type->target->index;
        return result;
    }

    if (IRValueEnumItem* item = CastTo<IRValueEnumItem>(value))
    {
        ValueEnum* const result = arena_.New<ValueEnum>();
        result->location = value->location;
        result->item = item->item->index;
        return result;
    }

    if (IRValueInitializerList* initializerList = CastTo<IRValueInitializerList>(value))
    {
        if (initializerList->type->kind == IRTypeKind::IndirectArray)
        {
            ValueArray* const result = arena_.New<ValueArray>();
            result->type = initializerList->type->index;
            result->location = value->location;

            Array<ValueIndex> elements = arena_.NewArrayCapacity<ValueIndex>(initializerList->positional.Size());
            for (IRValue* const element : initializerList->positional)
                elements.PushBack(arena_, ResolveIndex(element));

            result->elements = elements;
            return result;
        }

        if (IRTypeStruct* typeStruct = CastTo<IRTypeStruct>(initializerList->type); typeStruct != nullptr)
        {
            ValueObject* const result = arena_.New<ValueObject>();
            result->type = initializerList->type->index;
            result->location = value->location;

            Array<Argument> fields = arena_.NewArrayCapacity<Argument>(initializerList->positional.Size() + initializerList->named.Size());

            std::uint32_t fieldIndex = 0;
            for (IRValue* const element : initializerList->positional)
            {
                Argument& field = fields.EmplaceBack(arena_);
                field.field = typeStruct->fields[fieldIndex]->index;
                field.value = ResolveIndex(element);
                ++fieldIndex;
            }

            for (IRInitializerNamedArgument* const named : initializerList->named)
            {
                Argument& field = fields.EmplaceBack(arena_);
                field.field = named->field->index;
                field.value = ResolveIndex(named->value);
                ++fieldIndex;
            }

            result->fields = fields;
            return result;
        }
    }

    assert(false);
    return nullptr;
}

ValueIndex SchemaGenerator::ResolveIndex(IRValue* value)
{
    if (value == nullptr)
        return InvalidIndex;

    return value->index;
}
