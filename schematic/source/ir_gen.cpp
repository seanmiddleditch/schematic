// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "ir_gen.h"

#include "find.h"
#include "lexer.h"
#include "parser.h"

#include <utility>

using namespace schematic;
using namespace schematic::compiler;

static auto MatchNamePred(const char* name) noexcept
{
    return [name](auto* const entity) noexcept -> bool
    {
        return entity->name != nullptr && std::strcmp(entity->name, name) == 0;
    };
}

static auto MatchAstIdentPred(const char* name) noexcept
{
    return [name](auto* const entity) noexcept -> bool
    {
        return entity->name != nullptr && std::strcmp(entity->name->name, name) == 0;
    };
}

namespace
{
    struct SchemaFieldPrinter
    {
        const AstNodeSchemaDecl* parent = nullptr;
        const AstNodeField* ast = nullptr;
    };
} // namespace
template <>
struct fmt::formatter<SchemaFieldPrinter> : fmt::formatter<fmt::string_view>
{
    template <typename FormatContext>
    constexpr auto format(const SchemaFieldPrinter& val, FormatContext& ctx) const
        -> decltype(ctx.out())
    {
        if (val.parent == nullptr || val.ast == nullptr)
            return fmt::format_to(ctx.out(), "null");

        auto out = fmt::format_to(ctx.out(), "{}.{}", val.parent->name->name, val.ast->name->name);

        if (val.ast->minVersion != nullptr)
            out = fmt::format_to(out, "#{}", val.ast->minVersion->value);
        if (val.ast->maxVersion != nullptr)
            out = fmt::format_to(out, "..{}", val.ast->maxVersion->value);

        return out;
    }
};

IRSchema* IRGenerator::CompileRoot()
{
    if (state_.schema == nullptr)
        state_.schema = arena_.New<IRSchema>();

    state_.schema->root = CompileModule();
    if (state_.schema->root == nullptr)
        return nullptr;

    AssignIndices(state_.schema->root, state_.schema);

    for (IRType* const type : state_.schema->root->types)
        AssignIndices(type, state_.schema);

    state_.schema->modules = state_.modules;

    return state_.schema;
}

bool IRGenerator::CompilePreamble()
{
    IRModule* const preamble = CompileModule();
    if (preamble == nullptr)
        return false;

    state_.preambles.PushBack(arena_, preamble);
    return true;
}

IRModule* IRGenerator::CompileModule()
{
    source_ = ctx_.ReadFileContents(arena_, filename_);

    Lexer lexer(arena_, ctx_, filename_, source_);
    tokens_ = lexer.Tokenize();
    if (tokens_.IsEmpty())
        return nullptr;

    Parser parser(arena_, ctx_, filename_, source_, tokens_);
    ast_ = parser.Parse();
    if (ast_ == nullptr)
        return nullptr;

    module_ = arena_.New<IRModule>();
    module_->filename = arena_.NewString(filename_);

    for (IRModule* preamble : state_.preambles)
    {
        IRImport* const preambleImport = arena_.New<IRImport>();
        preambleImport->resolved = preamble;
        module_->imports.PushBack(arena_, preambleImport);
    }

    state_.stack.PushBack(arena_, module_);
    const bool success = CompileDecls();
    state_.stack.PopBack();

    if (!success)
        return nullptr;

    state_.modules.PushBack(arena_, module_);
    return module_;
}

bool IRGenerator::CompileDecls()
{
    for (const AstNode* const node : ast_->nodes)
    {
        if (const AstNodeAliasDecl* const declNode = node->CastTo<AstNodeAliasDecl>(); declNode != nullptr)
        {
            IRTypeAlias* const type = arena_.New<IRTypeAlias>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->parent = module_;
            type->target = LowerType(declNode->target);
            type->annotations = LowerAnnotations(declNode->annotations);
            type->location = GetLocation(declNode);

            ValidateTypeUnique(type);

            module_->types.PushBack(arena_, type);
            continue;
        }

        if (const AstNodeAttributeDecl* const declNode = node->CastTo<AstNodeAttributeDecl>(); declNode != nullptr)
        {
            IRTypeAttribute* const type = arena_.New<IRTypeAttribute>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->parent = module_;
            type->annotations = LowerAnnotations(declNode->annotations);
            type->location = GetLocation(declNode);

            ValidateTypeUnique(type);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRField* const field = arena_.New<IRField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;

                if (Find(type->fields, MatchNamePred(field->name)) != nullptr)
                    Error(field->ast, "Field already defined: {}.{}", type->name, field->name);

                field->type = LowerType(fieldNode->type);
                field->value = LowerValue(fieldNode->value);
                field->annotations = LowerAnnotations(fieldNode->annotations);
                field->location = GetLocation(fieldNode);
                type->fields.PushBack(arena_, field);
            }

            module_->types.PushBack(arena_, type);
            continue;
        }

        if (const AstNodeEnumDecl* const declNode = node->CastTo<AstNodeEnumDecl>(); declNode != nullptr)
        {
            IRTypeEnum* const type = arena_.New<IRTypeEnum>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->parent = module_;
            type->base = LowerType(declNode->base);
            type->annotations = LowerAnnotations(declNode->annotations);
            type->location = GetLocation(declNode);

            ValidateTypeUnique(type);

            std::int64_t nextValue = 0;

            for (const AstNodeEnumItem* const itemNode : declNode->items)
            {
                IREnumItem* const item = arena_.New<IREnumItem>();
                item->ast = itemNode;
                item->name = itemNode->name->name;

                if (Find(type->items, MatchNamePred(item->name)) != nullptr)
                    Error(item->ast, "Enum item already defined: {}.{}", type->name, item->name);

                IRValue* const value = LowerValue(itemNode->value);
                if (IRValueLiteral* const literal = CastTo<IRValueLiteral>(value); literal != nullptr)
                {
                    if (const AstNodeLiteralInt* intNode = CastTo<AstNodeLiteralInt>(literal->ast); intNode != nullptr)
                        nextValue = static_cast<const AstNodeLiteralInt*>(literal->ast)->value;
                    else
                        Error(literal->ast, "Enum value must be an integer");
                }
                item->value = nextValue++;

                item->annotations = LowerAnnotations(itemNode->annotations);
                item->location = GetLocation(itemNode);
                type->items.PushBack(arena_, item);
            }

            module_->types.PushBack(arena_, type);
            continue;
        }

        if (const AstNodeMessageDecl* const declNode = node->CastTo<AstNodeMessageDecl>(); declNode != nullptr)
        {
            IRTypeMessage* const type = arena_.New<IRTypeMessage>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->parent = module_;
            type->annotations = LowerAnnotations(declNode->annotations);
            type->location = GetLocation(declNode);

            ValidateTypeUnique(type);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRField* const field = arena_.New<IRField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;

                if (Find(type->fields, MatchNamePred(field->name)) != nullptr)
                    Error(field->ast, "Message field already defined: {}.{}", type->name, field->name);

                field->type = LowerType(fieldNode->type);
                field->value = LowerValue(fieldNode->value);
                if (fieldNode->proto->value > UINT32_MAX)
                    Error(fieldNode->proto, "Message proto must be no greater than {}: {}", UINT32_MAX, fieldNode->proto->value);
                field->proto = static_cast<std::uint32_t>(fieldNode->proto->value);
                field->annotations = LowerAnnotations(fieldNode->annotations);
                field->location = GetLocation(fieldNode);
                type->fields.PushBack(arena_, field);
            }

            module_->types.PushBack(arena_, type);
            continue;
        }

        if (const AstNodeImport* const importNode = node->CastTo<AstNodeImport>(); importNode != nullptr)
        {
            IRImport* const import = arena_.New<IRImport>();
            import->ast = importNode;

            const std::string_view target = ctx_.ResolveModule(arena_, importNode->target->value, filename_);
            if (target.empty())
            {
                Error(importNode, "Module not found: {}", importNode->target->value);
                continue;
            }

            bool doImport = true;
            for (IRModule* const mod : state_.stack)
            {
                if (target == mod->filename)
                {
                    Error(importNode, "Recursive import: {}", target);
                    doImport = false;
                    break;
                }
            }

            if (doImport)
            {
                IRGenerator generator(arena_, ctx_, state_, target);
                import->resolved = generator.CompileModule();
                if (import->resolved == nullptr)
                    failed_ = true;
            }

            module_->imports.PushBack(arena_, import);
            continue;
        }

        if (const AstNodeStructDecl* const declNode = node->CastTo<AstNodeStructDecl>(); declNode != nullptr)
        {
            IRTypeStruct* const type = arena_.New<IRTypeStruct>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->parent = module_;
            type->base = LowerType(declNode->base);
            type->annotations = LowerAnnotations(declNode->annotations);
            type->location = GetLocation(declNode);

            ValidateTypeUnique(type);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRField* const field = arena_.New<IRField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;

                if (Find(type->fields, MatchNamePred(field->name)) != nullptr)
                    Error(field->ast, "Struct field already defined: {}.{}", type->name, field->name);

                field->type = LowerType(fieldNode->type);
                field->value = LowerValue(fieldNode->value);
                field->annotations = LowerAnnotations(fieldNode->annotations);
                field->location = GetLocation(fieldNode);
                type->fields.PushBack(arena_, field);
            }

            module_->types.PushBack(arena_, type);
            continue;
        }

        if (const AstNodeSchemaDecl* const declNode = node->CastTo<AstNodeSchemaDecl>(); declNode != nullptr)
        {
            const IRVersionRange versionRange = ReadVersion(declNode->minVersion, declNode->maxVersion);

            IRType* const previousType = Find(module_->types, MatchNamePred(declNode->name->name));
            IRTypeSchema* const otherSchemaType = CastTo<IRTypeSchema>(previousType);

            if (previousType != nullptr && otherSchemaType == nullptr)
                Error(declNode, "Type already defined: {}", declNode->name->name);

            IRSchemaMeta* meta = nullptr;
            if (otherSchemaType != nullptr)
            {
                meta = otherSchemaType->meta;
            }
            else
            {
                meta = arena_.New<IRSchemaMeta>();
                module_->versionMetas.PushBack(arena_, meta);
            }

            for (IRTypeSchema* const version : meta->versions)
            {
                if (versionRange.min <= version->version && version->version <= versionRange.max)
                    Error(declNode, "Schema versions overlap with previous declaration: {}", declNode->name->name);
            }

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                // To validate versions
                ReadVersion(fieldNode->minVersion, fieldNode->maxVersion);

                if ((fieldNode->minVersion != nullptr && fieldNode->minVersion->value < versionRange.min) ||
                    (fieldNode->maxVersion != nullptr && fieldNode->maxVersion->value > versionRange.max))
                {
                    Error(fieldNode, "Field version range must be within schema version range: {}", SchemaFieldPrinter{ declNode, fieldNode });
                }

                const AstNodeField* const existingFieldNode = Find(declNode->fields, MatchAstIdentPred(fieldNode->name->name));
                if (existingFieldNode == fieldNode)
                    continue;

                if (existingFieldNode->minVersion != nullptr && fieldNode->maxVersion != nullptr && existingFieldNode->minVersion->value > fieldNode->maxVersion->value)
                    continue;
                if (existingFieldNode->maxVersion != nullptr && fieldNode->minVersion != nullptr && existingFieldNode->maxVersion->value < fieldNode->minVersion->value)
                    continue;

                Error(fieldNode, "Schema field version already defined: {}", SchemaFieldPrinter{ declNode, fieldNode });
            }

            for (std::uint64_t version = versionRange.min; version <= versionRange.max; ++version)
            {
                IRTypeSchema* const type = arena_.New<IRTypeSchema>();
                type->name = declNode->name->name;
                type->ast = declNode;
                type->parent = module_;
                type->base = LowerType(declNode->base);
                type->annotations = LowerAnnotations(declNode->annotations);
                type->location = GetLocation(declNode);
                type->version = version;
                type->meta = meta;

                for (const AstNodeField* const fieldNode : declNode->fields)
                {
                    if (fieldNode->minVersion != nullptr && fieldNode->minVersion->value > version)
                        continue;
                    if (fieldNode->maxVersion != nullptr && fieldNode->maxVersion->value < version)
                        continue;

                    IRField* const field = arena_.New<IRField>();
                    field->ast = fieldNode;
                    field->name = fieldNode->name->name;
                    field->type = LowerType(fieldNode->type);
                    field->value = LowerValue(fieldNode->value);
                    field->version = ReadVersion(fieldNode->minVersion, fieldNode->maxVersion);

                    field->annotations = LowerAnnotations(fieldNode->annotations);
                    field->location = GetLocation(fieldNode);
                    type->fields.PushBack(arena_, field);
                }

                module_->types.PushBack(arena_, type);
                meta->versions.PushBack(arena_, type);
            }
        }
    }

    if (failed_)
        return false;

    for (IRSchemaMeta* const irVersionedMeta : module_->versionMetas)
    {
        irVersionedMeta->alias = arena_.New<IRTypeAlias>();
        irVersionedMeta->alias->parent = module_;
        module_->types.PushBack(arena_, irVersionedMeta->alias);

        IRTypeSchema* newestVersion = nullptr;
        for (IRTypeSchema* const versionedStruct : irVersionedMeta->versions)
        {
            if (newestVersion == nullptr || newestVersion->version < versionedStruct->version)
                newestVersion = versionedStruct;
        }

        irVersionedMeta->alias->name = newestVersion->name;
        irVersionedMeta->alias->ast = newestVersion->ast;
        irVersionedMeta->alias->target = newestVersion;
    }

    for (IRType* const irTypeIter : module_->types)
    {
        if (IRTypeAlias* type = CastTo<IRTypeAlias>(irTypeIter); type != nullptr)
        {
            type->target = ResolveType(type->target);
            ResolveAttributes(type->annotations);
            continue;
        }

        if (IRTypeAttribute* type = CastTo<IRTypeAttribute>(irTypeIter); type != nullptr)
        {
            ResolveAttributes(type->annotations);
            for (IRField* field : type->fields)
            {
                field->type = ResolveType(field->type);
                field->value = ResolveValue(field->type, field->value);
                ResolveAttributes(field->annotations);
            }
            continue;
        }

        if (IRTypeEnum* type = CastTo<IRTypeEnum>(irTypeIter); type != nullptr)
        {
            IRType* const base = ResolveAlias(ResolveType(type->base));
            ResolveAttributes(type->annotations);
            if (base != nullptr)
            {
                if (base->kind != IRTypeKind::Builtin)
                    Error(type->ast, "Enum base type is not an integer type: {}", type->name);
                else if (static_cast<IRTypeBuiltin*>(base)->typeKind != TypeKind::Int)
                    Error(type->ast, "Enum base type is not an integer type: {}", type->name);
                else
                    type->base = base;
            }
            for (IREnumItem* item : type->items)
            {
                ResolveAttributes(item->annotations);
            }

            continue;
        }

        if (IRTypeMessage* type = CastTo<IRTypeMessage>(irTypeIter); type != nullptr)
        {
            for (IRField* field : type->fields)
            {
                field->type = ResolveType(field->type);
                field->value = ResolveValue(field->type, field->value);
                ResolveAttributes(field->annotations);
            }
            ResolveAttributes(type->annotations);
            continue;
        }

        if (IRTypeStruct* type = CastTo<IRTypeStruct>(irTypeIter); type != nullptr)
        {
            IRType* const base = ResolveAlias(ResolveType(type->base));
            if (base != nullptr)
            {
                if (base->kind != IRTypeKind::Struct)
                    Error(type->ast, "Struct base type is not an unversioned struct type: {}", type->name);
                else
                    type->base = base;
            }

            for (IRField* field : type->fields)
            {
                field->type = ResolveType(field->type);
                field->value = ResolveValue(field->type, field->value);
                ResolveAttributes(field->annotations);
            }

            ResolveAttributes(type->annotations);
            continue;
        }

        if (IRTypeSchema* type = CastTo<IRTypeSchema>(irTypeIter); type != nullptr)
        {
            IRType* const base = ResolveAlias(ResolveType(type->base));
            if (base != nullptr)
            {
                if (base->kind != IRTypeKind::Struct && base->kind != IRTypeKind::Schema)
                    Error(type->ast, "Struct base type is not a struct type: {}", type->name);
                else
                    type->base = base;
            }

            for (IRField* field : type->fields)
            {
                field->type = ResolveType(field->type);
                field->value = ResolveValue(field->type, field->value);
                ResolveAttributes(field->annotations);
            }

            ResolveAttributes(type->annotations);
            continue;
        }
    }

    return !failed_;
}

IRVersionRange IRGenerator::ReadVersion(const AstNodeLiteralInt* min, const AstNodeLiteralInt* max)
{
    if (min == nullptr)
        return {};

    IRVersionRange version;

    if (min->value <= 0)
        Error(min, "Version must be positive: {}", min->value);
    else if (min->value > UINT32_MAX)
        Error(min, "Version must be no greater than {}: {}", UINT32_MAX, min->value);
    version.max = version.min = static_cast<std::uint32_t>(min->value);

    if (max != nullptr)
    {
        if (max->value <= 0)
            Error(max, "Version must be positive: {}", max->value);
        else if (max->value > UINT32_MAX)
            Error(max, "Version must be no greater than {}: {}", UINT32_MAX, max->value);
        else
            version.max = static_cast<std::uint32_t>(max->value);

        if (max->value < min->value)
        {
            Error(max, "Version range must be lower to higher: {}..{}", version.min, version.max);
            return {};
        }
    }

    return version;
}

void IRGenerator::ValidateTypeUnique(IRType* type)
{
    IRType* const existing = Find(module_->types, MatchNamePred(type->name));
    if (existing != nullptr)
        Error(type->ast, "Type already defined: {}", type->name);
}

IRType* IRGenerator::FindType(const char* name)
{
    if (IRType* type = Find(module_->types, MatchNamePred(name)); type != nullptr)
        return type;

    for (IRImport* import : module_->imports)
    {
        if (import->resolved == nullptr)
            continue;

        if (IRType* type = Find(import->resolved->types, MatchNamePred(name)); type != nullptr)
            return type;
    }

    return nullptr;
}

IRType* IRGenerator::LowerType(const AstNode* ast)
{
    if (ast == nullptr)
        return nullptr;

    if (const AstNodeTypeArray* const astArray = ast->CastTo<AstNodeTypeArray>(); astArray != nullptr)
    {
        IRTypeIndirectArray* const indirect = arena_.New<IRTypeIndirectArray>();
        indirect->ast = astArray;
        indirect->parent = module_;
        indirect->location = GetLocation(astArray);
        indirect->target = LowerType(astArray->type);
        if (astArray->size != nullptr)
        {
            const std::int64_t size = astArray->size->value;
            if (size <= 0)
                Error(astArray, "Array size must be positive, got {}", size);
            else if (size > UINT32_MAX)
                Error(astArray, "Array size must be no greater than {}, got {}", UINT32_MAX, size);
            else
                indirect->size = static_cast<std::uint32_t>(size);
        }
        return indirect;
    }

    if (const AstNodeIdentifier* const astName = ast->CastTo<AstNodeIdentifier>(); astName != nullptr)
    {
        IRTypeIndirectIdentifier* const indirect = arena_.New<IRTypeIndirectIdentifier>();
        indirect->ast = astName;
        indirect->parent = module_;
        indirect->location = GetLocation(astName);
        indirect->name = astName->name;
        return indirect;
    }

    if (const AstNodeTypeNullable* const astNullable = ast->CastTo<AstNodeTypeNullable>(); astNullable != nullptr)
    {
        IRTypeIndirectNullable* const indirect = arena_.New<IRTypeIndirectNullable>();
        indirect->ast = astNullable;
        indirect->parent = module_;
        indirect->location = GetLocation(astNullable);
        indirect->target = LowerType(astNullable->type);
        return indirect;
    }

    if (const AstNodeTypePointer* const astPointer = ast->CastTo<AstNodeTypePointer>(); astPointer != nullptr)
    {
        IRTypeIndirectPointer* const indirect = arena_.New<IRTypeIndirectPointer>();
        indirect->ast = astPointer;
        indirect->parent = module_;
        indirect->location = GetLocation(astPointer);
        indirect->target = LowerType(astPointer->type);
        return indirect;
    }

    Error(ast, "Internal error: unknown AST node type: {}", std::to_underlying(ast->kind));
    return nullptr;
}

IRType* IRGenerator::ResolveType(IRType* type)
{
    if (type == nullptr)
        return nullptr;

    if (IRTypeIndirectIdentifier* indirect = CastTo<IRTypeIndirectIdentifier>(type); indirect != nullptr)
    {
        if (IRType* resolved = FindType(indirect->name); resolved != nullptr)
            return resolved;

        Error(indirect->ast, "Type not found: {}", indirect->name);
        return type;
    }

    if (IRTypeIndirectArray* indirect = CastTo<IRTypeIndirectArray>(type); indirect != nullptr)
    {
        indirect->target = ResolveType(indirect->target);
        if (indirect->target == nullptr)
            return nullptr;
        for (IRTypeIndirectArray* array : indirect->target->arrayTypes)
        {
            if (array->size == indirect->size)
                return array;
        }
        indirect->target->arrayTypes.PushBack(arena_, indirect);
        return indirect;
    }

    if (IRTypeIndirectNullable* indirect = CastTo<IRTypeIndirectNullable>(type); indirect != nullptr)
    {
        indirect->target = ResolveType(indirect->target);
        if (indirect->target == nullptr)
            return nullptr;
        if (indirect->target->nullableType != nullptr)
            return indirect->target->nullableType;
        indirect->target->nullableType = indirect;

        return indirect;
    }

    if (IRTypeIndirectPointer* indirect = CastTo<IRTypeIndirectPointer>(type); indirect != nullptr)
    {
        indirect->target = ResolveType(indirect->target);
        if (indirect->target == nullptr)
            return nullptr;
        if (indirect->target->pointerType != nullptr)
            return indirect->target->pointerType;
        indirect->target->pointerType = indirect;
        return indirect;
    }

    return type;
}

IRType* IRGenerator::ResolveAlias(IRType* type)
{
    if (type == nullptr)
        return nullptr;

    if (type->kind != IRTypeKind::Alias)
        return type;

    return ResolveAlias(static_cast<IRTypeAlias*>(type)->target);
}

void IRGenerator::ResolveAttributes(Array<IRAnnotation*> annotations)
{
    for (IRAnnotation* const annotation : annotations)
    {
        annotation->attribute = ResolveType(annotation->attribute);
        if (annotation->attribute == nullptr)
            continue;

        IRTypeAttribute* const attribute = CastTo<IRTypeAttribute>(annotation->attribute);
        if (attribute == nullptr)
        {
            Error(annotation->ast, "Annotation must be an attribute type: {}", annotation->attribute->name);
            continue;
        }

        std::uint32_t nextArgumentIndex = 0;
        for (const AstNode* const node : annotation->ast->arguments)
        {
            if (const AstNodeNamedArgument* const namedNode = CastTo<AstNodeNamedArgument>(node); namedNode != nullptr)
            {
                IRField* const field = Find(attribute->fields, MatchNamePred(namedNode->name->name));
                if (field == nullptr)
                {
                    Error(node, "Field does not exist on attribute type: {}.{}", attribute->name, namedNode->name->name);
                    continue;
                }

                IRAnnotationArgument* const arg = arena_.New<IRAnnotationArgument>();
                arg->ast = node;
                arg->field = field;
                arg->value = ResolveValue(field->type, LowerValue(namedNode->value));
                annotation->arguments.PushBack(arena_, arg);
            }
            else if (nextArgumentIndex < attribute->fields.Size())
            {
                IRField* const field = attribute->fields[nextArgumentIndex++];
                IRAnnotationArgument* const arg = arena_.New<IRAnnotationArgument>();
                arg->ast = node;
                arg->field = field;
                arg->value = ResolveValue(field->type, LowerValue(node));
                annotation->arguments.PushBack(arena_, arg);
            }
            else
            {
                Error(node, "Too many arguments for attribute: {}", attribute->name);
            }
        }
    }
}

Array<IRAnnotation*> IRGenerator::LowerAnnotations(Array<const AstNodeAnnotation*> astNodes)
{
    Array<IRAnnotation*> annotations = arena_.NewArrayCapacity<IRAnnotation*>(astNodes.Size());
    for (const AstNodeAnnotation* const astAnnotation : astNodes)
    {
        IRAnnotation* const annotation = arena_.New<IRAnnotation>();
        annotation->ast = astAnnotation;
        annotation->attribute = LowerType(astAnnotation->type);
        annotations.PushBack(arena_, annotation);
    }
    return annotations;
}

IRValue* IRGenerator::LowerValue(const AstNode* node)
{
    if (node == nullptr)
        return nullptr;

    switch (node->kind)
    {
        case AstNodeKind::LiteralBool:
        case AstNodeKind::LiteralFloat:
        case AstNodeKind::LiteralInt:
        case AstNodeKind::LiteralNull:
        case AstNodeKind::LiteralString:
        {
            IRValue* const value = arena_.New<IRValueLiteral>();
            value->ast = node;
            value->location = GetLocation(node);
            return value;
        }
        case AstNodeKind::Identifier:
        {
            IRValueIdentifier* const value = arena_.New<IRValueIdentifier>();
            value->ast = node;
            value->name = static_cast<const AstNodeIdentifier*>(node)->name;
            value->location = GetLocation(node);
            return value;
        }
        case AstNodeKind::InitializerList:
        {
            const AstNodeInitializerList* const initializerList = CastTo<AstNodeInitializerList>(node);
            IRValueInitializerList* const value = arena_.New<IRValueInitializerList>();
            value->ast = initializerList;
            value->location = GetLocation(node);
            value->type = LowerType(initializerList->type);

            for (const AstNode* element : initializerList->elements)
            {
                if (const AstNodeNamedArgument* const namedNode = CastTo<AstNodeNamedArgument>(element))
                {
                    IRInitializerNamedArgument* const named = arena_.New<IRInitializerNamedArgument>();
                    named->ast = namedNode;
                    named->name = namedNode->name->name;
                    named->value = LowerValue(namedNode->value);
                    named->location = GetLocation(namedNode);
                    value->named.PushBack(arena_, named);
                }
                else
                {
                    if (!value->named.IsEmpty())
                        Error(element, "Positional initializer arguments must come before any named arguments");

                    IRValue* const positional = LowerValue(element);
                    value->positional.PushBack(arena_, positional);
                }
            }
            return value;
        }
        default:
            assert(false);
            break;
    }

    return nullptr;
}

IRValue* IRGenerator::ResolveValue(IRType* type, IRValue* value)
{
    if (value == nullptr)
        return nullptr;

    type = ResolveAlias(type);

    if (value->kind == IRValueKind::Literal)
        return value;

    if (IRValueIdentifier* const ident = CastTo<IRValueIdentifier>(value); ident != nullptr)
    {
        if (IRType* const target = FindType(ident->name); target != nullptr)
        {
            IRValueType* valueType = arena_.New<IRValueType>();
            valueType->ast = ident->ast;
            valueType->location = ident->location;
            valueType->target = target;
            return valueType;
        }

        if (IRTypeEnum* const enumType = CastTo<IRTypeEnum>(type); enumType != nullptr)
        {
            IREnumItem* const item = Find(enumType->items, MatchNamePred(ident->name));
            if (item != nullptr)
            {
                IRValueEnumItem* valueItem = arena_.New<IRValueEnumItem>();
                valueItem->ast = ident->ast;
                valueItem->location = ident->location;
                valueItem->type = enumType;
                valueItem->item = item;
                return valueItem;
            }
        }

        Error(ident->ast, "Type not found: {}", ident->name);
        return value;
    }

    if (IRValueInitializerList* const initializerList = CastTo<IRValueInitializerList>(value); initializerList != nullptr)
    {
        if (initializerList->type == nullptr)
        {
            initializerList->type = type;
        }
        else
        {
            initializerList->type = ResolveType(initializerList->type);
            type = ResolveAlias(initializerList->type);
        }

        if (type == nullptr)
        {
            Error(initializerList->ast, "Initializer list requires known type");
        }
        else if (IRTypeIndirectArray* const arrayType = CastTo<IRTypeIndirectArray>(type))
        {
            if (!initializerList->named.IsEmpty())
                Error(initializerList->named.Front()->ast, "Named initializer elements may only be used for struct types");

            if (arrayType->size != 0 && initializerList->positional.Size() > arrayType->size)
                Error(initializerList->ast, "Too many initializer elements for fixed-size array type: {}", arrayType->name);

            IRType* const elementType = arrayType->target;

            for (IRValue*& positional : initializerList->positional)
                positional = ResolveValue(elementType, positional);
        }
        else if (IRTypeStruct* const structType = CastTo<IRTypeStruct>(type))
        {
            std::uint32_t firstNamedIndex = UINT32_MAX;
            IRInitializerNamedArgument* firstNamedField = nullptr;

            std::uint32_t namedIndex = 0;
            for (IRInitializerNamedArgument* const named : initializerList->named)
            {
                const std::uint32_t currentNamedIndex = namedIndex++;

                const auto fieldIndex = FindIndex(structType->fields, MatchNamePred(named->name));
                if (fieldIndex >= structType->fields.Size())
                {
                    Error(named->ast, "Field does not exist on struct type: {}.{}", structType->name, named->name);
                    continue;
                }

                if (FindIndex(initializerList->named, MatchNamePred(named->name)) != currentNamedIndex)
                {
                    Error(named->ast, "Field initializer cannot be used more than once: {}", named->name);
                    continue;
                }

                IRField* const field = structType->fields[fieldIndex];

                if (fieldIndex < firstNamedIndex)
                {
                    firstNamedIndex = fieldIndex;
                    firstNamedField = named;
                }

                named->field = field;
                named->value = ResolveValue(field->type, named->value);
            }

            if (!initializerList->positional.IsEmpty() && structType->base != nullptr)
                Error(initializerList->positional.Front()->ast, "Positional field initializer cannot be used for struct type with base: {}", structType->name);
            else if (initializerList->positional.Size() > firstNamedIndex)
                Error(firstNamedField->ast, "Named and positional field initializer cannot be used for the same field: {}", firstNamedField->name);

            std::uint32_t fieldIndex = 0;
            for (IRValue*& positional : initializerList->positional)
            {
                if (fieldIndex >= structType->fields.Size())
                    Error(positional->ast, "Too many positional initializers for struct type: {}", structType->name);

                IRField* const field = structType->fields[fieldIndex];
                positional = ResolveValue(field->type, positional);

                ++fieldIndex;
            }
        }
        else
        {
            Error(initializerList->ast, "Initializer list may only be used for array or struct types");
        }

        return value;
    }

    assert(false);
    return value;
}

void IRGenerator::AssignIndices(IRType* type, IRSchema* schema)
{
    if (type->index != InvalidIndex)
        return;

    schema->types.PushBack(arena_, type);

    AssignIndices(type->annotations, schema);

    if (IRTypeAttribute* const attributeType = CastTo<IRTypeAttribute>(type); attributeType != nullptr)
    {
        for (IRField* const field : attributeType->fields)
        {
            field->index = schema->maxFieldIndex++;

            AssignIndices(field->type, schema);
            if (field->value != nullptr)
                AssignIndices(field->value, schema);

            AssignIndices(field->annotations, schema);
        }
    }
    else if (IRTypeEnum* const enumType = CastTo<IRTypeEnum>(type); enumType != nullptr)
    {
        if (enumType->base != nullptr)
            AssignIndices(enumType->base, schema);

        for (IREnumItem* const item : enumType->items)
        {
            item->index = schema->maxEnumItemIndex++;

            AssignIndices(item->annotations, schema);
        }
    }
    else if (IRTypeMessage* const messageType = CastTo<IRTypeMessage>(type); messageType != nullptr)
    {
        for (IRField* const field : messageType->fields)
        {
            field->index = schema->maxFieldIndex++;

            AssignIndices(field->type, schema);
            if (field->value != nullptr)
                AssignIndices(field->value, schema);

            AssignIndices(field->annotations, schema);
        }
    }
    else if (IRTypeStruct* const structType = CastTo<IRTypeStruct>(type); structType != nullptr)
    {
        if (structType->base != nullptr)
            AssignIndices(structType->base, schema);

        for (IRField* const field : structType->fields)
        {
            field->index = schema->maxFieldIndex++;

            AssignIndices(field->type, schema);
            if (field->value != nullptr)
                AssignIndices(field->value, schema);

            AssignIndices(field->annotations, schema);
        }
    }
    else if (IRTypeSchema* const structVersionedType = CastTo<IRTypeSchema>(type); structVersionedType != nullptr)
    {
        if (structVersionedType->base != nullptr)
            AssignIndices(structVersionedType->base, schema);

        for (IRField* const field : structVersionedType->fields)
        {
            field->index = schema->maxFieldIndex++;

            AssignIndices(field->type, schema);
            if (field->value != nullptr)
                AssignIndices(field->value, schema);

            AssignIndices(field->annotations, schema);
        }
    }
    else if (IRTypeIndirectArray* const arrayType = CastTo<IRTypeIndirectArray>(type); arrayType != nullptr)
    {
        AssignIndices(arrayType->target, schema);
    }
    else if (IRTypeIndirectPointer* const pointerType = CastTo<IRTypeIndirectPointer>(type); pointerType != nullptr)
    {
        AssignIndices(pointerType->target, schema);
    }
    else if (IRTypeIndirectNullable* const nullableType = CastTo<IRTypeIndirectNullable>(type); nullableType != nullptr)
    {
        AssignIndices(nullableType->target, schema);
    }

    AssignIndices(type->parent, schema);
    type->index = schema->maxTypeIndex++;
}

void IRGenerator::AssignIndices(IRValue* value, IRSchema* schema)
{
    if (value->index != InvalidIndex)
        return;

    schema->values.PushBack(arena_, value);

    if (IRValueType* const valueType = CastTo<IRValueType>(value); valueType != nullptr)
    {
        AssignIndices(valueType->target, schema);
    }
    else if (IRValueEnumItem* const enumValue = CastTo<IRValueEnumItem>(value); enumValue != nullptr)
    {
        AssignIndices(enumValue->type, schema);
    }
    else if (IRValueInitializerList* const initValue = CastTo<IRValueInitializerList>(value); initValue != nullptr)
    {
        AssignIndices(initValue->type, schema);
        for (IRInitializerNamedArgument* const named : initValue->named)
            AssignIndices(named->value, schema);
        for (IRValue* const pos : initValue->positional)
            AssignIndices(pos, schema);
    }

    value->index = schema->maxValueIndex++;
}

void IRGenerator::AssignIndices(Array<IRAnnotation*> annotations, IRSchema* schema)
{
    for (IRAnnotation* const annotation : annotations)
    {
        AssignIndices(annotation->attribute, schema);

        for (IRAnnotationArgument* const arg : annotation->arguments)
            AssignIndices(arg->value, schema);
    }

    // need to ensure indices in an attribute set are contiguous, so we separate
    // index assignment from assigning indices to referenced entities, as they
    // may have attributes of their own.
    for (IRAnnotation* const annotation : annotations)
        annotation->index = schema->maxAnnotationIndex++;
}

void IRGenerator::AssignIndices(IRModule* module, IRSchema* schema)
{
    if (module->index != InvalidIndex)
        return;

    for (IRImport* const import : module->imports)
        AssignIndices(import->resolved, schema);

    module->index = schema->maxModuleIndex++;
}

Location IRGenerator::GetLocation(const AstNode* node)
{
    if (node == nullptr)
        return {};

    if (node->tokenIndex >= tokens_.Size())
        return {};

    const Token& token = tokens_[node->tokenIndex];

    const LogLocation log = FindRange(filename_, source_, token);
    return Location{
        .line = log.line,
        .column = log.column,
    };
}

template <typename... Args>
void IRGenerator::Error(const AstNode* node, fmt::format_string<Args...> format, Args&&... args)
{
    failed_ = true;

    char buffer[512];
    const auto rs = fmt::format_to_n(buffer, sizeof buffer, format, std::forward<Args>(args)...);

    if (node != nullptr && node->tokenIndex < tokens_.Size())
        ctx_.LogMessage(FindRange(filename_, source_, tokens_[node->tokenIndex]), std::string_view(buffer, rs.out));
    else
        ctx_.LogMessage(LogLocation{ .file = filename_ }, std::string_view(buffer, rs.out));
}
