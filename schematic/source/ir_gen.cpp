// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "ir_gen.h"

#include "find.h"
#include "lexer.h"
#include "parser.h"

using namespace potato::schematic;
using namespace potato::schematic::compiler;

static auto MatchNamePred(const char* name) noexcept
{
    return [name](auto* const entity) noexcept -> bool
    {
        return entity->name != nullptr && std::strcmp(entity->name, name) == 0;
    };
}

static auto MatchNameAndVersionPred(const char* name, std::uint32_t version) noexcept
{
    return [name, version](auto* const entity) noexcept -> bool
    {
        return entity->version.min <= version && entity.version.max >= version && entity->name != nullptr && std::strcmp(entity->name, name) == 0;
    };
}

static bool IsReservedName(const char* name)
{
    return name[0] == '$';
}

IRModule* IRGenerator::Compile()
{
    Lexer lexer(arena_, logger_, filename_, source_);
    tokens_ = lexer.Tokenize();
    if (tokens_.IsEmpty())
        return nullptr;

    Parser parser(arena_, logger_, filename_, source_, tokens_);
    ast_ = parser.Parse();
    if (ast_ == nullptr)
        return nullptr;

    if (state_.builtins == nullptr)
    {
        state_.builtins = CreateBuiltins();
        state_.modules.PushBack(arena_, state_.builtins);
    }

    module_ = arena_.New<IRModule>();
    module_->filename = arena_.NewString(filename_);
    module_->index = nextModuleIndex_++;
    state_.stack.PushBack(arena_, module_);

    {
        IRImport* const builtinsImport = arena_.New<IRImport>();
        builtinsImport->resolved = state_.builtins;
        module_->imports.PushBack(arena_, builtinsImport);
    }

    for (const AstNode* const node : ast_->nodes)
    {
        if (const AstNodeAliasDecl* const declNode = node->CastTo<AstNodeAliasDecl>(); declNode != nullptr)
        {
            IRTypeAlias* const type = arena_.New<IRTypeAlias>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->target = LowerType(declNode->target);
            type->annotations = LowerAnnotations(declNode->annotations);

            ValidateTypeName(type);
            ValidateTypeUnique(type);

            module_->types.PushBack(arena_, type);
            continue;
        }

        if (const AstNodeAttributeDecl* const declNode = node->CastTo<AstNodeAttributeDecl>(); declNode != nullptr)
        {
            IRTypeAttribute* const type = arena_.New<IRTypeAttribute>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->annotations = LowerAnnotations(declNode->annotations);

            ValidateTypeName(type);
            ValidateTypeUnique(type);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRAttributeField* const field = arena_.New<IRAttributeField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;

                if (IsReservedName(field->name))
                    ErrorReservedName(field->ast, field->name, "Field");

                if (Find(type->fields, MatchNamePred(field->name)) != nullptr)
                    Error(field->ast, "Field already defined: {}.{}", type->name, field->name);

                field->type = LowerType(fieldNode->type);
                field->value = LowerValue(fieldNode->value);
                field->annotations = LowerAnnotations(fieldNode->annotations);
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
            type->base = LowerType(declNode->base);
            type->annotations = LowerAnnotations(declNode->annotations);

            ValidateTypeName(type);
            ValidateTypeUnique(type);

            std::int64_t nextValue = 0;

            for (const AstNodeEnumItem* const itemNode : declNode->items)
            {
                IREnumItem* const item = arena_.New<IREnumItem>();
                item->ast = itemNode;
                item->name = itemNode->name->name;

                if (IsReservedName(item->name))
                    ErrorReservedName(item->ast, item->name, "Enum item");

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
            type->annotations = LowerAnnotations(declNode->annotations);

            ValidateTypeName(type);
            ValidateTypeUnique(type);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRMessageField* const field = arena_.New<IRMessageField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;

                if (IsReservedName(field->name))
                    ErrorReservedName(field->ast, field->name, "Field");

                if (Find(type->fields, MatchNamePred(field->name)) != nullptr)
                    Error(field->ast, "Enum item already defined: {}.{}", type->name, field->name);

                field->type = LowerType(fieldNode->type);
                field->value = LowerValue(fieldNode->value);
                if (fieldNode->proto->value > UINT32_MAX)
                    Error(fieldNode->proto, "Message proto must be no greater than {}: {}", UINT32_MAX, fieldNode->proto->value);
                field->proto = static_cast<std::uint32_t>(fieldNode->proto->value);
                field->annotations = LowerAnnotations(fieldNode->annotations);
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
                const std::string_view source = ctx_.ReadFileContents(arena_, target);

                IRGenerator generator(arena_, logger_, ctx_, state_, target, source);
                import->resolved = generator.Compile();
            }

            module_->imports.PushBack(arena_, import);
            continue;
        }

        if (const AstNodeStructDecl* const declNode = node->CastTo<AstNodeStructDecl>(); declNode != nullptr)
        {
            IRTypeStruct* const type = arena_.New<IRTypeStruct>();
            type->name = declNode->name->name;
            type->ast = declNode;
            type->base = LowerType(declNode->base);
            type->annotations = LowerAnnotations(declNode->annotations);

            ValidateTypeName(type);

            type->version = ReadVersion(declNode->minVersion, declNode->maxVersion);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRStructField* const field = arena_.New<IRStructField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;
                if (IsReservedName(field->name))
                    ErrorReservedName(field->ast, field->name, "Field");
                field->type = LowerType(fieldNode->type);
                field->value = LowerValue(fieldNode->value);
                field->version = ReadVersion(fieldNode->minVersion, fieldNode->maxVersion);
                field->annotations = LowerAnnotations(fieldNode->annotations);
                ValidateStructField(type, field);
                type->fields.PushBack(arena_, field);
            }

            if (declNode->minVersion == nullptr)
            {
                ValidateTypeUnique(type);
                module_->types.PushBack(arena_, type);
                continue;
            }

            IRType* const struct_ = Find(module_->types, MatchNamePred(type->name));
            if (struct_ == nullptr)
            {
                IRTypeStructVersioned* const versioned = arena_.New<IRTypeStructVersioned>();
                versioned->name = type->name;
                versioned->ast = declNode;
                versioned->latest = type;
                versioned->versions.PushBack(arena_, type);
                module_->types.PushBack(arena_, versioned);
                continue;
            }

            IRTypeStructVersioned* const versioned = CastTo<IRTypeStructVersioned>(struct_);
            if (versioned == nullptr)
            {
                Error(type->ast, "Type already defined: {}", type->name);
                module_->types.PushBack(arena_, type);
                continue;
            }

            for (IRTypeStruct* const existing : versioned->versions)
            {
                if (type->version.min < existing->version.max && existing->version.min < type->version.max)
                    Error(type->ast, "Struct versions overlap with previous declaration: {}", type->name);
            }

            versioned->versions.PushBack(arena_, type);
            if (type->version.max > versioned->latest->version.max)
                versioned->latest = type;
        }
    }

    if (failed_)
        return nullptr;

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
            for (IRAttributeField* field : type->fields)
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
            for (IRMessageField* field : type->fields)
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
                    Error(type->ast, "Struct base type is not a struct type: {}", type->name);
                else
                    type->base = base;
            }
            ResolveAttributes(type->annotations);
            for (IRStructField* field : type->fields)
            {
                field->type = ResolveType(field->type);
                field->value = ResolveValue(field->type, field->value);
                ResolveAttributes(field->annotations);
            }
            continue;
        }

        if (IRTypeStructVersioned* type = CastTo<IRTypeStructVersioned>(irTypeIter); type != nullptr)
        {
            for (IRTypeStruct* version : type->versions)
            {
                version->base = ResolveAlias(ResolveType(version->base));
                ResolveAttributes(type->annotations);
                for (IRStructField* field : version->fields)
                {
                    field->type = ResolveType(field->type);
                    field->value = ResolveValue(field->type, field->value);
                    ResolveAttributes(field->annotations);
                }
            }
            continue;
        }
    }

    if (failed_)
        return nullptr;

    return module_;
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

void IRGenerator::ValidateTypeName(IRType* type)
{
    if (IsReservedName(type->name))
        ErrorReservedName(type->ast, type->name, "Type");
}

void IRGenerator::ValidateTypeUnique(IRType* type)
{
    IRType* const existing = Find(module_->types, MatchNamePred(type->name));
    if (existing != nullptr)
        Error(type->ast, "Type already defined: {}", type->name);
}

void IRGenerator::ValidateStructField(IRTypeStruct* type, IRStructField* field)
{
    IRStructField* const existing = Find(type->fields, MatchNamePred(field->name));
    if (existing == nullptr)
        return;

    // if either the existing or new field is not versioned, raise an error
    if (existing->version.min == 0 || field->version.min == 0)
    {
        Error(field->ast, "Field already defined: {}.{}", type->name, field->name);
        return;
    }

    // if the versions overlap, raise an error
    if (existing->version.min < field->version.min)
    {
        if (existing->version.max == 0)
        {
            Error(field->ast, "Field version already defined: {}.{}#{}", type->name, field->name, existing->version.min);
            return;
        }
        if (existing->version.max >= field->version.min)
        {
            Error(field->ast, "Field version already defined: {}.{}#{}..{}", type->name, field->name, existing->version.min, existing->version.max);
            return;
        }
    }
    else if (existing->version.min == field->version.min)
    {
        Error(field->ast, "Field version already defined: {}.{}#{}..{}", type->name, field->name, existing->version.min, existing->version.max);
        return;
    }
}

template <typename T>
static T* CreateBuiltinType(ArenaAllocator& arena, Array<IRType*>& types, TypeKind kind, const char* name)
{
    T* const type = arena.New<T>();
    type->name = name;
    type->typeKind = kind;

    types.PushBack(arena, type);
    return type;
};

IRModule* IRGenerator::CreateBuiltins()
{
    if (builtins_ != nullptr)
        return builtins_;

    builtins_ = arena_.New<IRModule>();
    builtins_->filename = "$builtins";

    auto AddInt = [this]<typename T>(const char* name, T)
    {
        IRTypeBuiltin* const type = CreateBuiltinType<IRTypeBuiltin>(arena_, builtins_->types, TypeKind::Int, name);
        type->isSigned = std::is_signed_v<T>;
        type->width = CHAR_BIT * sizeof(T);
    };
    auto AddFloat = [this]<typename T>(const char* name, T)
    {
        IRTypeBuiltin* const type = CreateBuiltinType<IRTypeBuiltin>(arena_, builtins_->types, TypeKind::Float, name);
        type->width = CHAR_BIT * sizeof(T);
    };

    CreateBuiltinType<IRTypeBuiltin>(arena_, builtins_->types, TypeKind::Type, "$type");
    CreateBuiltinType<IRTypeBuiltin>(arena_, builtins_->types, TypeKind::Bool, "bool");
    CreateBuiltinType<IRTypeBuiltin>(arena_, builtins_->types, TypeKind::String, "string");

    AddInt("int8", std::int8_t{});
    AddInt("uint8", std::uint8_t{});
    AddInt("int16", std::int16_t{});
    AddInt("uint16", std::uint16_t{});
    AddInt("int32", std::int32_t{});
    AddInt("uint32", std::uint32_t{});
    AddInt("int64", std::int64_t{});
    AddInt("uint64", std::uint64_t{});

    AddFloat("float32", float{});
    AddFloat("float64", double{});

    return builtins_;
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
        indirect->name = astName->name;
        return indirect;
    }

    if (const AstNodeTypeNullable* const astNullable = ast->CastTo<AstNodeTypeNullable>(); astNullable != nullptr)
    {
        IRTypeIndirectNullable* const indirect = arena_.New<IRTypeIndirectNullable>();
        indirect->ast = astNullable;
        indirect->target = LowerType(astNullable->type);
        return indirect;
    }

    if (const AstNodeTypePointer* const astPointer = ast->CastTo<AstNodeTypePointer>(); astPointer != nullptr)
    {
        IRTypeIndirectPointer* const indirect = arena_.New<IRTypeIndirectPointer>();
        indirect->ast = astPointer;
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
                IRAttributeField* const field = Find(attribute->fields, MatchNamePred(namedNode->name->name));
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
                IRAttributeField* const field = attribute->fields[nextArgumentIndex++];
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
    Array<IRAnnotation*> annotations = arena_.NewArray<IRAnnotation*>(astNodes.Size());
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
            return value;
        }
        case AstNodeKind::Identifier:
        {
            IRValueIdentifier* const value = arena_.New<IRValueIdentifier>();
            value->ast = node;
            value->name = static_cast<const AstNodeIdentifier*>(node)->name;
            return value;
        }
        default:
            break;
    }

    return nullptr;
}

IRValue* IRGenerator::ResolveValue(IRType* type, IRValue* value)
{
    if (value == nullptr)
        return nullptr;

    if (value->kind == IRValueKind::Literal)
        return value;

    if (IRValueIdentifier* const ident = CastTo<IRValueIdentifier>(value); ident != nullptr)
    {
        if (IRType* const target = FindType(ident->name); target != nullptr)
        {
            IRValueType* valueType = arena_.New<IRValueType>();
            valueType->ast = ident->ast;
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
                valueItem->type = enumType;
                valueItem->item = item;
                return valueItem;
            }
        }

        Error(ident->ast, "Type not found: {}", ident->name);
        return value;
    }

    assert(false);
    return value;
}

template <typename... Args>
void IRGenerator::Error(const AstNode* node, fmt::format_string<Args...> format, Args&&... args)
{
    failed_ = true;

    char buffer[512];
    const auto rs = fmt::format_to_n(buffer, sizeof buffer, format, std::forward<Args>(args)...);

    if (node != nullptr && node->tokenIndex < tokens_.Size())
        logger_.Error(filename_, FindRange(source_, tokens_[node->tokenIndex]), std::string_view(buffer, rs.out));
    else
        logger_.Error(filename_, {}, std::string_view(buffer, rs.out));
}

void IRGenerator::ErrorReservedName(const AstNode* node, const char* name, const char* entityTypeName)
{
    Error(node, "{} declared with reserved name: {}", entityTypeName, name);
}
