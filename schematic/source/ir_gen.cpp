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

            ValidateTypeName(type);
            ValidateTypeUnique(type);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRAttributeField* const field = arena_.New<IRAttributeField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;
                field->type = LowerType(fieldNode->type);
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

            ValidateTypeName(type);
            ValidateTypeUnique(type);

            for (const AstNodeEnumItem* const itemNode : declNode->items)
            {
                IREnumItem* const item = arena_.New<IREnumItem>();
                item->ast = itemNode;
                item->name = itemNode->name->name;
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

            ValidateTypeName(type);
            ValidateTypeUnique(type);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRMessageField* const field = arena_.New<IRMessageField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;
                field->type = LowerType(fieldNode->type);
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
                    Error(importNode, "Recursive import: {}", mod->filename);
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

            ValidateTypeName(type);

            type->version = ReadVersion(declNode->minVersion, declNode->maxVersion);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRStructField* const field = arena_.New<IRStructField>();
                field->ast = fieldNode;
                field->name = fieldNode->name->name;
                field->type = LowerType(fieldNode->type);
                field->version = ReadVersion(fieldNode->minVersion, fieldNode->maxVersion);
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
                Error(type->ast, "Type already declared: {}", type->name);
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

    for (IRType* const type : module_->types)
    {
        if (IRTypeStruct* struct_ = CastTo<IRTypeStruct>(type); struct_ != nullptr)
        {
            struct_->base = ResolveType(struct_->base);
            for (IRStructField* field : struct_->fields)
                field->type = ResolveType(field->type);
        }
    }

    return module_;
}

IRVersionRange IRGenerator::ReadVersion(const AstNodeLiteralInt* min, const AstNodeLiteralInt* max)
{
    if (min == nullptr)
        return {};

    IRVersionRange version;

    if (min->value <= 0)
        Error(min, "Struct version must be positive, got {}", min->value);
    else if (min->value > UINT32_MAX)
        Error(min, "Struct version must be no greater than {}, got {}", UINT32_MAX, min->value);
    version.max = version.min = static_cast<std::uint32_t>(min->value);

    if (max != nullptr)
    {
        if (max->value <= 0)
            Error(max, "Struct version must be positive, got {}", max->value);
        else if (max->value > UINT32_MAX)
            Error(max, "Struct version must be no greater than {}, got {}", UINT32_MAX, max->value);
        else
            version.max = static_cast<std::uint32_t>(max->value);

        if (max->value < min->value)
        {
            Error(max, "Struct version range must be lower to higher, got {}..{}", version.min, version.max);
            return {};
        }
    }

    return version;
}

void IRGenerator::ValidateTypeName(IRType* type)
{
    if (type->name[0] == '$')
        Error(type->ast, "Type declared with reserved name: {}", type->name);
}

void IRGenerator::ValidateTypeUnique(IRType* type)
{
    IRType* const existing = Find(module_->types, MatchNamePred(type->name));
    if (existing != nullptr)
        Error(type->ast, "Type already declared: {}", type->name);
}

void IRGenerator::ValidateStructField(IRTypeStruct* type, const AstNodeField* field)
{
    IRStructField* const existing = Find(type->fields, MatchNamePred(field->name->name));
    if (existing == nullptr)
        return;

    // if either the existing or new field is not versioned, raise an error
    if (existing->version.min == 0 || field->minVersion == nullptr)
    {
        Error(field, "Field already declared: {}.{}", type->name, field->name->name);
        return;
    }

    // if the versions overlap, raise an error
    if (existing->version.min < field->minVersion->value)
    {
        if (existing->version.max == 0)
        {
            Error(field, "Field version already declared: {}.{}#{}", type->name, field->name->name, existing->version.min);
            return;
        }
        if (existing->version.max >= field->minVersion->value)
        {
            Error(field, "Field version already declared: {}.{}#{}..{}", type->name, field->name->name, existing->version.min, existing->version.max);
            return;
        }
    }
    else if (existing->version.min == field->minVersion->value)
    {
        Error(field, "Field version already declared: {}.{}#{}..{}", type->name, field->name->name, existing->version.min, existing->version.max);
        return;
    }

    if (field->minVersion != nullptr)

        return;
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
        IRType* resolved = Find(module_->types, MatchNamePred(indirect->name));
        if (resolved == nullptr)
        {
            for (IRImport* import : module_->imports)
            {
                if (import->resolved == nullptr)
                    continue;
                resolved = Find(import->resolved->types, MatchNamePred(indirect->name));
                if (resolved != nullptr)
                    break;
            }
        }

        if (resolved == nullptr)
            Error(indirect->ast, "Type not found: {}", indirect->name);
        return resolved;
    }

    if (IRTypeIndirectArray* indirect = CastTo<IRTypeIndirectArray>(type); indirect != nullptr)
    {
        indirect->target = ResolveType(indirect->target);
        return indirect;
    }

    if (IRTypeIndirectNullable* indirect = CastTo<IRTypeIndirectNullable>(type); indirect != nullptr)
    {
        indirect->target = ResolveType(indirect->target);
        return indirect;
    }

    if (IRTypeIndirectPointer* indirect = CastTo<IRTypeIndirectPointer>(type); indirect != nullptr)
    {
        indirect->target = ResolveType(indirect->target);
        return indirect;
    }

    return type;
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
