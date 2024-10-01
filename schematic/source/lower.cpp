// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "lower.h"

#include "find.h"
#include "lexer.h"
#include "parser.h"

using namespace potato::schematic;
using namespace potato::schematic::compiler;

static auto MatchFieldNamePred(const char* name) noexcept
{
    return [name](IRStructField* const field) noexcept -> bool
    {
        return std::strcmp(field->name, name) == 0;
    };
}

const IRModule* LowerAstToIr::Lower()
{
    Lexer lexer(arena_, logger_, filename_, source_);
    tokens_ = lexer.Tokenize();
    if (tokens_.IsEmpty())
        return nullptr;

    Parser parser(arena_, logger_, filename_, source_, tokens_);
    ast_ = parser.Parse();
    if (ast_ == nullptr)
        return nullptr;

    const IRModule* const builtins = CreateBuiltins();

    module_ = arena_.New<IRModule>();

    for (const AstNode* const node : ast_->nodes)
    {
        if (const AstNodeAttributeDecl* const declNode = node->CastTo<AstNodeAttributeDecl>(); declNode != nullptr)
        {
            IRTypeAttribute* const type = arena_.New<IRTypeAttribute>();
            type->name = declNode->name->name;
            type->ast = declNode;

            ValidateTypeName(type);

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

        if (const AstNodeImport* const importNode = node->CastTo<AstNodeImport>(); importNode != nullptr)
        {
            IRImport* const import = arena_.New<IRImport>();
            import->ast = importNode;
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

            module_->types.PushBack(arena_, type);
            continue;
        }
    }

    if (failed_)
        return nullptr;

    return module_;
}

IRVersionRange LowerAstToIr::ReadVersion(const AstNodeLiteralInt* min, const AstNodeLiteralInt* max)
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

void LowerAstToIr::ValidateTypeName(IRType* type)
{
    if (type->name[0] == '$')
    {
        Error(type->ast, "Type declared with reserved name: {}", type->name);
    }

    IRType* const* const existingIter = Find(module_->types, [type](IRType* other)
        {
            return std::strcmp(other->name, type->name) == 0;
        });
    if (existingIter != nullptr)
    {
        IRType* const existing = *existingIter;
        Error(type->ast, "Type already declared with name: {}", type->name);
    }
}

void LowerAstToIr::ValidateStructField(IRTypeStruct* type, const AstNodeField* field)
{
    IRStructField* const* const existingIter = Find(type->fields, MatchFieldNamePred(field->name->name));
    if (existingIter == nullptr)
        return;

    IRStructField* const existing = *existingIter;

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

const IRModule* LowerAstToIr::CreateBuiltins()
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

IRType* LowerAstToIr::LowerType(const AstNode* ast)
{
    if (ast == nullptr)
        return nullptr;

    if (const AstNodeTypeArray* const astArray = ast->CastTo<AstNodeTypeArray>(); astArray != nullptr)
    {
        IRTypeIndirectArray* const indirect = arena_.New<IRTypeIndirectArray>();
        indirect->ast = astArray;
        indirect->type = LowerType(astArray->type);
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
        indirect->type = LowerType(astNullable->type);
        return indirect;
    }

    if (const AstNodeTypePointer* const astPointer = ast->CastTo<AstNodeTypePointer>(); astPointer != nullptr)
    {
        IRTypeIndirectPointer* const indirect = arena_.New<IRTypeIndirectPointer>();
        indirect->ast = astPointer;
        indirect->type = LowerType(astPointer->type);
        return indirect;
    }

    Error(ast, "Internal error: unknown AST node type: {}", std::to_underlying(ast->kind));
    return nullptr;
}

template <typename... Args>
void LowerAstToIr::Error(const AstNode* node, fmt::format_string<Args...> format, Args&&... args)
{
    failed_ = true;

    char buffer[512];
    const auto rs = fmt::format_to_n(buffer, sizeof buffer, format, std::forward<Args>(args)...);

    if (node != nullptr && node->tokenIndex < tokens_.Size())
        logger_.Error(filename_, FindRange(source_, tokens_[node->tokenIndex]), std::string_view(buffer, rs.out));
    else
        logger_.Error(filename_, {}, std::string_view(buffer, rs.out));
}
