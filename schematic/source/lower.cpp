// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "lower.h"

#include "lexer.h"
#include "parser.h"

using namespace potato::schematic;
using namespace potato::schematic::compiler;

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

    IRModule* const module = arena_.New<IRModule>();

    for (const AstNode* const node : ast_->nodes)
    {
        if (const AstNodeImport* const importNode = node->CastTo<AstNodeImport>(); importNode != nullptr)
        {
            IRImport* const import = arena_.New<IRImport>();
            import->ast = importNode;
            continue;
        }

        if (const AstNodeStructDecl* const declNode = node->CastTo<AstNodeStructDecl>(); declNode != nullptr)
        {
            IRTypeStruct* const type = arena_.New<IRTypeStruct>();
            type->ast = declNode;
            type->base = LowerType(declNode->base);

            for (const AstNodeField* const fieldNode : declNode->fields)
            {
                IRField* const field = arena_.New<IRField>();
                field->ast = fieldNode;
                field->name = fieldNode->name;
                field->type = LowerType(fieldNode->type);
                type->fields.PushBack(arena_, field);
            }

            module->types.PushBack(arena_, type);
        }
    }

    return module;
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

IRType* LowerAstToIr::LowerType(const AstNodeType* ast)
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
                logger_.Error(filename_, FindRange(source_, tokens_[astArray->tokenIndex]), "Array sizes must be positive");
            else if (size > UINT32_MAX)
                logger_.Error(filename_, FindRange(source_, tokens_[astArray->tokenIndex]), "Array sizes must be within uint32 (FIXME: better error)");
            else
                indirect->size = static_cast<std::uint32_t>(size);
        }
        return indirect;
    }

    if (const AstNodeTypeName* const astName = ast->CastTo<AstNodeTypeName>(); astName != nullptr)
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

    logger_.Error(filename_, FindRange(source_, tokens_[ast->tokenIndex]), "Unknown ast node type");
    return nullptr;
}
