// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "array.h"

#include "schematic/allocator.h"

#include <cstdint>

namespace potato::schematic::compiler
{
    enum class AstNodeKind : std::uint8_t
    {
        None,
        Module,
        Import,
        AliasDecl,
        StructDecl,
        MessageDecl,
        AttributeDecl,
        Field,
        EnumDecl,
        EnumItem,
        TypeName,
        TypeArray,
        TypePointer,
        TypeNullable,
        Annotation,
        LiteralBool,
        LiteralNull,
        LiteralInt,
        LiteralFloat,
        LiteralString,
        NamedArgument,
        InitializerList,
        Identifier,
    };

    struct AstNodeIdentifier;
    struct AstNodeModule;
    struct AstNodeImport;
    struct AstNodeAliasDecl;
    struct AstNodeStructDecl;
    struct AstNodeMessageDecl;
    struct AstNodeAttributeDecl;
    struct AstNodeField;
    struct AstNodeEnumDecl;
    struct AstNodeEnumItem;
    struct AstNodeTypeName;
    struct AstNodeTypeArray;
    struct AstNodeTypePointer;
    struct AstNodeTypeNullable;
    struct AstNodeAnnotation;
    struct AstNodeLiteralBool;
    struct AstNodeLiteralNull;
    struct AstNodeLiteralInt;
    struct AstNodeLiteralFloat;
    struct AstNodeLiteralString;
    struct AstNodeNamedArgument;
    struct AstNodeInitializerList;

#define AST_NODE(SELF, KIND) \
    explicit SELF(std::uint32_t tokenIndex) : AstNode((KIND), tokenIndex) { } \
    static constexpr AstNodeKind Kind = (KIND);

    struct AstNode
    {
        explicit AstNode(AstNodeKind kind, std::uint32_t tokenIndex) noexcept
            : kind(kind)
            , tokenIndex(tokenIndex)
        {
        }

        template <typename To>
        [[nodiscard]] const To* CastTo() const noexcept
        {
            if (kind == To::Kind)
                return static_cast<const To*>(this);

            return nullptr;
        }

        AstNodeKind kind = AstNodeKind::None;
        std::uint32_t tokenIndex = 0;
    };

    struct AstNodeIdentifier : AstNode
    {
        AST_NODE(AstNodeIdentifier, AstNodeKind::Identifier);

        const char* name = nullptr;
    };

    struct AstNodeModule : AstNode
    {
        AST_NODE(AstNodeModule, AstNodeKind::Module);

        Array<const AstNode*> nodes;
    };

    struct AstNodeImport : AstNode
    {
        AST_NODE(AstNodeImport, AstNodeKind::Import);

        const AstNodeLiteralString* target = nullptr;
    };

    struct AstNodeAliasDecl : AstNode
    {
        AST_NODE(AstNodeAliasDecl, AstNodeKind::AliasDecl);
        const AstNodeIdentifier* name = nullptr;
        Array<const AstNodeAnnotation*> annotations;
        const AstNode* target = nullptr;
    };

    struct AstNodeStructDecl : AstNode
    {
        AST_NODE(AstNodeStructDecl, AstNodeKind::StructDecl);

        const AstNodeIdentifier* name = nullptr;
        Array<const AstNodeAnnotation*> annotations;
        const AstNodeIdentifier* base = nullptr;
        Array<const AstNodeField*> fields;
        const AstNodeLiteralInt* minVersion = nullptr;
        const AstNodeLiteralInt* maxVersion = nullptr;
    };

    struct AstNodeMessageDecl : AstNode
    {
        AST_NODE(AstNodeMessageDecl, AstNodeKind::MessageDecl);

        const AstNodeIdentifier* name = nullptr;
        Array<const AstNodeAnnotation*> annotations;
        Array<const AstNodeField*> fields;
    };

    struct AstNodeAttributeDecl : AstNode
    {
        AST_NODE(AstNodeAttributeDecl, AstNodeKind::Annotation);

        const AstNodeIdentifier* name = nullptr;
        Array<const AstNodeAnnotation*> annotations;
        Array<const AstNodeField*> fields;
    };

    struct AstNodeField : AstNode
    {
        AST_NODE(AstNodeField, AstNodeKind::StructDecl);

        const AstNodeIdentifier* name = nullptr;
        Array<const AstNodeAnnotation*> annotations;
        const AstNode* type = nullptr;
        const AstNode* value = nullptr;
        const AstNodeLiteralInt* proto = nullptr;
        const AstNodeLiteralInt* minVersion = nullptr;
        const AstNodeLiteralInt* maxVersion = nullptr;
    };

    struct AstNodeEnumDecl : AstNode
    {
        AST_NODE(AstNodeEnumDecl, AstNodeKind::EnumDecl);

        const AstNodeIdentifier* name = nullptr;
        const AstNodeIdentifier* base = nullptr;
        Array<const AstNodeAnnotation*> annotations;
        Array<const AstNodeEnumItem*> items;
    };

    struct AstNodeEnumItem : AstNode
    {
        AST_NODE(AstNodeEnumItem, AstNodeKind::EnumItem);

        const AstNodeIdentifier* name = nullptr;
        Array<const AstNodeAnnotation*> annotations;
        const AstNodeLiteralInt* value = nullptr;
    };

    struct AstNodeTypeName : AstNode
    {
        AST_NODE(AstNodeTypeName, AstNodeKind::TypeName);

        const AstNodeIdentifier* name = nullptr;
    };

    struct AstNodeTypeArray : AstNode
    {
        AST_NODE(AstNodeTypeArray, AstNodeKind::TypeArray);

        const AstNode* type = nullptr;
        const AstNodeLiteralInt* size = nullptr;
    };

    struct AstNodeTypePointer : AstNode
    {
        AST_NODE(AstNodeTypePointer, AstNodeKind::TypePointer);

        const AstNode* type = nullptr;
    };

    struct AstNodeTypeNullable : AstNode
    {
        AST_NODE(AstNodeTypeNullable, AstNodeKind::TypeNullable);

        const AstNode* type = nullptr;
    };

    struct AstNodeAnnotation : AstNode
    {
        AST_NODE(AstNodeAnnotation, AstNodeKind::Annotation);

        const AstNodeIdentifier* type = nullptr;
        Array<const AstNode*> arguments;
    };

    struct AstNodeLiteralBool : AstNode
    {
        AST_NODE(AstNodeLiteralBool, AstNodeKind::LiteralBool);

        bool value = true;
    };

    struct AstNodeLiteralNull : AstNode
    {
        AST_NODE(AstNodeLiteralNull, AstNodeKind::LiteralNull);
    };

    struct AstNodeLiteralInt : AstNode
    {
        AST_NODE(AstNodeLiteralInt, AstNodeKind::LiteralInt);

        int base = 10; // NOLINT(readability-magic-numbers)
        std::int64_t value = 0;
    };

    struct AstNodeLiteralFloat : AstNode
    {
        AST_NODE(AstNodeLiteralFloat, AstNodeKind::LiteralFloat);

        double value = 0.0;
    };

    struct AstNodeLiteralString : AstNode
    {
        AST_NODE(AstNodeLiteralString, AstNodeKind::LiteralString);

        const char* value = nullptr;
    };

    struct AstNodeNamedArgument : AstNode
    {
        AST_NODE(AstNodeNamedArgument, AstNodeKind::NamedArgument);

        const AstNodeIdentifier* name = nullptr;
        const AstNode* value = nullptr;
    };

    struct AstNodeInitializerList : AstNode
    {
        AST_NODE(AstNodeInitializerList, AstNodeKind::InitializerList);

        const AstNodeIdentifier* type = nullptr;
        Array<const AstNode*> elements;
    };

#undef AST_NODE
} // namespace potato::schematic::compiler
