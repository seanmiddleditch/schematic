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

    struct AstIdentifier;

    struct AstNodeModule;
    struct AstNodeImport;
    struct AstNodeStructDecl;
    struct AstNodeMessageDecl;
    struct AstNodeAttributeDecl;
    struct AstNodeField;
    struct AstNodeEnumDecl;
    struct AstNodeEnumItem;
    struct AstNodeType;
    struct AstNodeTypeName;
    struct AstNodeTypeArray;
    struct AstNodeTypePointer;
    struct AstNodeTypeNullable;
    struct AstNodeAnnotation;
    struct AstNodeExpression;
    struct AstNodeLiteralBool;
    struct AstNodeLiteralNull;
    struct AstNodeLiteralInt;
    struct AstNodeLiteralFloat;
    struct AstNodeLiteralString;
    struct AstNodeNamedArgument;
    struct AstNodeInitializerList;
    struct AstNodeIdentifier;

#define AST_NODE(SELF, PARENT, KIND) \
    explicit SELF(std::uint32_t tokenIndex) : PARENT((KIND), tokenIndex) { } \
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

    struct AstIdentifier
    {
        const char* name = nullptr;
        std::uint32_t tokenIndex = 0;
    };

    struct AstNodeModule : AstNode
    {
        AST_NODE(AstNodeModule, AstNode, AstNodeKind::Module);

        Array<const AstNode*> nodes;
    };

    struct AstNodeImport : AstNode
    {
        AST_NODE(AstNodeImport, AstNode, AstNodeKind::Import);

        const AstNodeLiteralString* target = nullptr;
    };

    struct AstNodeDecl : AstNode
    {
        using AstNode::AstNode;

        AstIdentifier name;
        Array<const AstNodeAnnotation*> annotations;
    };

    struct AstNodeStructDecl : AstNodeDecl
    {
        AST_NODE(AstNodeStructDecl, AstNodeDecl, AstNodeKind::StructDecl);

        AstIdentifier base;
        Array<const AstNodeField*> fields;
        const AstNodeLiteralInt* minVersion = nullptr;
        const AstNodeLiteralInt* maxVersion = nullptr;
    };

    struct AstNodeMessageDecl : AstNodeDecl
    {
        AST_NODE(AstNodeMessageDecl, AstNodeDecl, AstNodeKind::MessageDecl);

        Array<const AstNodeField*> fields;
    };

    struct AstNodeAttributeDecl : AstNodeDecl
    {
        AST_NODE(AstNodeAttributeDecl, AstNodeDecl, AstNodeKind::Annotation);

        Array<const AstNodeField*> fields;
    };

    struct AstNodeField : AstNodeDecl
    {
        AST_NODE(AstNodeField, AstNodeDecl, AstNodeKind::StructDecl);

        const AstNodeType* type = nullptr;
        const AstNodeExpression* value = nullptr;
        const AstNodeLiteralInt* proto = nullptr;
        const AstNodeLiteralInt* minVersion = nullptr;
        const AstNodeLiteralInt* maxVersion = nullptr;
    };

    struct AstNodeEnumDecl : AstNodeDecl
    {
        AST_NODE(AstNodeEnumDecl, AstNodeDecl, AstNodeKind::EnumDecl);

        AstIdentifier base;
        Array<const AstNodeEnumItem*> items;
    };

    struct AstNodeEnumItem : AstNodeDecl
    {
        AST_NODE(AstNodeEnumItem, AstNodeDecl, AstNodeKind::EnumItem);

        const AstNodeLiteralInt* value = nullptr;
    };

    struct AstNodeType : AstNode
    {
        using AstNode::AstNode;
    };

    struct AstNodeTypeName : AstNodeType
    {
        AST_NODE(AstNodeTypeName, AstNodeType, AstNodeKind::TypeName);

        AstIdentifier name;
    };

    struct AstNodeTypeArray : AstNodeType
    {
        AST_NODE(AstNodeTypeArray, AstNodeType, AstNodeKind::TypeArray);

        const AstNodeType* type = nullptr;
        const AstNodeLiteralInt* size = nullptr;
    };

    struct AstNodeTypePointer : AstNodeType
    {
        AST_NODE(AstNodeTypePointer, AstNodeType, AstNodeKind::TypePointer);

        const AstNodeType* type = nullptr;
    };

    struct AstNodeTypeNullable : AstNodeType
    {
        AST_NODE(AstNodeTypeNullable, AstNodeType, AstNodeKind::TypeNullable);

        const AstNodeType* type = nullptr;
    };

    struct AstNodeAnnotation : AstNode
    {
        AST_NODE(AstNodeAnnotation, AstNode, AstNodeKind::Annotation);

        AstIdentifier name;
        Array<const AstNode*> arguments;
    };

    struct AstNodeExpression : AstNode
    {
        using AstNode::AstNode;
    };

    struct AstNodeLiteral : AstNodeExpression
    {
        using AstNodeExpression::AstNodeExpression;
    };

    struct AstNodeLiteralBool : AstNodeLiteral
    {
        AST_NODE(AstNodeLiteralBool, AstNodeLiteral, AstNodeKind::LiteralBool);

        bool value = true;
    };

    struct AstNodeLiteralNull : AstNodeLiteral
    {
        AST_NODE(AstNodeLiteralNull, AstNodeLiteral, AstNodeKind::LiteralNull);
    };

    struct AstNodeLiteralInt : AstNodeLiteral
    {
        AST_NODE(AstNodeLiteralInt, AstNodeLiteral, AstNodeKind::LiteralInt);

        int base = 10; // NOLINT(readability-magic-numbers)
        std::int64_t value = 0;
    };

    struct AstNodeLiteralFloat : AstNodeLiteral
    {
        AST_NODE(AstNodeLiteralFloat, AstNodeLiteral, AstNodeKind::LiteralFloat);

        double value = 0.0;
    };

    struct AstNodeLiteralString : AstNodeLiteral
    {
        AST_NODE(AstNodeLiteralString, AstNodeLiteral, AstNodeKind::LiteralString);

        const char* value = nullptr;
    };

    struct AstNodeNamedArgument : AstNode
    {
        AST_NODE(AstNodeNamedArgument, AstNode, AstNodeKind::NamedArgument);

        AstIdentifier name;
        const AstNodeExpression* value = nullptr;
    };

    struct AstNodeInitializerList : AstNodeExpression
    {
        AST_NODE(AstNodeInitializerList, AstNodeExpression, AstNodeKind::InitializerList);

        AstIdentifier type;
        Array<const AstNode*> elements;
    };

    struct AstNodeIdentifier : AstNodeExpression
    {
        AST_NODE(AstNodeIdentifier, AstNodeExpression, AstNodeKind::Identifier);

        AstIdentifier name;
    };

#undef AST_NODE
} // namespace potato::schematic::compiler
