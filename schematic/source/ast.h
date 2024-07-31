// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/arena.h"

#include <fmt/core.h>

#include <cassert>
#include <cstdint>

namespace potato::schematic::compiler
{
    enum class AstNodeKind
    {
        None,
        Module,
        Import,
        KeywordDecl,
        Keyword,
        AggregateDecl,
        AttributeDecl,
        Field,
        EnumDecl,
        EnumItem,
        TypeQualified,
        TypeArray,
        TypePolymorphic,
        TypeNullable,
        Annotation,
        LiteralBool,
        LiteralNull,
        LiteralInt,
        LiteralFloat,
        LiteralString,
        NamedArgument,
        InitializerList,
        QualifiedId,
    };

    struct AstIdentifier;

    struct AstNodeModule;
    struct AstNodeImport;
    struct AstNodeAggregateDecl;
    struct AstNodeKeywordDecl;
    struct AstNodeKeyword;
    struct AstNodeAttributeDecl;
    struct AstNodeField;
    struct AstNodeEnumDecl;
    struct AstNodeEnumItem;
    struct AstNodeType;
    struct AstNodeTypeQualified;
    struct AstNodeTypeArray;
    struct AstNodeTypePolymorphic;
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
    struct AstNodeQualifiedId;

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
        CStringView name;
        std::uint32_t tokenIndex = 0;
    };

    struct AstQualifiedName
    {
        Array<AstIdentifier> parts;
    };

    struct AstNodeModule : AstNode
    {
        AST_NODE(AstNodeModule, AstNode, AstNodeKind::Module);

        Array<const AstNode*> nodes;
    };

    struct AstNodeImport : AstNode
    {
        AST_NODE(AstNodeImport, AstNode, AstNodeKind::Import);

        AstIdentifier target;
    };

    struct AstNodeDecl : AstNode
    {
        using AstNode::AstNode;

        AstIdentifier name;
        Array<const AstNodeAnnotation*> annotations;
        Array<const AstNodeKeyword*> keywords;
    };

    struct AstNodeKeywordDecl : AstNodeDecl
    {
        AST_NODE(AstNodeKeywordDecl, AstNodeDecl, AstNodeKind::KeywordDecl);
    };

    struct AstNodeKeyword : AstNode
    {
        AST_NODE(AstNodeKeyword, AstNode, AstNodeKind::Keyword);

        const AstNodeKeywordDecl* decl = nullptr;
    };

    struct AstNodeAggregateDecl : AstNodeDecl
    {
        AST_NODE(AstNodeAggregateDecl, AstNodeDecl, AstNodeKind::AggregateDecl);

        AstQualifiedName base;
        Array<const AstNodeField*> fields;
    };

    struct AstNodeAttributeDecl : AstNodeDecl
    {
        AST_NODE(AstNodeAttributeDecl, AstNodeDecl, AstNodeKind::Annotation);

        Array<const AstNodeField*> fields;
    };

    struct AstNodeField : AstNodeDecl
    {
        AST_NODE(AstNodeField, AstNodeDecl, AstNodeKind::AggregateDecl);

        const AstNodeType* type = nullptr;
        const AstNodeExpression* value = nullptr;
    };

    struct AstNodeEnumDecl : AstNodeDecl
    {
        AST_NODE(AstNodeEnumDecl, AstNodeDecl, AstNodeKind::EnumDecl);

        AstQualifiedName base;
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

    struct AstNodeTypeQualified : AstNodeType
    {
        AST_NODE(AstNodeTypeQualified, AstNodeType, AstNodeKind::TypeQualified);

        AstQualifiedName name;
    };

    struct AstNodeTypeArray : AstNodeType
    {
        AST_NODE(AstNodeTypeArray, AstNodeType, AstNodeKind::TypeArray);

        const AstNodeType* type = nullptr;
        const AstNodeLiteralInt* size = nullptr;
    };

    struct AstNodeTypePolymorphic : AstNodeType
    {
        AST_NODE(AstNodeTypePolymorphic, AstNodeType, AstNodeKind::TypePolymorphic);

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

        AstQualifiedName name;
        Array<const AstNode*> arguments;
    };

    struct AstNodeExpression : AstNode
    {
        using AstNode::AstNode;
    };

    struct AstNodeLiteralBool : AstNodeExpression
    {
        AST_NODE(AstNodeLiteralBool, AstNodeExpression, AstNodeKind::LiteralBool);

        bool value = true;
    };

    struct AstNodeLiteralNull : AstNodeExpression
    {
        AST_NODE(AstNodeLiteralNull, AstNodeExpression, AstNodeKind::LiteralNull);
    };

    struct AstNodeLiteralInt : AstNodeExpression
    {
        AST_NODE(AstNodeLiteralInt, AstNodeExpression, AstNodeKind::LiteralInt);

        int base = 10; // NOLINT(readability-magic-numbers)
        std::int64_t value = 0;
    };

    struct AstNodeLiteralFloat : AstNodeExpression
    {
        AST_NODE(AstNodeLiteralFloat, AstNodeExpression, AstNodeKind::LiteralFloat);

        double value = 0.0;
    };

    struct AstNodeLiteralString : AstNodeExpression
    {
        AST_NODE(AstNodeLiteralString, AstNodeExpression, AstNodeKind::LiteralString);

        CStringView value;
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

        AstQualifiedName type;
        Array<const AstNode*> elements;
    };

    struct AstNodeQualifiedId : AstNodeExpression
    {
        AST_NODE(AstNodeQualifiedId, AstNodeExpression, AstNodeKind::QualifiedId);

        AstQualifiedName id;
    };

#undef AST_NODE
} // namespace potato::schematic::compiler

template <>
struct fmt::formatter<potato::schematic::compiler::AstQualifiedName> : fmt::formatter<const char*>
{
    template <typename FormatContext>
    FMT_CONSTEXPR auto format(const potato::schematic::compiler::AstQualifiedName& name, FormatContext& ctx) const
        -> decltype(ctx.out())
    {
        bool first = true;
        for (const auto& part : name.parts)
        {
            if (!first)
                fmt::format_to(ctx.out(), ".");
            first = false;
            fmt::format_to(ctx.out(), "{}", part.name.CStr());
        }
        return ctx.out();
    }
};
