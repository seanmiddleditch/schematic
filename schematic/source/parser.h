// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ast.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/compiler.h"

#include <span>

namespace potato::schematic::compiler
{
    class ParseContext
    {
    public:
        virtual const AstNodeModule* LoadImport(const AstNodeImport& imp) = 0;

    protected:
        ~ParseContext() = default;
    };

    class Parser final
    {
    public:
        Parser(ParseContext& pctx, CompileContext& cctx, ArenaAllocator& arena, FileId file, const Array<Token>& tokens) noexcept
            : pctx_(pctx)
            , cctx_(cctx)
            , arena_(arena)
            , file_(file)
            , tokens_(tokens)
            , mod(arena.New<AstNodeModule>(0))
        {
        }

        const AstNodeModule* Parse();

    private:
        enum class RecoverType
        {
            Expression, // end at any ) ] } ;
            Statement, // end at } ;
            Declaration, // end at }
        };

        void Error(std::string_view message);
        void ErrorExpect(std::string_view expected);

        bool ParseAnnotations();

        bool ParseImport(const AstNodeImport*& imp);
        bool ParseAggregateDecl();
        bool ParseAttributeDecl();
        bool ParseEnumDecl();

        const bool ParseField(Array<const AstNodeField*>& fields);
        const AstNodeExpression* ParseExpression();
        const AstNodeExpression* ParseInitializer(const AstQualifiedName& name);
        const AstNode* ParseArgument();
        const AstNodeType* ParseType();

        void Recover(RecoverType type);

        bool ConsumeInt(const AstNodeLiteralInt*& lit);
        bool ExpectInt(const AstNodeLiteralInt*& lit);

        bool ConsumeFloat(const AstNodeLiteralFloat*& lit);

        bool ConsumeString(const AstNodeLiteralString*& lit);

        bool Match(TokenType type, const Token** out = nullptr);

        bool Consume(TokenType type, const Token** out = nullptr);
        bool Expect(TokenType type, const Token** out = nullptr);

        bool ConsumeIdent(AstIdentifier& out);
        bool ExpectIdent(AstIdentifier& out);

        bool ConsumeQualifiedName(AstQualifiedName& out);
        bool ExpectQualifiedName(AstQualifiedName& out);

        bool ConsumeKey(std::string_view keyword);

        void Unwind(std::uint32_t count = 1) noexcept;

        std::uint32_t Pos(const Token* token = nullptr) const;

        ParseContext& pctx_;
        CompileContext& cctx_;
        ArenaAllocator& arena_;
        FileId file_;
        std::string_view contents_;
        std::span<const Token> tokens_;
        AstNodeModule* mod = nullptr;
        size_t next_ = 0;
        bool failed_ = false;
        Array<const AstNodeAnnotation*> annotations_;
    };
} // namespace potato::schematic::compiler
