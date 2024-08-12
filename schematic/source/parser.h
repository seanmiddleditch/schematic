// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "arena.h"
#include "ast.h"
#include "token.h"

#include "schematic/source.h"

#include <span>

namespace potato::schematic
{
    class CompileContext;
}

namespace potato::schematic::compiler
{
    class ParseContext
    {
    public:
        virtual const AstNodeModule* LoadImport(const AstNodeImport& imp) = 0;

    protected:
        ~ParseContext() = default;
    };

    class Parser
    {
    public:
        Parser(ParseContext& pctx, CompileContext& cctx, ArenaAllocator& alloc, const Source* source, const Array<Token>& tokens)
            : pctx_(pctx)
            , cctx_(cctx)
            , alloc_(alloc)
            , source_(source)
            , tokens_(tokens)
            , mod(alloc.Create<AstNodeModule>(0))
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
        ArenaAllocator& alloc_;
        const Source* source_ = nullptr;
        std::span<const Token> tokens_;
        AstNodeModule* mod = nullptr;
        size_t next_ = 0;
        bool failed_ = false;
        Array<const AstNodeAnnotation*> annotations_;
    };
} // namespace potato::schematic::compiler
