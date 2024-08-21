// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ast.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/compiler.h"

#include <span>

namespace potato::schematic::compiler
{
    class Parser final
    {
    public:
        Parser(CompileContext& ctx, ArenaAllocator& arena, ModuleId moduleId, const Array<Token>& tokens) noexcept
            : ctx_(ctx)
            , arena_(arena)
            , moduleId_(moduleId)
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

        bool ParseImport();
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

        CompileContext& ctx_;
        ArenaAllocator& arena_;
        ModuleId moduleId_;
        std::string_view contents_;
        std::span<const Token> tokens_;
        AstNodeModule* mod = nullptr;
        size_t next_ = 0;
        bool failed_ = false;
        Array<const AstNodeAnnotation*> annotations_;
    };
} // namespace potato::schematic::compiler
