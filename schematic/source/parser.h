// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ast.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/compiler.h"

#include <fmt/core.h>

#include <span>

namespace schematic::compiler
{
    class Parser final
    {
    public:
        Parser(ArenaAllocator& arena, Logger& logger, std::string_view filename, std::string_view source, const Array<Token>& tokens) noexcept
            : arena_(arena)
            , logger_(logger)
            , filename_(filename)
            , source_(source)
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

        enum class FieldMode
        {
            Struct,
            Attribute,
            Message,
        };

        void Error(std::string_view message);
        template <typename... Args>
        void Error(fmt::format_string<Args...> format, Args&&... args);
        void ErrorExpect(std::string_view expected);

        bool ParseAnnotations();

        bool ParseImport();
        bool ParseAliasDecl();
        bool ParseStructDecl();
        bool ParseSchemaDecl();
        bool ParseMessageDecl();
        bool ParseAttributeDecl();
        bool ParseEnumDecl();

        const bool ParseField(Array<const AstNodeField*>& fields, FieldMode mode);
        const AstNode* ParseExpression();
        const AstNode* ParseInitializer();
        const AstNode* ParseArgument();
        const AstNode* ParseType();

        void Recover(RecoverType type);

        bool ConsumeInt(const AstNodeLiteralInt*& lit);
        bool ExpectInt(const AstNodeLiteralInt*& lit);

        bool ConsumeFloat(const AstNodeLiteralFloat*& lit);

        bool ConsumeString(const AstNodeLiteralString*& lit);

        bool Match(TokenType type, const Token** out = nullptr);

        bool Consume(TokenType type, const Token** out = nullptr);
        bool Expect(TokenType type, const Token** out = nullptr);

        bool ConsumeIdent(const AstNodeIdentifier** out);
        bool ExpectIdent(const AstNodeIdentifier** out);

        bool ConsumeKey(std::string_view keyword);

        void Unwind(std::uint32_t count = 1) noexcept;

        std::uint32_t Pos(const Token* token = nullptr) const;

        ArenaAllocator& arena_;
        Logger& logger_;
        std::string_view filename_;
        std::string_view source_;
        std::span<const Token> tokens_;
        AstNodeModule* mod = nullptr;
        size_t next_ = 0;
        bool failed_ = false;
        Array<const AstNodeAnnotation*> annotations_;
    };
} // namespace schematic::compiler
