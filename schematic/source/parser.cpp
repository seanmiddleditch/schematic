// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "parser.h"

#include "ast.h"

#include "schematic/arena.h"
#include "schematic/logger.h"
#include "schematic/source.h"

#include <fmt/core.h>

#include <charconv>
#include <cstdint>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

namespace
{
    enum class RecoverType
    {
        Expression, // end at any ) ] } ;
        Statement, // end at } ;
        Declaration, // end at }
    };

    struct Parser
    {
        ParseContext& ctx;
        Logger& logger;
        ArenaAllocator& alloc;
        const Source* source = nullptr;
        std::span<const Token> tokens;
        AstNodeModule* mod = nullptr;
        size_t next = 0;
        bool result = true;
        Array<const AstNodeAnnotation*> annotations;
        Array<const AstNodeKeywordDecl*> keywordDecls;
        Array<const AstNodeKeyword*> keywords;

        bool Parse();

        void Error(std::string_view message);
        void ErrorExpect(std::string_view expected);

        bool ParseAnnotations();
        bool ParseKeywords();

        bool ParseImport(const AstNodeImport*& imp);
        bool ParseKeywordDecl();
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
    };

    struct PrintToken
    {
        const Token& token;
        const Source* source = nullptr;
    };
} // namespace

template <>
struct fmt::formatter<PrintToken> : fmt::formatter<const char*>
{
    template <typename FormatContext>
    FMT_CONSTEXPR auto format(const PrintToken& value, FormatContext& ctx) const
        -> decltype(ctx.out())
    {
        fmt::format_to(ctx.out(), "{}", ToCStr(value.token.type));

        if (value.source == nullptr || value.token.length == 0)
            return ctx.out();

        const std::string_view repr = value.source->Data().substr(value.token.offset, value.token.length);

        switch (value.token.type)
        {
            using enum TokenType;
            case Integer: return fmt::format_to(ctx.out(), " {}", repr);
            case Float: return fmt::format_to(ctx.out(), " {}", repr);
            case Identifier: return fmt::format_to(ctx.out(), " `{}`", repr);
            default: return ctx.out();
        }
    }
};

const AstNodeModule* potato::schematic::compiler::Parse(ParseContext& ctx, Logger& logger, ArenaAllocator& alloc, const Source* source, std::span<const Token> tokens)
{
    if (tokens.empty())
        return nullptr;

    Parser parser{ .ctx = ctx, .logger = logger, .alloc = alloc, .source = source, .tokens = tokens, .mod = alloc.Create<AstNodeModule>(0) };
    if (!parser.Parse())
        return nullptr;

    return parser.mod;
}

bool Parser::Parse()
{
    while (!Consume(TokenType::End))
    {
        if (ConsumeKey("import"))
        {
            const AstNodeImport* imp = nullptr;
            if (!ParseImport(imp))
            {
                Recover(RecoverType::Declaration);
                continue;
            }

            const AstNodeModule* const imported = ctx.LoadImport(*imp);
            if (imported == nullptr)
                continue;

            for (const AstNode* const decl : imported->nodes)
                if (const AstNodeKeywordDecl* const kwDecl = decl->CastTo<AstNodeKeywordDecl>(); kwDecl != nullptr)
                    keywordDecls.EmplaceBack(alloc, kwDecl);
            continue;
        }

        if (!ParseAnnotations())
            continue;

        if (ConsumeKey("keyword"))
        {
            if (!ParseKeywordDecl())
                Recover(RecoverType::Declaration);
            continue;
        }

        if (!ParseKeywords())
            continue;

        if (ConsumeKey("struct"))
        {
            if (!ParseAggregateDecl())
                Recover(RecoverType::Declaration);
            continue;
        }

        if (ConsumeKey("attribute"))
        {
            if (!ParseAttributeDecl())
                Recover(RecoverType::Declaration);
            continue;
        }

        if (ConsumeKey("enum"))
        {
            if (!ParseEnumDecl())
                Recover(RecoverType::Declaration);
            continue;
        }

        ErrorExpect("declaration");
        break;
    }

    return result;
}

bool Parser::ParseAnnotations()
{
    annotations = {};

    while (Consume(TokenType::LBracket))
    {
        while (!Match(TokenType::RBracket))
        {
            if (Consume(TokenType::End))
                break;

            AstNodeAnnotation* const attr = alloc.Create<AstNodeAnnotation>(Pos());

            if (!ExpectQualifiedName(attr->name))
                return false;

            annotations.PushBack(alloc, attr);

            if (Consume(TokenType::LParen) && !Consume(TokenType::RParen))
            {
                while (!Match(TokenType::End))
                {
                    const AstNode* const arg = ParseArgument();
                    if (arg == nullptr)
                        break;

                    attr->arguments.PushBack(alloc, arg);

                    if (Match(TokenType::RParen))
                        break;

                    if (Consume(TokenType::Comma))
                        continue;

                    break;
                }

                if (!Expect(TokenType::RParen))
                    return false;
            }

            if (!Consume(TokenType::Comma))
                break;
        }

        if (!Expect(TokenType::RBracket))
            return false;
    }

    return true;
}

bool Parser::ParseKeywords()
{
    keywords = {};

    const auto FindKeyword = [this](std::string_view name) -> const AstNodeKeywordDecl*
    {
        for (const AstNodeKeywordDecl* decl : keywordDecls)
            if (decl->name.name == name)
                return decl;
        return nullptr;
    };

    const Token* token = nullptr;
    while (Match(TokenType::Identifier, &token))
    {
        const std::string_view name = source->Data().substr(token->offset, token->length);
        const AstNodeKeywordDecl* const decl = FindKeyword(name);
        if (decl == nullptr)
            break;

        AstNodeKeyword* const kw = alloc.Create<AstNodeKeyword>(Pos());
        kw->decl = decl;
        keywords.EmplaceBack(alloc, kw);

        Expect(TokenType::Identifier);
    }

    return true;
}

bool Parser::ParseImport(const AstNodeImport*& imp)
{
    AstNodeImport* const local = alloc.Create<AstNodeImport>(Pos());
    imp = local;

    if (!ExpectIdent(local->target))
        return false;

    if (!Expect(TokenType::SemiColon))
        return false;

    mod->nodes.EmplaceBack(alloc, local);
    return true;
}

bool Parser::ParseKeywordDecl()
{
    AstNodeKeywordDecl* const kw = alloc.Create<AstNodeKeywordDecl>(Pos());
    kw->annotations = annotations;

    mod->nodes.EmplaceBack(alloc, kw);

    if (!ExpectIdent(kw->name))
        return false;

    if (!Expect(TokenType::SemiColon))
        return false;

    keywordDecls.EmplaceBack(alloc, kw);

    return true;
}

bool Parser::ParseAggregateDecl()
{
    AstNodeAggregateDecl* const agg = alloc.Create<AstNodeAggregateDecl>(Pos());
    agg->annotations = annotations;
    agg->keywords = keywords;

    mod->nodes.EmplaceBack(alloc, agg);

    if (!ExpectIdent(agg->name))
        return false;

    if (Consume(TokenType::Colon))
    {
        if (!ExpectQualifiedName(agg->base))
            return false;
    }

    if (Consume(TokenType::SemiColon))
    {
        // empty body
    }
    else if (Expect(TokenType::LBrace))
    {
        while (!Match(TokenType::RBrace))
        {
            if (Consume(TokenType::End))
                break;

            if (!ParseAnnotations())
                return false;

            if (!ParseKeywords())
                return false;

            if (!ParseField(agg->fields))
                return false;

            if (!Expect(TokenType::SemiColon))
                return false;
        }

        Expect(TokenType::RBrace);
    }
    else
    {
        return false;
    }

    return true;
}

bool Parser::ParseAttributeDecl()
{
    AstNodeAttributeDecl* const attr = alloc.Create<AstNodeAttributeDecl>(Pos());
    attr->annotations = annotations;
    attr->keywords = keywords;

    mod->nodes.EmplaceBack(alloc, attr);

    if (!ExpectIdent(attr->name))
        return false;

    if (Consume(TokenType::SemiColon))
    {
        // empty body
    }
    else if (Expect(TokenType::LBrace))
    {
        while (!Match(TokenType::RBrace))
        {
            if (Consume(TokenType::End))
                break;

            if (!ParseField(attr->fields))
                return false;

            if (!Expect(TokenType::SemiColon))
                return false;
        }

        Expect(TokenType::RBrace);
    }
    else
    {
        return false;
    }

    return true;
}

const bool Parser::ParseField(Array<const AstNodeField*>& fields)
{
    AstNodeField* const field = alloc.Create<AstNodeField>(Pos());
    field->annotations = annotations;
    field->keywords = keywords;
    fields.PushBack(alloc, field);

    field->type = ParseType();
    if (field->type == nullptr)
        return false;

    if (!ExpectIdent(field->name))
        return false;

    if (Consume(TokenType::Equals))
    {
        if (field->value = ParseExpression(); field->value == nullptr)
            return false;
    }

    return true;
}

bool Parser::ParseEnumDecl()
{
    AstNodeEnumDecl* const enum_ = alloc.Create<AstNodeEnumDecl>(Pos());
    enum_->annotations = annotations;
    enum_->keywords = keywords;

    mod->nodes.EmplaceBack(alloc, enum_);

    if (!ExpectIdent(enum_->name))
        return false;

    if (Consume(TokenType::Colon))
    {
        if (!ExpectQualifiedName(enum_->base))
            return false;
    }

    if (Consume(TokenType::SemiColon))
    {
        // empty body
    }
    else if (Consume(TokenType::LBrace))
    {
        while (!Match(TokenType::RBrace))
        {
            if (Consume(TokenType::End))
                break;

            if (!ParseAnnotations())
                return false;

            if (!ParseKeywords())
                return false;

            AstNodeEnumItem* const item = alloc.Create<AstNodeEnumItem>(Pos());
            item->annotations = annotations;
            item->keywords = keywords;

            enum_->items.EmplaceBack(alloc, item);

            if (!ExpectIdent(item->name))
                continue;

            if (Consume(TokenType::Equals))
            {
                if (!ExpectInt(item->value))
                    continue;
            }

            if (!Consume(TokenType::Comma))
                break;
        }

        Expect(TokenType::RBrace);
    }
    else
    {
        return false;
    }

    return true;
}

const AstNodeExpression* Parser::ParseExpression()
{
    const auto pos = Pos();

    if (ConsumeKey("true"))
    {
        AstNodeLiteralBool* const literal = alloc.Create<AstNodeLiteralBool>(pos);
        literal->value = true;
        return literal;
    }

    if (ConsumeKey("false"))
    {
        AstNodeLiteralBool* const literal = alloc.Create<AstNodeLiteralBool>(pos);
        literal->value = false;
        return literal;
    }

    if (ConsumeKey("null"))
        return alloc.Create<AstNodeLiteralNull>(pos);

    if (const AstNodeLiteralInt* literal = nullptr; ConsumeInt(literal))
        return literal;

    if (const AstNodeLiteralFloat* literal = nullptr; ConsumeFloat(literal))
        return literal;

    AstQualifiedName qual;
    if (!ConsumeQualifiedName(qual) && !Match(TokenType::LBrace))
    {
        ErrorExpect("literal or initializer list");
        return nullptr;
    }

    if (Match(TokenType::LBrace))
        return ParseInitializer(qual);

    AstNodeQualifiedId* const literal = alloc.Create<AstNodeQualifiedId>(pos);
    literal->id = qual;
    return literal;
}

const AstNodeExpression* Parser::ParseInitializer(const AstQualifiedName& name)
{
    const std::uint32_t tokenIndex = name.parts ? name.parts.Front().tokenIndex : next;

    AstNodeInitializerList* const list = alloc.Create<AstNodeInitializerList>(tokenIndex);
    list->type = name;

    if (!Expect(TokenType::LBrace))
        return nullptr;

    while (!Match(TokenType::RBrace))
    {
        if (Consume(TokenType::End))
            break;

        const AstNode* element = ParseArgument();
        if (element == nullptr)
            return nullptr;

        list->elements.PushBack(alloc, element);

        if (!Consume(TokenType::Comma))
            break;
    }

    if (!Expect(TokenType::RBrace))
        return nullptr;

    return list;
}

const AstNode* Parser::ParseArgument()
{
    AstIdentifier ident;
    if (ConsumeIdent(ident))
    {
        if (Consume(TokenType::Equals))
        {
            AstNodeNamedArgument* const named = alloc.Create<AstNodeNamedArgument>(Pos());

            named->name = ident;
            named->value = ParseExpression();

            if (named->value == nullptr)
                return nullptr;

            return named;
        }
        else
        {
            Unwind();
        }
    }

    return ParseExpression();
}

const AstNodeType* Parser::ParseType()
{
    AstNodeType* type = nullptr;

    {
        AstQualifiedName name;
        if (!ExpectQualifiedName(name))
            return type;

        AstNodeTypeQualified* qual = alloc.Create<AstNodeTypeQualified>(name.parts.Front().tokenIndex);
        qual->name = name;
        type = qual;
    }

    for (;;)
    {
        if (Consume(TokenType::LBracket))
        {
            AstNodeTypeArray* const array = alloc.Create<AstNodeTypeArray>(type->tokenIndex);
            array->type = type;
            type = array;

            ConsumeInt(array->size);

            if (!Expect(TokenType::RBracket))
                return nullptr;

            continue;
        }

        if (Consume(TokenType::Star))
        {
            AstNodeTypePolymorphic* const poly = alloc.Create<AstNodeTypePolymorphic>(type->tokenIndex);
            poly->type = type;
            type = poly;

            continue;
        }

        if (Consume(TokenType::Question))
        {
            AstNodeTypeNullable* const nullable = alloc.Create<AstNodeTypeNullable>(type->tokenIndex);
            nullable->type = type;
            type = nullable;

            continue;
        }

        break;
    }

    return type;
}

void Parser::Recover(RecoverType type)
{
    while (!Match(TokenType::End))
    {
        switch (type)
        {
            using enum RecoverType;
            case Expression:
                if (Consume(TokenType::RParen))
                    return;
                if (Consume(TokenType::RBracket))
                    return;
                [[fallthrough]];
            case Statement:
                if (Consume(TokenType::SemiColon))
                    return;
                [[fallthrough]];
            case Declaration:
                if (Consume(TokenType::RBrace))
                    return;
                break;
        }

        ++next;
    }
}

bool Parser::ConsumeInt(const AstNodeLiteralInt*& lit)
{
    const bool neg = Consume(TokenType::Minus);

    auto pos = Pos();

    AstNodeLiteralInt* result = nullptr;

    if (Consume(TokenType::Integer))
    {
        lit = result = alloc.Create<AstNodeLiteralInt>(pos);
        result->base = 10;
    }
    else if (Consume(TokenType::HexInteger))
    {
        lit = result = alloc.Create<AstNodeLiteralInt>(pos);
        result->base = 16;
    }
    else if (Consume(TokenType::BinaryInteger))
    {
        lit = result = alloc.Create<AstNodeLiteralInt>(pos);
        result->base = 2;
    }
    else
    {
        if (neg)
            Unwind();
        return false;
    }

    const Token& token = tokens[pos];
    std::string_view number = source->Data().substr(token.offset, token.length);

    // skip the 0x or 0b prefix
    if (result->base != 10)
        number = number.substr(2);

    const auto err = std::from_chars(number.data(), number.data() + number.size(), result->value, result->base);
    if (err.ec != std::errc{})
        Error("Value out of range");

    if (neg)
        result->value = -result->value;

    return true;
}

bool Parser::ExpectInt(const AstNodeLiteralInt*& lit)
{
    if (ConsumeInt(lit))
        return true;

    ErrorExpect("integer");
    return false;
}

bool Parser::ConsumeFloat(const AstNodeLiteralFloat*& lit)
{
    const bool neg = Consume(TokenType::Minus);

    auto pos = Pos();

    if (!Consume(TokenType::Float))
    {
        if (neg)
            Unwind();
        return false;
    }

    AstNodeLiteralFloat* const result = alloc.Create<AstNodeLiteralFloat>(pos);
    lit = result;

    const Token& token = tokens[pos];
    const std::string_view number = source->Data().substr(token.offset, token.length);

    const auto err = std::from_chars(number.data(), number.data() + number.size(), result->value);
    if (err.ec != std::errc{})
        Error("Value out of range");

    if (neg)
        result->value = -result->value;

    return true;
}

void Parser::Error(std::string_view message)
{
    const Token& token = tokens[next];
    logger.Error({ .source = source, .offset = token.offset, .length = token.length }, message);
    result = false;
}

void Parser::ErrorExpect(std::string_view expected)
{
    if (next != 0)
        Error(fmt::format("Unexpected {} after {} expected {}",
            PrintToken{ .token = tokens[next], .source = source },
            PrintToken{ .token = tokens[next - 1], .source = source },
            expected));
    else
        Error(fmt::format("Unexpected {} expected {}",
            PrintToken{ .token = tokens[next], .source = source },
            expected));
}

bool Parser::Match(TokenType type, const Token** out)
{
    if (next == tokens.size())
        return type == TokenType::End;
    if (tokens[next].type != type)
        return false;
    if (out != nullptr)
        *out = &tokens[next];
    return true;
}

bool Parser::Consume(TokenType type, const Token** out)
{
    if (!Match(type, out))
        return false;
    if (next + 1 < tokens.size())
        ++next;
    return true;
}

bool Parser::Expect(TokenType type, const Token** out)
{
    if (Consume(type, out))
        return true;
    ErrorExpect(ToCStr(type));
    return false;
}

bool Parser::ConsumeIdent(AstIdentifier& out)
{
    const auto pos = Pos();

    const Token* token = nullptr;
    if (!Consume(TokenType::Identifier, &token))
        return false;

    const std::string_view ident = source->Data().substr(token->offset, token->length);
    out.name = alloc.NewString(ident);
    out.tokenIndex = pos;
    return true;
}

bool Parser::ExpectIdent(AstIdentifier& out)
{
    if (ConsumeIdent(out))
        return true;

    ErrorExpect("identifier");
    return false;
}

bool Parser::ConsumeQualifiedName(AstQualifiedName& out)
{
    AstIdentifier ident;
    if (!ConsumeIdent(ident))
        return false;

    out.parts.PushBack(alloc, ident);

    while (Consume(TokenType::Dot))
    {
        if (!ExpectIdent(ident))
            break;

        out.parts.PushBack(alloc, ident);
    }

    return true;
}

bool Parser::ExpectQualifiedName(AstQualifiedName& out)
{
    if (ConsumeQualifiedName(out))
        return true;

    ErrorExpect("qualified name");
    return false;
}

bool Parser::ConsumeKey(std::string_view keyword)
{
    const Token* token = nullptr;
    if (!Match(TokenType::Identifier, &token))
        return false;
    const std::string_view extracted = source->Data().substr(token->offset, token->length);
    if (extracted != keyword)
        return false;
    ++next;
    return true;
}

void Parser::Unwind(std::uint32_t count) noexcept
{
    assert(next >= count);
    next -= count;
}

std::uint32_t Parser::Pos(const Token* token) const
{
    if (token == nullptr)
        token = &tokens[next];

    return static_cast<std::uint32_t>(token - tokens.data());
}
