// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "parser.h"

#include "arena.h"
#include "ast.h"

#include "schematic/compiler.h"
#include "schematic/source.h"

#include <fmt/core.h>

#include <charconv>
#include <cstdint>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

namespace
{
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

const AstNodeModule* Parser::Parse()
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

            pctx_.LoadImport(*imp);
            continue;
        }

        if (!ParseAnnotations())
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

    if (failed_)
        return nullptr;

    return mod;
}

bool Parser::ParseAnnotations()
{
    annotations_ = {};

    while (Consume(TokenType::LBracket))
    {
        while (!Match(TokenType::RBracket))
        {
            if (Consume(TokenType::End))
                break;

            AstNodeAnnotation* const attr = alloc_.Create<AstNodeAnnotation>(Pos());

            if (!ExpectQualifiedName(attr->name))
                return false;

            annotations_.PushBack(alloc_, attr);

            if (Consume(TokenType::LParen) && !Consume(TokenType::RParen))
            {
                while (!Match(TokenType::End))
                {
                    const AstNode* const arg = ParseArgument();
                    if (arg == nullptr)
                        break;

                    attr->arguments.PushBack(alloc_, arg);

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

bool Parser::ParseImport(const AstNodeImport*& imp)
{
    AstNodeImport* const local = alloc_.Create<AstNodeImport>(Pos());
    imp = local;

    if (!ExpectIdent(local->target))
        return false;

    if (!Expect(TokenType::SemiColon))
        return false;

    mod->nodes.EmplaceBack(alloc_, local);
    return true;
}

bool Parser::ParseAggregateDecl()
{
    AstNodeAggregateDecl* const agg = alloc_.Create<AstNodeAggregateDecl>(Pos());
    agg->annotations = annotations_;

    mod->nodes.EmplaceBack(alloc_, agg);

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
    AstNodeAttributeDecl* const attr = alloc_.Create<AstNodeAttributeDecl>(Pos());
    attr->annotations = annotations_;

    mod->nodes.EmplaceBack(alloc_, attr);

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
    AstNodeField* const field = alloc_.Create<AstNodeField>(Pos());
    field->annotations = annotations_;
    fields.PushBack(alloc_, field);

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
    AstNodeEnumDecl* const enum_ = alloc_.Create<AstNodeEnumDecl>(Pos());
    enum_->annotations = annotations_;

    mod->nodes.EmplaceBack(alloc_, enum_);

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

            AstNodeEnumItem* const item = alloc_.Create<AstNodeEnumItem>(Pos());
            item->annotations = annotations_;

            enum_->items.EmplaceBack(alloc_, item);

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
        AstNodeLiteralBool* const literal = alloc_.Create<AstNodeLiteralBool>(pos);
        literal->value = true;
        return literal;
    }

    if (ConsumeKey("false"))
    {
        AstNodeLiteralBool* const literal = alloc_.Create<AstNodeLiteralBool>(pos);
        literal->value = false;
        return literal;
    }

    if (ConsumeKey("null"))
        return alloc_.Create<AstNodeLiteralNull>(pos);

    if (const AstNodeLiteralInt* literal = nullptr; ConsumeInt(literal))
        return literal;

    if (const AstNodeLiteralFloat* literal = nullptr; ConsumeFloat(literal))
        return literal;

    if (const AstNodeLiteralString* literal = nullptr; ConsumeString(literal))
        return literal;

    AstQualifiedName qual;
    if (!ConsumeQualifiedName(qual) && !Match(TokenType::LBrace))
    {
        ErrorExpect("literal or initializer list");
        return nullptr;
    }

    if (Match(TokenType::LBrace))
        return ParseInitializer(qual);

    AstNodeQualifiedId* const literal = alloc_.Create<AstNodeQualifiedId>(pos);
    literal->id = qual;
    return literal;
}

const AstNodeExpression* Parser::ParseInitializer(const AstQualifiedName& name)
{
    const std::uint32_t tokenIndex = name.parts ? name.parts.Front().tokenIndex : next_;

    AstNodeInitializerList* const list = alloc_.Create<AstNodeInitializerList>(tokenIndex);
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

        list->elements.PushBack(alloc_, element);

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
            AstNodeNamedArgument* const named = alloc_.Create<AstNodeNamedArgument>(Pos());

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

        AstNodeTypeQualified* qual = alloc_.Create<AstNodeTypeQualified>(name.parts.Front().tokenIndex);
        qual->name = name;
        type = qual;
    }

    for (;;)
    {
        if (Consume(TokenType::LBracket))
        {
            AstNodeTypeArray* const array = alloc_.Create<AstNodeTypeArray>(type->tokenIndex);
            array->type = type;
            type = array;

            ConsumeInt(array->size);

            if (!Expect(TokenType::RBracket))
                return nullptr;

            continue;
        }

        if (Consume(TokenType::Star))
        {
            AstNodeTypePolymorphic* const poly = alloc_.Create<AstNodeTypePolymorphic>(type->tokenIndex);
            poly->type = type;
            type = poly;

            continue;
        }

        if (Consume(TokenType::Question))
        {
            AstNodeTypeNullable* const nullable = alloc_.Create<AstNodeTypeNullable>(type->tokenIndex);
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

        ++next_;
    }
}

bool Parser::ConsumeInt(const AstNodeLiteralInt*& lit)
{
    const bool neg = Consume(TokenType::Minus);

    auto pos = Pos();

    AstNodeLiteralInt* result = nullptr;

    if (Consume(TokenType::Integer))
    {
        lit = result = alloc_.Create<AstNodeLiteralInt>(pos);
        result->base = 10;
    }
    else if (Consume(TokenType::HexInteger))
    {
        lit = result = alloc_.Create<AstNodeLiteralInt>(pos);
        result->base = 16;
    }
    else if (Consume(TokenType::BinaryInteger))
    {
        lit = result = alloc_.Create<AstNodeLiteralInt>(pos);
        result->base = 2;
    }
    else
    {
        if (neg)
            Unwind();
        return false;
    }

    const Token& token = tokens_[pos];
    std::string_view number = source_->Data().substr(token.offset, token.length);

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

    AstNodeLiteralFloat* const result = alloc_.Create<AstNodeLiteralFloat>(pos);
    lit = result;

    const Token& token = tokens_[pos];
    const std::string_view number = source_->Data().substr(token.offset, token.length);

    const auto err = std::from_chars(number.data(), number.data() + number.size(), result->value);
    if (err.ec != std::errc{})
        Error("Value out of range");

    if (neg)
        result->value = -result->value;

    return true;
}

bool Parser::ConsumeString(const AstNodeLiteralString*& lit)
{
    auto pos = Pos();
    const Token* token = nullptr;

    if (Consume(TokenType::String, &token))
    {
        AstNodeLiteralString* const result = alloc_.Create<AstNodeLiteralString>(pos);
        lit = result;

        const std::string_view content = source_->Data().substr(token->offset + 1 /*"*/, token->length - 2 /*double "*/);

        std::size_t length = 0;
        for (const char c : content)
        {
            if (c != '\\')
                ++length;
        }

        char* out = static_cast<char*>(alloc_.Allocate(length + 1 /*NUL*/, 1));
        const char* const string = out;

        for (const char* c = content.data(); c != content.data() + content.size(); ++c)
        {
            if (*c != '\\')
            {
                *out++ = *c;
                continue;
            }

            switch (*++c)
            {
                case '\\':
                    *out++ = '\\';
                    break;
                case 'n':
                    *out++ = '\n';
                    break;
            }
        }

        *out = '\0';
        result->value = CStringView(string, length);

        return true;
    }

    if (Consume(TokenType::MultilineString, &token))
    {
        AstNodeLiteralString* const result = alloc_.Create<AstNodeLiteralString>(pos);
        lit = result;

        result->value = alloc_.NewString(source_->Data().substr(token->offset + 3 /*"""*/, token->length - 6 /*double """*/));
        return true;
    }

    return false;
}

void Parser::Error(std::string_view message)
{
    const Token& token = tokens_[next_];
    cctx_.Error({ .source = source_, .offset = token.offset, .length = token.length }, message);
    failed_ = true;
}

void Parser::ErrorExpect(std::string_view expected)
{
    if (next_ != 0)
        Error(fmt::format("Unexpected {} after {} expected {}",
            PrintToken{ .token = tokens_[next_], .source = source_ },
            PrintToken{ .token = tokens_[next_ - 1], .source = source_ },
            expected));
    else
        Error(fmt::format("Unexpected {} expected {}",
            PrintToken{ .token = tokens_[next_], .source = source_ },
            expected));
}

bool Parser::Match(TokenType type, const Token** out)
{
    if (next_ == tokens_.size())
        return type == TokenType::End;
    if (tokens_[next_].type != type)
        return false;
    if (out != nullptr)
        *out = &tokens_[next_];
    return true;
}

bool Parser::Consume(TokenType type, const Token** out)
{
    if (!Match(type, out))
        return false;
    if (next_ + 1 < tokens_.size())
        ++next_;
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

    const std::string_view ident = source_->Data().substr(token->offset, token->length);
    out.name = alloc_.NewString(ident);
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

    out.parts.PushBack(alloc_, ident);

    while (Consume(TokenType::Dot))
    {
        if (!ExpectIdent(ident))
            break;

        out.parts.PushBack(alloc_, ident);
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
    const std::string_view extracted = source_->Data().substr(token->offset, token->length);
    if (extracted != keyword)
        return false;
    ++next_;
    return true;
}

void Parser::Unwind(std::uint32_t count) noexcept
{
    assert(next_ >= count);
    next_ -= count;
}

std::uint32_t Parser::Pos(const Token* token) const
{
    if (token == nullptr)
        token = &tokens_[next_];

    return static_cast<std::uint32_t>(token - tokens_.data());
}
