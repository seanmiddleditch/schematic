// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "lexer.h"
#include "test_context.h"
#include "test_strings.h"
#include "token.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

#include <span>

namespace potato::schematic::test::_detail
{
    template <typename SchemaValueType, typename ValueType>
    struct IsValue : Catch::Matchers::MatcherGenericBase
    {
        IsValue(const ValueType& value)
            : value_(value)
        {
        }

        bool match(const Field* field) const
        {
            if (field == nullptr)
            {
                UNSCOPED_INFO("Null value");
                return false;
            }

            return match(field->value);
        }

        bool match(const EnumItem* item) const
        {
            if (item == nullptr)
            {
                UNSCOPED_INFO("Null value");
                return false;
            }

            return match(item->value);
        }

        bool match(const Value* value) const
        {
            if (value == nullptr)
            {
                UNSCOPED_INFO("Null value");
                return false;
            }

            const SchemaValueType* const casted = CastTo<SchemaValueType>(value);
            if (value == nullptr)
            {
                UNSCOPED_INFO("Incorrect type");
                return false;
            }

            return casted->value == value_;
        }

        std::string describe() const override
        {
            return fmt::format("== {}", value_);
        }

    private:
        ValueType value_;
    };
} // namespace potato::schematic::test::_detail

namespace potato::schematic::test
{
    struct IsTokenType : Catch::Matchers::MatcherGenericBase
    {
        IsTokenType(potato::schematic::compiler::TokenType type)
            : type_(type)
        {
        }

        bool match(const char* text) const
        {
            using namespace potato::schematic::compiler;

            TestContext ctx;
            NewDeleteAllocator alloc;
            ArenaAllocator arena(alloc);

            ctx.AddFile("<test>", text);

            Lexer lexer(ctx, arena, FileId{ 0 });
            Array<Token> tokens = lexer.Tokenize();

            if (tokens.IsEmpty())
                UNSCOPED_INFO("Tokenize failed");

            const TokenType actual = tokens.IsEmpty() ? TokenType::Unknown : tokens.Front().type;
            if (actual != type_)
                UNSCOPED_INFO("Expected " << ToCStr(type_) << " got " << ToCStr(actual));

            return !tokens.IsEmpty() &&
                tokens.Size() == 2 &&
                tokens.Front().offset == 0 &&
                tokens.Front().length == ctx.ReadFileContents(FileId{ 0 }).size() &&
                tokens.Front().type == type_;
        }

        std::string describe() const override
        {
            return fmt::format("== {}", potato::schematic::compiler::ToCStr(type_));
        }

    private:
        potato::schematic::compiler::TokenType type_;
    };

    template <typename SchemaValueType, typename ValueType>
    _detail::IsValue<SchemaValueType, ValueType> IsValue(const ValueType& value)
    {
        return value;
    }

    struct IsEnumValue : Catch::Matchers::MatcherGenericBase
    {
        IsEnumValue(const TypeEnum* en, const EnumItem* item)
            : enum_(en)
            , item_(item)
        {
        }

        bool match(const Field* field) const
        {
            if (field == nullptr)
            {
                UNSCOPED_INFO("Null value");
                return false;
            }

            return match(field->value);
        }

        bool match(const EnumItem* item) const
        {
            if (item == nullptr)
            {
                UNSCOPED_INFO("Null value");
                return false;
            }

            return match(item->value);
        }

        bool match(const Value* value) const
        {
            if (value == nullptr)
            {
                UNSCOPED_INFO("Null value");
                return false;
            }

            const ValueEnum* const casted = CastTo<ValueEnum>(value);
            if (value == nullptr)
            {
                UNSCOPED_INFO("Incorrect type");
                return false;
            }

            return casted->item == item_;
        }

        std::string describe() const override
        {
            return fmt::format("== {}.{} ({})", enum_->name, item_->name, item_->value->value);
        }

    private:
        const TypeEnum* enum_ = nullptr;
        const EnumItem* item_ = nullptr;
    };

    struct IsStringValue : Catch::Matchers::MatcherGenericBase
    {
        IsStringValue(std::string_view value)
            : value_(value)
        {
        }

        bool match(const Value* value) const
        {
            if (value == nullptr)
            {
                UNSCOPED_INFO("Null value");
                return false;
            }

            const ValueString* const casted = CastTo<ValueString>(value);
            if (value == nullptr)
            {
                UNSCOPED_INFO("Incorrect type");
                return false;
            }

            return casted->value == value_;
        }

        std::string describe() const override
        {
            return fmt::format("== {}", value_);
        }

    private:
        std::string_view value_;
    };

    struct IsLexError : Catch::Matchers::MatcherGenericBase
    {
        bool match(const char* text) const
        {
            using namespace potato::schematic::compiler;

            TestContext ctx;
            NewDeleteAllocator alloc;
            ArenaAllocator arena(alloc);
            Array<Token> tokens;

            ctx.AddFile("<test>", text);

            Lexer lexer(ctx, arena, FileId{ 0 });
            tokens = lexer.Tokenize();
            return !tokens.IsEmpty();
        }

        std::string describe() const override
        {
            return "IsLexError";
        }
    };

    struct IsExactSequence : Catch::Matchers::MatcherGenericBase
    {
        explicit IsExactSequence(std::initializer_list<const uint8_t> expected)
            : expected_(expected)
        {
        }

        bool match(Buffer value) const
        {
            using namespace potato::schematic::compiler;

            return value.size() == expected_.size() && std::memcmp(value.data(), expected_.data(), value.size()) == 0;
        }

        std::string describe() const override
        {
            return "== " + Catch::StringMaker<Buffer>{}.convert(Buffer{ expected_ });
        }

    private:
        std::span<const uint8_t> expected_;
    };
} // namespace potato::schematic::test
