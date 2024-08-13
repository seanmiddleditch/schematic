// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/compiler.h"
#include "schematic/schema.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

#include <span>

#include "../source/lexer.h"
#include "../source/location.h"
#include "../source/token.h"

namespace potato::schematic::test
{
    struct Buffer : std::span<const uint8_t>
    {
    };
}

template <>
struct Catch::StringMaker<potato::schematic::test::Buffer>
{
    static std::string convert(const potato::schematic::test::Buffer& value);
};

template <>
struct Catch::StringMaker<const potato::schematic::EnumItem*>
{
    static std::string convert(const potato::schematic::EnumItem* item);
};

template <>
struct Catch::StringMaker<const potato::schematic::Value*>
{
    static std::string convert(const potato::schematic::Value* value);
};

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
    struct TestSource final
    {
        TestSource(std::string name, std::string data) noexcept
            : name(std::move(name))
            , data(std::move(data))
        {
        }

        std::string name;
        std::string data;
    };

    struct TestContext final : potato::schematic::CompileContext
    {
        inline void Error(FileId file, const Range& range, std::string_view message) override;
        inline std::string_view ReadFileContents(FileId id) override;
        inline std::string_view GetFileName(FileId id) override;
        inline FileId ResolveModule(std::string_view name, FileId referrer) override;

        inline void AddFile(std::string name, std::string source);

        std::vector<TestSource> files;
    };

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
            ArenaAllocator alloc;
            Array<Token> tokens;

            ctx.AddFile("<test>", text);

            const bool result = Tokenize(ctx, alloc, FileId{ 0 }, tokens);

            if (!result)
                UNSCOPED_INFO("Tokenize failed");

            const TokenType actual = tokens.IsEmpty() ? TokenType::Unknown : tokens.Front().type;
            if (actual != type_)
                UNSCOPED_INFO("Expected " << ToCStr(type_) << " got " << ToCStr(actual));

            return result &&
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
            return fmt::format("== {}.{} ({})", enum_->name.CStr(), item_->name.CStr(), item_->value->value);
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
            ArenaAllocator alloc;
            Array<Token> tokens;

            ctx.AddFile("<test>", text);

            const bool result = Tokenize(ctx, alloc, FileId{ 0 }, tokens);
            return !result;
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

    void TestContext::Error(FileId file, const Range& range, std::string_view message)
    {
        UNSCOPED_INFO(message);
        if (file.value == FileId::InvalidValue)
            return;

        std::string_view source = ReadFileContents(file);
        std::string_view line = potato::schematic::compiler::ExtractLine(source, range.start.line);
        std::string buffer;
        fmt::format_to(std::back_inserter(buffer), "{}({}): ", GetFileName(file), range.start.line);
        const auto prefix = buffer.size();
        buffer.append(line);
        UNSCOPED_INFO(buffer);

        buffer.clear();
        buffer.append(prefix, ' ');
        unsigned col = 0;
        for (const char c : line)
        {
            ++col;
            if (col >= range.start.column)
            {
                if (range.start.line == range.end.line && col >= range.end.column)
                    break;
                buffer.push_back('^');
            }
            else if (c == '\n')
            {
                break;
            }
            else if (std::isspace(c))
            {
                buffer.push_back(c);
            }
            else
            {
                buffer.push_back(' ');
            }
        }
        UNSCOPED_INFO(buffer);
    }

    std::string_view TestContext::ReadFileContents(FileId id)
    {
        if (id.value >= files.size())
            return {};

        return files[id.value].data;
    }

    std::string_view TestContext::GetFileName(FileId id)
    {
        if (id.value >= files.size())
            return {};

        return files[id.value].name;
    }

    FileId TestContext::ResolveModule(std::string_view name, FileId referrer)
    {
        for (std::size_t i = 0; i != files.size(); ++i)
            if (files[i].name == name)
                return FileId{ i };
        return FileId{};
    }

    void TestContext::AddFile(std::string name, std::string source)
    {
        files.emplace_back(std::move(name), std::move(source));
    }
} // namespace potato::schematic::test

std::string Catch::StringMaker<potato::schematic::test::Buffer>::convert(const potato::schematic::test::Buffer& buffer)
{
    std::string result;
    result.append("{ ");
    for (const uint8_t octet : buffer)
        fmt::format_to(std::back_inserter(result), "{:02x} ", octet);
    result.push_back('}');
    return result;
}

std::string Catch::StringMaker<const potato::schematic::EnumItem*>::convert(const potato::schematic::EnumItem* item)
{
    if (item == nullptr)
        return "invalid(nullptr)";

    return fmt::format("{}({})", item->name.CStr(), item->value->value);
}

std::string Catch::StringMaker<const potato::schematic::Value*>::convert(const potato::schematic::Value* value)
{
    using namespace potato::schematic;

    if (value == nullptr)
        return "invalid";

    switch (value->kind)
    {
        using enum ValueKind;
        case Bool: return std::format("{}", CastTo<ValueBool>(value)->value);
        case Null: return std::format("nullptr");
        case Int: return std::format("{}", CastTo<ValueInt>(value)->value);
        case Enum: return Catch::StringMaker<const EnumItem*>::convert(CastTo<ValueEnum>(value)->item);
        case Object: return std::format("object");
        case Array: return std::format("array");
        case Type: return std::format("type");
    }

    return fmt::format("unknown(kind={})", std::to_underlying(value->kind));
}
