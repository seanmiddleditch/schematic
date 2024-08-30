// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "embed_tests.h"
#include "test_context.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <sstream>
#include <string>
#include <vector>

using namespace potato::schematic;
using namespace potato::schematic::compiler;
using namespace potato::schematic::test;

namespace evaluate
{
    enum class Op
    {
        Invalid,
        Is,
        IsA,
        IsKind,
    };

    struct Check
    {
        std::string input;
        std::string components;
        std::string reference;
        Op op = Op::Invalid;
    };

    struct Invalid
    {
    };

    Op ParseOp(std::string_view input)
    {
        if (input == "is")
            return Op::Is;
        if (input == "is-a")
            return Op::IsA;
        if (input == "is-kind")
            return Op::IsKind;
        return Op::Invalid;
    }

    void ParseCheck(Check& check, std::string_view input)
    {
        check.input = input;

        auto space_pos = input.find(' ');
        REQUIRE(space_pos != std::string_view::npos);
        check.components = input.substr(0, space_pos);

        input = input.substr(space_pos + 1);

        space_pos = input.find(' ');
        REQUIRE(space_pos != std::string_view::npos);
        check.op = ParseOp(input.substr(0, space_pos));

        check.reference = input.substr(space_pos + 1);
    }

    std::string_view PullNext(std::string_view& inout_components)
    {
        std::string_view result;

        const auto pos = inout_components.find('.');
        if (pos != std::string_view::npos)
        {
            result = inout_components.substr(0, pos);
            inout_components = inout_components.substr(pos + 1);
        }
        else
        {
            result = inout_components;
            inout_components = {};
        }

        return result;
    }

    template <typename T>
    bool ExecuteIs(T value, std::string_view reference)
    {
        if constexpr (std::is_same_v<T, bool>)
        {
            const char* const bstr = value ? "true" : "false";
            CHECK(bstr == reference);
            return true;
        }
        else if constexpr (std::is_integral_v<T>)
        {
            CHECK(std::to_string(value) == reference);
            return true;
        }
        FAIL("<invalid>");
        return false;
    }

    bool ExecuteIs(const EnumItem* item, std::string_view reference)
    {
        if (reference == item->name)
        {
            CHECK(item->name == reference);
            return true;
        }
        return ExecuteIs(item->value->value, reference);
    }

    bool ExecuteIs(const Type* type, std::string_view reference)
    {
        if (type == nullptr)
            return ExecuteIs(Invalid{}, reference);

        CHECK(type->name == reference);
        return true;
    }

    bool ExecuteIs(const Value* value, std::string_view reference);

    bool ExecuteIs(const Field* field, std::string_view reference)
    {
        if (field == nullptr)
            return ExecuteIs(Invalid{}, reference);

        return ExecuteIs(field->value, reference);
    }

    bool ExecuteIs(const Value* value, std::string_view reference)
    {
        if (value == nullptr)
            return ExecuteIs(Invalid{}, reference);

        switch (value->kind)
        {
            case ValueKind::Array: return ExecuteIs(Invalid{}, reference);
            case ValueKind::Bool: return ExecuteIs(static_cast<const ValueBool*>(value)->value, reference);
            case ValueKind::Enum: return ExecuteIs(static_cast<const ValueEnum*>(value)->item, reference);
            case ValueKind::Float: return ExecuteIs(static_cast<const ValueFloat*>(value)->value, reference);
            case ValueKind::Int: return ExecuteIs(static_cast<const ValueInt*>(value)->value, reference);
            case ValueKind::Null: return ExecuteIs(nullptr, reference);
            case ValueKind::Object: return ExecuteIs(Invalid{}, reference);
            case ValueKind::String: return ExecuteIs(static_cast<const ValueInt*>(value)->value, reference);
            case ValueKind::Type: return ExecuteIs(static_cast<const ValueType*>(value)->type, reference);
        }

        REQUIRE(false);
        return false;
    }

    template <typename T>
    bool ExecuteIsA(T, std::string_view)
    {
        FAIL("<invalid>");
        return false;
    }

    bool ExecuteIsA(const Field* field, std::string_view reference)
    {
        if (field == nullptr)
            return ExecuteIsA(Invalid{}, reference);

        return ExecuteIsA(field->type, reference);
    }

    bool ExecuteIsA(const Type* type, std::string_view reference)
    {
        if (type == nullptr)
        {
            CHECK("null" == reference);
            return false;
        }

        if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        {
            while (const TypeStruct* base = struct_->base)
            {
                if (type->name == reference)
                    return true;
            }
            FAIL(type->name << " is-a " << reference);
        }
        else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
        {
            return ExecuteIsA(enum_->base, reference);
        }
        return false;
    }

    template <typename T>
    bool ExecuteIsKind(T, std::string_view)
    {
        FAIL("<invalid>");
        return false;
    }

    bool ExecuteIsKind(const Type* type, std::string_view reference)
    {
        if (type == nullptr)
        {
            CHECK("null" == reference);
            return false;
        }

        switch (type->kind)
        {
            case TypeKind::Array: CHECK("Array" == reference); return true;
            case TypeKind::Attribute: CHECK("Attribute" == reference); return true;
            case TypeKind::Bool: CHECK("Bool" == reference); return true;
            case TypeKind::Enum: CHECK("Enum" == reference); return true;
            case TypeKind::Float: CHECK("Float" == reference); return true;
            case TypeKind::Int: CHECK("Int" == reference); return true;
            case TypeKind::Message: CHECK("Message" == reference); return true;
            case TypeKind::Nullable: CHECK("Nullable" == reference); return true;
            case TypeKind::Pointer: CHECK("Pointer" == reference); return true;
            case TypeKind::String: CHECK("String" == reference); return true;
            case TypeKind::Struct: CHECK("Struct" == reference); return true;
            case TypeKind::Type: CHECK("Type" == reference); return true;
        }

        REQUIRE(false);
        return false;
    }

    template <typename T>
    bool Execute(T value, Op op, std::string_view reference)
    {
        switch (op)
        {
            case Op::Invalid:
                return false;
            case Op::Is:
                return ExecuteIs(value, reference);
            case Op::IsA:
                return ExecuteIsA(value, reference);
            case Op::IsKind:
                return ExecuteIsKind(value, reference);
        }
        return false;
    }

    template <typename T>
    bool Evaluate(T value, std::string_view components, Op op, std::string_view reference)
    {
        if (!components.empty())
            return false;

        return Execute(value, op, reference);
    }

    bool Evaluate(Invalid, std::string_view components, Op op, std::string_view reference)
    {
        return Execute(Invalid{}, op, reference);
    }

    bool Evaluate(const Type* type, std::string_view components, Op op, std::string_view reference);

    bool Evaluate(const Field* field, std::string_view components, Op op, std::string_view reference)
    {
        const auto comp = PullNext(components);
        if (comp.empty())
            return Execute(field, op, reference);

        if (comp == "@type")
            return Evaluate(field->type, components, op, reference);
        if (comp == "@default")
            return Evaluate(field->value, components, op, reference);

        return Evaluate(Invalid{}, components, op, reference);
    }

    bool Evaluate(const Type* type, std::string_view components, Op op, std::string_view reference)
    {
        const auto comp = PullNext(components);
        if (comp.empty())
            return Execute(type, op, reference);

        if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        {
            if (comp == "@base")
                return Evaluate(struct_->base, components, op, reference);
            if (comp == "@fields")
                return Evaluate(struct_->fields.size(), components, op, reference);

            for (const Field& field : struct_->fields)
            {
                if (field.name == comp)
                    return Evaluate(&field, components, op, reference);
            }
        }
        else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
        {
            if (comp == "@base")
                return Evaluate(enum_->base, components, op, reference);
            if (comp == "@items")
                return Evaluate(enum_->items.size(), components, op, reference);

            for (const EnumItem& item : enum_->items)
            {
                if (item.name == comp)
                    return Evaluate(&item, components, op, reference);
            }
        }

        return Evaluate(Invalid{}, components, op, reference);
    }

    bool Evaluate(const Schema* schema, std::string_view components, Op op, std::string_view reference)
    {
        const auto comp = PullNext(components);
        if (comp.empty())
            return Execute(schema, op, reference);

        for (const Type* type : schema->root->types)
        {
            if (type->name == comp)
                return Evaluate(type, components, op, reference);
        }

        return Evaluate(Invalid{}, components, op, reference);
    }
} // namespace evaluate

TEST_CASE("Schemas", "[potato][schematic]")
{
    TestContext ctx;
    ArenaAllocator arena;
    Compiler compiler(ctx, arena);
    compiler.SetUseBuiltins(true);

    for (std::size_t i = 0; i != test_embeds_count; ++i)
    {
        const EmbeddedTest& test = test_embeds[i];
        DYNAMIC_SECTION(test.name)
        {
            std::vector<std::string> expected_errors;
            std::vector<evaluate::Check> checks;

            std::istringstream source(test.source);
            std::string line;
            while (std::getline(source, line))
            {
                constexpr char error_prefix[] = "ERROR: ";
                const auto error_pos = line.find(error_prefix);
                if (error_pos != std::string::npos)
                    expected_errors.push_back(line.substr(error_pos + std::strlen(error_prefix)));

                constexpr char check_prefix[] = "CHECK: ";
                const auto check_pos = line.find(check_prefix);
                if (check_pos != std::string::npos)
                {
                    evaluate::Check& check = checks.emplace_back();
                    evaluate::ParseCheck(check, line.substr(check_pos + std::strlen(check_prefix)));
                }
            }

            const Schema* const schema = compiler.Compile(ModuleId{ i });

            if (expected_errors.empty())
            {
                REQUIRE(schema != nullptr);
                REQUIRE(schema->root != nullptr);

                for (const evaluate::Check& check : checks)
                {
                    INFO(check.input);
                    CHECK(evaluate::Evaluate(schema, check.components, check.op, check.reference));
                }
            }
            else
            {
                CHECK(schema == nullptr);

                std::size_t next = 0;
                for (const std::string& error : ctx.errors)
                {
                    if (next < expected_errors.size())
                    {
                        const std::string& expected = expected_errors[next];
                        if (expected == error)
                            ++next;
                        else
                            FAIL_CHECK(error);
                    }
                    else
                    {
                        FAIL_CHECK(error);
                    }
                }
                CHECK(next == expected_errors.size());
            }
        }
    }
}
