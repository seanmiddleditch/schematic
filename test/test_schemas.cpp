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
    struct OpIsImpl
    {
        // static void Execute(T, std::string_view);
    };

    template <>
    struct OpIsImpl<Invalid>
    {
        static void Execute(Invalid, std::string_view)
        {
            FAIL("Expression did not evaluate");
        }
    };

    template <>
    struct OpIsImpl<bool>
    {
        static void Execute(bool value, std::string_view reference)
        {
            if (value)
                CHECK("true" == reference);
            else
                CHECK("false" == reference);
        }
    };

    template <>
    struct OpIsImpl<std::nullptr_t>
    {
        static void Execute(std::nullptr_t, std::string_view reference)
        {
            CHECK("null" == reference);
        }
    };

    template <typename T>
        requires std::is_integral_v<T> || std::is_floating_point_v<T>
    struct OpIsImpl<T>
    {
        static void Execute(T value, std::string_view reference)
        {
            CHECK(fmt::format("{}", value) == reference);
        }
    };

    template <typename T>
    void ExecuteIs(T value, std::string_view reference)
    {
        return OpIsImpl<T>::Execute(value, reference);
    }

    void ExecuteIs(const EnumItem* item, std::string_view reference)
    {
        if (reference == item->name)
            CHECK(item->name == reference);
        else
            ExecuteIs(item->value->value, reference);
    }

    void ExecuteIs(const Type* type, std::string_view reference)
    {
        if (type == nullptr)
            return ExecuteIs(Invalid{}, reference);

        CHECK(type->name == reference);
    }

    void ExecuteIs(const Value* value, std::string_view reference);

    void ExecuteIs(const Field* field, std::string_view reference)
    {
        if (field == nullptr)
            ExecuteIs(Invalid{}, reference);
        else
            ExecuteIs(field->value, reference);
    }

    void ExecuteIs(const Value* value, std::string_view reference)
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
    }

    template <typename T>
    void ExecuteIsA(T, std::string_view)
    {
        FAIL("Expression is not a type");
    }

    void ExecuteIsA(const Field* field, std::string_view reference)
    {
        if (field == nullptr)
            return ExecuteIsA(Invalid{}, reference);

        return ExecuteIsA(field->type, reference);
    }

    void ExecuteIsA(const Type* type, std::string_view reference)
    {
        if (type == nullptr)
            return ExecuteIsA(Invalid{}, reference);

        if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        {
            for (const TypeStruct* comp = struct_; comp != nullptr; comp = comp->base)
            {
                if (comp->name == reference)
                {
                    CHECK(comp->name == reference);
                    return;
                }
            }
            FAIL(type->name << " is-a " << reference);
        }
        else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
        {
            ExecuteIsA(enum_->base, reference);
        }
    }

    template <typename T>
    void ExecuteIsKind(T, std::string_view)
    {
        FAIL("Expression has no kind");
    }

    void ExecuteIsKind(const Type* type, std::string_view reference)
    {
        if (type == nullptr)
            return ExecuteIsKind(Invalid{}, reference);

        switch (type->kind)
        {
            case TypeKind::Array: CHECK("Array" == reference); return;
            case TypeKind::Attribute: CHECK("Attribute" == reference); return;
            case TypeKind::Bool: CHECK("Bool" == reference); return;
            case TypeKind::Enum: CHECK("Enum" == reference); return;
            case TypeKind::Float: CHECK("Float" == reference); return;
            case TypeKind::Int: CHECK("Int" == reference); return;
            case TypeKind::Message: CHECK("Message" == reference); return;
            case TypeKind::Nullable: CHECK("Nullable" == reference); return;
            case TypeKind::Pointer: CHECK("Pointer" == reference); return;
            case TypeKind::String: CHECK("String" == reference); return;
            case TypeKind::Struct: CHECK("Struct" == reference); return;
            case TypeKind::Type: CHECK("Type" == reference); return;
        }

        REQUIRE(false);
    }

    template <typename T>
    void Execute(T value, Op op, std::string_view reference)
    {
        switch (op)
        {
            case Op::Invalid:
                FAIL("Invalid operator");
            case Op::Is:
                return ExecuteIs(value, reference);
            case Op::IsA:
                return ExecuteIsA(value, reference);
            case Op::IsKind:
                return ExecuteIsKind(value, reference);
        }
        FAIL("Unknown operator");
    }

    template <typename T>
    void Evaluate(T value, std::string_view components, Op op, std::string_view reference)
    {
        if (!components.empty())
        {
            const auto comp = PullNext(components);
            FAIL("Unknown component " << comp);
            return;
        }

        Execute(value, op, reference);
    }

    void Evaluate(Invalid, std::string_view components, Op op, std::string_view reference)
    {
        Execute(Invalid{}, op, reference);
    }

    void Evaluate(const Type* type, std::string_view components, Op op, std::string_view reference);
    void Evaluate(const Value* value, std::string_view components, Op op, std::string_view reference);

    void Evaluate(const Field* field, std::string_view components, Op op, std::string_view reference)
    {
        std::string_view temp = components;
        const auto comp = PullNext(temp);
        if (comp.empty())
            return Execute(field, op, reference);

        if (comp == "@type")
            Evaluate(field->type, temp, op, reference);
        else if (comp == "@default")
            Evaluate(field->value, temp, op, reference);
        else
            Evaluate(field->value, components, op, reference);
    }

    void Evaluate(const Argument* arg, std::string_view components, Op op, std::string_view reference)
    {
        const auto comp = PullNext(components);
        if (comp.empty())
            return Execute(arg->value, op, reference);

        if (comp == "@field")
            Evaluate(arg->field, components, op, reference);
        else
            Evaluate(arg->value, components, op, reference);
    }

    void Evaluate(const Type* type, std::string_view components, Op op, std::string_view reference)
    {
        const auto comp = PullNext(components);
        if (comp.empty())
            return Execute(type, op, reference);

        if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        {
            if (comp == "@base")
                return Evaluate(static_cast<const Type*>(struct_->base), components, op, reference);
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

        Evaluate(Invalid{}, components, op, reference);
    }

    void Evaluate(const Value* value, std::string_view components, Op op, std::string_view reference)
    {
        const auto comp = PullNext(components);
        if (comp.empty())
            return Execute(value, op, reference);

        if (const ValueObject* object = CastTo<ValueObject>(value); object != nullptr)
        {
            if (comp == "@type")
                return Evaluate(object->type, components, op, reference);
            if (comp == "@fields")
                return Evaluate(object->fields.size(), components, op, reference);

            for (const Argument& arg : object->fields)
            {
                if (arg.field->name == comp)
                    return Evaluate(&arg, components, op, reference);
            }
        }
        else if (const ValueArray* array = CastTo<ValueArray>(value); array != nullptr)
        {
            if (comp == "@element-type")
                return Evaluate(array->type, components, op, reference);
            if (comp == "@elements")
                return Evaluate(array->elements.size(), components, op, reference);

            const std::size_t index = std::stoull(std::string(comp));
            if (index < array->elements.size())
                return Evaluate(array->elements[index], components, op, reference);
        }
        Evaluate(Invalid{}, components, op, reference);
    }

    void Evaluate(const Schema* schema, std::string_view components, Op op, std::string_view reference)
    {
        const auto comp = PullNext(components);
        if (comp.empty())
            return Execute(Invalid{}, op, reference);

        for (const Type* type : schema->types)
        {
            if (type->name == comp)
                return Evaluate(type, components, op, reference);
        }

        Evaluate(Invalid{}, components, op, reference);
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
                    evaluate::Evaluate(schema, check.components, check.op, check.reference);
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
