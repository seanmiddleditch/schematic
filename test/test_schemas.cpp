// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "embed_tests.h"
#include "test_context.h"
#include "test_strings.h"

#include "schematic/compiler.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <catch2/matchers/catch_matchers_templated.hpp>

#include <sstream>
#include <string>
#include <vector>

using namespace potato::schematic;
using namespace potato::schematic::compiler;
using namespace potato::schematic::test;

namespace potato::schematic::test
{
    class CheckEvaluator
    {
    public:
        explicit CheckEvaluator(std::string_view test, std::string_view filename, std::size_t line)
            : filename_(filename)
            , line_(line)
            , test_(test)
        {
            std::string_view input = test_;

            auto space_pos = input.find(' ');
            expression = input.substr(0, space_pos);

            if (space_pos == std::string_view::npos)
                return;

            input = input.substr(space_pos + 1);
            space_pos = input.find(' ');
            op = input.substr(0, space_pos);

            if (space_pos != std::string_view::npos)
                reference = input.substr(space_pos + 1);
        }

        void Check(const Schema* schema)
        {
            REQUIRE(schema != nullptr);

            remaining = expression;
            Advance();
            Evaluate(schema);
        }

    private:
        struct Invalid
        {
        };

        struct OpIs;
        struct OpIsA;
        struct OpIsKind;

        template <typename Impl>
        struct OpBase : Catch::Matchers::MatcherGenericBase
        {
            explicit OpBase(std::string_view reference)
                : reference(reference)
            {
            }

            template <typename T>
            bool match(T value) const
            {
                return Impl::Check(value, reference);
            }

            std::string describe() const final
            {
                return fmt::format("{} {}", Impl::Name, reference);
            }

            std::string_view reference;
        };

        struct OpIs : OpBase<OpIs>
        {
            using OpBase::OpBase;

            static constexpr char Name[] = "is";

            static bool Check(Invalid, std::string_view)
            {
                return false;
            }

            static bool Check(bool value, std::string_view reference)
            {
                if (value)
                    return "true" == reference;
                else
                    return "false" == reference;
            }

            static bool Check(std::nullptr_t, std::string_view reference)
            {
                return "null" == reference;
            }

            template <typename T>
            static bool Check(T value, std::string_view reference)
                requires std::is_integral_v<T> || std::is_floating_point_v<T>
            {
                return fmt::format("{}", value) == reference;
            }

            static bool Check(const EnumItem* item, std::string_view reference)
            {
                if (reference == item->name)
                    return true;

                return Check(item->value->value, reference);
            }

            static bool Check(const Type* type, std::string_view reference)
            {
                if (type == nullptr)
                    return Check(Invalid{}, reference);

                return type->name == reference;
            }

            static bool Check(const Field* field, std::string_view reference)
            {
                if (field == nullptr)
                    return Check(Invalid{}, reference);

                return Check(field->value, reference);
            }

            static bool Check(const Value* value, std::string_view reference)
            {
                if (value == nullptr)
                    return Check(Invalid{}, reference);

                switch (value->kind)
                {
                    case ValueKind::Array: return Check(Invalid{}, reference);
                    case ValueKind::Bool: return Check(static_cast<const ValueBool*>(value)->value, reference);
                    case ValueKind::Enum: return Check(static_cast<const ValueEnum*>(value)->item, reference);
                    case ValueKind::Float: return Check(static_cast<const ValueFloat*>(value)->value, reference);
                    case ValueKind::Int: return Check(static_cast<const ValueInt*>(value)->value, reference);
                    case ValueKind::Null: return Check(nullptr, reference);
                    case ValueKind::Object: return Check(Invalid{}, reference);
                    case ValueKind::String: return Check(static_cast<const ValueInt*>(value)->value, reference);
                    case ValueKind::Type: return Check(static_cast<const ValueType*>(value)->type, reference);
                }

                return Check(Invalid{}, reference);
            }
        };

        struct OpIsA : OpBase<OpIsA>
        {
            using OpBase::OpBase;

            static constexpr char Name[] = "is-a";

            template <typename T>
            static bool Check(T, std::string_view)
            {
                return false;
            }

            static bool Check(const Field* field, std::string_view reference)
            {
                if (field == nullptr)
                    return false;

                return Check(field->type, reference);
            }

            static bool Check(const Type* type, std::string_view reference)
            {
                if (type == nullptr)
                    return false;

                if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
                {
                    for (const TypeStruct* comp = struct_; comp != nullptr; comp = comp->base)
                    {
                        if (comp->name == reference)
                            return true;
                    }
                }
                else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
                {
                    return Check(enum_->base, reference);
                }

                return false;
            }
        };

        struct OpIsKind : OpBase<OpIsKind>
        {
            using OpBase::OpBase;

            static constexpr char Name[] = "is-kind";

            template <typename T>
            static bool Check(T, std::string_view)
            {
                return false;
            }

            static bool Check(const Type* type, std::string_view reference)
            {
                if (type == nullptr)
                    return false;

                switch (type->kind)
                {
                    case TypeKind::Array: return "Array" == reference;
                    case TypeKind::Attribute: return "Attribute" == reference;
                    case TypeKind::Bool: return "Bool" == reference;
                    case TypeKind::Enum: return "Enum" == reference;
                    case TypeKind::Float: return "Float" == reference;
                    case TypeKind::Int: return "Int" == reference;
                    case TypeKind::Message: return "Message" == reference;
                    case TypeKind::Nullable: return "Nullable" == reference;
                    case TypeKind::Pointer: return "Pointer" == reference;
                    case TypeKind::String: return "String" == reference;
                    case TypeKind::Struct: return "Struct" == reference;
                    case TypeKind::Type: return "Type" == reference;
                }

                return false;
            }
        };

        bool Match(std::string_view name)
        {
            if (next == name)
            {
                Advance();
                return true;
            }
            return false;
        }

        bool IsEnd()
        {
            return remaining.empty();
        }

        template <typename T>
        struct CatchFailedExpression final : Catch::ITransientExpression
        {
            CatchFailedExpression(CheckEvaluator& evaluator, const T& value)
                : ITransientExpression(true, false)
                , evaluator(evaluator)
                , value(value)
            {
            }

            void streamReconstructedExpression(std::ostream& os) const override
            {
                os << Catch::Detail::stringify(value) << ' ' << evaluator.op << ' ' << evaluator.reference;
            }

            CheckEvaluator& evaluator;
            const T& value;
        };

        void Fail()
        {
            Catch::AssertionHandler handler("EVALUATE", Catch::SourceLineInfo(filename_.c_str(), line_), test_, Catch::ResultDisposition::Normal);

            if (!next.empty())
                handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Unknown component ", next));
            else
                handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Invalid expression {}", expression));

            handler.complete();
        }

        template <typename T>
        void Finish(T value)
        {
            if (!IsEnd())
                return Fail();

            Catch::AssertionHandler handler("EVALUATE", Catch::SourceLineInfo(filename_.c_str(), line_), test_, Catch::ResultDisposition::Normal);

            if (op == "is")
            {
                if (!OpIs::Check(value, reference))
                    handler.handleExpr(CatchFailedExpression(*this, value));
            }
            else if (op == "is-a")
            {
                if (!OpIsA::Check(value, reference))
                    handler.handleExpr(CatchFailedExpression(*this, value));
            }
            else if (op == "is-kind")
            {
                if (!OpIsKind::Check(value, reference))
                    handler.handleExpr(CatchFailedExpression(*this, value));
            }
            else
            {
                handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Unkwnown operator {}", op));
            }

            handler.complete();
        }

        template <typename T>
        void Evaluate(T value)
        {
            Finish(value);
        }

        void Evaluate(const Field* field)
        {
            if (Match("@type"))
                Evaluate(field->type);
            else if (Match("@default"))
                Evaluate(field->value);
            else
                Evaluate(field->value);
        }

        void Evaluate(const Argument* arg)
        {
            if (Match("@field"))
                Evaluate(arg->field);
            else
                Evaluate(arg->value);
        }

        void Evaluate(const Type* type)
        {
            if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
            {
                if (Match("@base"))
                    return Evaluate(static_cast<const Type*>(struct_->base));
                if (Match("@fields"))
                    return Evaluate(struct_->fields.size());

                for (const Field& field : struct_->fields)
                {
                    if (Match(field.name))
                        return Evaluate(&field);
                }
            }
            else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
            {
                if (Match("@base"))
                    return Evaluate(enum_->base);
                if (Match("@items"))
                    return Evaluate(enum_->items.size());

                for (const EnumItem& item : enum_->items)
                {
                    if (Match(item.name))
                        return Evaluate(&item);
                }
            }

            Finish(type);
        }

        void Evaluate(const Value* value)
        {
            if (const ValueObject* object = CastTo<ValueObject>(value); object != nullptr)
            {
                if (Match("@type"))
                    return Evaluate(object->type);
                if (Match("@fields"))
                    return Evaluate(object->fields.size());

                for (const Argument& arg : object->fields)
                {
                    if (Match(arg.field->name))
                        return Evaluate(&arg);
                }
            }
            else if (const ValueArray* array = CastTo<ValueArray>(value); array != nullptr)
            {
                if (Match("@element-type"))
                    return Evaluate(array->type);
                if (Match("@elements"))
                    return Evaluate(array->elements.size());

                const std::size_t index = std::stoull(std::string(next));
                if (index < array->elements.size() && Match(std::to_string(index)))
                    return Evaluate(array->elements[index]);
            }

            Finish(value);
        }

        void Evaluate(const Module* mod)
        {
            if (Match("@types"))
                return Evaluate(mod->types.size());
            if (Match("@imports"))
                return Evaluate(mod->imports.size());

            for (const Type* type : mod->types)
            {
                if (Match(type->name))
                    return Evaluate(type);
            }

            Fail();
        }

        void Evaluate(const Schema* schema)
        {
            if (Match("@types"))
                return Evaluate(schema->types.size());
            if (Match("@modules"))
                return Evaluate(schema->modules.size());
            if (Match("@root"))
                return Evaluate(schema->root);

            for (const Type* type : schema->types)
            {
                if (Match(type->name))
                    return Evaluate(type);
            }

            Fail();
        }

        void Advance()
        {
            const auto pos = remaining.find('.');
            if (pos != std::string_view::npos)
            {
                next = remaining.substr(0, pos);
                remaining = remaining.substr(pos + 1);
            }
            else
            {
                next = remaining;
                remaining = {};
            }
        }

        // Source
        std::string filename_;
        std::size_t line_ = 1;

        // Inputs
        std::string test_;
        std::string expression;
        std::string op;
        std::string reference;

        // State
        std::string_view remaining;
        std::string_view next;
    };
} // namespace potato::schematic::test

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
            std::vector<CheckEvaluator> checks;

            std::istringstream source(test.source);
            std::string line;
            std::size_t number = 0;
            while (std::getline(source, line))
            {
                ++number;

                constexpr char error_prefix[] = "ERROR: ";
                const auto error_pos = line.find(error_prefix);
                if (error_pos != std::string::npos)
                    expected_errors.push_back(line.substr(error_pos + std::strlen(error_prefix)));

                constexpr char check_prefix[] = "CHECK: ";
                const auto check_pos = line.find(check_prefix);
                if (check_pos != std::string::npos)
                {
                    checks.emplace_back(line.substr(check_pos + std::strlen(check_prefix)), test.name, number);
                }
            }

            const Schema* const schema = compiler.Compile(ModuleId{ i });

            if (expected_errors.empty())
            {
                REQUIRE(schema != nullptr);
                REQUIRE(schema->root != nullptr);

                for (CheckEvaluator& check : checks)
                    check.Check(schema);
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
