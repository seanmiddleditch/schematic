// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "evaluator.h"

#include "schematic/schema.h"
#include "schematic/utility.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

#include <string>
#include <string_view>

namespace potato::schematic::test
{
    CheckEvaluator::CheckEvaluator(std::string_view test, std::string_view filename, std::size_t line)
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

    void CheckEvaluator::Check(const Schema* schema)
    {
        REQUIRE(schema != nullptr);

        remaining = expression;
        Advance();
        Evaluate(schema);
    }

    struct CheckEvaluator::Invalid
    {
    };

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

    struct CheckEvaluator::OpIs : OpBase<OpIs>
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

    struct CheckEvaluator::OpIsA : OpBase<OpIsA>
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

    struct CheckEvaluator::OpIsKind : OpBase<OpIsKind>
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

    bool CheckEvaluator::Match(std::string_view name)
    {
        if (next == name)
        {
            Advance();
            return true;
        }
        return false;
    }

    bool CheckEvaluator::IsEnd()
    {
        return remaining.empty();
    }

    template <typename T>
    struct CheckEvaluator::CatchFailedExpression final : Catch::ITransientExpression
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

    void CheckEvaluator::Fail()
    {
        Catch::AssertionHandler handler("EVALUATE", Catch::SourceLineInfo(filename_.c_str(), line_), test_, Catch::ResultDisposition::Normal);

        if (!next.empty())
            handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Unknown component ", next));
        else
            handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Invalid expression {}", expression));

        handler.complete();
    }

    template <typename T>
    void CheckEvaluator::Finish(T value)
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
    void CheckEvaluator::Evaluate(T value)
    {
        Finish(value);
    }

    void CheckEvaluator::Evaluate(const Field* field)
    {
        if (Match("@type"))
            Evaluate(field->type);
        else if (Match("@default"))
            Evaluate(field->value);
        else
            Evaluate(field->value);
    }

    void CheckEvaluator::Evaluate(const Argument* arg)
    {
        if (Match("@field"))
            Evaluate(arg->field);
        else
            Evaluate(arg->value);
    }

    void CheckEvaluator::Evaluate(const Type* type)
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

    void CheckEvaluator::Evaluate(const Value* value)
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

    void CheckEvaluator::Evaluate(const Module* mod)
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

    void CheckEvaluator::Evaluate(const Schema* schema)
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

    void CheckEvaluator::Advance()
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
} // namespace potato::schematic::test
