// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "evaluator.h"

#include "test_strings.h"

#include "schematic/schema.h"
#include "schematic/utility.h"

#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_templated.hpp>
#include <fmt/core.h>

#include <charconv>
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

    template <typename T>
    struct CheckEvaluator::CatchFailedExpression final : Catch::ITransientExpression
    {
        CatchFailedExpression(CheckEvaluator& evaluator, bool result, const T& value)
            : ITransientExpression(true, result)
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
        {
            return Catch::Detail::stringify(value) == reference;
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
                return false;

            return type->name == reference;
        }

        static bool Check(const Field* field, std::string_view reference)
        {
            if (field == nullptr)
                return false;

            return Check(field->value, reference);
        }

        static bool Check(const Value* value, std::string_view reference)
        {
            if (value == nullptr)
                return false;

            switch (value->kind)
            {
                case ValueKind::Array: return false;
                case ValueKind::Bool: return Check(static_cast<const ValueBool*>(value)->value, reference);
                case ValueKind::Enum: return Check(static_cast<const ValueEnum*>(value)->item, reference);
                case ValueKind::Float: return Check(static_cast<const ValueFloat*>(value)->value, reference);
                case ValueKind::Int: return Check(static_cast<const ValueInt*>(value)->value, reference);
                case ValueKind::Null: return Check(nullptr, reference);
                case ValueKind::Object: return false;
                case ValueKind::String: return Check(static_cast<const ValueString*>(value)->value, reference);
                case ValueKind::Type: return Check(static_cast<const ValueType*>(value)->type, reference);
            }

            return false;
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

            if (type->name == reference)
                return true;

            if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
            {
                for (const TypeStruct* comp = struct_->base; comp != nullptr; comp = comp->base)
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

    struct CheckEvaluator::MatchIndexResult
    {
        bool success = false;
        std::size_t index = 0;
    };

    CheckEvaluator::MatchIndexResult CheckEvaluator::MatchIndex(std::size_t max)
    {
        std::size_t index = 0;
        const auto result = std::from_chars(next.data(), next.data() + next.size(), index);
        if (result.ptr != next.data() + next.size())
            return { false, 0 };
        if (index >= max)
            return { false, 0 };
        return { true, index };
    }

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
        return next.empty();
    }

    void CheckEvaluator::Fail()
    {
        Catch::AssertionHandler handler("EVALUATE", Catch::SourceLineInfo(filename_.c_str(), line_), test_, Catch::ResultDisposition::Normal);

        if (!next.empty())
            handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Unknown component {}", next));
        else
            handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Invalid expression {}", expression));

        handler.complete();
    }

    template <typename T>
    bool CheckEvaluator::DispatchOp(T value)
    {
        if (op == "is")
        {
            return OpIs::Check(value, reference);
        }
        if (op == "is-a")
        {
            return OpIsA::Check(value, reference);
        }
        return false;
    }

    template <typename T>
    void CheckEvaluator::Finish(T value)
    {
        if (!IsEnd())
            return Fail();

        const bool result = DispatchOp(value);

        Catch::AssertionHandler handler("EVALUATE", Catch::SourceLineInfo(filename_.c_str(), line_), test_, Catch::ResultDisposition::ContinueOnFailure);
        handler.handleExpr(CatchFailedExpression(*this, result, value));
        handler.complete();
    }

    template <typename T>
    void CheckEvaluator::Evaluate(T value)
    {
        Finish(value);
    }

    void CheckEvaluator::Evaluate(const Annotation* annotation)
    {
        if (Match("@attribute"))
            Evaluate(annotation->attribute);
        else if (Match("@fields"))
            Evaluate(annotation->arguments.size());

        for (const Argument& arg : annotation->arguments)
        {
            if (Match(arg.field->name))
                return Evaluate(&arg);
        }

        if (auto [success, index] = MatchIndex(annotation->arguments.size()); success)
            return Evaluate(annotation->arguments[index]);

        Fail();
    }

    void CheckEvaluator::Evaluate(Span<const Annotation*> annotations)
    {
        if (Match("@length"))
            return Evaluate(annotations.size());

        for (const Annotation* anno : annotations)
        {
            if (Match(anno->attribute->name))
                return Evaluate(anno);
        }

        if (const auto [success, index] = MatchIndex(annotations.size()); success)
            return Evaluate(annotations[index]);

        Fail();
    }

    void CheckEvaluator::Evaluate(const Field* field)
    {
        if (Match("@type"))
            Evaluate(field->type);
        else if (Match("@default"))
            Evaluate(field->value);
        else if (Match("@proto"))
            Evaluate(field->proto);
        else if (Match("@annotations"))
            Evaluate(field->annotations);
        else
            Evaluate(field->type);
    }

    void CheckEvaluator::Evaluate(const EnumItem* item)
    {
        if (Match("@value"))
            Evaluate(item->value);
        else if (Match("@annotations"))
            Evaluate(item->annotations);
        else
            Finish(item);
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
        if (Match("@kind"))
            return Evaluate(type->kind);
        else if (Match("@annotations"))
            return Evaluate(type->annotations);

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
        else if (const TypeMessage* message = CastTo<TypeMessage>(type); message != nullptr)
        {
            if (Match("@fields"))
                return Evaluate(message->fields.size());

            for (const Field& field : message->fields)
            {
                if (Match(field.name))
                    return Evaluate(&field);
            }
        }
        else if (const TypeAttribute* attr = CastTo<TypeAttribute>(type); attr != nullptr)
        {
            if (Match("@fields"))
                return Evaluate(attr->fields.size());

            for (const Field& field : attr->fields)
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
        if (Match("@kind"))
            return Evaluate(value->kind);

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

            if (const auto [success, index] = MatchIndex(array->elements.size()); success)
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
