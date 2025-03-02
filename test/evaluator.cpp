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

namespace schematic::test
{

    CheckEvaluator::CheckEvaluator(std::string_view test, std::string_view filename, std::size_t line)
        : filename_(filename)
        , line_(line)
        , expression_(test)
    {
        components_.start = 0;
        components_.end = expression_.find(' ');

        if (components_.end == std::string::npos)
            return;

        op_.start = components_.end + 1;

        op_.end = expression_.find(' ', op_.start);

        if (op_.end == std::string::npos)
            return;

        reference_.start = op_.end + 1;
        reference_.end = expression_.size();
    }

    void CheckEvaluator::Check(const Schema* schema)
    {
        REQUIRE(schema != nullptr);

        schema_ = schema;
        nextPos_ = 0;

        Advance();
        Evaluate(schema_);
    }

    template <typename T>
    struct CheckEvaluator::CatchFailedExpression final : Catch::ITransientExpression
    {
        CatchFailedExpression(bool result, const T& value, std::string_view op, std::string_view reference)
            : ITransientExpression(true, result)
            , value(value)
            , op(op)
            , reference(reference)
        {
        }

        void streamReconstructedExpression(std::ostream& os) const override
        {
            os << Catch::Detail::stringify(value) << ' ' << op << ' ' << reference;
        }

        const T& value;
        std::string_view op;
        std::string_view reference;
    };

    struct OpIs
    {
        template <typename T>
        static bool Check(const Schema& schema, const T& value, std::string_view reference)
        {
            return Catch::Detail::stringify(NameOf<T>{ .schema = &schema, .what = value }) == reference;
        }

        static bool Check(const Schema& schema, TypeIndex typeIndex, std::string_view reference)
        {
            if (typeIndex == InvalidIndex)
                return false;

            if (typeIndex >= schema.types.Size())
                return false;

            return Catch::Detail::stringify(NameOf<const Type*>{ .schema = &schema, .what = GetType(&schema, typeIndex) }) == reference;
        }

        static bool Check(const Schema& schema, ValueIndex valueIndex, std::string_view reference)
        {
            if (valueIndex == InvalidIndex)
                return false;

            if (valueIndex >= schema.types.Size())
                return false;

            return Catch::Detail::stringify(NameOf<const Value*>{ .schema = &schema, .what = GetValue(&schema, valueIndex) }) == reference;
        }
    };

    struct OpIsA
    {
        template <typename T>
        static bool Check(const Schema&, T, std::string_view)
        {
            return false;
        }

        static bool Check(const Schema& schema, TypeIndex typeIndex, std::string_view reference)
        {
            if (typeIndex == InvalidIndex)
                return false;

            if (typeIndex >= schema.types.Size())
                return false;

            return Check(schema, GetType(&schema, typeIndex), reference);
        }

        static bool Check(const Schema& schema, const Type* type, std::string_view reference)
        {
            if (type == nullptr)
                return false;

            if (type->name == reference)
                return true;

            if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
            {
                for (const TypeStruct* comp = CastTo<TypeStruct>(GetType(&schema, struct_->base)); comp != nullptr; comp = CastTo<TypeStruct>(GetType(&schema, comp->base)))
                {
                    if (comp->name == reference)
                        return true;
                }
            }
            else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
            {
                return Check(schema, enum_->base, reference);
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
        const auto result = std::from_chars(next_.data(), next_.data() + next_.size(), index);
        if (result.ptr != next_.data() + next_.size())
            return { false, 0 };
        if (index >= max)
            return { false, 0 };
        return { true, index };
    }

    bool CheckEvaluator::Match(std::string_view name)
    {
        if (next_ == name)
        {
            Advance();
            return true;
        }

        return false;
    }

    template <typename T>
    bool CheckEvaluator::DispatchOp(T value)
    {
        Catch::AssertionHandler handler("EVALUATE", Catch::SourceLineInfo(filename_.c_str(), line_), expression_, Catch::ResultDisposition::ContinueOnFailure);
        return false;
    }

    template <typename T>
    void CheckEvaluator::Finish(T value)
    {
        const std::string_view op = std::string_view{ expression_ }.substr(op_.start, op_.end - op_.start);
        const std::string_view reference = std::string_view{ expression_ }.substr(reference_.start, reference_.end - reference_.start);

        Catch::AssertionHandler handler("EVALUATE", Catch::SourceLineInfo(filename_.c_str(), line_), expression_, Catch::ResultDisposition::ContinueOnFailure);

        if (!next_.empty())
        {
            handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Unknown component {}", next_));
        }
        else if (op == "==")
        {
            const bool result = OpIs::Check(*schema_, value, reference);
            handler.handleExpr(CatchFailedExpression(result, NameOf<T>{ .schema = schema_, .what = value }, op, reference));
        }
        else if (op == "~=")
        {
            const bool result = OpIsA::Check(*schema_, value, reference);
            handler.handleExpr(CatchFailedExpression(result, NameOf<T>{ .schema = schema_, .what = value }, op, reference));
        }
        else
        {
            handler.handleMessage(Catch::ResultWas::ExplicitFailure, fmt::format("Unknown operator {}", op));
        }

        handler.complete();
    }

    void CheckEvaluator::Evaluate(Annotations annotations)
    {
        if (Match("@length"))
            return Evaluate(annotations.count);

        for (const Annotation& anno : schema_->annotations.SubSpan(annotations.start, annotations.count))
        {
            if (anno.attribute != InvalidIndex)
            {
                const TypeAttribute* const attribute = GetTypeAs<TypeAttribute>(schema_, anno.attribute);
                if (attribute != nullptr && Match(attribute->name))
                    return Evaluate(&anno);
            }
        }

        if (const auto [success, index] = MatchIndex(annotations.count); success)
            return Evaluate(&schema_->annotations[annotations.start + index]);

        Finish(annotations);
    }

    void CheckEvaluator::Evaluate(const Annotation* annotation)
    {
        if (annotation == nullptr)
            return Finish(annotation);

        if (Match("@attribute"))
            return Evaluate(annotation->attribute);
        else if (Match("@fields"))
            return Evaluate(annotation->arguments.Size());

        for (const Argument& arg : annotation->arguments)
        {
            if (Match(schema_->fields[arg.field].name))
                return Evaluate(&arg);
        }

        if (auto [success, index] = MatchIndex(annotation->arguments.Size()); success)
            return Evaluate(annotation->arguments[index]);

        if (annotation->attribute != InvalidIndex)
        {
            const TypeAttribute* const attribute = GetTypeAs<TypeAttribute>(schema_, annotation->attribute);
            if (attribute != nullptr)
            {
                for (const Field& field : GetFields(schema_, attribute->fields))
                {
                    if (Match(field.name))
                    {
                        if (Match("@field"))
                            return Evaluate(&field);
                        return Evaluate(field.value);
                    }
                }

                if (auto [success, index] = MatchIndex(attribute->fields.count); success)
                    return Evaluate(schema_->fields[attribute->fields.start + index]);
            }
        }

        Finish(annotation);
    }

    void CheckEvaluator::Evaluate(const Field* field)
    {
        if (field == nullptr)
            return Finish(field);

        if (Match("@type"))
            return Evaluate(field->type);
        if (Match("@index"))
            return Evaluate(field->index.index);
        if (Match("@default"))
            return Evaluate(field->value);
        if (Match("@proto"))
            return Evaluate(field->proto);
        if (Match("@annotations"))
            return Evaluate(field->annotations);

        Evaluate(field->type);
    }

    void CheckEvaluator::Evaluate(const EnumItem* item)
    {
        if (item == nullptr)
            return Finish(item);

        if (Match("@value"))
            return Evaluate(item->value);
        if (Match("@annotations"))
            return Evaluate(item->annotations);

        Finish(item);
    }

    void CheckEvaluator::Evaluate(const Argument* arg)
    {
        if (arg == nullptr)
            return Finish(arg);

        if (Match("@field"))
            return Evaluate(arg->field);

        Evaluate(arg->value);
    }

    void CheckEvaluator::Evaluate(const Type* type)
    {
        if (type == nullptr)
            return Finish(type);

        if (const TypeAlias* alias = CastTo<TypeAlias>(type); alias != nullptr)
        {
            if (!Match("@self"))
                return Evaluate(alias->type);
        }

        if (Match("@kind"))
            return Evaluate(type->kind);
        if (Match("@annotations"))
            return Evaluate(type->annotations);

        if (const TypeArray* array = CastTo<TypeArray>(type); array != nullptr)
        {
            if (Match("@elements"))
                return Evaluate(array->elements);
            if (Match("@size"))
                return Evaluate(array->size);
        }

        else if (const TypeAttribute* attr = CastTo<TypeAttribute>(type); attr != nullptr)
        {
            if (Match("@fields"))
                return Evaluate(attr->fields.count);

            for (const Field& field : GetFields(schema_, attr->fields))
            {
                if (Match(field.name))
                    return Evaluate(&field);
            }
        }

        else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
        {
            if (Match("@base"))
                return Evaluate(enum_->base);
            if (Match("@count"))
                return Evaluate(enum_->items.count);

            for (const EnumItem& item : GetEnumItems(schema_, enum_->items))
            {
                if (Match(item.name))
                    return Evaluate(&item);
            }
        }

        else if (const TypeFloat* float_ = CastTo<TypeFloat>(type); float_ != nullptr)
        {
            if (Match("@width"))
                return Evaluate(float_->width);
        }

        else if (const TypeInt* int_ = CastTo<TypeInt>(type); int_ != nullptr)
        {
            if (Match("@width"))
                return Evaluate(int_->width);
            if (Match("@signed"))
                return Evaluate(int_->isSigned);
        }

        else if (const TypeMessage* message = CastTo<TypeMessage>(type); message != nullptr)
        {
            if (Match("@fields"))
                return Evaluate(message->fields.count);

            for (const Field& field : GetFields(schema_, message->fields))
            {
                if (Match(field.name))
                    return Evaluate(&field);
            }
        }

        else if (const TypePointer* pointer = CastTo<TypePointer>(type); pointer != nullptr)
        {
            if (Match("@type"))
                return Evaluate(pointer->target);
        }

        else if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
        {
            if (Match("@base"))
                return Evaluate(struct_->base);
            if (Match("@fields"))
                return Evaluate(struct_->fields.count);

            for (const Field& field : GetFields(schema_, struct_->fields))
            {
                if (Match(field.name))
                    return Evaluate(&field);
            }
        }

        else if (const TypeSchema* struct_ = CastTo<TypeSchema>(type); struct_ != nullptr)
        {
            if (Match("@base"))
                return Evaluate(struct_->base);
            if (Match("@fields"))
                return Evaluate(struct_->fields.count);
            if (Match("@version"))
                return Evaluate(struct_->version);

            for (const Field& field : GetFields(schema_, struct_->fields))
            {
                if (Match(field.name))
                    return Evaluate(&field);
            }
        }

        Finish(type);
    }

    void CheckEvaluator::Evaluate(TypeIndex typeIndex)
    {
        Evaluate(GetType(schema_, typeIndex));
    }

    void CheckEvaluator::Evaluate(ValueIndex valueIndex)
    {
        Evaluate(GetValue(schema_, valueIndex));
    }

    void CheckEvaluator::Evaluate(EnumItemIndex itemIndex)
    {
        Evaluate(GetEnumItem(schema_, itemIndex));
    }

    void CheckEvaluator::Evaluate(const Value* value)
    {
        if (value == nullptr)
            return Finish(nullptr);

        if (Match("@kind"))
            return Evaluate(value->kind);

        if (const ValueArray* const array = CastTo<ValueArray>(value); array != nullptr)
        {
            if (Match("@type"))
                return Evaluate(array->type);
            if (Match("@length"))
                return Evaluate(array->elements.Size());

            if (const auto [success, index] = MatchIndex(array->elements.Size()); success)
                return Evaluate(array->elements[index]);

            return Finish(value);
        }

        if (const ValueObject* const object = CastTo<ValueObject>(value); object != nullptr)
        {
            if (Match("@type"))
                return Evaluate(object->type);
            if (Match("@fields"))
                return Evaluate(object->fields.Size());

            for (const Argument& arg : object->fields)
            {
                if (Match(schema_->fields[arg.field].name))
                    return Evaluate(&arg);
            }

            return Finish(value);
        }

        switch (value->kind)
        {
            case ValueKind::Array: break; // unreachable
            case ValueKind::Bool: return Evaluate(static_cast<const ValueBool*>(value)->value);
            case ValueKind::Enum: return Evaluate(static_cast<const ValueEnum*>(value)->item);
            case ValueKind::Float: return Evaluate(static_cast<const ValueFloat*>(value)->value);
            case ValueKind::Int: return Evaluate(static_cast<const ValueInt*>(value)->value);
            case ValueKind::Null: return Finish(nullptr);
            case ValueKind::Object: break; // unreachable
            case ValueKind::String: return Evaluate(static_cast<const ValueString*>(value)->value);
            case ValueKind::Type: return Evaluate(static_cast<const ValueType*>(value)->type);
        }

        Finish(value);
    }

    void CheckEvaluator::Evaluate(const Module* mod)
    {
        if (Match("@imports"))
            return Evaluate(mod->imports.Size());

        Finish(mod);
    }

    void CheckEvaluator::Evaluate(const Schema* schema)
    {
        if (Match("@types"))
            return Evaluate(schema->types.Size());
        if (Match("@modules"))
            return Evaluate(schema->modules.Size());
        if (Match("@root"))
            return Evaluate(schema->root);

        for (const Type* type : schema->types)
        {
            if (type != nullptr && Match(type->name))
                return Evaluate(type);
        }

        Finish(schema);
    }

    void CheckEvaluator::Advance()
    {
        const std::string_view components = std::string_view{ expression_ }.substr(components_.start, components_.end);

        const auto pos = components.find('.', nextPos_);
        if (pos != std::string_view::npos)
        {
            next_ = components.substr(nextPos_, pos - nextPos_);
            nextPos_ = pos + 1;
        }
        else
        {
            next_ = components.substr(nextPos_);
            nextPos_ = components.size();
        }
    }
} // namespace schematic::test
