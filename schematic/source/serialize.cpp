// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/serialize.h"

#include "schematic.pb.h"

#include "schematic/schema.h"

#include <google/protobuf/util/json_util.h>

using namespace potato::schematic;

namespace
{
    class Serializer
    {
    public:
        explicit Serializer(const Schema& schema)
            : schema_(schema)
        {
        }

        void Serialize(proto::Schema& out);

    private:
        std::uint32_t IndexOfType(const Type* type) const noexcept;
        std::uint32_t IndexOfModule(const Module* mod) const noexcept;

        void Serialize(proto::Type& out, const Type& in);
        void Serialize(proto::Type::Aggregate& out, const TypeAggregate& in);
        void Serialize(proto::Type::Bool& out, const TypeBool& in);
        void Serialize(proto::Type::Int& out, const TypeInt& in);
        void Serialize(proto::Type::Float& out, const TypeFloat& in);
        void Serialize(proto::Type::Array& out, const TypeArray& in);
        void Serialize(proto::Type::String& out, const TypeString& in);
        void Serialize(proto::Type::Enum& out, const TypeEnum& in);
        void Serialize(proto::Type::TypeRef& out, const TypeType& in);
        void Serialize(proto::Type::Polymorphic& out, const TypePolymorphic& in);
        void Serialize(proto::Type::Attribute& out, const TypeAttribute& in);

        template <typename T>
        void SerializeTypeCommon(T& out, const Type& in);

        void Serialize(proto::Value& out, const Value& in);
        void Serialize(proto::Value::Object& out, const ValueObject& in);
        void Serialize(proto::Value::Bool& out, const ValueBool& in);
        void Serialize(proto::Value::Int& out, const ValueInt& in);
        void Serialize(proto::Value::Float& out, const ValueFloat& in);
        void Serialize(proto::Value::String& out, const ValueString& in);
        void Serialize(proto::Value::Array& out, const ValueArray& in);
        void Serialize(proto::Value::Enum& out, const ValueEnum& in);
        void Serialize(proto::Value::Type& out, const ValueType& in);
        void Serialize(proto::Value::Null& out, const ValueNull& in);

        void Serialize(proto::Annotation& out, const Annotation& in);

        const Schema& schema_;
    };
} // namespace

std::vector<char> potato::schematic::SerializeBinary(const Schema& schema)
{
    proto::Schema out;
    Serializer serializer(schema);
    serializer.Serialize(out);

    std::vector<char> result;
    result.resize(out.ByteSizeLong());
    out.SerializeToArray(result.data(), result.size());

    return result;
}
std::string potato::schematic::SerializeJson(const Schema& schema)
{
    proto::Schema out;
    Serializer serializer(schema);
    serializer.Serialize(out);

    std::string result;
    google::protobuf::util::JsonPrintOptions options;
    options.add_whitespace = true;
    options.preserve_proto_field_names = true;
    google::protobuf::util::MessageToJsonString(out, &result, options);

    return result;
}
void Serializer::Serialize(proto::Schema& out)
{
    for (const Module* const mod : schema_.modules)
    {
        proto::Module* const pmod = out.add_modules();
        pmod->set_filename(mod->filename.CStr());
    }

    for (const Type* const type : schema_.types)
        Serialize(*out.add_types(), *type);

    if (schema_.root != nullptr)
        out.set_root_module(IndexOfModule(schema_.root));
}

std::uint32_t Serializer::IndexOfType(const Type* type) const noexcept
{
    std::uint32_t index = 0;
    for (const Type* const candidate : schema_.types)
    {
        if (candidate == type)
            break;
        ++index;
    }
    return index;
}

std::uint32_t Serializer::IndexOfModule(const Module* mod) const noexcept
{
    std::uint32_t index = 0;
    for (const Module* const candidate : schema_.modules)
    {
        if (candidate == mod)
            break;
        ++index;
    }
    return index;
}

void Serializer::Serialize(proto::Type& out, const Type& in)
{
    switch (in.kind)
    {
        using enum TypeKind;
        case Aggregate:
            Serialize(*out.mutable_aggregate(), static_cast<const TypeAggregate&>(in));
            break;
        case Bool:
            Serialize(*out.mutable_bool_(), static_cast<const TypeBool&>(in));
            break;
        case Int:
            Serialize(*out.mutable_int_(), static_cast<const TypeInt&>(in));
            break;
        case Float:
            Serialize(*out.mutable_float_(), static_cast<const TypeFloat&>(in));
            break;
        case Array:
            Serialize(*out.mutable_array(), static_cast<const TypeArray&>(in));
            break;
        case String:
            Serialize(*out.mutable_string(), static_cast<const TypeString&>(in));
            break;
        case Enum:
            Serialize(*out.mutable_enum_(), static_cast<const TypeEnum&>(in));
            break;
        case Polymorphic:
            Serialize(*out.mutable_polymorphic(), static_cast<const TypePolymorphic&>(in));
            break;
        case Type:
            Serialize(*out.mutable_type(), static_cast<const TypeType&>(in));
            break;
        case Attribute:
            Serialize(*out.mutable_attribute(), static_cast<const TypeAttribute&>(in));
            break;
    }
}

void Serializer::Serialize(proto::Type::Aggregate& out, const TypeAggregate& in)
{
    SerializeTypeCommon(out, in);

    if (in.base != nullptr)
        out.set_base_type(IndexOfType(in.base));

    for (const Field& field : in.fields)
    {
        proto::Field& out_field = *out.add_fields();
        out_field.set_name(field.name.CStr());
        out_field.set_type(IndexOfType(field.type));

        if (field.value != nullptr)
            Serialize(*out_field.mutable_value(), *field.value);

        for (const Annotation* const annotation : field.annotations)
            Serialize(*out_field.add_annotations(), *annotation);
    }
}

void Serializer::Serialize(proto::Type::Bool& out, const TypeBool& in)
{
    SerializeTypeCommon(out, in);
}

void Serializer::Serialize(proto::Type::Int& out, const TypeInt& in)
{
    SerializeTypeCommon(out, in);

    out.set_width(in.bits);
    out.set_signed_(in.isSigned);
}

void Serializer::Serialize(proto::Type::Float& out, const TypeFloat& in)
{
    SerializeTypeCommon(out, in);

    out.set_width(in.bits);
}

void Serializer::Serialize(proto::Type::Array& out, const TypeArray& in)
{
    SerializeTypeCommon(out, in);

    out.set_element_type(IndexOfType(in.type));
    if (in.isFixed)
        out.set_size(in.size);
}

void Serializer::Serialize(proto::Type::String& out, const TypeString& in)
{
    SerializeTypeCommon(out, in);
}

void Serializer::Serialize(proto::Type::Enum& out, const TypeEnum& in)
{
    SerializeTypeCommon(out, in);

    if (in.base != nullptr)
        out.set_base_type(IndexOfType(in.base));

    for (const EnumItem& item : in.items)
    {
        proto::EnumItem& out_item = *out.add_items();
        out_item.set_name(item.name.CStr());
        if (item.value != nullptr)
            out_item.set_value(item.value->value);

        for (const Annotation* const annotation : item.annotations)
            Serialize(*out_item.add_annotations(), *annotation);
    }
}

void Serializer::Serialize(proto::Type::TypeRef& out, const TypeType& in)
{
    SerializeTypeCommon(out, in);
}

void Serializer::Serialize(proto::Type::Polymorphic& out, const TypePolymorphic& in)
{
    SerializeTypeCommon(out, in);

    if (in.type != nullptr)
        out.set_type(IndexOfType(in.type));

    out.set_nullable(in.isNullable);
}

void Serializer::Serialize(proto::Type::Attribute& out, const TypeAttribute& in)
{
    SerializeTypeCommon(out, in);

    for (const Field& field : in.fields)
    {
        proto::Field& out_field = *out.add_fields();
        out_field.set_name(field.name.CStr());
        out_field.set_type(IndexOfType(field.type));
        if (field.value != nullptr)
            Serialize(*out_field.mutable_value(), *field.value);

        for (const Annotation* const annotation : field.annotations)
            Serialize(*out_field.add_annotations(), *annotation);
    }
}

template <typename T>
void Serializer::SerializeTypeCommon(T& out, const Type& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));

    for (const Annotation* const annotation : in.annotations)
        Serialize(*out.add_annotations(), *annotation);
}

void Serializer::Serialize(proto::Value& out, const Value& in)
{
    switch (in.kind)
    {
        using enum ValueKind;
        case Object:
            Serialize(*out.mutable_object(), static_cast<const ValueObject&>(in));
            break;
        case Int:
            Serialize(*out.mutable_int_(), static_cast<const ValueInt&>(in));
            break;
        case Bool:
            Serialize(*out.mutable_bool_(), static_cast<const ValueBool&>(in));
            break;
        case Float:
            Serialize(*out.mutable_float_(), static_cast<const ValueFloat&>(in));
            break;
        case Array:
            Serialize(*out.mutable_array(), static_cast<const ValueArray&>(in));
            break;
        case Enum:
            Serialize(*out.mutable_enum_(), static_cast<const ValueEnum&>(in));
            break;
        case Null:
            Serialize(*out.mutable_null(), static_cast<const ValueNull&>(in));
            break;
        case Type:
            Serialize(*out.mutable_type(), static_cast<const ValueType&>(in));
            break;
        case String:
            Serialize(*out.mutable_string(), static_cast<const ValueString&>(in));
            break;
    }
}

void Serializer::Serialize(proto::Value::Object& out, const ValueObject& in)
{
    out.set_type(IndexOfType(in.type));

    for (const Argument& arg : in.fields)
    {
        proto::Argument& out_arg = *out.add_arguments();
        out_arg.set_field_name(arg.field->name.CStr());

        if (arg.value != nullptr)
            Serialize(*out_arg.mutable_value(), *arg.value);
    }
}

void Serializer::Serialize(proto::Value::Bool& out, const ValueBool& in)
{
    out.set_value(in.value);
}

void Serializer::Serialize(proto::Value::Int& out, const ValueInt& in)
{
    out.set_value(in.value);
}

void Serializer::Serialize(proto::Value::Float& out, const ValueFloat& in)
{
    out.set_value(in.value);
}

void Serializer::Serialize(proto::Value::String& out, const ValueString& in)
{
    out.set_value(in.value.CStr());
}

void Serializer::Serialize(proto::Value::Array& out, const ValueArray& in)
{
    out.set_type(IndexOfType(in.type));

    for (const Value* const value : in.elements)
        Serialize(*out.add_elements(), *value);
}

void Serializer::Serialize(proto::Value::Enum& out, const ValueEnum& in)
{
    if (in.item != nullptr)
    {
        out.set_type(IndexOfType(in.item->owner));

        std::uint32_t index = 0;
        for (const EnumItem& item : in.item->owner->items)
        {
            if (&item == in.item)
            {
                out.set_item(index);
                break;
            }
            ++index;
        }
    }
}

void Serializer::Serialize(proto::Value::Type& out, const ValueType& in)
{
    out.set_type(IndexOfType(in.type));
}

void Serializer::Serialize(proto::Value::Null& out, const ValueNull& in)
{
}

void Serializer::Serialize(proto::Annotation& out, const Annotation& in)
{
    out.set_attribute_type(IndexOfType(in.attribute));

    for (const Argument& arg : in.arguments)
    {
        proto::Argument& out_arg = *out.add_arguments();
        out_arg.set_field_name(arg.field->name.CStr());

        if (arg.value != nullptr)
            Serialize(*out_arg.mutable_value(), *arg.value);
    }
}
