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
            break;
    }
}

void Serializer::Serialize(proto::Type::Aggregate& out, const TypeAggregate& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));

    if (in.base != nullptr)
        out.set_base_type(IndexOfType(in.base));

    for (const Field& field : in.fields)
    {
        proto::Field& out_field = *out.add_fields();
        out_field.set_name(field.name.CStr());
        out_field.set_type(IndexOfType(field.type));
    }
}

void Serializer::Serialize(proto::Type::Bool& out, const TypeBool& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));
}

void Serializer::Serialize(proto::Type::Int& out, const TypeInt& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));
    out.set_width(in.bits);
    out.set_signed_(in.isSigned);
}

void Serializer::Serialize(proto::Type::Float& out, const TypeFloat& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));
    out.set_width(in.bits);
}

void Serializer::Serialize(proto::Type::Array& out, const TypeArray& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));

    out.set_element_type(IndexOfType(in.type));
    if (in.isFixed)
        out.set_size(in.size);
}

void Serializer::Serialize(proto::Type::String& out, const TypeString& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));
}

void Serializer::Serialize(proto::Type::Enum& out, const TypeEnum& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));

    if (in.base != nullptr)
        out.set_base_type(IndexOfType(in.base));

    for (const EnumItem& item : in.items)
    {
        proto::EnumItem& out_item = *out.add_items();
        out_item.set_name(item.name.CStr());
        if (item.value != nullptr)
            out_item.set_value(item.value->value);
    }
}

void Serializer::Serialize(proto::Type::TypeRef& out, const TypeType& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));
}

void Serializer::Serialize(proto::Type::Polymorphic& out, const TypePolymorphic& in)
{
    out.set_name(in.name.CStr());
    out.set_module(IndexOfModule(in.owner));

    if (in.type != nullptr)
        out.set_type(IndexOfType(in.type));

    out.set_nullable(in.isNullable);
}
