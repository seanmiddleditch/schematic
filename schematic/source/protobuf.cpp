// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/protobuf.h"

#include "array.h"

#include "schematic/logger.h"
#include "schematic/schema.h"
#include "schematic/schematic.pb.h"
#include "schematic/utility.h"

#include <fmt/core.h>

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

        void Serialize(proto::Type& out, const Type& in);
        void Serialize(proto::Type::Alias& out, const TypeAlias& in);
        void Serialize(proto::Type::Array& out, const TypeArray& in);
        void Serialize(proto::Type::Attribute& out, const TypeAttribute& in);
        void Serialize(proto::Type::Bool& out, const TypeBool& in);
        void Serialize(proto::Type::Enum& out, const TypeEnum& in);
        void Serialize(proto::Type::Float& out, const TypeFloat& in);
        void Serialize(proto::Type::Int& out, const TypeInt& in);
        void Serialize(proto::Type::Message& out, const TypeMessage& in);
        void Serialize(proto::Type::Nullable& out, const TypeNullable& in);
        void Serialize(proto::Type::Pointer& out, const TypePointer& in);
        void Serialize(proto::Type::String& out, const TypeString& in);
        void Serialize(proto::Type::Struct& out, const TypeStruct& in);
        void Serialize(proto::Type::TypeRef& out, const TypeType& in);

        void SerializeLocation(proto::Location* out, const Location& in);

        void SerializeField(proto::Field& out, const Field& in);

        template <typename T>
        void SerializeTypeCommon(T& out, const Type& in);
        template <typename T>
        void SerializeValueCommon(T& out, const Value& in);

        void Serialize(proto::Value& out, const Value& in);
        void Serialize(proto::Value::Array& out, const ValueArray& in);
        void Serialize(proto::Value::Bool& out, const ValueBool& in);
        void Serialize(proto::Value::Enum& out, const ValueEnum& in);
        void Serialize(proto::Value::Float& out, const ValueFloat& in);
        void Serialize(proto::Value::Int& out, const ValueInt& in);
        void Serialize(proto::Value::Null& out, const ValueNull& in);
        void Serialize(proto::Value::Object& out, const ValueObject& in);
        void Serialize(proto::Value::String& out, const ValueString& in);
        void Serialize(proto::Value::Type& out, const ValueType& in);

        void Serialize(proto::Annotation& out, const Annotation& in);

        const Schema& schema_;
    };

    class Deserializer
    {
    public:
        explicit Deserializer(ArenaAllocator& arena, Logger& logger, const proto::Schema& proto)
            : arena_(arena)
            , logger_(logger)
            , proto_(proto)
        {
        }

        const Schema* Deserialize();

    private:
        void Deserialize(Type& out, const proto::Type& in);
        void Deserialize(TypeAlias& out, const proto::Type::Alias& in);
        void Deserialize(TypeAttribute& out, const proto::Type::Attribute& in);
        void Deserialize(TypeArray& out, const proto::Type::Array& in);
        void Deserialize(TypeBool& out, const proto::Type::Bool& in);
        void Deserialize(TypeInt& out, const proto::Type::Int& in);
        void Deserialize(TypeEnum& out, const proto::Type::Enum& in);
        void Deserialize(TypeFloat& out, const proto::Type::Float& in);
        void Deserialize(TypeMessage& out, const proto::Type::Message& in);
        void Deserialize(TypeNullable& out, const proto::Type::Nullable& in);
        void Deserialize(TypePointer& out, const proto::Type::Pointer& in);
        void Deserialize(TypeString& out, const proto::Type::String& in);
        void Deserialize(TypeStruct& out, const proto::Type::Struct& in);
        void Deserialize(TypeType& out, const proto::Type::TypeRef& in);

        void DeserializeLocation(Location& out, const proto::Location& in);

        void DeserializeField(Field& out, const proto::Field& in);

        template <typename T>
        Array<Annotation*> DeserializeAnnotations(const T& in);

        template <typename T>
        void DeserializeTypeCommon(Type& out, const T& in);
        template <typename T>
        void DeserializeValueCommon(Value& out, const T& in);

        void Deserialize(Value& out, const proto::Value& in);
        void Deserialize(ValueObject& out, const proto::Value::Object& in);
        void Deserialize(ValueBool& out, const proto::Value::Bool& in);
        void Deserialize(ValueInt& out, const proto::Value::Int& in);
        void Deserialize(ValueFloat& out, const proto::Value::Float& in);
        void Deserialize(ValueString& out, const proto::Value::String& in);
        void Deserialize(ValueArray& out, const proto::Value::Array& in);
        void Deserialize(ValueEnum& out, const proto::Value::Enum& in);
        void Deserialize(ValueType& out, const proto::Value::Type& in);
        void Deserialize(ValueNull& out, const proto::Value::Null& in);

        void Deserialize(Annotation& out, const proto::Annotation& in);

        template <typename... Args>
        void ReportVerifyFailure(fmt::format_string<Args...> format, Args&&... args);

        ArenaAllocator& arena_;
        Logger& logger_;
        const proto::Schema& proto_;
        Schema* schema_ = nullptr;
        Array<Module> modules_;
        Array<Type*> types_;
        Array<Field> fields_;
        Array<Value*> values_;
        bool failed_ = false;
    };
} // namespace

const proto::Schema* potato::schematic::SerializeSchemaProto(google::protobuf::Arena& arena, const Schema* schema)
{
    if (schema == nullptr)
        return nullptr;
    proto::Schema* const proto = google::protobuf::Arena::Create<proto::Schema>(&arena);
    Serializer serializer(*schema);
    serializer.Serialize(*proto);
    return proto;
}

const Schema* potato::schematic::ParseSchemaProto(ArenaAllocator& arena, Logger& logger, const proto::Schema* proto)
{
    if (proto == nullptr)
        return nullptr;
    Deserializer deserializer(arena, logger, *proto);
    const Schema* const schema = deserializer.Deserialize();
    return schema;
}

// --- Serializer ---

void Serializer::Serialize(proto::Schema& out)
{
    for (const Module& mod : schema_.modules)
    {
        proto::Module* const pmod = out.add_modules();
        pmod->set_filename(mod.filename);
        for (const ModuleIndex index : mod.imports)
            pmod->add_imports(index);
    }

    out.set_root(schema_.root);

    for (const Type* const type : schema_.types)
        Serialize(*out.add_types(), *type);

    for (const Field& field : schema_.fields)
        SerializeField(*out.add_fields(), field);

    for (const Value* const value : schema_.values)
        Serialize(*out.add_values(), *value);
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

void Serializer::Serialize(proto::Type& out, const Type& in)
{
    switch (in.kind)
    {
        using enum TypeKind;
        case Alias:
            Serialize(*out.mutable_alias(), static_cast<const TypeAlias&>(in));
            break;
        case Array:
            Serialize(*out.mutable_array(), static_cast<const TypeArray&>(in));
            break;
        case Attribute:
            Serialize(*out.mutable_attribute(), static_cast<const TypeAttribute&>(in));
            break;
        case Bool:
            Serialize(*out.mutable_bool_(), static_cast<const TypeBool&>(in));
            break;
        case Enum:
            Serialize(*out.mutable_enum_(), static_cast<const TypeEnum&>(in));
            break;
        case Float:
            Serialize(*out.mutable_float_(), static_cast<const TypeFloat&>(in));
            break;
        case Int:
            Serialize(*out.mutable_int_(), static_cast<const TypeInt&>(in));
            break;
        case Message:
            Serialize(*out.mutable_message(), static_cast<const TypeMessage&>(in));
            break;
        case Nullable:
            Serialize(*out.mutable_nullable(), static_cast<const TypeNullable&>(in));
            break;
        case Pointer:
            Serialize(*out.mutable_pointer(), static_cast<const TypePointer&>(in));
            break;
        case String:
            Serialize(*out.mutable_string(), static_cast<const TypeString&>(in));
            break;
        case Struct:
            Serialize(*out.mutable_struct_(), static_cast<const TypeStruct&>(in));
            break;
        case Type:
            Serialize(*out.mutable_type(), static_cast<const TypeType&>(in));
            break;
    }
}

void Serializer::Serialize(proto::Type::Struct& out, const TypeStruct& in)
{
    SerializeTypeCommon(out, in);

    if (in.base != InvalidIndex)
        out.set_base(in.base);
}

void Serializer::Serialize(proto::Type::Bool& out, const TypeBool& in)
{
    SerializeTypeCommon(out, in);
}

void Serializer::Serialize(proto::Type::Int& out, const TypeInt& in)
{
    SerializeTypeCommon(out, in);

    out.set_width(in.width);
    out.set_signed_(in.isSigned);
}

void Serializer::Serialize(proto::Type::Float& out, const TypeFloat& in)
{
    SerializeTypeCommon(out, in);

    out.set_width(in.width);
}

void Serializer::Serialize(proto::Type::Alias& out, const TypeAlias& in)
{
    SerializeTypeCommon(out, in);

    out.set_type(in.type);
}

void Serializer::Serialize(proto::Type::Array& out, const TypeArray& in)
{
    SerializeTypeCommon(out, in);

    out.set_elements(in.elements);
    out.set_size(in.size);
}

void Serializer::Serialize(proto::Type::String& out, const TypeString& in)
{
    SerializeTypeCommon(out, in);
}

void Serializer::Serialize(proto::Type::Enum& out, const TypeEnum& in)
{
    SerializeTypeCommon(out, in);

    if (in.base != InvalidIndex)
        out.set_base(in.base);

    for (const EnumItem& item : in.items)
    {
        proto::EnumItem& out_item = *out.add_items();
        out_item.set_name(item.name);

        SerializeLocation(out_item.mutable_location(), item.location);

        out_item.set_value(item.value);

        for (const Annotation* const annotation : item.annotations)
            Serialize(*out_item.add_annotations(), *annotation);
    }
}

void Serializer::Serialize(proto::Type::TypeRef& out, const TypeType& in)
{
    SerializeTypeCommon(out, in);
}

void Serializer::Serialize(proto::Type::Nullable& out, const TypeNullable& in)
{
    SerializeTypeCommon(out, in);

    if (in.target != InvalidIndex)
        out.set_target(in.target);
}

void Serializer::Serialize(proto::Type::Pointer& out, const TypePointer& in)
{
    SerializeTypeCommon(out, in);

    if (in.target != InvalidIndex)
        out.set_target(in.target);
}

void Serializer::Serialize(proto::Type::Message& out, const TypeMessage& in)
{
    SerializeTypeCommon(out, in);

    out.set_field_start(in.fields.start);
    out.set_field_count(in.fields.count);
}

void Serializer::Serialize(proto::Type::Attribute& out, const TypeAttribute& in)
{
    SerializeTypeCommon(out, in);

    out.set_field_start(in.fields.start);
    out.set_field_count(in.fields.count);
}

void Serializer::SerializeLocation(proto::Location* out, const Location& in)
{
    out->set_line(in.line);
    out->set_column(in.column);
}

void Serializer::SerializeField(proto::Field& out, const Field& in)
{
    out.set_name(in.name);
    out.set_type(in.type);

    SerializeLocation(out.mutable_location(), in.location);

    if (in.proto != 0)
        out.set_proto(in.proto);

    out.set_value(in.value);

    for (const Annotation* const annotation : in.annotations)
        Serialize(*out.add_annotations(), *annotation);
}

template <typename T>
void Serializer::SerializeTypeCommon(T& out, const Type& in)
{
    out.set_name(in.name);
    out.set_module(in.parent);

    for (const Annotation* const annotation : in.annotations)
        Serialize(*out.add_annotations(), *annotation);

    SerializeLocation(out.mutable_location(), in.location);
}

template <typename T>
void Serializer::SerializeValueCommon(T& out, const Value& in)
{
    SerializeLocation(out.mutable_location(), in.location);
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
    SerializeValueCommon(out, in);
    out.set_type(in.type);

    for (const Argument& arg : in.fields)
    {
        proto::Argument& out_arg = *out.add_arguments();
        out_arg.set_field(arg.field);

        SerializeLocation(out_arg.mutable_location(), arg.location);

        if (arg.value != InvalidIndex)
            out_arg.set_value(arg.value);
    }
}

void Serializer::Serialize(proto::Value::Bool& out, const ValueBool& in)
{
    SerializeValueCommon(out, in);
    out.set_value(in.value);
}

void Serializer::Serialize(proto::Value::Int& out, const ValueInt& in)
{
    SerializeValueCommon(out, in);
    out.set_value(in.value);
}

void Serializer::Serialize(proto::Value::Float& out, const ValueFloat& in)
{
    SerializeValueCommon(out, in);
    out.set_value(in.value);
}

void Serializer::Serialize(proto::Value::String& out, const ValueString& in)
{
    SerializeValueCommon(out, in);
    out.set_value(in.value);
}

void Serializer::Serialize(proto::Value::Array& out, const ValueArray& in)
{
    SerializeValueCommon(out, in);
    out.set_type(in.type);

    for (const ValueIndex valueIndex : in.elements)
        out.add_elements(valueIndex);
}

void Serializer::Serialize(proto::Value::Enum& out, const ValueEnum& in)
{
    SerializeValueCommon(out, in);
    if (in.item != nullptr)
    {
        out.set_type(in.item->parent);

        const TypeEnum* enum_ = CastTo<TypeEnum>(GetType(&schema_, in.item->parent));
        if (enum_ == nullptr)
            return;

        std::uint32_t index = 0;
        for (const EnumItem& item : enum_->items)
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
    SerializeValueCommon(out, in);
    out.set_type(in.type);
}

void Serializer::Serialize(proto::Value::Null& out, const ValueNull& in)
{
    SerializeValueCommon(out, in);
}

void Serializer::Serialize(proto::Annotation& out, const Annotation& in)
{
    out.set_attribute(in.attribute);
    SerializeLocation(out.mutable_location(), in.location);

    for (const Argument& arg : in.arguments)
    {
        proto::Argument& out_arg = *out.add_arguments();
        out_arg.set_field(arg.field);
        SerializeLocation(out_arg.mutable_location(), arg.location);

        if (arg.value != InvalidIndex)
            out_arg.set_value(arg.value);
    }
}

// --- Deserializer ---

#define VERIFY(EXPR, MESSAGE, ...) \
    ((EXPR) || !(ReportVerifyFailure("" MESSAGE, ##__VA_ARGS__), failed_ = true))

#define VERIFY_INDEX(ARRAY, INDEX, MESSAGE, ...) \
    VERIFY((INDEX) < (ARRAY).Size(), "" MESSAGE, ##__VA_ARGS__)

const Schema* Deserializer::Deserialize()
{
    schema_ = arena_.New<Schema>();

    modules_ = arena_.NewArray<Module>(proto_.modules_size());
    types_ = arena_.NewArray<Type*>(proto_.types_size());
    fields_ = arena_.NewArray<Field>(proto_.fields_size());
    values_ = arena_.NewArray<Value*>(proto_.values_size());

    // Instantiate modules
    for (std::size_t index = 0; index != modules_.Capacity(); ++index)
        modules_.EmplaceBack(arena_);

    const std::uint32_t rootIndex = proto_.root();
    if (VERIFY_INDEX(modules_, rootIndex, "Invalid root module index {}", rootIndex))
        schema_->root = rootIndex;

    // Deserialize modules
    std::size_t moduleIndex = 0;
    for (const proto::Module& mod : proto_.modules())
    {
        Module& out_mod = modules_[moduleIndex++];
        out_mod.filename = arena_.NewString(mod.filename());

        Array<ModuleIndex> imports = arena_.NewArray<ModuleIndex>(mod.imports_size());
        for (std::uint32_t targetIndex : mod.imports())
        {
            if (VERIFY_INDEX(modules_, targetIndex, "Invalid import index {}", targetIndex))
                imports.PushBack(arena_, targetIndex);
        }
        out_mod.imports = imports;
    }
    schema_->modules = modules_;

    // Instantiate types
    for (const proto::Type& type : proto_.types())
    {
        switch (type.Types_case())
        {
            case proto::Type::kAlias:
                types_.PushBack(arena_, arena_.New<TypeAlias>());
                continue;
            case proto::Type::kArray:
                types_.PushBack(arena_, arena_.New<TypeArray>());
                continue;
            case proto::Type::kAttribute:
                types_.PushBack(arena_, arena_.New<TypeAttribute>());
                continue;
            case proto::Type::kBool:
                types_.PushBack(arena_, arena_.New<TypeBool>());
                continue;
            case proto::Type::kEnum:
                types_.PushBack(arena_, arena_.New<TypeEnum>());
                continue;
            case proto::Type::kFloat:
                types_.PushBack(arena_, arena_.New<TypeFloat>());
                continue;
            case proto::Type::kInt:
                types_.PushBack(arena_, arena_.New<TypeInt>());
                continue;
            case proto::Type::kMessage:
                types_.PushBack(arena_, arena_.New<TypeMessage>());
                continue;
            case proto::Type::kNullable:
                types_.PushBack(arena_, arena_.New<TypeNullable>());
                continue;
            case proto::Type::kPointer:
                types_.PushBack(arena_, arena_.New<TypePointer>());
                continue;
            case proto::Type::kString:
                types_.PushBack(arena_, arena_.New<TypeString>());
                continue;
            case proto::Type::kStruct:
                types_.PushBack(arena_, arena_.New<TypeStruct>());
                continue;
            case proto::Type::kType:
                types_.PushBack(arena_, arena_.New<TypeType>());
                continue;
        }
        VERIFY(false, "Uknown type kind tag");
        return nullptr;
    }
    schema_->types = types_;

    // Instantiate values
    for (const proto::Value& value : proto_.values())
    {
        switch (value.Values_case())
        {
            case proto::Value::kArray:
                values_.PushBack(arena_, arena_.New<ValueArray>());
                continue;
            case proto::Value::kBool:
                values_.PushBack(arena_, arena_.New<ValueBool>());
                continue;
            case proto::Value::kEnum:
                values_.PushBack(arena_, arena_.New<ValueEnum>());
                continue;
            case proto::Value::kFloat:
                values_.PushBack(arena_, arena_.New<ValueFloat>());
                continue;
            case proto::Value::kInt:
                values_.PushBack(arena_, arena_.New<ValueInt>());
                continue;
            case proto::Value::kNull:
                values_.PushBack(arena_, arena_.New<ValueNull>());
                continue;
            case proto::Value::kObject:
                values_.PushBack(arena_, arena_.New<ValueObject>());
                continue;
            case proto::Value::kString:
                values_.PushBack(arena_, arena_.New<ValueString>());
                continue;
            case proto::Value::kType:
                values_.PushBack(arena_, arena_.New<ValueType>());
                continue;
        }
        VERIFY(false, "Uknown type kind tag");
        return nullptr;
    }
    schema_->values = values_;

    // Instantiate fields
    for (const proto::Field& _ : proto_.fields())
        fields_.EmplaceBack(arena_);

    // Deserialize types
    TypeIndex typeIndex = 0;
    for (const proto::Type& type : proto_.types())
    {
        potato::schematic::Type* const outType = types_[typeIndex];
        assert(outType != nullptr); // previous unknown tag check should return, cannot reach this statement
        outType->index = typeIndex++;
        Deserialize(*outType, type);
    }

    // Deserialize fields
    FieldIndex fieldIndex = 0;
    for (const proto::Field& field : proto_.fields())
    {
        potato::schematic::Field& outField = fields_[fieldIndex];
        outField.index = fieldIndex++;
        DeserializeField(outField, field);
    }
    schema_->fields = fields_;

    // Deserialize values
    ValueIndex valueIndex = 0;
    for (const proto::Value& value : proto_.values())
    {
        potato::schematic::Value* const outValue = values_[valueIndex++];
        Deserialize(*outValue, value);
    }
    schema_->fields = fields_;

    if (failed_)
        return nullptr;

    return schema_;
}

void Deserializer::Deserialize(Type& out, const proto::Type& in)
{
    switch (out.kind)
    {
        case TypeKind::Alias:
            Deserialize(static_cast<TypeAlias&>(out), in.alias());
            return;
        case TypeKind::Struct:
            Deserialize(static_cast<TypeStruct&>(out), in.struct_());
            return;
        case TypeKind::Array:
            Deserialize(static_cast<TypeArray&>(out), in.array());
            return;
        case TypeKind::Attribute:
            Deserialize(static_cast<TypeAttribute&>(out), in.attribute());
            return;
        case TypeKind::Bool:
            Deserialize(static_cast<TypeBool&>(out), in.bool_());
            return;
        case TypeKind::Enum:
            Deserialize(static_cast<TypeEnum&>(out), in.enum_());
            return;
        case TypeKind::Float:
            Deserialize(static_cast<TypeFloat&>(out), in.float_());
            return;
        case TypeKind::Int:
            Deserialize(static_cast<TypeInt&>(out), in.int_());
            return;
        case TypeKind::Message:
            Deserialize(static_cast<TypeMessage&>(out), in.message());
            return;
        case TypeKind::Nullable:
            Deserialize(static_cast<TypeNullable&>(out), in.nullable());
            return;
        case TypeKind::Pointer:
            Deserialize(static_cast<TypePointer&>(out), in.pointer());
            return;
        case TypeKind::String:
            Deserialize(static_cast<TypeString&>(out), in.string());
            return;
        case TypeKind::Type:
            Deserialize(static_cast<TypeType&>(out), in.type());
            return;
    }
    assert(false); // should be unreachable
}

void Deserializer::Deserialize(TypeAlias& out, const proto::Type::Alias& in)
{
    DeserializeTypeCommon(out, in);

    if (VERIFY_INDEX(types_, in.type(), "Invalid alias type index {}", in.type()))
        out.type = in.type();
}

void Deserializer::Deserialize(TypeStruct& out, const proto::Type::Struct& in)
{
    DeserializeTypeCommon(out, in);

    if (in.has_base())
    {
        if (VERIFY_INDEX(types_, in.base(), "Invalid base type index {}", in.base()))
        {
            const Type* const base = types_[in.base()];
            if (VERIFY(base->kind == TypeKind::Struct, "Invalid base type kind"))
                out.base = in.base();
        }
    }

    if (in.field_start() != InvalidIndex)
    {
        if (VERIFY_INDEX(fields_, in.field_start(), "Invalid field start index {}", in.field_start()))
            out.fields.start = in.field_start();

        if (VERIFY(out.fields.start + in.field_count() <= fields_.Size(), "Overflow field range {},{}", out.fields.start, in.field_count()))
            out.fields.count = in.field_count();
    }
}

void Deserializer::Deserialize(TypeBool& out, const proto::Type::Bool& in)
{
    DeserializeTypeCommon(out, in);
}

void Deserializer::Deserialize(TypeInt& out, const proto::Type::Int& in)
{
    DeserializeTypeCommon(out, in);
    out.width = in.width();
    out.isSigned = in.signed_();
}

void Deserializer::Deserialize(TypeFloat& out, const proto::Type::Float& in)
{
    DeserializeTypeCommon(out, in);
    out.width = in.width();
}

void Deserializer::Deserialize(TypeArray& out, const proto::Type::Array& in)
{
    DeserializeTypeCommon(out, in);
    if (in.has_size())
        out.size = in.size();

    if (VERIFY_INDEX(types_, in.elements(), "Invalid element type index {}", in.elements()))
        out.elements = in.elements();
}

void Deserializer::Deserialize(TypeString& out, const proto::Type::String& in)
{
    DeserializeTypeCommon(out, in);
}

void Deserializer::Deserialize(TypeEnum& out, const proto::Type::Enum& in)
{
    DeserializeTypeCommon(out, in);

    if (in.has_base())
    {
        if (VERIFY_INDEX(types_, in.base(), "Invalid enum base type index {}", in.base()))
        {
            const Type* const base = types_[in.base()];
            if (VERIFY(base->kind == TypeKind::Int, "Invalid enum base type kind {}", std::to_underlying(base->kind)))
                out.base = in.base();
        }
    }

    Array<EnumItem> items = arena_.NewArray<EnumItem>(in.items_size());
    for (const proto::EnumItem& item : in.items())
    {
        EnumItem& out_item = items.EmplaceBack(arena_);
        out_item.name = arena_.NewString(item.name());
        out_item.parent = out.index;

        DeserializeLocation(out_item.location, item.location());

        if (VERIFY_INDEX(values_, item.value(), "Invalid enum item value index {}", item.value()))
        {
            if (VERIFY(values_[item.value()]->kind == ValueKind::Int, "Invalid enum item value kind"))
                out_item.value = item.value();
        }

        out_item.annotations = DeserializeAnnotations(item);
    }
    out.items = items;
}

void Deserializer::Deserialize(TypeType& out, const proto::Type::TypeRef& in)
{
    DeserializeTypeCommon(out, in);
}

void Deserializer::Deserialize(TypeMessage& out, const proto::Type::Message& in)
{
    DeserializeTypeCommon(out, in);

    if (in.field_start() != InvalidIndex)
    {
        if (VERIFY_INDEX(fields_, in.field_start(), "Invalid field start index {}", in.field_start()))
            out.fields.start = in.field_start();

        if (VERIFY(out.fields.start + in.field_count() <= fields_.Size(), "Overflow field range {},{}", out.fields.start, in.field_count()))
            out.fields.count = in.field_count();
    }
}

void Deserializer::Deserialize(TypeNullable& out, const proto::Type::Nullable& in)
{
    DeserializeTypeCommon(out, in);

    if (VERIFY_INDEX(types_, in.target(), "Invalid nullable type index {}", in.target()))
        out.target = in.target();
}

void Deserializer::Deserialize(TypePointer& out, const proto::Type::Pointer& in)
{
    DeserializeTypeCommon(out, in);

    if (VERIFY_INDEX(types_, in.target(), "Invalid pointer type index {}", in.target()))
        out.target = in.target();
}

void Deserializer::Deserialize(TypeAttribute& out, const proto::Type::Attribute& in)
{
    DeserializeTypeCommon(out, in);

    if (in.field_start() != InvalidIndex)
    {
        if (VERIFY_INDEX(fields_, in.field_start(), "Invalid field start index {}", in.field_start()))
            out.fields.start = in.field_start();

        if (VERIFY(out.fields.start + in.field_count() <= fields_.Size(), "Overflow field range {},{}", out.fields.start, in.field_count()))
            out.fields.count = in.field_count();
    }
}

void Deserializer::DeserializeLocation(Location& out, const proto::Location& in)
{
    out.line = in.line();
    out.column = in.column();
}

void Deserializer::DeserializeField(Field& out, const proto::Field& in)
{
    out.name = arena_.NewString(in.name());

    if (VERIFY_INDEX(types_, in.parent(), "Invalid field parent index {}", in.parent()))
        out.parent = in.parent();

    DeserializeLocation(out.location, in.location());

    if (in.has_proto())
        out.proto = in.proto();

    if (VERIFY_INDEX(types_, in.type(), "Invalid field type index {}", in.type()))
        out.type = in.type();

    if (in.value() != InvalidIndex && VERIFY_INDEX(values_, in.value(), "Invalid field value index {}", in.value()))
        out.value = in.value();

    out.annotations = DeserializeAnnotations(in);
}

template <typename T>
Array<Annotation*> Deserializer::DeserializeAnnotations(const T& in)
{
    Array<Annotation*> annotations = arena_.NewArray<Annotation*>(in.annotations_size());
    for (const proto::Annotation& anno : in.annotations())
    {
        Annotation* const out_anno = arena_.New<Annotation>();
        Deserialize(*out_anno, anno);
        annotations.PushBack(arena_, out_anno);
    }
    return annotations;
}

template <typename T>
void Deserializer::DeserializeTypeCommon(Type& out, const T& in)
{
    out.name = arena_.NewString(in.name());
    if (VERIFY_INDEX(modules_, in.module(), "Invalid module index {}", in.module()))
        out.parent = in.module();

    out.annotations = DeserializeAnnotations(in);

    out.location.line = in.location().line();
    out.location.column = in.location().column();
}

template <typename T>
void Deserializer::DeserializeValueCommon(Value& out, const T& in)
{
    DeserializeLocation(out.location, in.location());
}

void Deserializer::Deserialize(Value& out, const proto::Value& in)
{
    switch (in.Values_case())
    {
        case proto::Value::kArray: return Deserialize(static_cast<ValueArray&>(out), in.array());
        case proto::Value::kBool: return Deserialize(static_cast<ValueBool&>(out), in.bool_());
        case proto::Value::kEnum: return Deserialize(static_cast<ValueEnum&>(out), in.enum_());
        case proto::Value::kFloat: return Deserialize(static_cast<ValueFloat&>(out), in.float_());
        case proto::Value::kInt: return Deserialize(static_cast<ValueInt&>(out), in.int_());
        case proto::Value::kNull: return Deserialize(static_cast<ValueNull&>(out), in.null());
        case proto::Value::kObject: return Deserialize(static_cast<ValueObject&>(out), in.object());
        case proto::Value::kString: return Deserialize(static_cast<ValueString&>(out), in.string());
        case proto::Value::kType: return Deserialize(static_cast<ValueType&>(out), in.type());
    }
    failed_ = true; // unknown type tag
}

void Deserializer::Deserialize(ValueObject& out, const proto::Value::Object& in)
{
    DeserializeValueCommon(out, in);

    if (!VERIFY_INDEX(types_, in.type(), "Invalid object type index {}", in.type()))
        return;

    if (!VERIFY(types_[in.type()]->kind == TypeKind::Struct, "Invalid object type kind"))
        return;

    out.type = in.type();

    Array<Argument> args = arena_.NewArray<Argument>(in.arguments_size());
    for (const proto::Argument& arg : in.arguments())
    {
        Argument& out_arg = args.EmplaceBack(arena_);
        DeserializeLocation(out_arg.location, arg.location());

        if (VERIFY_INDEX(fields_, arg.field(), "Invalid object field index {}", arg.field()))
            out_arg.field = arg.field();
        if (VERIFY_INDEX(values_, arg.value(), "Invalid object value index: {}", arg.value()))
            out_arg.value = arg.value();
    }
    out.fields = args;
}

void Deserializer::Deserialize(ValueBool& out, const proto::Value::Bool& in)
{
    DeserializeValueCommon(out, in);
    out.value = in.value();
}

void Deserializer::Deserialize(ValueInt& out, const proto::Value::Int& in)
{
    DeserializeValueCommon(out, in);
    out.value = in.value();
}

void Deserializer::Deserialize(ValueFloat& out, const proto::Value::Float& in)
{
    DeserializeValueCommon(out, in);
    out.value = in.value();
}

void Deserializer::Deserialize(ValueString& out, const proto::Value::String& in)
{
    DeserializeValueCommon(out, in);
    out.value = arena_.NewString(in.value());
}

void Deserializer::Deserialize(ValueArray& out, const proto::Value::Array& in)
{
    DeserializeValueCommon(out, in);
    if (VERIFY_INDEX(types_, in.type(), "Invalid array type index {}", in.type()))
    {
        if (VERIFY(types_[in.type()]->kind == TypeKind::Array, "Invalid array type kind"))
            out.type = in.type();
    }
    Array<ValueIndex> elements = arena_.NewArray<ValueIndex>(in.elements_size());
    for (const std::uint32_t valueIndex : in.elements())
    {
        if (VERIFY_INDEX(values_, valueIndex, "Invalid array element value index {}", valueIndex))
            elements.PushBack(arena_, valueIndex);
    }
    out.elements = elements;
}

void Deserializer::Deserialize(ValueEnum& out, const proto::Value::Enum& in)
{
    DeserializeValueCommon(out, in);
    if (!VERIFY_INDEX(types_, in.type(), "Invalid enum value type index {}", in.type()))
        return;

    if (!VERIFY(types_[in.type()]->kind == TypeKind::Enum, "Invalid enum value type kind"))
        return;

    const TypeEnum* const enum_ = static_cast<const TypeEnum*>(types_[in.type()]);

    if (!VERIFY(in.item() < enum_->items.size(), "Invalid enum value item index {}", in.item()))
        return;

    out.item = &enum_->items[in.item()];
}

void Deserializer::Deserialize(ValueType& out, const proto::Value::Type& in)
{
    DeserializeValueCommon(out, in);
    if (!VERIFY(in.type() < types_.Size(), "Invalid type value type index {}", in.type()))
        return;
    out.type = in.type();
}

void Deserializer::Deserialize(ValueNull& out, const proto::Value::Null& in)
{
    DeserializeValueCommon(out, in);
}

void Deserializer::Deserialize(Annotation& out, const proto::Annotation& in)
{
    if (!VERIFY_INDEX(types_, in.attribute(), "Invalid attribute index {}", in.attribute()))
        return;

    if (!VERIFY(types_[in.attribute()]->kind == TypeKind::Attribute, "Invalid attribute kind"))
        return;

    out.attribute = in.attribute();

    Array<Argument> args = arena_.NewArray<Argument>(in.arguments_size());
    for (const proto::Argument& arg : in.arguments())
    {
        Argument& out_arg = args.EmplaceBack(arena_);
        DeserializeLocation(out_arg.location, arg.location());
        if (VERIFY_INDEX(fields_, arg.field(), "Invalid annotation field index: {}", arg.field()))
            out_arg.field = arg.field();
        if (VERIFY_INDEX(values_, arg.value(), "Invalid annotation value index: {}", arg.value()))
            out_arg.value = arg.value();
    }
    out.arguments = args;

    DeserializeLocation(out.location, in.location());
}

template <typename... Args>
void Deserializer::ReportVerifyFailure(fmt::format_string<Args...> format, Args&&... args)
{
    char buffer[512];
    const auto rs = fmt::format_to_n(buffer, sizeof(buffer), format, std::forward<Args>(args)...);
    logger_.Error("<ParseSchemaProto>", {}, std::string_view{ buffer, rs.out });
}
