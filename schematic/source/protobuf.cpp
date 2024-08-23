// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/protobuf.h"

#include "array.h"

#include "schematic/schema.h"
#include "schematic/schematic.pb.h"
#include "schematic/utility.h"

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

        void SerializeField(proto::Field& out, const Field& in);

        template <typename T>
        void SerializeTypeCommon(T& out, const Type& in);

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
        explicit Deserializer(const proto::Schema& proto, ArenaAllocator& arena)
            : proto_(proto)
            , arena_(arena)
        {
        }

        void Deserialize(Schema& out);

    private:
        void Deserialize(Type& out, const proto::Type& in);
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

        void DeserializeField(Field& out, const Type* owner, const proto::Field& in);

        template <typename T>
        void DeserializeTypeCommon(Type& out, const T& in);

        Value* Deserialize(const proto::Value& in);
        ValueObject* Deserialize(const proto::Value::Object& in);
        ValueBool* Deserialize(const proto::Value::Bool& in);
        ValueInt* Deserialize(const proto::Value::Int& in);
        ValueFloat* Deserialize(const proto::Value::Float& in);
        ValueString* Deserialize(const proto::Value::String& in);
        ValueArray* Deserialize(const proto::Value::Array& in);
        ValueEnum* Deserialize(const proto::Value::Enum& in);
        ValueType* Deserialize(const proto::Value::Type& in);
        ValueNull* Deserialize(const proto::Value::Null& in);

        void Deserialize(Annotation& out, const proto::Annotation& in);

        const proto::Schema& proto_;
        ArenaAllocator& arena_;
        Array<Module*> modules_;
        Array<Type*> types_;
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

const Schema* potato::schematic::ParseSchemaProto(ArenaAllocator& arena, const proto::Schema* proto)
{
    if (proto == nullptr)
        return nullptr;
    Schema* const schema = arena.New<Schema>();
    Deserializer serializer(*proto, arena);
    serializer.Deserialize(*schema);
    return schema;
}

// --- Serializer ---

void Serializer::Serialize(proto::Schema& out)
{
    for (const Module* const mod : schema_.modules)
    {
        proto::Module* const pmod = out.add_modules();
        pmod->set_filename(mod->filename);
        for (const Module* const import : mod->imports)
            pmod->add_imports(IndexOfModule(import));
    }

    if (schema_.root != nullptr)
        out.set_root(IndexOfModule(schema_.root));

    for (const Type* const type : schema_.types)
        Serialize(*out.add_types(), *type);
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

    if (in.base != nullptr)
        out.set_base(IndexOfType(in.base));

    for (const Field& field : in.fields)
        SerializeField(*out.add_fields(), field);
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

void Serializer::Serialize(proto::Type::Array& out, const TypeArray& in)
{
    SerializeTypeCommon(out, in);

    out.set_element(IndexOfType(in.type));
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
        out.set_base(IndexOfType(in.base));

    for (const EnumItem& item : in.items)
    {
        proto::EnumItem& out_item = *out.add_items();
        out_item.set_name(item.name);
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

void Serializer::Serialize(proto::Type::Nullable& out, const TypeNullable& in)
{
    SerializeTypeCommon(out, in);

    if (in.type != nullptr)
        out.set_type(IndexOfType(in.type));
}

void Serializer::Serialize(proto::Type::Pointer& out, const TypePointer& in)
{
    SerializeTypeCommon(out, in);

    if (in.type != nullptr)
        out.set_type(IndexOfType(in.type));
}

void Serializer::Serialize(proto::Type::Message& out, const TypeMessage& in)
{
    SerializeTypeCommon(out, in);

    for (const Field& field : in.fields)
        SerializeField(*out.add_fields(), field);
}

void Serializer::Serialize(proto::Type::Attribute& out, const TypeAttribute& in)
{
    SerializeTypeCommon(out, in);

    for (const Field& field : in.fields)
        SerializeField(*out.add_fields(), field);
}

void Serializer::SerializeField(proto::Field& out, const Field& in)
{
    out.set_name(in.name);
    out.set_type(IndexOfType(in.type));

    if (in.proto != 0)
        out.set_proto(in.proto);

    if (in.value != nullptr)
        Serialize(*out.mutable_default_(), *in.value);

    for (const Annotation* const annotation : in.annotations)
        Serialize(*out.add_annotations(), *annotation);
}

template <typename T>
void Serializer::SerializeTypeCommon(T& out, const Type& in)
{
    out.set_name(in.name);
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
        out_arg.set_field(arg.field->name);

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
    out.set_value(in.value);
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
    out.set_attribute(IndexOfType(in.attribute));

    for (const Argument& arg : in.arguments)
    {
        proto::Argument& out_arg = *out.add_arguments();
        out_arg.set_field(arg.field->name);

        if (arg.value != nullptr)
            Serialize(*out_arg.mutable_value(), *arg.value);
    }
}

// --- Deserializer ---

void Deserializer::Deserialize(Schema& out)
{
    // Deserialize modules, excluding their child types (we haven't deserialized those yet)
    modules_ = arena_.NewArray<Module*>(proto_.modules_size());
    for (std::size_t index = 0; index != modules_.Capacity(); ++index)
        modules_.PushBack(arena_, arena_.New<Module>());

    const std::size_t root_index = proto_.root();
    if (root_index < modules_.Size())
        out.root = modules_[root_index];

    std::size_t module_index = 0;
    for (const proto::Module& mod : proto_.modules())
    {
        Module& out_mod = *modules_[module_index++];
        out_mod.filename = arena_.NewString(mod.filename());

        Array<const Module*> imports = arena_.NewArray<const Module*>(mod.imports_size());
        for (std::uint32_t target_index : mod.imports())
        {
            if (target_index < modules_.Size())
                imports.EmplaceBack(arena_, modules_[target_index]);
        }
        out_mod.imports = imports;
    }
    out.modules = modules_;

    // Instantiate types
    types_ = arena_.NewArray<Type*>(proto_.types_size());

    for (const proto::Type& type : proto_.types())
    {
        switch (type.Types_case())
        {
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
        return;
    }
    out.types = types_;

    // Deserialize types
    std::size_t type_index = 0;
    for (const proto::Type& type : proto_.types())
    {
        potato::schematic::Type* const out_type = types_[type_index++];
        if (out_type != nullptr)
            Deserialize(*out_type, type);
    }

    // Link types to modules (could be way less... bad)
    for (Module* const mod : modules_)
    {
        std::size_t type_count = 0;
        for (const Type* const type : types_)
        {
            if (type->owner == mod)
                ++type_count;
        }

        Array<const Type*> mod_types = arena_.NewArray<const Type*>(type_count);
        for (const Type* const type : types_)
        {
            if (type->owner == mod)
                mod_types.PushBack(arena_, type);
        }
        mod->types = mod_types;
    }
}

void Deserializer::Deserialize(Type& out, const proto::Type& in)
{
    switch (out.kind)
    {
        case TypeKind::Struct:
            Deserialize(static_cast<TypeStruct&>(out), in.struct_());
            break;
        case TypeKind::Array:
            Deserialize(static_cast<TypeArray&>(out), in.array());
            break;
        case TypeKind::Attribute:
            Deserialize(static_cast<TypeAttribute&>(out), in.attribute());
            break;
        case TypeKind::Bool:
            Deserialize(static_cast<TypeBool&>(out), in.bool_());
            break;
        case TypeKind::Enum:
            Deserialize(static_cast<TypeEnum&>(out), in.enum_());
            break;
        case TypeKind::Float:
            Deserialize(static_cast<TypeFloat&>(out), in.float_());
            break;
        case TypeKind::Int:
            Deserialize(static_cast<TypeInt&>(out), in.int_());
            break;
        case TypeKind::Message:
            Deserialize(static_cast<TypeMessage&>(out), in.message());
            break;
        case TypeKind::Nullable:
            Deserialize(static_cast<TypeNullable&>(out), in.nullable());
            break;
        case TypeKind::Pointer:
            Deserialize(static_cast<TypePointer&>(out), in.pointer());
            break;
        case TypeKind::String:
            Deserialize(static_cast<TypeString&>(out), in.string());
            break;
        case TypeKind::Type:
            Deserialize(static_cast<TypeType&>(out), in.type());
            break;
    }
}

void Deserializer::Deserialize(TypeStruct& out, const proto::Type::Struct& in)
{
    DeserializeTypeCommon(out, in);

    if (in.has_base())
    {
        if (in.base() < types_.Size())
        {
            const Type* const base = types_[in.base()];
            if (base->kind == TypeKind::Struct)
                out.base = static_cast<const TypeStruct*>(base);
        }
    }

    Array<Field> fields = arena_.NewArray<Field>(in.fields_size());
    for (const proto::Field& field : in.fields())
        DeserializeField(fields.EmplaceBack(arena_), &out, field);
    out.fields = fields;
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
    {
        out.isFixed = true;
        out.size = in.size();
    }

    if (in.element() < types_.Size())
        out.type = types_[in.element()];
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
        if (in.base() < types_.Size())
        {
            const Type* const base = types_[in.base()];
            if (base->kind == TypeKind::Int)
                out.base = base;
        }
    }

    Array<EnumItem> items = arena_.NewArray<EnumItem>(in.items_size());
    for (const proto::EnumItem& item : in.items())
    {
        EnumItem& out_item = items.EmplaceBack(arena_);
        out_item.name = arena_.NewString(item.name());
        out_item.owner = &out;

        ValueInt* const value = arena_.New<ValueInt>();
        value->value = item.value();
        out_item.value = value;
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

    Array<Field> fields = arena_.NewArray<Field>(in.fields_size());
    for (const proto::Field& field : in.fields())
        DeserializeField(fields.EmplaceBack(arena_), &out, field);
    out.fields = fields;
}

void Deserializer::Deserialize(TypeNullable& out, const proto::Type::Nullable& in)
{
    DeserializeTypeCommon(out, in);

    if (in.type() < types_.Size())
        out.type = types_[in.type()];
}

void Deserializer::Deserialize(TypePointer& out, const proto::Type::Pointer& in)
{
    DeserializeTypeCommon(out, in);

    if (in.type() < types_.Size())
        out.type = types_[in.type()];
}

void Deserializer::Deserialize(TypeAttribute& out, const proto::Type::Attribute& in)
{
    DeserializeTypeCommon(out, in);

    Array<Field> fields = arena_.NewArray<Field>(in.fields_size());
    for (const proto::Field& field : in.fields())
        DeserializeField(fields.EmplaceBack(arena_), &out, field);
    out.fields = fields;
}

void Deserializer::DeserializeField(Field& out, const Type* owner, const proto::Field& in)
{
    out.name = arena_.NewString(in.name());
    out.owner = owner;

    if (in.has_proto())
        out.proto = in.proto();

    if (in.type() < types_.Size())
        out.type = types_[in.type()];

    if (in.has_default_())
        out.value = Deserialize(in.default_());
}

template <typename T>
void Deserializer::DeserializeTypeCommon(Type& out, const T& in)
{
    out.name = arena_.NewString(in.name());
    if (in.module() < modules_.Size())
        out.owner = modules_[in.module()];

    Array<Annotation*> annotations = arena_.NewArray<Annotation*>(in.annotations_size());
    for (const proto::Annotation& anno : in.annotations())
    {
        Annotation* const out_anno = arena_.New<Annotation>();
        Deserialize(*out_anno, anno);
        annotations.PushBack(arena_, out_anno);
    }
    out.annotations = annotations;
}

Value* Deserializer::Deserialize(const proto::Value& in)
{
    switch (in.Values_case())
    {
        case proto::Value::kNull: return Deserialize(in.null());
        case proto::Value::kBool: return Deserialize(in.bool_());
        case proto::Value::kInt: return Deserialize(in.int_());
        case proto::Value::kFloat: return Deserialize(in.float_());
        case proto::Value::kString: return Deserialize(in.string());
        case proto::Value::kEnum: return Deserialize(in.enum_());
        case proto::Value::kArray: return Deserialize(in.array());
        case proto::Value::kObject: return Deserialize(in.object());
    }
    return nullptr;
}

ValueObject* Deserializer::Deserialize(const proto::Value::Object& in)
{
    ValueObject* const value = arena_.New<ValueObject>();
    return value;
}

ValueBool* Deserializer::Deserialize(const proto::Value::Bool& in)
{
    ValueBool* const value = arena_.New<ValueBool>();
    value->value = in.value();
    return value;
}

ValueInt* Deserializer::Deserialize(const proto::Value::Int& in)
{
    ValueInt* const value = arena_.New<ValueInt>();
    value->value = in.value();
    return value;
}

ValueFloat* Deserializer::Deserialize(const proto::Value::Float& in)
{
    ValueFloat* const value = arena_.New<ValueFloat>();
    value->value = in.value();
    return value;
}

ValueString* Deserializer::Deserialize(const proto::Value::String& in)
{
    ValueString* const value = arena_.New<ValueString>();
    value->value = arena_.NewString(in.value());
    return value;
}

ValueArray* Deserializer::Deserialize(const proto::Value::Array& in)
{
    ValueArray* const value = arena_.New<ValueArray>();
    Array<Value*> elements = arena_.NewArray<Value*>(in.elements_size());
    for (const proto::Value& element : in.elements())
        elements.PushBack(arena_, Deserialize(element));
    value->elements = elements;
    return value;
}

ValueEnum* Deserializer::Deserialize(const proto::Value::Enum& in)
{
    ValueEnum* const value = arena_.New<ValueEnum>();
    if (in.type() < types_.Size() && types_[in.type()]->kind == TypeKind::Enum)
    {
        const TypeEnum* const enum_ = static_cast<const TypeEnum*>(types_[in.type()]);
        if (in.item() < enum_->items.size())
            value->item = &enum_->items[in.item()];
    }
    return value;
}

ValueType* Deserializer::Deserialize(const proto::Value::Type& in)
{
    return arena_.New<ValueType>();
}

ValueNull* Deserializer::Deserialize(const proto::Value::Null& in)
{
    return arena_.New<ValueNull>();
}

void Deserializer::Deserialize(Annotation& out, const proto::Annotation& in)
{
    if (in.attribute() < types_.Size())
        if (types_[in.attribute()]->kind == TypeKind::Attribute)
            out.attribute = static_cast<const TypeAttribute*>(types_[in.attribute()]);

    Array<Argument> args = arena_.NewArray<Argument>(in.arguments_size());
    for (const proto::Argument& arg : in.arguments())
    {
        Argument& out_arg = args.EmplaceBack(arena_);
        out_arg.field = FindField(out.attribute, arg.field());
        if (arg.has_value())
            out_arg.value = Deserialize(arg.value());
    }
    out.arguments = args;
}
