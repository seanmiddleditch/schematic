// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "generator.h"

#include "ast.h"
#include "lexer.h"
#include "location.h"
#include "parser.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/compiler.h"
#include "schematic/logger.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <fmt/core.h>

#include <charconv>
#include <climits>
#include <cstdint>
#include <utility>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

template <>
struct fmt::formatter<AstIdentifier> : fmt::formatter<const char*>
{
    template <typename FormatContext>
    FMT_CONSTEXPR auto format(const AstIdentifier& ident, FormatContext& ctx) const
        -> decltype(ctx.out())
    {
        fmt::format_to(ctx.out(), ident.name);
        return ctx.out();
    }
};

template <typename... Args>
static const char* NewStringFmt(ArenaAllocator& arena_, fmt::format_string<Args...> format, Args&&... args)
{
    const int length = fmt::formatted_size(format, args...);
    char* const buffer = static_cast<char*>(arena_.Allocate(length + 1, 1));
    fmt::format_to(buffer, format, std::forward<Args>(args)...);
    buffer[length] = '\0';
    return buffer;
}

struct Generator::AnnotationInfo
{
    const AstNodeAnnotation* node = nullptr;
    Annotation* anno = nullptr;
};

struct Generator::EnumItemInfo
{
    const AstNodeEnumItem* node = nullptr;
    TypeInfo* enumInfo = nullptr;
    EnumItem* item = nullptr;
};

struct Generator::FieldInfo
{
    const AstNodeField* node = nullptr;
    TypeInfo* typeInfo = nullptr;
    Field* field = nullptr;
};

struct Generator::StructInfo
{
    const char* name = nullptr;
    Array<TypeStruct*> structs;
};

struct Generator::TypeInfo
{
    const AstNodeDecl* node = nullptr;
    Type* type = nullptr;
    std::int64_t enumItemNext = 0; // for auto-assigning values to enum items
};

void Generator::PassImports()
{
    for (const AstNode* node : ast_->nodes)
    {
        if (const AstNodeImport* const imp = node->CastTo<AstNodeImport>(); imp != nullptr)
        {
            const std::string_view filename = ctx_.ResolveModule(arena_, imp->target->value, module_->filename);
            if (filename.empty())
            {
                Error(imp->tokenIndex, "Module not found: {}", imp->target->value);
                continue;
            }

            // check for a circular dependency
            bool circular = false;
            for (const Module* const mod : state_.stack)
            {
                if (mod->filename == filename)
                {
                    Error(imp->tokenIndex, "Recursive import: {}", imp->target->value);
                    circular = true;
                    break;
                }
            }
            if (circular)
                continue;

            // check if we've already imported this module
            bool exists = false;
            for (const Module* const mod : state_.modules)
            {
                if (mod->filename == filename)
                {
                    exists = true;
                    break;
                }
            }
            if (exists)
                continue;

            const std::string_view source = ctx_.ReadFileContents(arena_, filename);

            Generator gen(arena_, logger_, ctx_, state_);
            const Module* const module = gen.Compile(filename, source);

            if (module != nullptr)
            {
                imports_.PushBack(arena_, module);
                module_->imports = imports_;
            }
        }
    }
}

void Generator::PassBuildTypeInfos()
{
    for (const AstNode* node : ast_->nodes)
    {
        if (node->kind == AstNodeKind::Import)
            continue;

        if (const AstNodeStructDecl* const declNode = node->CastTo<AstNodeStructDecl>())
        {
            if (declNode->minVersion == nullptr)
            {
                auto [type, info] = BuildTypeInfo<TypeStruct>(declNode);
                if (type == nullptr)
                    continue;

                BuildFieldInfos(type, info, declNode->fields);
                BuildAnnotationInfos(type->annotations, declNode->annotations);
                continue;
            }

            if (declNode->minVersion != nullptr)
            {
                if (IsReserved(declNode->name.name))
                    Error(declNode->tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", declNode->name.name);

                if (const Type* const previous = FindType(module_, declNode->name.name); previous != nullptr)
                {
                    Error(node->tokenIndex, "Type already defined: {}", declNode->name.name);
                    continue;
                }

                StructInfo* structInfo = nullptr;
                for (StructInfo* const info : structInfos_)
                {
                    if (std::strcmp(info->name, declNode->name.name) == 0)
                    {
                        structInfo = info;
                        break;
                    }
                }
                if (structInfo == nullptr)
                {
                    structInfo = structInfos_.EmplaceBack(arena_, arena_.New<StructInfo>());
                    structInfo->name = declNode->name.name;
                }

                const std::int64_t minVersion = declNode->minVersion->value;
                const std::int64_t maxVersion = declNode->maxVersion != nullptr ? declNode->maxVersion->value : minVersion;

                if (minVersion < 1)
                {
                    Error(declNode->minVersion->tokenIndex, "Start version must be positive: {}#{}", declNode->name.name, minVersion);
                    continue;
                }
                if (maxVersion < minVersion)
                {
                    Error(declNode->maxVersion->tokenIndex, "End version of range is less than start version: {}#{}..{}", declNode->name.name, minVersion, maxVersion);
                    continue;
                }

                for (std::int64_t version = minVersion; version <= maxVersion; ++version)
                {
                    const char* const name = NewStringFmt(arena_, "{}#{}", declNode->name.name, version);

                    if (const Type* const previous = FindType(module_, name); previous != nullptr)
                    {
                        Error(declNode->tokenIndex, "Struct version already defined: {}#{}", declNode->name.name, version);
                        continue;
                    }

                    TypeStruct* type = CreateType<TypeStruct>(declNode->tokenIndex, name);
                    if (type == nullptr)
                        continue;
                    type->version = static_cast<std::uint32_t>(version);

                    TypeInfo* const info = typeInfos_.EmplaceBack(arena_, arena_.New<TypeInfo>());
                    info->node = declNode;
                    info->type = type;

                    BuildFieldInfos(type, info, declNode->fields, version);
                    BuildAnnotationInfos(type->annotations, declNode->annotations);

                    for (std::size_t i = 0; i != structInfo->structs.Size(); ++i)
                    {
                        if (type->version > structInfo->structs[i]->version)
                            std::swap(type, structInfo->structs[i]);
                    }
                    structInfo->structs.PushBack(arena_, type);
                }
            }

            continue;
        }

        if (const AstNodeMessageDecl* const declNode = node->CastTo<AstNodeMessageDecl>())
        {
            auto [type, info] = BuildTypeInfo<TypeMessage>(declNode);
            if (type == nullptr)
                continue;

            BuildFieldInfos(type, info, declNode->fields);
            BuildAnnotationInfos(type->annotations, declNode->annotations);
            continue;
        }

        if (const AstNodeAttributeDecl* const declNode = node->CastTo<AstNodeAttributeDecl>())
        {
            auto [type, info] = BuildTypeInfo<TypeAttribute>(declNode);
            if (type == nullptr)
                continue;

            BuildFieldInfos(type, info, declNode->fields);
            BuildAnnotationInfos(type->annotations, declNode->annotations);
            continue;
        }

        if (const AstNodeEnumDecl* const decl = node->CastTo<AstNodeEnumDecl>())
        {
            auto [type, info] = BuildTypeInfo<TypeEnum>(decl);

            // Create the EnumItem entries so item lookups will work during value resolution
            Array<EnumItem> items = arena_.NewArray<EnumItem>(decl->items.Size());
            for (const AstNodeEnumItem* itemNode : decl->items)
            {
                EnumItem& item = items.EmplaceBack(arena_);
                item.name = itemNode->name.name;
                item.owner = type;
                item.line = TokenLine(itemNode->tokenIndex);

                EnumItemInfo* const itemInfo = enumItemInfos_.EmplaceBack(arena_, arena_.New<EnumItemInfo>());
                itemInfo->node = itemNode;
                itemInfo->enumInfo = info;
                itemInfo->item = &item;

                BuildAnnotationInfos(item.annotations, itemNode->annotations);
            }
            type->items = items;

            BuildAnnotationInfos(type->annotations, decl->annotations);
            continue;
        }

        Error(node->tokenIndex, "Internal error: unexpected top-level node kind {}", std::to_underlying(node->kind));
    }
}

void Generator::PassStructAliases()
{
    for (const StructInfo* const info : structInfos_)
    {
        TypeAlias* const alias = CreateType<TypeAlias>(0, info->name);
        if (alias != nullptr)
            alias->type = info->structs.Front();
    }
}

void Generator::PassResolveBaseTypes()
{
    for (const TypeInfo* const info : typeInfos_)
    {
        if (const AstNodeStructDecl* node = info->node->CastTo<AstNodeStructDecl>(); node != nullptr)
        {
            const Type* const base = Resolve(node->base);
            if (base == nullptr)
                continue;

            if (base->kind != TypeKind::Struct)
            {
                Error(node->base.tokenIndex, "Struct base type is not a struct: {} : {}", info->type->name, base->name);
                continue;
            }

            static_cast<TypeStruct*>(info->type)->base = static_cast<const TypeStruct*>(base);
            continue;
        }

        if (const AstNodeEnumDecl* node = info->node->CastTo<AstNodeEnumDecl>(); node != nullptr)
        {
            const Type* const base = Resolve(node->base);
            if (base == nullptr)
                continue;

            if (base->kind != TypeKind::Int)
            {
                Error(node->base.tokenIndex, "Enum base type is not an integer type: {} : {}", info->type->name, base->name);
                continue;
            }

            static_cast<TypeEnum*>(info->type)->base = static_cast<const TypeInt*>(base);
            continue;
        }
    }
}

void Generator::PassResolveFieldTypes()
{
    for (const FieldInfo* const info : fieldInfos_)
    {
        if (auto* found = FindField(info->field->owner, info->field->name); found != info->field)
            Error(info->node->tokenIndex, "Duplicate field: {}.{}", info->field->owner->name, info->field->name);

        if (IsReserved(info->field->name))
            Error(info->node->tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}.{}", info->field->owner->name, info->field->name);

        info->field->type = Resolve(info->node->type);

        if (info->node->proto != nullptr)
            info->field->proto = info->node->proto->value;
        if (info->node->value != nullptr)
            info->field->value = BuildExpression(info->field->type, *info->node->value);
    }
}

void Generator::PassResolveAnnotations()
{
    for (const AnnotationInfo* const info : annotationItemInfos_)
    {
        const Type* const type = Resolve(info->node->name);
        if (type == nullptr)
            continue;

        const TypeAttribute* const attribute = CastTo<TypeAttribute>(type);
        if (attribute == nullptr)
        {
            Error(info->node->tokenIndex, "Not an attribute: {}", type->name);
            continue;
        }

        info->anno->attribute = attribute;

        BuildArguments(info->anno->arguments, attribute, attribute->fields, nullptr, info->node->arguments);
    }
}

void Generator::PassAssignEnumItemValues()
{
    for (const EnumItemInfo* const info : enumItemInfos_)
    {
        if (auto* found = FindItem(info->item->owner, info->item->name); found != info->item)
            Error(info->node->tokenIndex, "Duplicate item: {}.{}", info->item->owner->name, info->item->name);

        if (IsReserved(info->item->name))
            Error(info->node->tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}.{}", info->item->owner->name, info->item->name);

        if (info->node->value != nullptr)
        {
            info->item->value = BuildLiteral<ValueInt>(static_cast<const AstNodeLiteralInt&>(*info->node->value));
            info->enumInfo->enumItemNext = info->item->value->value + 1;
        }
        else
        {
            ValueInt* const value = arena_.New<ValueInt>();
            value->value = info->enumInfo->enumItemNext++;
            info->item->value = value;
        }
    }
}

const Module* Generator::Compile(std::string_view filename, std::string_view source)
{
    Lexer lexer(arena_, logger_, filename, source);
    tokens_ = lexer.Tokenize();
    if (tokens_.IsEmpty())
        return nullptr;

    Parser parser(arena_, logger_, filename, source, tokens_);
    ast_ = parser.Parse();
    if (ast_ == nullptr)
        return nullptr;

    if (state_.builtins == nullptr)
        CreateBuiltins();

    module_ = arena_.New<Module>();
    module_->filename = arena_.NewString(filename);
    source_ = arena_.NewString(source);

    state_.stack.PushBack(arena_, module_);

    imports_.EmplaceBack(arena_, state_.builtins);
    module_->imports = imports_;

    // Handle all imports before any local declarations, so we ensure that
    // imported types are available independent of order
    PassImports();

    // Instantiate all type declarations
    PassBuildTypeInfos();

    // Create aliases for the newest version of multi-version structs
    PassStructAliases();

    // Resolve struct and enum base types now that all types are created
    PassResolveBaseTypes();

    // Fully build out fields now that all types have been created and initialized
    PassResolveFieldTypes();

    // Build out annotations now that attribute fields are complete
    PassResolveAnnotations();

    // Fully build enum items now that attribute types are built
    PassAssignEnumItemValues();

    state_.stack.PopBack();

    if (failed_)
        return nullptr;

    state_.modules.PushBack(arena_, module_);
    return module_;
}

template <typename T>
T* CreateBuiltinType(ArenaAllocator& arena_, Array<Type*>& types, Module* module, const char* name)
{
    T* const type = arena_.New<T>();
    type->name = arena_.NewString(name);
    type->owner = module;
    types.PushBack(arena_, type);
    return type;
};

const Module* Generator::CreateBuiltins()
{
    if (state_.builtins != nullptr)
        return state_.builtins;

    Module* const builtins = arena_.New<Module>();
    state_.builtins = builtins;

    builtins->filename = arena_.NewString("$builtins");

    Array<Type*> types;

    auto AddInt = [this, &types, builtins]<typename T>(const char* name, T)
    {
        TypeInt* const type = CreateBuiltinType<TypeInt>(arena_, types, builtins, name);
        type->isSigned = std::is_signed_v<T>;
        type->width = CHAR_BIT * sizeof(T);
    };
    auto AddFloat = [this, &types, builtins]<typename T>(const char* name, T)
    {
        TypeFloat* const type = CreateBuiltinType<TypeFloat>(arena_, types, builtins, name);
        type->width = CHAR_BIT * sizeof(T);
    };

    CreateBuiltinType<TypeType>(arena_, types, builtins, "$type");
    CreateBuiltinType<TypeBool>(arena_, types, builtins, "bool");
    CreateBuiltinType<TypeString>(arena_, types, builtins, "string");

    AddInt("int8", std::int8_t{});
    AddInt("uint8", std::uint8_t{});
    AddInt("int16", std::int16_t{});
    AddInt("uint16", std::uint16_t{});
    AddInt("int32", std::int32_t{});
    AddInt("uint32", std::uint32_t{});
    AddInt("int64", std::int64_t{});
    AddInt("uint64", std::uint64_t{});

    AddFloat("float32", float{});
    AddFloat("float64", double{});

    builtins->types = types;

    return state_.builtins;
}

template <typename T, typename A>
    requires std::is_base_of_v<Type, T> && std::is_base_of_v<AstNodeDecl, A>
Generator::BuildTypeInfoResult<T> Generator::BuildTypeInfo(const A* node)
{
    if (IsReserved(node->name.name))
        Error(node->tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", node->name.name);

    if (const Type* const previous = FindType(module_, node->name.name); previous != nullptr)
    {
        Error(node->tokenIndex, "Type already defined: {}", node->name.name);
        return {};
    }

    T* const type = CreateType<T>(node->tokenIndex, node->name.name);
    if (type == nullptr)
        return {};

    TypeInfo* const info = typeInfos_.EmplaceBack(arena_, arena_.New<TypeInfo>());
    info->node = node;
    info->type = type;

    return BuildTypeInfoResult<T>{ .type = type, .info = info };
}

template <typename T>
    requires std::is_base_of_v<Type, T>
void Generator::BuildFieldInfos(T* type, TypeInfo* info, const std::span<const AstNodeField*> fieldNodes, std::int64_t version)
{
    Array<Field> fields = arena_.NewArray<Field>(fieldNodes.size());
    for (const AstNodeField* fieldNode : fieldNodes)
    {
        if (version != 0)
        {
            if (fieldNode->minVersion != nullptr && fieldNode->minVersion->value > version)
                continue;
            if (fieldNode->maxVersion != nullptr && fieldNode->maxVersion->value < version)
                continue;
        }

        Field& field = fields.EmplaceBack(arena_);
        field.name = fieldNode->name.name;
        field.owner = type;
        field.line = TokenLine(fieldNode->tokenIndex);

        FieldInfo* const itemInfo = fieldInfos_.EmplaceBack(arena_, arena_.New<FieldInfo>());
        itemInfo->node = fieldNode;
        itemInfo->typeInfo = info;
        itemInfo->field = &field;

        BuildAnnotationInfos(field.annotations, fieldNode->annotations);
    }
    type->fields = fields;
}

void Generator::BuildAnnotationInfos(std::span<const Annotation* const>& out, Array<const AstNodeAnnotation*> ast)
{
    Array<Annotation*> annotations;
    for (const AstNodeAnnotation* const node : ast)
    {
        AnnotationInfo* const info = annotationItemInfos_.PushBack(arena_, arena_.New<AnnotationInfo>());
        Annotation* const anno = annotations.PushBack(arena_, arena_.New<Annotation>());
        info->node = node;
        info->anno = anno;
        info->anno->line = TokenLine(node->tokenIndex);
    }
    out = annotations;
}

void Generator::BuildArguments(std::span<const Argument>& out, const Type* type, const std::span<const Field>& fields, const TypeStruct* baseType, Array<const AstNode*> ast)
{
    bool hasNamed = false;
    size_t index = 0;

    bool hasPositionalAfterNamedError = false;
    bool hasPositionalWithBaseError = false;

    Array<Argument> temp;

    for (const AstNode* const elem : ast)
    {
        const AstNodeNamedArgument* const named = elem->CastTo<AstNodeNamedArgument>();

        if (named != nullptr)
        {
            const char* const name = named->name.name;

            hasNamed = true;

            const Field* field = nullptr;
            for (const Field& f : fields)
            {
                if (std::strcmp(f.name, name) == 0)
                {
                    field = &f;
                    break;
                }
            }
            if (field == nullptr && baseType != nullptr)
                field = FindField(baseType, name);

            if (field == nullptr)
            {
                Error(elem->tokenIndex, "Field does not exist on type: {}", name);
                continue;
            }

            bool isDuplicate = false;
            for (const Argument& arg : temp)
            {
                if (arg.field == field)
                {
                    isDuplicate = true;
                    break;
                }
            }
            if (isDuplicate)
            {
                Error(elem->tokenIndex, "Argument already has a value provided: {}", name);
                continue;
            }

            const Value* const value = BuildExpression(field->type, *named->value);

            temp.PushBack(arena_, Argument{ .field = field, .value = value, .line = TokenLine(named->tokenIndex) });
        }
        else if (hasNamed)
        {
            if (!hasPositionalAfterNamedError)
            {
                hasPositionalAfterNamedError = true;
                Error(elem->tokenIndex, "Positional initializers cannot follow named initializers");
            }
        }
        else if (baseType != nullptr)
        {
            if (!hasPositionalWithBaseError)
            {
                hasPositionalWithBaseError = true;
                Error(elem->tokenIndex, "Positional initializers may not be used for types with base types");
            }
            continue;
        }
        else if (index >= fields.size())
        {
            Error(elem->tokenIndex, "Too many initializers");
            break;
        }
        else
        {
            const Field& field = fields[index++];
            const Value* const value = BuildExpression(field.type, *elem);
            temp.PushBack(arena_, Argument{ .field = &field, .value = value, .line = TokenLine(elem->tokenIndex) });
        }
    }

    out = temp;
}

template <typename V, typename A>
    requires std::is_base_of_v<Value, V> && std::is_base_of_v<AstNodeLiteral, A>
const V* Generator::BuildLiteral(const A& node)
{
    V* const value = arena_.New<V>();
    value->value = node.value;
    value->line = TokenLine(node.tokenIndex);
    return value;
}

const Value* Generator::BuildExpression(const Type* type, const AstNode& expr)
{
    switch (expr.kind)
    {
        case AstNodeKind::LiteralBool:
            return BuildLiteral<ValueBool>(static_cast<const AstNodeLiteralBool&>(expr));
        case AstNodeKind::LiteralInt:
            return BuildLiteral<ValueInt>(static_cast<const AstNodeLiteralInt&>(expr));
        case AstNodeKind::LiteralFloat:
            return BuildLiteral<ValueFloat>(static_cast<const AstNodeLiteralFloat&>(expr));
        case AstNodeKind::LiteralNull:
        {
            ValueNull* const value = arena_.New<ValueNull>();
            value->line = TokenLine(expr.tokenIndex);
            return value;
        }
        case AstNodeKind::LiteralString:
            return BuildLiteral<ValueString>(static_cast<const AstNodeLiteralString&>(expr));
        case AstNodeKind::Identifier:
            return BuildIdentValue(type, *expr.CastTo<AstNodeIdentifier>());
        case AstNodeKind::InitializerList:
            if (const TypeStruct* struct_ = CastTo<TypeStruct>(type); struct_ != nullptr)
                return BuildObject(struct_, *expr.CastTo<AstNodeInitializerList>());
            if (const TypeArray* array = CastTo<TypeArray>(type); array != nullptr)
                return BuildArray(array, *expr.CastTo<AstNodeInitializerList>());
            Error(expr.tokenIndex, "Not implemented");
        default:
            Error(expr.tokenIndex, "Unknown expression AST node type: {}", std::to_underlying(expr.kind));
            return nullptr;
    }
}

const Value* Generator::BuildIdentValue(const Type* type, const AstNodeIdentifier& id)
{
    if (const TypeEnum* const enumType = CastTo<TypeEnum>(type); enumType != nullptr)
    {
        const EnumItem* const enumItem = FindItem(enumType, id.name.name);
        if (enumItem == nullptr)
        {
            Error(id.tokenIndex, "No such enumeration item: {}.{}", enumType->name, id.name.name);
            return nullptr;
        }

        ValueEnum* enumValue = arena_.New<ValueEnum>();
        enumValue->item = enumItem;
        enumValue->line = TokenLine(id.tokenIndex);
        return enumValue;
    }

    if (CastTo<TypeType>(type) != nullptr)
    {
        const Type* type = Resolve(id.name);
        if (type == nullptr)
            return nullptr;

        ValueType* value = arena_.New<ValueType>();
        value->type = type;
        value->line = TokenLine(id.tokenIndex);
        return value;
    }

    Error(id.tokenIndex, "Not found: {}", id.name.name);
    return nullptr;
}

const ValueArray* Generator::BuildArray(const Type* type, const AstNodeInitializerList& expr)
{
    ValueArray* const value = arena_.New<ValueArray>();
    value->type = type;
    value->line = TokenLine(expr.tokenIndex);

    Array elements = arena_.NewArray<const Value*>(expr.elements.Size());

    for (const AstNode* const elem : expr.elements)
    {
        const Value* const elemValue = BuildExpression(type, *elem);
        if (elemValue != nullptr)
            elements.PushBack(arena_, elemValue);
    }

    value->elements = elements;

    return value;
}

const ValueObject* Generator::BuildObject(const TypeStruct* type, const AstNodeInitializerList& expr)
{
    ValueObject* const value = arena_.New<ValueObject>();
    value->type = type;
    value->line = TokenLine(expr.tokenIndex);

    if (expr.type.name != nullptr)
    {
        value->type = Resolve(expr.type);
        if (value->type != nullptr && !IsA(value->type, type))
            Error(expr.tokenIndex, "Type is not compatible: {}", value->type->name);
    }

    BuildArguments(value->fields, type, type->fields, type->base, expr.elements);

    return value;
}

const Type* Generator::TryResolve(const char* name)
{
    if (name == nullptr || *name == '\0')
        return nullptr;

    for (const TypeInfo* const info : typeInfos_)
        if (std::strcmp(info->type->name, name) == 0)
            return info->type;

    if (const Type* const type = FindType(module_, name); type != nullptr)
        return type;

    if (state_.builtins != nullptr)
        if (const Type* const type = FindType(state_.builtins, name); type != nullptr)
            return type;

    for (const Module* imp : module_->imports)
        if (const Type* const type = FindType(imp, name); type != nullptr)
            return type;

    return nullptr;
}

const Type* Generator::Resolve(const AstIdentifier& ident)
{
    // FIXME: we shouldn't be relying on this check; optional identifiers should be nodes
    if (ident.name == nullptr)
        return nullptr;

    if (const Type* const type = TryResolve(ident.name); type != nullptr)
        return type;

    Error(ident.tokenIndex, "Not found: {}", ident.name);
    return nullptr;
}

const Type* Generator::Resolve(const AstNodeType* type)
{
    if (const AstNodeTypeName* qual = type->CastTo<AstNodeTypeName>(); qual != nullptr)
    {
        return Resolve(qual->name);
    }

    if (const AstNodeTypeArray* array = type->CastTo<AstNodeTypeArray>(); array != nullptr)
    {
        const Type* inner = Resolve(array->type);
        if (inner == nullptr)
            return nullptr;

        if (array->size == nullptr)
        {
            const char* const name = NewStringFmt(arena_, "{}[]", inner->name);

            if (const Type* const type = TryResolve(name); type != nullptr)
                return type;

            TypeArray* const type = CreateType<TypeArray>(array->tokenIndex, name);
            type->type = inner;
            return type;
        }

        {
            const char* const name = NewStringFmt(arena_, "{}[{}]", inner->name, array->size->value);

            if (const Type* const type = TryResolve(name); type != nullptr)
                return type;

            TypeArray* const type = CreateType<TypeArray>(array->tokenIndex, name);
            type->type = inner;
            type->size = array->size->value;
            return type;
        }
    }

    if (const AstNodeTypePointer* pointer = type->CastTo<AstNodeTypePointer>(); pointer != nullptr)
    {
        const Type* inner = Resolve(pointer->type);
        if (inner == nullptr)
            return nullptr;

        const char* const name = NewStringFmt(arena_, "{}*", inner->name);

        if (const Type* const type = TryResolve(name); type != nullptr)
            return type;

        TypePointer* const type = CreateType<TypePointer>(pointer->tokenIndex, name);
        type->type = inner;

        return type;
    }

    if (const AstNodeTypeNullable* nullable = type->CastTo<AstNodeTypeNullable>(); nullable != nullptr)
    {
        const Type* inner = Resolve(nullable->type);
        if (inner == nullptr)
            return nullptr;

        const char* const name = NewStringFmt(arena_, "{}?", inner->name);

        if (const Type* const type = TryResolve(name); type != nullptr)
            return type;

        TypeNullable* const type = CreateType<TypeNullable>(nullable->tokenIndex, name);
        type->type = inner;

        return type;
    }

    return nullptr;
}

bool Generator::IsReserved(const char* ident) const noexcept
{
    if (ident == nullptr)
        return false;
    if (*ident == '$')
        return true;
    return false;
}

template <typename... Args>
void Generator::Error(std::uint32_t tokenIndex, fmt::format_string<Args...> format, const Args&... args)
{
    failed_ = true;
    if (tokenIndex < tokens_.Size())
    {
        const Token& token = tokens_[tokenIndex];
        logger_.Error(module_->filename, FindRange(source_, token), fmt::vformat(format, fmt::make_format_args(args...)));
    }
    else
    {
        logger_.Error(module_->filename, {}, fmt::vformat(format, fmt::make_format_args(args...)));
    }
}

template <typename T>
    requires std::is_base_of_v<Type, T>
T* Generator::CreateType(std::uint32_t tokenIndex, const char* name)
{
    T* const type = arena_.New<T>();
    type->name = name;
    type->owner = module_;
    type->line = TokenLine(tokenIndex);

    types_.PushBack(arena_, type);
    module_->types = types_;

    return type;
}

std::uint16_t Generator::TokenLine(std::uint32_t tokenIndex) const noexcept
{
    if (tokenIndex >= tokens_.Size())
        return 0;
    return tokens_[tokenIndex].line;
}
