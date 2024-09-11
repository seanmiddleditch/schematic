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
struct fmt::formatter<AstQualifiedName> : fmt::formatter<const char*>
{
    template <typename FormatContext>
    FMT_CONSTEXPR auto format(const AstQualifiedName& name, FormatContext& ctx) const
        -> decltype(ctx.out())
    {
        bool first = true;
        for (const auto& part : name.parts)
        {
            if (!first)
                fmt::format_to(ctx.out(), ".");
            first = false;
            fmt::format_to(ctx.out(), "{}", part.name);
        }
        return ctx.out();
    }
};

struct Generator::State
{
    struct TypeDecl
    {
        const AstNodeDecl* adecl = nullptr;
        Type* type = nullptr;
    };

    const AstNodeModule* ast = nullptr;
    Module* mod = nullptr;
    std::string_view source;
    Array<const Module*> imports;
    Array<TypeDecl> decls;
    Array<const Type*> types;
    Array<Token> tokens;
};

const Module* Generator::CompileModule()
{
    State& state = *stack.Back();

    Lexer lexer(arena, logger_, state.mod->filename, state.source);
    state.tokens = lexer.Tokenize();
    if (state.tokens.IsEmpty())
        return nullptr;

    Parser parser(arena, logger_, state.mod->filename, state.source, state.tokens);
    state.ast = parser.Parse();

    if (state.ast == nullptr)
        return nullptr;

    // Handle all imports before any local declarations, so we ensure that
    // imported types are available independent of order
    for (const AstNode* node : state.ast->nodes)
    {
        if (const AstNodeImport* const imp = node->CastTo<AstNodeImport>())
        {
            if (!HandleImport(*imp))
                return nullptr;
        }
    }

    // Instantiate types, so that we can lookup a type by name (and grab a
    // pointer) early
    for (const AstNode* node : state.ast->nodes)
    {
        if (node->kind == AstNodeKind::Import)
            continue;

        State::TypeDecl& typeDecl = state.decls.EmplaceBack(arena);
        typeDecl.adecl = static_cast<const AstNodeDecl*>(node);

        if (const AstNodeStructDecl* const struct_ = node->CastTo<AstNodeStructDecl>())
            typeDecl.type = CreateType<TypeStruct>(struct_->tokenIndex, struct_->name.name);
        else if (const AstNodeMessageDecl* const message = node->CastTo<AstNodeMessageDecl>())
            typeDecl.type = CreateType<TypeMessage>(message->tokenIndex, message->name.name);
        else if (const AstNodeAttributeDecl* const attr = node->CastTo<AstNodeAttributeDecl>())
            typeDecl.type = CreateType<TypeAttribute>(attr->tokenIndex, attr->name.name);
        else if (const AstNodeEnumDecl* const enum_ = node->CastTo<AstNodeEnumDecl>())
            typeDecl.type = CreateType<TypeEnum>(enum_->tokenIndex, enum_->name.name);
        else
            Error(node->tokenIndex, "Internal error: unexpected top-level node kind {}", std::to_underlying(node->kind));
    }

    // Fully build types (including fields, bases, values, etc.) now that we have instantiations
    // of all types
    for (State::TypeDecl& typeDecl : state.decls)
    {
        // skips types we couldn't instantiate; an error has already been raised for them
        if (typeDecl.type == nullptr)
            continue;

        switch (typeDecl.type->kind)
        {
            case TypeKind::Struct:
                BuildStruct(*static_cast<TypeStruct*>(typeDecl.type), *static_cast<const AstNodeStructDecl*>(typeDecl.adecl));
                break;
            case TypeKind::Message:
                BuildMessage(*static_cast<TypeMessage*>(typeDecl.type), *static_cast<const AstNodeMessageDecl*>(typeDecl.adecl));
                break;
            case TypeKind::Attribute:
                BuildAttribute(*static_cast<TypeAttribute*>(typeDecl.type), *static_cast<const AstNodeAttributeDecl*>(typeDecl.adecl));
                break;
            case TypeKind::Enum:
                BuildEnum(*static_cast<TypeEnum*>(typeDecl.type), *static_cast<const AstNodeEnumDecl*>(typeDecl.adecl));
                break;
            default:
                Error(typeDecl.adecl->tokenIndex, "Internal error: unexpected type kind: {}, {}", typeDecl.type->name, std::to_underlying(typeDecl.type->kind));
        }
    }

    if (!result)
        return nullptr;

    return state.mod;
}

const Module* Generator::Compile(std::string_view filename, std::string_view source, bool useBuiltins)
{
    builtins = nullptr;
    schema = nullptr;
    result = true;
    stack = Array<State*>{};

    if (filename.empty())
        return nullptr;

    if (useBuiltins && builtins == nullptr)
        CreateBuiltins();

    State* const state = arena.New<State>();
    stack.PushBack(arena, state);

    state->mod = arena.New<Module>();
    state->mod->filename = arena.NewString(filename);
    state->source = arena.NewString(source);

    modules.PushBack(arena, state->mod);

    if (useBuiltins)
    {
        state->imports.EmplaceBack(arena, builtins);
        state->mod->imports = state->imports;
    }

    if (!CompileModule())
        return nullptr;

    stack.PopBack();
    assert(stack.IsEmpty());

    return state->mod;
}

bool Generator::HandleImport(const AstNodeImport& imp)
{
    const std::string_view filename = ctx.ResolveModule(arena, imp.target->value, stack.Back()->mod->filename);
    if (filename.empty())
    {
        Error(imp.tokenIndex, "Module not found: {}", imp.target->value);
        return false;
    }

    // check for a circular dependency
    for (const State* const state : stack)
    {
        if (state->mod->filename == filename)
        {
            Error(imp.tokenIndex, "Recursive import: {}", imp.target->value);
            return false;
        }
    }

    // check if we've already imported this module
    for (const Module* const mod : modules)
    {
        if (mod->filename == filename)
            return true;
    }

    State* const state = arena.New<State>();
    state->mod = arena.New<Module>();
    state->mod->filename = arena.NewString(filename);
    state->source = arena.NewString(ctx.ReadFileContents(arena, state->mod->filename));

    modules.PushBack(arena, state->mod);

    stack.PushBack(arena, state);
    const bool success = CompileModule() != nullptr;
    stack.PopBack();

    if (success)
    {
        State* const parent = stack.Back();
        parent->imports.PushBack(arena, state->mod);
        parent->mod->imports = parent->imports;
    }

    return success;
}

const Module* Generator::CreateBuiltins()
{
    if (builtins != nullptr)
        return builtins;

    Module* const mod = arena.New<Module>();
    builtins = mod;

    mod->filename = arena.NewString("$builtins");

    State state{ .mod = mod };
    stack.PushBack(arena, &state);

    auto AddInt = [this]<typename T>(const char* name, T)
    {
        TypeInt* const type = CreateType<TypeInt>(0, arena.NewString(name));
        type->isSigned = std::is_signed_v<T>;
        type->width = CHAR_BIT * sizeof(T);
    };
    auto AddFloat = [this]<typename T>(const char* name, T)
    {
        TypeFloat* const type = CreateType<TypeFloat>(0, arena.NewString(name));
        type->width = CHAR_BIT * sizeof(T);
    };

    CreateType<TypeType>(0, arena.NewString("$type"));
    CreateType<TypeBool>(0, arena.NewString("bool"));
    CreateType<TypeString>(0, arena.NewString("string"));

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

    stack.PopBack();
    return builtins;
}

void Generator::BuildStruct(TypeStruct& type, const AstNodeStructDecl& ast)
{
    if (IsReserved(type.name))
        Error(ast.name.tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", type.name);

    const Type* const baseType = Resolve(ast.base);
    if (baseType != nullptr)
    {
        type.base = CastTo<TypeStruct>(baseType);
        if (type.base == nullptr)
            Error(ast.base.parts.Front().tokenIndex, "Base type is not a struct: {}", baseType->name);
    }

    BuildAnnotations(type.annotations, ast.annotations);
    BuildFields(type.fields, &type, ast.fields);
}

void Generator::BuildMessage(TypeMessage& type, const AstNodeMessageDecl& ast)
{
    if (IsReserved(type.name))
        Error(ast.name.tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", type.name);

    BuildAnnotations(type.annotations, ast.annotations);
    BuildFields(type.fields, &type, ast.fields);
}

void Generator::BuildAttribute(TypeAttribute& type, const AstNodeAttributeDecl& ast)
{
    if (IsReserved(type.name))
        Error(ast.name.tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", type.name);

    BuildAnnotations(type.annotations, ast.annotations);
    BuildFields(type.fields, &type, ast.fields);
}

void Generator::BuildEnum(TypeEnum& type, const AstNodeEnumDecl& ast)
{
    if (IsReserved(type.name))
        Error(ast.name.tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", type.name);

    type.base = Resolve(ast.base);
    if (type.base != nullptr && type.base->kind != TypeKind::Int)
        Error(ast.base.parts.Front().tokenIndex, "Base type is not an integer: {}", type.base->name);

    BuildAnnotations(type.annotations, ast.annotations);

    std::int64_t next = 0;
    Array<EnumItem> items = arena.NewArray<EnumItem>(ast.items.Size());

    auto HasItem = [&items](const std::string_view name) -> bool
    {
        for (const EnumItem& item : items)
            if (item.name == name)
                return true;
        return false;
    };

    for (const AstNodeEnumItem* ast_item : ast.items)
    {
        if (HasItem(ast_item->name.name))
            Error(ast_item->tokenIndex, "Duplicate item name: {}.{}", type.name, ast_item->name.name);

        EnumItem& item = items.EmplaceBack(arena);
        item.owner = &type;
        item.name = ast_item->name.name;
        item.line = TokenLine(ast_item->tokenIndex);
        if (IsReserved(item.name))
            Error(ast_item->name.tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", item.name);
        if (ast_item->value != nullptr)
        {
            item.value = BuildInteger(*ast_item->value);
            next = item.value->value + 1;
        }
        else
        {
            ValueInt* const value = arena.New<ValueInt>();
            value->value = next++;
            item.value = value;
        }
        BuildAnnotations(item.annotations, ast_item->annotations);
    }
    type.items = items;
}

void Generator::BuildFields(std::span<const Field>& out, const Type* owner, Array<const AstNodeField*> fields)
{
    Array<Field> temp = arena.NewArray<Field>(fields.Size());

    auto HasField = [&temp](const std::string_view name) -> bool
    {
        for (const Field& field : temp)
            if (field.name == name)
                return true;
        return false;
    };

    for (const AstNodeField* ast_field : fields)
    {
        if (HasField(ast_field->name.name))
            Error(ast_field->tokenIndex, "Duplicate field name: {}.{}", owner->name, ast_field->name.name);

        Field& field = temp.EmplaceBack(arena);
        field.owner = owner;
        field.name = ast_field->name.name;
        field.type = Resolve(ast_field->type);
        field.line = TokenLine(ast_field->tokenIndex);
        if (IsReserved(field.name))
            Error(ast_field->tokenIndex, "Reserved identifier ($) is not allowed in declarations: {}", field.name);
        if (ast_field->proto != nullptr)
            field.proto = ast_field->proto->value;
        if (ast_field->value != nullptr)
            field.value = BuildExpression(field.type, *ast_field->value);
        BuildAnnotations(field.annotations, ast_field->annotations);
    }
    out = temp;
}

void Generator::BuildAnnotations(std::span<const Annotation* const>& out, Array<const AstNodeAnnotation*> ast)
{
    Array<const Annotation*> temp = arena.NewArray<const Annotation*>(ast.Size());

    for (const AstNodeAnnotation* const astAttr : ast)
    {
        const Type* const type = Resolve(astAttr->name);
        if (type == nullptr)
            continue;

        const TypeAttribute* const attrType = CastTo<TypeAttribute>(type);
        if (attrType == nullptr)
        {
            Error(astAttr->tokenIndex, "Not an attribute: {}", type->name);
            continue;
        }

        Annotation* const annotation = arena.New<Annotation>();
        temp.PushBack(arena, annotation);

        annotation->attribute = attrType;
        annotation->line = astAttr->tokenIndex;

        BuildArguments(annotation->arguments, attrType, attrType->fields, nullptr, astAttr->arguments);
    }

    out = temp;
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

            temp.PushBack(arena, Argument{ .field = field, .value = value, .line = TokenLine(named->tokenIndex) });
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
            temp.PushBack(arena, Argument{ .field = &field, .value = value, .line = TokenLine(elem->tokenIndex) });
        }
    }

    out = temp;
}

const ValueBool* Generator::BuildBool(const AstNodeLiteralBool& lit)
{
    ValueBool* const value = arena.New<ValueBool>();
    value->value = lit.value;
    value->line = TokenLine(lit.tokenIndex);
    return value;
}

const ValueInt* Generator::BuildInteger(const AstNodeLiteralInt& lit)
{
    ValueInt* const value = arena.New<ValueInt>();
    value->value = lit.value;
    value->line = TokenLine(lit.tokenIndex);
    return value;
}

const ValueFloat* Generator::BuildFloat(const AstNodeLiteralFloat& lit)
{
    ValueFloat* const value = arena.New<ValueFloat>();
    value->value = lit.value;
    value->line = TokenLine(lit.tokenIndex);
    return value;
}

const ValueNull* Generator::BuildNull(const AstNodeLiteralNull& lit)
{
    ValueNull* const value = arena.New<ValueNull>();
    value->line = TokenLine(lit.tokenIndex);
    return value;
}

const ValueString* Generator::BuildString(const AstNodeLiteralString& lit)
{
    ValueString* const value = arena.New<ValueString>();
    value->value = lit.value;
    value->line = TokenLine(lit.tokenIndex);
    return value;
}

const Value* Generator::BuildExpression(const Type* type, const AstNode& expr)
{
    switch (expr.kind)
    {
        case AstNodeKind::LiteralBool:
            return BuildBool(*expr.CastTo<AstNodeLiteralBool>());
        case AstNodeKind::LiteralInt:
            return BuildInteger(*expr.CastTo<AstNodeLiteralInt>());
        case AstNodeKind::LiteralFloat:
            return BuildFloat(*expr.CastTo<AstNodeLiteralFloat>());
        case AstNodeKind::LiteralNull:
            return BuildNull(*expr.CastTo<AstNodeLiteralNull>());
        case AstNodeKind::LiteralString:
            return BuildString(*expr.CastTo<AstNodeLiteralString>());
        case AstNodeKind::QualifiedId:
            return BuildQualifiedId(*expr.CastTo<AstNodeQualifiedId>());
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

const Value* Generator::BuildQualifiedId(const AstNodeQualifiedId& id)
{
    if (id.id.parts.Size() == 1)
    {
        const Type* type = Resolve(id.id);
        if (type != nullptr)
        {
            ValueType* value = arena.New<ValueType>();
            value->type = type;
            value->line = TokenLine(id.tokenIndex);
            return value;
        }
    }

    if (id.id.parts.Size() != 2)
    {
        Error(id.tokenIndex, "Not found: {}", id.id);
        return nullptr;
    }

    AstQualifiedName enumPart;
    enumPart.parts = arena.NewArray<AstIdentifier>(1);
    enumPart.parts.PushBack(arena, id.id.parts.Front());
    const Type* const type = Resolve(enumPart);
    if (type == nullptr)
    {
        Error(id.tokenIndex, "Not found: {}", id.id);
        return nullptr;
    }

    const TypeEnum* const enumType = CastTo<TypeEnum>(type);
    if (enumType == nullptr)
    {
        Error(id.tokenIndex, "Not an enumeration type: {}", enumType->name);
        return nullptr;
    }

    const EnumItem* const enumItem = FindItem(enumType, id.id.parts[1].name);
    if (enumItem == nullptr)
    {
        Error(id.tokenIndex, "No such enumeration item: {}", id.id);
        return nullptr;
    }

    ValueEnum* enumValue = arena.New<ValueEnum>();
    enumValue->item = enumItem;
    enumValue->line = TokenLine(id.tokenIndex);
    return enumValue;
}

const ValueArray* Generator::BuildArray(const Type* type, const AstNodeInitializerList& expr)
{
    ValueArray* const value = arena.New<ValueArray>();
    value->type = type;
    value->line = TokenLine(expr.tokenIndex);

    Array elements = arena.NewArray<const Value*>(expr.elements.Size());

    for (const AstNode* const elem : expr.elements)
    {
        const Value* const elemValue = BuildExpression(type, *elem);
        if (elemValue != nullptr)
            elements.PushBack(arena, elemValue);
    }

    value->elements = elements;

    return value;
}

const ValueObject* Generator::BuildObject(const TypeStruct* type, const AstNodeInitializerList& expr)
{
    ValueObject* const value = arena.New<ValueObject>();
    value->type = type;
    value->line = TokenLine(expr.tokenIndex);

    if (expr.type.parts)
    {
        value->type = Resolve(expr.type);
        if (value->type != nullptr && !IsA(value->type, type))
            Error(expr.tokenIndex, "Type is not compatible: {}", value->type->name);
    }

    BuildArguments(value->fields, type, type->fields, type->base, expr.elements);

    return value;
}

const Type* Generator::Resolve(const AstQualifiedName& name)
{
    if (!name.parts)
        return nullptr;

    if (name.parts.Size() != 1)
    {
        Error(name.parts.Front().tokenIndex, "Qualified names not yet supported: {}", name);
        return nullptr;
    }

    State& state = *stack.Back();
    const char* const ident = name.parts.Front().name;

    if (const Type* const type = FindType(state.mod, ident); type != nullptr)
        return type;

    if (builtins != nullptr)
        if (const Type* const type = FindType(builtins, ident); type != nullptr)
            return type;

    for (const Module* imp : state.mod->imports)
        if (const Type* const type = FindType(imp, ident); type != nullptr)
            return type;

    Error(name.parts.Front().tokenIndex, "Not found: {}", name);
    return nullptr;
}

const Type* Generator::Resolve(const AstNodeType* type)
{
    if (const AstNodeTypeQualified* qual = type->CastTo<AstNodeTypeQualified>(); qual != nullptr)
    {
        return Resolve(qual->name);
    }

    if (const AstNodeTypeArray* array = type->CastTo<AstNodeTypeArray>(); array != nullptr)
    {
        const Type* inner = Resolve(array->type);
        if (inner == nullptr)
            return nullptr;

        const char* const name = array->size == nullptr
            ? arena.NewString(fmt::format("{}[]", inner->name))
            : arena.NewString(fmt::format("{}[{}]", inner->name, array->size->value));
        TypeArray* const type = CreateType<TypeArray>(array->tokenIndex, name);
        type->type = inner;

        if (array->size != nullptr)
            type->size = array->size->value;

        return type;
    }

    if (const AstNodeTypePointer* pointer = type->CastTo<AstNodeTypePointer>(); pointer != nullptr)
    {
        const Type* inner = Resolve(pointer->type);
        if (inner == nullptr)
            return nullptr;

        const char* const name = arena.NewString(fmt::format("{}*", inner->name));
        TypePointer* const type = CreateType<TypePointer>(pointer->tokenIndex, name);
        type->type = inner;

        return type;
    }

    if (const AstNodeTypeNullable* nullable = type->CastTo<AstNodeTypeNullable>(); nullable != nullptr)
    {
        const Type* inner = Resolve(nullable->type);
        if (inner == nullptr)
            return nullptr;

        const char* const name = arena.NewString(fmt::format("{}?", inner->name));
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
    result = false;
    if (tokenIndex < stack.Back()->tokens.Size())
    {
        const Token& token = stack.Back()->tokens[tokenIndex];
        logger_.Error(stack.Back()->mod->filename, FindRange(stack.Back()->source, token), fmt::vformat(format, fmt::make_format_args(args...)));
    }
    else
    {
        logger_.Error(stack.Back()->mod->filename, {}, fmt::vformat(format, fmt::make_format_args(args...)));
    }
}

template <typename T>
T* Generator::CreateType(std::uint32_t tokenIndex, const char* name)
{
    State* const state = stack.Back();

    if (const Type* const previous = FindType(state->mod, name); previous != nullptr)
    {
        Error(tokenIndex, "Type already defined: {}", name);
        return nullptr;
    }

    T* const type = arena.New<T>();
    state->types.PushBack(arena, type);
    state->mod->types = state->types;
    type->name = name;
    type->owner = state->mod;
    type->line = TokenLine(tokenIndex);

    return type;
}

std::uint16_t Generator::TokenLine(std::uint32_t tokenIndex) const noexcept
{
    const State* const state = stack.Back();
    if (tokenIndex >= state->tokens.Size())
        return 0;
    return state->tokens[tokenIndex].line;
}
