// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"

#include "ast.h"
#include "lexer.h"
#include "location.h"
#include "parser.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <fmt/core.h>

#include <charconv>
#include <climits>
#include <cstdint>
#include <utility>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

namespace
{
    struct State
    {
        ModuleId moduleId;
        const AstNodeModule* ast = nullptr;
        Module* mod = nullptr;
        Array<const Module*> imports;
        Array<const Type*> types;
        Array<Token> tokens;
    };
} // namespace

struct potato::schematic::Compiler::Impl final
{
    explicit Impl(CompileContext& ctx, ArenaAllocator& arena) noexcept
        : ctx(ctx)
        , arena(arena)
    {
    }

    const Schema* Compile(ModuleId moduleId);
    const Module* CompileModule();

    bool HandleImport(const AstNodeImport& imp);

    const Module* CreateBuiltins();

    void BuildAggregate(const AstNodeAggregateDecl& ast);
    void BuildAttribute(const AstNodeAttributeDecl& ast);
    void BuildEnum(const AstNodeEnumDecl& ast);

    void BuildAnnotations(std::span<const Annotation* const>& out, Array<const AstNodeAnnotation*> ast);
    void BuildArguments(std::span<const Argument>& out, const Type* type, const std::span<const Field>& fields, const TypeAggregate* baseType, Array<const AstNode*> ast);

    const ValueBool* BuildBool(const AstNodeLiteralBool& lit);
    const ValueInt* BuildInteger(const AstNodeLiteralInt& lit);
    const ValueFloat* BuildFloat(const AstNodeLiteralFloat& lit);
    const ValueString* BuildString(const AstNodeLiteralString& lit);
    const Value* BuildExpression(const Type* type, const AstNode& expr);
    const Value* BuildQualifiedId(const AstNodeQualifiedId& id);
    const ValueArray* BuildArray(const Type* type, const AstNodeInitializerList& expr);
    const ValueObject* BuildObject(const TypeAggregate* type, const AstNodeInitializerList& expr);

    const Type* Resolve(const AstQualifiedName& name);
    const Type* Resolve(const AstNodeType* type);

    template <typename... Args>
    void Error(std::uint32_t tokenIndex, fmt::format_string<Args...> format, const Args&... args);

    template <typename T>
    T* AddType(std::uint32_t tokenIndex, const char* name);

    void VisitTypes(const Module* mod, Array<const Type*>& visited);
    void VisitTypes(const Type* type, Array<const Type*>& visited);
    void VisitTypes(const Annotation* annotation, Array<const Type*>& visited);
    void VisitTypes(const Value* value, Array<const Type*>& visited);
    void VisitModules(const Module* mod, Array<const Module*>& visited);

    CompileContext& ctx;
    ArenaAllocator& arena;
    bool useBuiltins = false;
    bool result = true;
    const Module* builtins = nullptr;
    const Schema* schema = nullptr;
    Array<State*> stack;
};

template <>
struct fmt::formatter<potato::schematic::compiler::AstQualifiedName> : fmt::formatter<const char*>
{
    template <typename FormatContext>
    FMT_CONSTEXPR auto format(const potato::schematic::compiler::AstQualifiedName& name, FormatContext& ctx) const
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

potato::schematic::Compiler::Compiler(CompileContext& ctx, ArenaAllocator& arena)
    : impl_(arena.New<Impl>(ctx, arena))
    , arena_(arena)
{
}

potato::schematic::Compiler::~Compiler()
{
}

void potato::schematic::Compiler::SetUseBuiltins(bool useBuiltins)
{
    impl_->useBuiltins = useBuiltins;
}

bool potato::schematic::Compiler::Compile(ModuleId moduleId)
{
    impl_->builtins = nullptr;
    impl_->schema = nullptr;
    impl_->result = true;
    impl_->stack = Array<State*>{};
    return impl_->Compile(moduleId) != nullptr;
}

const Schema* potato::schematic::Compiler::GetSchema()
{
    return impl_->schema;
}

const Module* potato::schematic::Compiler::Impl::CompileModule()
{
    State& state = *stack.Back();

    Lexer lexer(ctx, arena, state.moduleId);
    state.tokens = lexer.Tokenize();
    if (state.tokens.IsEmpty())
        return nullptr;

    Parser parser(ctx, arena, state.moduleId, state.tokens);
    state.ast = parser.Parse();

    if (state.ast == nullptr)
        return nullptr;

    bool importFailed = false;
    for (const AstNode* adecl : state.ast->nodes)
    {
        if (const AstNodeImport* const imp = adecl->CastTo<AstNodeImport>())
        {
            if (!HandleImport(*imp))
                importFailed = true;
        }
    }

    if (importFailed)
        return nullptr;

    for (const AstNode* adecl : state.ast->nodes)
    {
        if (const AstNodeAggregateDecl* const agg = adecl->CastTo<AstNodeAggregateDecl>())
        {
            BuildAggregate(*agg);
            continue;
        }

        if (const AstNodeAttributeDecl* const attr = adecl->CastTo<AstNodeAttributeDecl>())
        {
            BuildAttribute(*attr);
            continue;
        }

        if (const AstNodeEnumDecl* const enum_ = adecl->CastTo<AstNodeEnumDecl>())
        {
            BuildEnum(*enum_);
            continue;
        }

        if (adecl->kind == AstNodeKind::Import)
            continue;

        Error(adecl->tokenIndex, "Internal error: unexpected top-level node kind {}", std::to_underlying(adecl->kind));
        break;
    }

    if (!result)
        return nullptr;

    return state.mod;
}

const Schema* potato::schematic::Compiler::Impl::Compile(ModuleId moduleId)
{
    if (moduleId.value == ModuleId::InvalidValue)
        return nullptr;

    if (useBuiltins && builtins == nullptr)
        CreateBuiltins();

    State* const state = arena.New<State>();
    stack.PushBack(arena, state);

    state->moduleId = moduleId;
    state->mod = arena.New<Module>();
    state->mod->filename = arena.NewString(ctx.GetFileName(moduleId));

    if (useBuiltins)
        state->imports.EmplaceBack(arena, builtins);

    if (!CompileModule())
        return nullptr;

    stack.PopBack();
    assert(stack.IsEmpty());

    Schema* const schema = arena.New<Schema>();
    schema->root = state->mod;

    {
        Array<const Type*> visited;
        VisitTypes(schema->root, visited);
        schema->types = visited;
    }

    {
        Array<const Module*> visited;
        VisitModules(schema->root, visited);
        schema->modules = visited;
    }

    this->schema = schema;
    return schema;
}

bool potato::schematic::Compiler::Impl::HandleImport(const AstNodeImport& imp)
{
    const ModuleId moduleId = ctx.ResolveModule(imp.target.name, stack.Back()->moduleId);
    if (moduleId.value == ModuleId::InvalidValue)
    {
        Error(imp.tokenIndex, "Module not found: {}", imp.target.name);
        return false;
    }

    State* const state = arena.New<State>();
    state->moduleId = moduleId;
    state->mod = arena.New<Module>();
    state->mod->filename = arena.NewString(ctx.GetFileName(moduleId));

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

const Module* potato::schematic::Compiler::Impl::CreateBuiltins()
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
        TypeInt* const type = AddType<TypeInt>(0, arena.NewString(name));
        type->isSigned = std::is_signed_v<T>;
        type->width = CHAR_BIT * sizeof(T);
    };
    auto AddFloat = [this]<typename T>(const char* name, T)
    {
        TypeFloat* const type = AddType<TypeFloat>(0, arena.NewString(name));
        type->width = CHAR_BIT * sizeof(T);
    };

    AddType<TypeType>(0, arena.NewString("$type"));
    AddType<TypeBool>(0, arena.NewString("bool"));
    AddType<TypeString>(0, arena.NewString("string"));

    AddInt("int8", std::int8_t{});
    AddInt("uint8", std::uint8_t{});
    AddInt("int16", std::int16_t{});
    AddInt("uint16", std::uint16_t{});
    AddInt("int32", std::int32_t{});
    AddInt("uint32", std::uint32_t{});
    AddInt("int64", std::int64_t{});
    AddInt("uint64", std::uint64_t{});

    AddFloat("float", float{});
    AddFloat("double", double{});

    stack.PopBack();
    return builtins;
}

void potato::schematic::Compiler::Impl::BuildAggregate(const AstNodeAggregateDecl& ast)
{
    TypeAggregate* const type = AddType<TypeAggregate>(ast.tokenIndex, ast.name.name);
    if (type == nullptr)
        return;

    type->name = ast.name.name;
    const Type* const baseType = Resolve(ast.base);
    if (baseType != nullptr)
    {
        type->base = CastTo<TypeAggregate>(baseType);
        if (type->base == nullptr)
            Error(ast.base.parts.Front().tokenIndex, "Base type is not an aggregate: {}", baseType->name);
    }

    BuildAnnotations(type->annotations, ast.annotations);

    auto fields = arena.NewArray<Field>(ast.fields.Size());
    for (const AstNodeField* ast_field : ast.fields)
    {
        Field& field = fields.EmplaceBack(arena);
        field.owner = type;
        if (ast_field->name.name != nullptr)
            field.name = ast_field->name.name;
        field.type = Resolve(ast_field->type);
        if (ast_field->value != nullptr)
            field.value = BuildExpression(field.type, *ast_field->value);
        BuildAnnotations(field.annotations, ast_field->annotations);
    }
    type->fields = fields;
}

void potato::schematic::Compiler::Impl::BuildAttribute(const AstNodeAttributeDecl& ast)
{
    TypeAttribute* const type = AddType<TypeAttribute>(ast.tokenIndex, ast.name.name);
    if (type == nullptr)
        return;

    type->name = ast.name.name;

    BuildAnnotations(type->annotations, ast.annotations);

    auto fields = arena.NewArray<Field>(ast.fields.Size());
    for (const AstNodeField* ast_field : ast.fields)
    {
        Field& field = fields.EmplaceBack(arena);
        field.owner = type;
        if (ast_field->name.name != nullptr)
            field.name = ast_field->name.name;
        field.type = Resolve(ast_field->type);
        if (ast_field->value != nullptr)
            field.value = BuildExpression(field.type, *ast_field->value);
        BuildAnnotations(field.annotations, ast_field->annotations);
    }
    type->fields = fields;
}

void potato::schematic::Compiler::Impl::BuildEnum(const AstNodeEnumDecl& ast)
{
    TypeEnum* const type = AddType<TypeEnum>(ast.tokenIndex, ast.name.name);
    if (type == nullptr)
        return;

    type->name = ast.name.name;

    type->base = Resolve(ast.base);
    if (type->base != nullptr && type->kind != TypeKind::Int)
        Error(ast.base.parts.Front().tokenIndex, "Base type is not an integer: {}", type->base->name);

    BuildAnnotations(type->annotations, ast.annotations);

    std::int64_t next = 0;
    Array<EnumItem> items = arena.NewArray<EnumItem>(ast.items.Size());
    for (const AstNodeEnumItem* ast_item : ast.items)
    {
        EnumItem& item = items.EmplaceBack(arena);
        item.owner = type;
        item.name = ast_item->name.name;
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
    type->items = items;
}

void potato::schematic::Compiler::Impl::BuildAnnotations(std::span<const Annotation* const>& out, Array<const AstNodeAnnotation*> ast)
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

        Annotation* const attr = arena.New<Annotation>();
        temp.PushBack(arena, attr);

        attr->attribute = attrType;

        BuildArguments(attr->arguments, attrType, attrType->fields, nullptr, astAttr->arguments);
    }

    out = temp;
}

void potato::schematic::Compiler::Impl::BuildArguments(std::span<const Argument>& out, const Type* type, const std::span<const Field>& fields, const TypeAggregate* baseType, Array<const AstNode*> ast)
{
    bool hasNamed = false;
    size_t index = 0;

    bool hasUnnamedAfterNamedError = false;
    bool hasUnnamedWithBaseError = false;

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

            // FIXME: check for duplicate initializers

            const Value* const value = BuildExpression(field->type, *named->value);

            temp.PushBack(arena, Argument{ .field = field, .value = value });
        }
        else if (hasNamed)
        {
            if (!hasUnnamedAfterNamedError)
            {
                hasUnnamedAfterNamedError = true;
                Error(elem->tokenIndex, "Unnamed initializers cannot follow named initializers");
            }
        }
        else if (baseType != nullptr)
        {
            if (!hasUnnamedWithBaseError)
            {
                hasUnnamedWithBaseError = true;
                Error(elem->tokenIndex, "Unnamed initializers may not be used on aggregate with base types");
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
            temp.PushBack(arena, Argument{ .field = &field, .value = value });
        }
    }

    out = temp;
}

const ValueBool* potato::schematic::Compiler::Impl::BuildBool(const AstNodeLiteralBool& lit)
{
    ValueBool* const value = arena.New<ValueBool>();
    value->value = lit.value;
    return value;
}

const ValueInt* potato::schematic::Compiler::Impl::BuildInteger(const AstNodeLiteralInt& lit)
{
    ValueInt* const value = arena.New<ValueInt>();
    value->value = lit.value;
    return value;
}

const ValueFloat* potato::schematic::Compiler::Impl::BuildFloat(const AstNodeLiteralFloat& lit)
{
    ValueFloat* const value = arena.New<ValueFloat>();
    value->value = lit.value;
    return value;
}

const ValueString* potato::schematic::Compiler::Impl::BuildString(const AstNodeLiteralString& lit)
{
    ValueString* const value = arena.New<ValueString>();
    value->value = lit.value;
    return value;
}

const Value* potato::schematic::Compiler::Impl::BuildExpression(const Type* type, const AstNode& expr)
{
    switch (expr.kind)
    {
        case AstNodeKind::LiteralBool:
            return BuildBool(*expr.CastTo<AstNodeLiteralBool>());
        case AstNodeKind::LiteralInt:
            return BuildInteger(*expr.CastTo<AstNodeLiteralInt>());
        case AstNodeKind::LiteralFloat:
            return BuildFloat(*expr.CastTo<AstNodeLiteralFloat>());
        case AstNodeKind::LiteralString:
            return BuildString(*expr.CastTo<AstNodeLiteralString>());
        case AstNodeKind::LiteralNull:
            return arena.New<ValueNull>();
        case AstNodeKind::QualifiedId:
            return BuildQualifiedId(*expr.CastTo<AstNodeQualifiedId>());
        case AstNodeKind::InitializerList:
            if (const TypeAggregate* agg = CastTo<TypeAggregate>(type); agg != nullptr)
                return BuildObject(agg, *expr.CastTo<AstNodeInitializerList>());
            if (const TypeArray* array = CastTo<TypeArray>(type); array != nullptr)
                return BuildArray(array, *expr.CastTo<AstNodeInitializerList>());
            Error(expr.tokenIndex, "Not implemented");
        default:
            Error(expr.tokenIndex, "Unknown expression AST node type: {}", std::to_underlying(expr.kind));
            return nullptr;
    }
}

const Value* potato::schematic::Compiler::Impl::BuildQualifiedId(const AstNodeQualifiedId& id)
{
    if (id.id.parts.Size() == 1)
    {
        const Type* type = Resolve(id.id);
        if (type != nullptr)
        {
            ValueType* value = arena.New<ValueType>();
            value->type = type;
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
    return enumValue;
}

const ValueArray* potato::schematic::Compiler::Impl::BuildArray(const Type* type, const AstNodeInitializerList& expr)
{
    ValueArray* const value = arena.New<ValueArray>();
    value->type = type;

    Array elements = arena.NewArray<const Value*>(expr.elements.Size());

    for (const AstNode* const elem : expr.elements)
    {
        const Value* const elemValue = BuildExpression(nullptr, *elem);
        if (elemValue != nullptr)
            elements.PushBack(arena, elemValue);
    }

    value->elements = elements;

    return value;
}

const ValueObject* potato::schematic::Compiler::Impl::BuildObject(const TypeAggregate* type, const AstNodeInitializerList& expr)
{
    ValueObject* const obj = arena.New<ValueObject>();
    obj->type = type;

    if (expr.type.parts)
    {
        obj->type = Resolve(expr.type);
        if (obj->type != nullptr && !IsA(obj->type, type))
            Error(expr.tokenIndex, "Type is not compatible: {}", obj->type->name);
    }

    BuildArguments(obj->fields, type, type->fields, type->base, expr.elements);

    return obj;
}

const Type* potato::schematic::Compiler::Impl::Resolve(const AstQualifiedName& name)
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

const Type* potato::schematic::Compiler::Impl::Resolve(const AstNodeType* type)
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

        if (array->size != nullptr)
        {
            Error(array->tokenIndex, "Sized arrays not implemented yet");
            return nullptr;
        }

        const char* const name = arena.NewString(fmt::format("{}[]", inner->name));
        TypeArray* const type = AddType<TypeArray>(array->tokenIndex, name);
        type->type = inner;

        return type;
    }

    if (const AstNodeTypePointer* pointer = type->CastTo<AstNodeTypePointer>(); pointer != nullptr)
    {
        const Type* inner = Resolve(pointer->type);
        if (inner == nullptr)
            return nullptr;

        const char* const name = arena.NewString(fmt::format("{}*", inner->name));
        TypePointer* const type = AddType<TypePointer>(pointer->tokenIndex, name);
        type->type = inner;
        type->isNullable = false;

        return type;
    }

    if (const AstNodeTypeNullable* nullable = type->CastTo<AstNodeTypeNullable>(); nullable != nullptr)
    {
        const Type* inner = Resolve(nullable->type);
        if (inner == nullptr)
            return nullptr;

        const char* const name = arena.NewString(fmt::format("{}?", inner->name));
        TypePointer* const type = AddType<TypePointer>(nullable->tokenIndex, name);
        type->type = inner;
        type->isNullable = true;

        return type;
    }

    return nullptr;
}

template <typename... Args>
void potato::schematic::Compiler::Impl::Error(std::uint32_t tokenIndex, fmt::format_string<Args...> format, const Args&... args)
{
    result = false;
    if (tokenIndex < stack.Back()->tokens.Size())
    {
        const Token& token = stack.Back()->tokens[tokenIndex];

        const std::string_view source = ctx.ReadFileContents(stack.Back()->moduleId);
        ctx.Error(stack.Back()->moduleId, FindRange(source, token.offset, token.length), fmt::vformat(format, fmt::make_format_args(args...)));
    }
    else
    {
        ctx.Error(stack.Back()->moduleId, {}, fmt::vformat(format, fmt::make_format_args(args...)));
    }
}

template <typename T>
T* potato::schematic::Compiler::Impl::AddType(std::uint32_t tokenIndex, const char* name)
{
    if (const Type* const previous = FindType(stack.Back()->mod, name); previous != nullptr)
    {
        Error(tokenIndex, "Type already defined: {}", name);
        return nullptr;
    }

    T* const type = arena.New<T>();
    State* const state = stack.Back();
    state->types.PushBack(arena, type);
    state->mod->types = state->types;
    type->name = name;
    type->owner = stack.Back()->mod;

    return type;
}

void potato::schematic::Compiler::Impl::VisitTypes(const Module* mod, Array<const Type*>& visited)
{
    if (mod == nullptr)
        return;

    for (const Type* const type : mod->types)
        VisitTypes(type, visited);
}

void potato::schematic::Compiler::Impl::VisitTypes(const Type* type, Array<const Type*>& visited)
{
    if (type == nullptr)
        return;

    for (const Type* const exists : visited)
        if (exists == type)
            return;

    for (const Annotation* const annotation : type->annotations)
        VisitTypes(annotation, visited);

    if (const TypeAggregate* const agg = CastTo<TypeAggregate>(type); agg != nullptr)
    {
        VisitTypes(agg->base, visited);

        for (const Field& field : agg->fields)
        {
            VisitTypes(field.type, visited);
            VisitTypes(field.value, visited);

            for (const Annotation* const annotation : field.annotations)
                VisitTypes(annotation, visited);
        }
    }
    else if (const TypeAttribute* const attr = CastTo<TypeAttribute>(type); attr != nullptr)
    {
        for (const Field& field : attr->fields)
        {
            VisitTypes(field.type, visited);
            VisitTypes(field.value, visited);

            for (const Annotation* const annotation : field.annotations)
                VisitTypes(annotation, visited);
        }
    }
    else if (const TypeEnum* const enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
    {
        VisitTypes(enum_->base, visited);

        for (const EnumItem& item : enum_->items)
        {
            for (const Annotation* const annotation : item.annotations)
                VisitTypes(annotation, visited);
        }
    }
    else if (const TypePointer* const pointer = CastTo<TypePointer>(type); pointer != nullptr)
    {
        VisitTypes(pointer->type, visited);
    }
    else if (const TypeArray* const array = CastTo<TypeArray>(type); array != nullptr)
    {
        VisitTypes(array->type, visited);
    }

    visited.PushBack(arena, type);
}

void potato::schematic::Compiler::Impl::VisitTypes(const Annotation* annotation, Array<const Type*>& visited)
{
    if (annotation == nullptr)
        return;

    VisitTypes(annotation->attribute, visited);

    for (const Argument& arg : annotation->arguments)
        VisitTypes(arg.value, visited);
}

void potato::schematic::Compiler::Impl::VisitTypes(const Value* value, Array<const Type*>& visited)
{
    if (value == nullptr)
        return;

    if (const ValueType* const type = CastTo<ValueType>(value); type != nullptr)
        VisitTypes(type->type, visited);
}

void potato::schematic::Compiler::Impl::VisitModules(const Module* mod, Array<const Module*>& visited)
{
    if (mod == nullptr)
        return;

    visited.PushBack(arena, mod);

    for (const Module* const imp : mod->imports)
        VisitModules(imp, visited);
}
