// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "compile.h"

#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "resolver.h"
#include "token.h"

#include "schematic/source.h"

#include <fmt/core.h>

#include <charconv>
#include <climits>
#include <cstdint>

using namespace potato::schematic;
using namespace potato::schematic::compiler;

namespace
{
    struct State
    {
        const Source* source = nullptr;
        const AstNodeModule* ast = nullptr;
        Module* mod = nullptr;
        Array<Token> tokens;
    };

    struct Compiler
    {
        const Module* Compile();
        const AstNodeModule* HandleImport(const AstNodeImport& imp);

        const Module* CreateBuiltins();

        static void VisitTypes(ArenaAllocator& alloc, const Module* mod, Array<const Type*>& visited);
        static void VisitTypes(ArenaAllocator& alloc, const Type* type, Array<const Type*>& visited);
        static void VisitTypes(ArenaAllocator& alloc, const Annotation* annotation, Array<const Type*>& visited);
        static void VisitTypes(ArenaAllocator& alloc, const Value* value, Array<const Type*>& visited);
        static void VisitModules(ArenaAllocator& alloc, const Module* mod, Array<const Module*>& visited);

        Logger& logger;
        Resolver& resolver;
        ArenaAllocator& alloc;
        const Module* builtins = nullptr;
        bool result = true;
        Array<State*> stack;

    private:
        void BuildAggregate(const AstNodeAggregateDecl& ast);
        void BuildAttribute(const AstNodeAttributeDecl& ast);
        void BuildEnum(const AstNodeEnumDecl& ast);

        void BuildAnnotations(Array<const Annotation*>& out, Array<const AstNodeAnnotation*> ast);
        void BuildArguments(const Type* type, const Array<Field>& fields, const TypeAggregate* baseType, Array<Argument>& out, Array<const AstNode*> ast);

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
        T* AddType(std::uint32_t tokenIndex, CStringView name);
    };

    struct Context : ParseContext
    {
        explicit Context(Compiler& compiler) noexcept
            : compiler(compiler)
        {
        }

        const AstNodeModule* LoadImport(const AstNodeImport& imp) override
        {
            return compiler.HandleImport(imp);
        }

        Compiler& compiler;
    };

} // namespace

const Schema* potato::schematic::compiler::Compile(Logger& logger, Resolver& resolver, ArenaAllocator& alloc, const Source* source, const CompileOptions& options)
{
    Compiler compiler{ .logger = logger, .resolver = resolver, .alloc = alloc };

    if (options.builtins)
        compiler.CreateBuiltins();

    State* const state = alloc.Create<State>();
    compiler.stack.PushBack(alloc, state);

    state->source = source;
    state->mod = alloc.Create<Module>();
    state->mod->filename = alloc.NewString(source->Name());

    if (!compiler.Compile())
        return nullptr;

    compiler.stack.PopBack();
    assert(compiler.stack.IsEmpty());

    Schema* const schema = alloc.Create<Schema>();
    schema->root = state->mod;

    compiler.VisitTypes(alloc, schema->root, schema->types);
    if (compiler.builtins != nullptr)
        schema->modules.PushBack(alloc, compiler.builtins);
    compiler.VisitModules(alloc, schema->root, schema->modules);

    return schema;
}

const Module* Compiler::Compile()
{
    State& state = *stack.Back();

    if (!Tokenize(logger, alloc, state.source, state.tokens))
        return nullptr;

    Context ctx(*this);
    Parser parser(ctx, logger, alloc, state.source, state.tokens);
    state.ast = parser.Parse();

    if (state.ast == nullptr)
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

const AstNodeModule* Compiler::HandleImport(const AstNodeImport& imp)
{
    const Source* const source = resolver.ResolveModule(imp.target.name, stack.Back()->source);
    if (source == nullptr)
    {
        Error(imp.tokenIndex, "Module not found: {}", imp.target.name.CStr());
        return nullptr;
    }

    State* const state = alloc.Create<State>();
    state->source = source;
    state->mod = alloc.Create<Module>();
    state->mod->filename = alloc.NewString(source->Name());

    stack.PushBack(alloc, state);
    const bool success = Compile();
    stack.PopBack();

    if (success)
        stack.Back()->mod->imports.PushBack(alloc, state->mod);

    if (!success)
        return nullptr;

    return state->ast;
}

const Module* Compiler::CreateBuiltins()
{
    if (builtins != nullptr)
        return builtins;

    Module* const mod = alloc.Create<Module>();
    builtins = mod;

    mod->filename = alloc.NewString("$builtins");

    State state{ .mod = mod };
    stack.PushBack(alloc, &state);

    auto AddInt = [this]<typename T>(const char* name, T)
    {
        TypeInt* const type = AddType<TypeInt>(0, alloc.NewString(name));
        type->isSigned = std::is_signed_v<T>;
        type->bits = CHAR_BIT * sizeof(T);
    };
    auto AddFloat = [this]<typename T>(const char* name, T)
    {
        TypeFloat* const type = AddType<TypeFloat>(0, alloc.NewString(name));
        type->bits = CHAR_BIT * sizeof(T);
    };

    AddType<TypeType>(0, alloc.NewString("$type"));
    AddType<TypeBool>(0, alloc.NewString("bool"));
    AddType<TypeString>(0, alloc.NewString("string"));

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

void Compiler::BuildAggregate(const AstNodeAggregateDecl& ast)
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
            Error(ast.base.parts.Front().tokenIndex, "Base type is not an aggregate: {}", baseType->name.CStr());
    }

    BuildAnnotations(type->annotations, ast.annotations);

    type->fields = alloc.NewArray<Field>(ast.fields.Size());
    for (const AstNodeField* ast_field : ast.fields)
    {
        Field& field = type->fields.EmplaceBack(alloc);
        field.owner = type;
        if (ast_field->name.name)
            field.name = ast_field->name.name;
        field.type = Resolve(ast_field->type);
        if (ast_field->value != nullptr)
            field.value = BuildExpression(field.type, *ast_field->value);
        BuildAnnotations(field.annotations, ast_field->annotations);
    }
}

void Compiler::BuildAttribute(const AstNodeAttributeDecl& ast)
{
    TypeAttribute* const type = AddType<TypeAttribute>(ast.tokenIndex, ast.name.name);
    if (type == nullptr)
        return;

    type->name = ast.name.name;

    BuildAnnotations(type->annotations, ast.annotations);

    type->fields = alloc.NewArray<Field>(ast.fields.Size());
    for (const AstNodeField* ast_field : ast.fields)
    {
        Field& field = type->fields.EmplaceBack(alloc);
        field.owner = type;
        if (ast_field->name.name)
            field.name = ast_field->name.name;
        field.type = Resolve(ast_field->type);
        if (ast_field->value != nullptr)
            field.value = BuildExpression(field.type, *ast_field->value);
        BuildAnnotations(field.annotations, ast_field->annotations);
    }
}

void Compiler::BuildEnum(const AstNodeEnumDecl& ast)
{
    TypeEnum* const type = AddType<TypeEnum>(ast.tokenIndex, ast.name.name);
    if (type == nullptr)
        return;

    type->name = ast.name.name;

    type->base = Resolve(ast.base);
    if (type->base != nullptr && type->kind != TypeKind::Int)
        Error(ast.base.parts.Front().tokenIndex, "Base type is not an integer: {}", type->base->name.CStr());

    BuildAnnotations(type->annotations, ast.annotations);

    type->items = alloc.NewArray<EnumItem>(ast.items.Size());
    std::int64_t next = 0;

    for (const AstNodeEnumItem* ast_item : ast.items)
    {
        EnumItem& item = type->items.EmplaceBack(alloc);
        item.owner = type;
        item.name = ast_item->name.name;
        if (ast_item->value != nullptr)
        {
            item.value = BuildInteger(*ast_item->value);
            next = item.value->value + 1;
        }
        else
        {
            ValueInt* const value = alloc.Create<ValueInt>();
            value->value = next++;
            item.value = value;
        }
        BuildAnnotations(item.annotations, ast_item->annotations);
    }
}

void Compiler::BuildAnnotations(Array<const Annotation*>& out, Array<const AstNodeAnnotation*> ast)
{
    out = alloc.NewArray<const Annotation*>(ast.Size());

    for (const AstNodeAnnotation* const astAttr : ast)
    {
        const Type* const type = Resolve(astAttr->name);
        if (type == nullptr)
            continue;

        const TypeAttribute* const attrType = CastTo<TypeAttribute>(type);
        if (attrType == nullptr)
        {
            Error(astAttr->tokenIndex, "Not an attribute: {}", type->name.CStr());
            continue;
        }

        Annotation* const attr = alloc.Create<Annotation>();
        out.PushBack(alloc, attr);

        attr->attribute = attrType;

        BuildArguments(attrType, attrType->fields, nullptr, attr->arguments, astAttr->arguments);
    }
}

void Compiler::BuildArguments(const Type* type, const Array<Field>& fields, const TypeAggregate* baseType, Array<Argument>& out, Array<const AstNode*> ast)
{
    bool hasNamed = false;
    size_t index = 0;

    bool hasUnnamedAfterNamedError = false;
    bool hasUnnamedWithBaseError = false;

    for (const AstNode* const elem : ast)
    {
        const AstNodeNamedArgument* const named = elem->CastTo<AstNodeNamedArgument>();

        if (named != nullptr)
        {
            const CStringView name = named->name.name;

            hasNamed = true;

            const Field* field = nullptr;
            for (const Field& f : fields)
            {
                if (f.name == name)
                {
                    field = &f;
                    break;
                }
            }
            if (field == nullptr && baseType != nullptr)
                field = FindField(baseType, name);

            if (field == nullptr)
            {
                Error(elem->tokenIndex, "Field does not exist on type: {}", name.CStr());
                continue;
            }

            // FIXME: check for duplicate initializers

            const Value* const value = BuildExpression(field->type, *named->value);

            out.PushBack(alloc, Argument{ .field = field, .value = value });
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
        else if (index >= fields.Size())
        {
            Error(elem->tokenIndex, "Too many initializers");
            break;
        }
        else
        {
            const Field& field = fields[index++];
            const Value* const value = BuildExpression(field.type, *elem);
            out.PushBack(alloc, Argument{ .field = &field, .value = value });
        }
    }
}

const ValueBool* Compiler::BuildBool(const AstNodeLiteralBool& lit)
{
    ValueBool* const value = alloc.Create<ValueBool>();
    value->value = lit.value;
    return value;
}

const ValueInt* Compiler::BuildInteger(const AstNodeLiteralInt& lit)
{
    ValueInt* const value = alloc.Create<ValueInt>();
    value->value = lit.value;
    return value;
}

const ValueFloat* Compiler::BuildFloat(const AstNodeLiteralFloat& lit)
{
    ValueFloat* const value = alloc.Create<ValueFloat>();
    value->value = lit.value;
    return value;
}

const ValueString* Compiler::BuildString(const AstNodeLiteralString& lit)
{
    ValueString* const value = alloc.Create<ValueString>();
    value->value = lit.value;
    return value;
}

const Value* Compiler::BuildExpression(const Type* type, const AstNode& expr)
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
            return alloc.Create<ValueNull>();
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

const Value* Compiler::BuildQualifiedId(const AstNodeQualifiedId& id)
{
    if (id.id.parts.Size() == 1)
    {
        const Type* type = Resolve(id.id);
        if (type != nullptr)
        {
            ValueType* value = alloc.Create<ValueType>();
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
    enumPart.parts = alloc.NewArray<AstIdentifier>(1);
    enumPart.parts.PushBack(alloc, id.id.parts.Front());
    const Type* const type = Resolve(enumPart);
    if (type == nullptr)
    {
        Error(id.tokenIndex, "Not found: {}", id.id);
        return nullptr;
    }

    const TypeEnum* const enumType = CastTo<TypeEnum>(type);
    if (enumType == nullptr)
    {
        Error(id.tokenIndex, "Not an enumeration type: {}", enumType->name.CStr());
        return nullptr;
    }

    const EnumItem* const enumItem = FindItem(enumType, id.id.parts[1].name.CStr());
    if (enumItem == nullptr)
    {
        Error(id.tokenIndex, "No such enumeration item: {}", id.id);
        return nullptr;
    }

    ValueEnum* enumValue = alloc.Create<ValueEnum>();
    enumValue->item = enumItem;
    return enumValue;
}

const ValueArray* Compiler::BuildArray(const Type* type, const AstNodeInitializerList& expr)
{
    ValueArray* const value = alloc.Create<ValueArray>();
    value->type = type;

    value->elements = alloc.NewArray<const Value*>(expr.elements.Size());

    for (const AstNode* const elem : expr.elements)
    {
        const Value* const elemValue = BuildExpression(nullptr, *elem);
        if (elemValue != nullptr)
            value->elements.PushBack(alloc, elemValue);
    }

    return value;
}

const ValueObject* Compiler::BuildObject(const TypeAggregate* type, const AstNodeInitializerList& expr)
{
    ValueObject* const obj = alloc.Create<ValueObject>();
    obj->type = type;

    if (expr.type.parts)
    {
        obj->type = Resolve(expr.type);
        if (obj->type != nullptr && !IsA(obj->type, type))
            Error(expr.tokenIndex, "Type is not compatible: {}", obj->type->name.CStr());
    }

    BuildArguments(type, type->fields, type->base, obj->fields, expr.elements);

    return obj;
}

const Type* Compiler::Resolve(const AstQualifiedName& name)
{
    if (!name.parts)
        return nullptr;

    if (name.parts.Size() != 1)
    {
        Error(name.parts.Front().tokenIndex, "Qualified names not yet supported: {}", name);
        return nullptr;
    }

    State& state = *stack.Back();
    CStringView ident = name.parts.Front().name;

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

const Type* Compiler::Resolve(const AstNodeType* type)
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

        const CStringView name = alloc.NewString(fmt::format("{}[]", inner->name.CStr()));
        TypeArray* const type = AddType<TypeArray>(array->tokenIndex, name);
        type->type = inner;

        return type;
    }

    if (const AstNodeTypePolymorphic* poly = type->CastTo<AstNodeTypePolymorphic>(); poly != nullptr)
    {
        const Type* inner = Resolve(poly->type);
        if (inner == nullptr)
            return nullptr;

        const CStringView name = alloc.NewString(fmt::format("{}*", inner->name.CStr()));
        TypePolymorphic* const type = AddType<TypePolymorphic>(poly->tokenIndex, name);
        type->type = inner;
        type->isNullable = false;

        return type;
    }

    if (const AstNodeTypeNullable* nullable = type->CastTo<AstNodeTypeNullable>(); nullable != nullptr)
    {
        const Type* inner = Resolve(nullable->type);
        if (inner == nullptr)
            return nullptr;

        const CStringView name = alloc.NewString(fmt::format("{}?", inner->name.CStr()));
        TypePolymorphic* const type = AddType<TypePolymorphic>(nullable->tokenIndex, name);
        type->type = inner;
        type->isNullable = true;

        return type;
    }

    return nullptr;
}

template <typename... Args>
void Compiler::Error(std::uint32_t tokenIndex, fmt::format_string<Args...> format, const Args&... args)
{
    result = false;
    if (tokenIndex < stack.Back()->tokens.Size())
    {
        const Token& token = stack.Back()->tokens[tokenIndex];

        logger.Error({ .source = stack.Back()->source, .offset = token.offset, .length = token.length },
            fmt::vformat(format, fmt::make_format_args(args...)));
    }
    else
    {
        logger.Error({ .source = stack.Back()->source }, fmt::vformat(format, fmt::make_format_args(args...)));
    }
}

template <typename T>
T* Compiler::AddType(std::uint32_t tokenIndex, CStringView name)
{
    if (const Type* const previous = FindType(stack.Back()->mod, name); previous != nullptr)
    {
        Error(tokenIndex, "Type already defined: {}", name.CStr());
        return nullptr;
    }

    T* const type = alloc.Create<T>();
    stack.Back()->mod->types.PushBack(alloc, type);
    type->name = name;
    type->owner = stack.Back()->mod;

    return type;
}

void Compiler::VisitTypes(ArenaAllocator& alloc, const Module* mod, Array<const Type*>& visited)
{
    if (mod == nullptr)
        return;

    for (const Type* const type : mod->types)
        VisitTypes(alloc, type, visited);
}

void Compiler::VisitTypes(ArenaAllocator& alloc, const Type* type, Array<const Type*>& visited)
{
    if (type == nullptr)
        return;

    for (const Type* const exists : visited)
        if (exists == type)
            return;

    for (const Annotation* const annotation : type->annotations)
        VisitTypes(alloc, annotation, visited);

    if (const TypeAggregate* const agg = CastTo<TypeAggregate>(type); agg != nullptr)
    {
        VisitTypes(alloc, agg->base, visited);

        for (const Field& field : agg->fields)
        {
            VisitTypes(alloc, field.type, visited);
            VisitTypes(alloc, field.value, visited);

            for (const Annotation* const annotation : field.annotations)
                VisitTypes(alloc, annotation, visited);
        }
    }
    else if (const TypeAttribute* const attr = CastTo<TypeAttribute>(type); attr != nullptr)
    {
        for (const Field& field : attr->fields)
        {
            VisitTypes(alloc, field.type, visited);
            VisitTypes(alloc, field.value, visited);

            for (const Annotation* const annotation : field.annotations)
                VisitTypes(alloc, annotation, visited);
        }
    }
    else if (const TypeEnum* const enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
    {
        VisitTypes(alloc, enum_->base, visited);

        for (const EnumItem& item : enum_->items)
        {
            for (const Annotation* const annotation : item.annotations)
                VisitTypes(alloc, annotation, visited);
        }
    }
    else if (const TypePolymorphic* const poly = CastTo<TypePolymorphic>(type); poly != nullptr)
    {
        VisitTypes(alloc, poly->type, visited);
    }
    else if (const TypeArray* const array = CastTo<TypeArray>(type); array != nullptr)
    {
        VisitTypes(alloc, array->type, visited);
    }

    visited.PushBack(alloc, type);
}

void Compiler::VisitTypes(ArenaAllocator& alloc, const Annotation* annotation, Array<const Type*>& visited)
{
    if (annotation == nullptr)
        return;

    VisitTypes(alloc, annotation->attribute, visited);

    for (const Argument& arg : annotation->arguments)
        VisitTypes(alloc, arg.value, visited);
}

void Compiler::VisitTypes(ArenaAllocator& alloc, const Value* value, Array<const Type*>& visited)
{
    if (value == nullptr)
        return;

    if (const ValueType* const type = CastTo<ValueType>(value); type != nullptr)
        VisitTypes(alloc, type->type, visited);
}

void Compiler::VisitModules(ArenaAllocator& alloc, const Module* mod, Array<const Module*>& visited)
{
    if (mod == nullptr)
        return;

    visited.PushBack(alloc, mod);

    for (const Module* const imp : mod->imports)
    {
        VisitModules(alloc, imp, visited);
    }
}
