// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compile.h"

#include "ast.h"
#include "builtins.h"
#include "lexer.h"
#include "parser.h"
#include "token.h"

#include "schematic/resolver.h"
#include "schematic/source.h"

#include <fmt/core.h>

#include <charconv>
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
        Array<const Module*> imports;
    };

    struct Compiler
    {
        const Module* Compile();
        const AstNodeModule* HandleImport(const AstNodeImport& imp);

        static void VisitTypes(ArenaAllocator& alloc, const Type* type, Array<const Type*>& visited);

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

        void BuildAttributes(Array<const Attribute*>& out, Array<const AstNodeAttribute*> ast);
        void BuildArguments(const Type* type, const Array<Field>& fields, const TypeAggregate* baseType, Array<Argument>& out, Array<const AstNode*> ast);

        const ValueBool* BuildBool(const AstNodeLiteralBool& lit);
        const ValueInt* BuildInteger(const AstNodeLiteralInt& lit);
        const ValueReal* BuildReal(const AstNodeLiteralReal& lit);
        const Value* BuildExpression(const Type* type, const AstNode& expr);
        const Value* BuildQualifiedId(const AstNodeQualifiedId& id);
        const ValueArray* BuildArray(const Type* type, const AstNodeInitializerList& expr);
        const ValueObject* BuildObject(const TypeAggregate* type, const AstNodeInitializerList& expr);

        const Type* Resolve(const AstQualifiedName& name);
        const Type* Resolve(const AstNodeType* type);

        template <typename... Args>
        void Error(unsigned tokenIndex, fmt::format_string<Args...> format, const Args&... args);

        template <typename T>
        T* AddType(unsigned tokenIndex, String name)
        {
            if (const Type* const previous = FindType(stack.Back()->mod, name); previous != nullptr)
            {
                Error(tokenIndex, "Type already defined: {}", name.CStr());
                return nullptr;
            }

            T* const type = alloc.Create<T>();
            stack.Back()->mod->types.PushBack(alloc, type);
            type->name = name;
            return type;
        }
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

const Module* potato::schematic::compiler::Compile(Logger& logger, Resolver& resolver, ArenaAllocator& alloc, const Source* source, const CompileOptions& options)
{
    const Module* const builtins = options.builtins ? CreateBuiltins(alloc) : nullptr;

    Compiler compiler{ .logger = logger, .resolver = resolver, .alloc = alloc, .builtins = builtins };

    State* const state = alloc.Create<State>();
    compiler.stack.PushBack(alloc, state);

    state->source = source;
    state->mod = alloc.Create<Module>();

    if (!compiler.Compile())
        return nullptr;

    compiler.stack.PopBack();
    assert(compiler.stack.IsEmpty());

    Array<const Type*> visited = alloc.NewArray<const Type*>(state->mod->types.Size());
    for (const Type* const type : state->mod->types)
        compiler.VisitTypes(alloc, type, visited);
    state->mod->types = visited;

    return state->mod;
}

const Module* Compiler::Compile()
{
    State& state = *stack.Back();

    if (!Tokenize(logger, alloc, state.source, state.tokens))
        return nullptr;

    Context ctx(*this);
    state.ast = Parse(ctx, logger, alloc, state.source, state.tokens);
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
        if (adecl->kind == AstNodeKind::KeywordDecl)
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

    stack.PushBack(alloc, state);
    const bool success = Compile();
    stack.PopBack();

    if (success)
        stack.Back()->imports.PushBack(alloc, state->mod);

    if (!success)
        return nullptr;

    return state->ast;
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

    BuildAttributes(type->attributes, ast.attributes);

    type->fields = alloc.NewArray<Field>(ast.fields.Size());
    for (const AstNodeField* ast_field : ast.fields)
    {
        Field& field = type->fields.EmplaceBack(alloc);
        field.parent = type;
        if (ast_field->name.name)
            field.name = ast_field->name.name;
        field.type = Resolve(ast_field->type);
        if (ast_field->value != nullptr)
            field.value = BuildExpression(field.type, *ast_field->value);
    }
}

void Compiler::BuildAttribute(const AstNodeAttributeDecl& ast)
{
    TypeAttribute* const type = AddType<TypeAttribute>(ast.tokenIndex, ast.name.name);
    if (type == nullptr)
        return;

    type->name = ast.name.name;

    BuildAttributes(type->attributes, ast.attributes);

    type->fields = alloc.NewArray<Field>(ast.fields.Size());
    for (const AstNodeField* ast_field : ast.fields)
    {
        Field& field = type->fields.EmplaceBack(alloc);
        field.parent = type;
        if (ast_field->name.name)
            field.name = ast_field->name.name;
        field.type = Resolve(ast_field->type);
        if (ast_field->value != nullptr)
            field.value = BuildExpression(field.type, *ast_field->value);
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

    BuildAttributes(type->attributes, ast.attributes);

    type->items = alloc.NewArray<EnumItem>(ast.items.Size());
    signed long long next = 0;

    for (const AstNodeEnumItem* ast_item : ast.items)
    {
        EnumItem& item = type->items.EmplaceBack(alloc);
        item.parent = type;
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
    }
}

void Compiler::BuildAttributes(Array<const Attribute*>& out, Array<const AstNodeAttribute*> ast)
{
    out = alloc.NewArray<const Attribute*>(ast.Size());

    for (const AstNodeAttribute* const astAttr : ast)
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

        Attribute* const attr = alloc.Create<Attribute>();
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
            const String name = named->name.name;

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

const ValueReal* Compiler::BuildReal(const AstNodeLiteralReal& lit)
{
    ValueReal* const value = alloc.Create<ValueReal>();
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
        case AstNodeKind::LiteralReal:
            return BuildReal(*expr.CastTo<AstNodeLiteralReal>());
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
    String ident = name.parts.Front().name;

    if (const Type* const type = FindType(state.mod, ident); type != nullptr)
        return type;

    if (builtins != nullptr)
        if (const Type* const type = FindType(builtins, ident); type != nullptr)
            return type;

    for (const Module* imp : state.imports)
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

        TypeArray* type = alloc.Create<TypeArray>();
        if (array->size != nullptr)
        {
            Error(array->tokenIndex, "Sized arrays not implemented yet");
            return nullptr;
        }

        type->name = alloc.NewString(fmt::format("{}[]", inner->name.CStr()));
        type->type = inner;

        return type;
    }

    if (const AstNodeTypePolymorphic* poly = type->CastTo<AstNodeTypePolymorphic>(); poly != nullptr)
    {
        const Type* inner = Resolve(poly->type);
        if (inner == nullptr)
            return nullptr;

        TypePolymorphic* type = alloc.Create<TypePolymorphic>();
        type->name = alloc.NewString(fmt::format("{}*", inner->name.CStr()));
        type->type = inner;
        type->isNullable = false;

        return type;
    }

    if (const AstNodeTypeNullable* nullable = type->CastTo<AstNodeTypeNullable>(); nullable != nullptr)
    {
        const Type* inner = Resolve(nullable->type);
        if (inner == nullptr)
            return nullptr;

        TypePolymorphic* type = alloc.Create<TypePolymorphic>();
        type->name = alloc.NewString(fmt::format("{}?", inner->name.CStr()));
        type->type = inner;
        type->isNullable = true;

        return type;
    }

    return nullptr;
}

template <typename... Args>
void Compiler::Error(unsigned tokenIndex, fmt::format_string<Args...> format, const Args&... args)
{
    const Token& token = stack.Back()->tokens[tokenIndex];

    logger.Error({ .source = stack.Back()->source, .offset = token.offset, .length = token.length },
        fmt::vformat(format, fmt::make_format_args(args...)));
    result = false;
}

void Compiler::VisitTypes(ArenaAllocator& alloc, const Type* type, Array<const Type*>& visited)
{
    if (type == nullptr)
        return;

    for (const Type* const exists : visited)
        if (exists == type)
            return;

    visited.PushBack(alloc, type);

    if (const TypeAggregate* agg = CastTo<TypeAggregate>(type); agg != nullptr)
    {
        VisitTypes(alloc, agg->base, visited);

        for (const Field& field : agg->fields)
            VisitTypes(alloc, field.type, visited);
    }
    else if (const TypeEnum* enum_ = CastTo<TypeEnum>(type); enum_ != nullptr)
    {
        VisitTypes(alloc, enum_->base, visited);
    }
}
