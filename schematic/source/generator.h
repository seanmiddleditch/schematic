// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ast.h"
#include "lexer.h"
#include "location.h"

#include "schematic/schema.h"

#include <fmt/core.h>

namespace potato::schematic::compiler
{
    class Generator final
    {
    public:
        explicit Generator(ArenaAllocator& arena, Logger& logger, CompileContext& ctx) noexcept
            : arena(arena)
            , logger_(logger)
            , ctx(ctx)
        {
        }

        const Module* Compile(std::string_view filename, std::string_view source, bool useBuiltins);

    private:
        struct AnnotationInfo;
        struct EnumItemInfo;
        struct FieldInfo;
        struct State;
        struct TypeInfo;

        const Module* CompileModule();
        bool HandleImport(const AstNodeImport& imp);

        const Module* CreateBuiltins();

        template <typename T, typename A>
            requires std::is_base_of_v<Type, T> && std::is_base_of_v<AstNodeDecl, A>
        void BuildAggregateTypeInfo(const A* node);
        void BuildAnnotationInfos(std::span<const Annotation* const>& out, Array<const AstNodeAnnotation*> ast);

        void BuildArguments(std::span<const Argument>& out, const Type* type, const std::span<const Field>& fields, const TypeStruct* baseType, Array<const AstNode*> ast);

        template <typename V, typename A>
            requires std::is_base_of_v<Value, V> && std::is_base_of_v<AstNodeLiteral, A>
        const V* BuildLiteral(const A& node);
        const Value* BuildExpression(const Type* type, const AstNode& expr);
        const Value* BuildIdentValue(const Type* type, const AstNodeIdentifier& id);
        const ValueArray* BuildArray(const Type* type, const AstNodeInitializerList& expr);
        const ValueObject* BuildObject(const TypeStruct* type, const AstNodeInitializerList& expr);

        const Type* TryResolve(const char* name);
        const Type* Resolve(const AstIdentifier& ident);
        const Type* Resolve(const AstNodeType* type);

        bool IsReserved(const char* ident) const noexcept;

        template <typename... Args>
        void Error(std::uint32_t tokenIndex, fmt::format_string<Args...> format, const Args&... args);

        template <typename T>
            requires std::is_base_of_v<Type, T>
        T* CreateType(std::uint32_t tokenIndex, const char* name);

        std::uint16_t TokenLine(std::uint32_t tokenIndex) const noexcept;

        ArenaAllocator& arena;
        Logger& logger_;
        CompileContext& ctx;
        bool result = true;
        const Module* builtins = nullptr;
        const Schema* schema = nullptr;
        Array<State*> stack;
        Array<const Module*> modules;
    };
} // namespace potato::schematic::compiler
