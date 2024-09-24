// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ast.h"
#include "lexer.h"
#include "location.h"

#include "schematic/schema.h"

#include <fmt/core.h>

namespace potato::schematic::compiler
{
    struct CompilerState
    {
        Array<const Module*> modules;
        Array<const Module*> stack;
        const Module* builtins = nullptr;
    };

    class Generator final
    {
    public:
        explicit Generator(ArenaAllocator& arena, Logger& logger, CompileContext& ctx, CompilerState& state) noexcept
            : arena_(arena)
            , logger_(logger)
            , ctx_(ctx)
            , state_(state)
        {
        }

        const Module* Compile(std::string_view filename, std::string_view source);

    private:
        struct AnnotationInfo;
        struct EnumItemInfo;
        struct FieldInfo;
        struct StructInfo;
        struct TypeInfo;

        template <typename T>
            requires std::is_base_of_v<Type, T>
        struct BuildTypeInfoResult
        {
            T* type = nullptr;
            TypeInfo* info = nullptr;
        };

        void PassImports();
        void PassBuildTypeInfos();
        void PassResolveBaseTypes();
        void PassResolveFieldTypes();
        void PassResolveAnnotations();
        void PassAssignEnumItemValues();
        void PassStructAliases();

        const Module* CreateBuiltins();

        template <typename T, typename A>
            requires std::is_base_of_v<Type, T> && std::is_base_of_v<AstNodeDecl, A>
        BuildTypeInfoResult<T> BuildTypeInfo(const A* node);
        template <typename T>
            requires std::is_base_of_v<Type, T>
        void BuildFieldInfos(T* type, TypeInfo* info, const std::span<const AstNodeField*> fieldNodes, std::int64_t version = 0);
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

        ArenaAllocator& arena_;
        Logger& logger_;
        CompileContext& ctx_;
        CompilerState& state_;

        const AstNodeModule* ast_ = nullptr;
        std::string_view source_;
        Array<Token> tokens_;

        bool failed_ = false;

        Module* module_ = nullptr;
        Array<const Module*> imports_;
        Array<const Type*> types_;

        Array<AnnotationInfo*> annotationItemInfos_;
        Array<EnumItemInfo*> enumItemInfos_;
        Array<FieldInfo*> fieldInfos_;
        Array<StructInfo*> structInfos_;
        Array<TypeInfo*> typeInfos_;
    };
} // namespace potato::schematic::compiler
