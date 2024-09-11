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
        struct State;

        const Module* CompileModule();
        bool HandleImport(const AstNodeImport& imp);

        const Module* CreateBuiltins();

        void BuildStruct(TypeStruct& type, const AstNodeStructDecl& ast);
        void BuildMessage(TypeMessage& type, const AstNodeMessageDecl& ast);
        void BuildAttribute(TypeAttribute& type, const AstNodeAttributeDecl& ast);
        void BuildEnum(TypeEnum& type, const AstNodeEnumDecl& ast);

        void BuildFields(std::span<const Field>& out, const Type* owner, Array<const AstNodeField*> fields);
        void BuildAnnotations(std::span<const Annotation* const>& out, Array<const AstNodeAnnotation*> ast);
        void BuildArguments(std::span<const Argument>& out, const Type* type, const std::span<const Field>& fields, const TypeStruct* baseType, Array<const AstNode*> ast);

        const ValueBool* BuildBool(const AstNodeLiteralBool& lit);
        const ValueInt* BuildInteger(const AstNodeLiteralInt& lit);
        const ValueFloat* BuildFloat(const AstNodeLiteralFloat& lit);
        const ValueNull* BuildNull(const AstNodeLiteralNull& lit);
        const ValueString* BuildString(const AstNodeLiteralString& lit);
        const Value* BuildExpression(const Type* type, const AstNode& expr);
        const Value* BuildQualifiedId(const AstNodeQualifiedId& id);
        const ValueArray* BuildArray(const Type* type, const AstNodeInitializerList& expr);
        const ValueObject* BuildObject(const TypeStruct* type, const AstNodeInitializerList& expr);

        const Type* Resolve(const AstQualifiedName& name);
        const Type* Resolve(const AstNodeType* type);

        bool IsReserved(const char* ident) const noexcept;

        template <typename... Args>
        void Error(std::uint32_t tokenIndex, fmt::format_string<Args...> format, const Args&... args);

        template <typename T>
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
