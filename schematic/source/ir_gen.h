// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ast.h"
#include "ir.h"
#include "location.h"
#include "token.h"

#include "schematic/schema.h"

#include <fmt/core.h>

namespace schematic::compiler
{
    struct IRState final
    {
        Array<IRModule*> modules;
        Array<IRModule*> stack;
        Array<IRModule*> preambles;
        IRSchema* schema = nullptr;
    };

    class IRGenerator final
    {
    public:
        explicit IRGenerator(ArenaAllocator& arena, CompileContext& ctx, IRState& state, std::string_view filename) noexcept
            : arena_(arena)
            , ctx_(ctx)
            , state_(state)
            , filename_(filename)
        {
        }

        IRSchema* CompileRoot();
        bool CompilePreamble();

    private:
        IRModule* CompileModule();
        bool CompileDecls();

        IRVersionRange ReadVersion(const AstNodeLiteralInt* min, const AstNodeLiteralInt* max);

        void ValidateTypeUnique(IRType* type);

        IRType* FindType(const char* name);

        IRType* LowerType(const AstNode* ast);
        IRType* ResolveType(IRType* type);
        IRType* ResolveAlias(IRType* type);
        void ResolveAttributes(Array<IRAnnotation*> annotations);

        Array<IRAnnotation*> LowerAnnotations(Array<const AstNodeAnnotation*> astNodes);

        IRValue* LowerValue(const AstNode* node);
        IRValue* ResolveValue(IRType* type, IRValue* value);

        void AssignIndices(Array<IRAnnotation*> annotations, IRSchema* schema);
        void AssignIndices(IRType* type, IRSchema* schema);
        void AssignIndices(IRValue* value, IRSchema* schema);
        void AssignIndices(IRModule* module, IRSchema* schema);

        Location GetLocation(const AstNode* node);

        template <typename... Args>
        void Error(const AstNode* node, fmt::format_string<Args...> format, Args&&... args);

        ArenaAllocator& arena_;
        CompileContext& ctx_;
        IRState& state_;

        std::string_view filename_;
        std::string_view source_;

        IRModule* builtins_ = nullptr;
        const AstNodeModule* ast_ = nullptr;
        Array<Token> tokens_;

        IRModule* module_ = nullptr;
        bool failed_ = false;
    };
} // namespace schematic::compiler
