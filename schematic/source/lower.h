// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "ast.h"
#include "ir.h"
#include "location.h"
#include "token.h"

#include "schematic/schema.h"

#include <fmt/core.h>

namespace potato::schematic::compiler
{
    struct IRState final
    {
        Array<IRModule*> modules;
        Array<IRModule*> stack;
        IRModule* builtins = nullptr;
    };

    class LowerAstToIr final
    {
    public:
        explicit LowerAstToIr(ArenaAllocator& arena, Logger& logger, CompileContext& ctx, IRState& state, std::string_view filename, std::string_view source) noexcept
            : arena_(arena)
            , logger_(logger)
            , ctx_(ctx)
            , state_(state)
            , filename_(filename)
            , source_(source)
        {
        }

        IRModule* Lower();

    private:
        IRVersionRange ReadVersion(const AstNodeLiteralInt* min, const AstNodeLiteralInt* max);

        void ValidateTypeName(IRType* type);
        void ValidateTypeUnique(IRType* type);
        void ValidateStructField(IRTypeStruct* type, const AstNodeField* field);

        IRModule* CreateBuiltins();

        IRType* LowerType(const AstNode* ast);
        IRType* ResolveType(IRType* type);

        template <typename... Args>
        void Error(const AstNode* node, fmt::format_string<Args...> format, Args&&... args);

        ArenaAllocator& arena_;
        Logger& logger_;
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
} // namespace potato::schematic::compiler
