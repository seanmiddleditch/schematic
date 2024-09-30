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
    class LowerAstToIr final
    {
    public:
        explicit LowerAstToIr(ArenaAllocator& arena, Logger& logger, CompileContext& ctx, std::string_view filename, std::string_view source) noexcept
            : arena_(arena)
            , logger_(logger)
            , ctx_(ctx)
            , filename_(filename)
            , source_(source)
        {
        }

        const IRModule* Lower();

    private:
        IRVersionRange ReadVersion(const AstNodeLiteralInt* min, const AstNodeLiteralInt* max);

        void ValidateTypeName(IRType* type);
        void ValidateStructField(IRTypeStruct* type, const AstNodeField* field);

        const IRModule* CreateBuiltins();

        IRType* LowerType(const AstNode* ast);

        template <typename... Args>
        void Error(const AstNode* node, fmt::format_string<Args...> format, Args&&... args);

        ArenaAllocator& arena_;
        Logger& logger_;
        CompileContext& ctx_;
        std::string_view filename_;
        std::string_view source_;

        IRModule* builtins_ = nullptr;
        const AstNodeModule* ast_ = nullptr;
        Array<Token> tokens_;

        IRModule* module_ = nullptr;
        bool failed_ = false;
    };
} // namespace potato::schematic::compiler
