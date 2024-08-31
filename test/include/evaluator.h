// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include "schematic/schema.h"

#include <string>
#include <string_view>
#include <type_traits>

namespace potato::schematic::test
{
    class CheckEvaluator
    {
    public:
        explicit CheckEvaluator(std::string_view expression, std::string_view filename, std::size_t line);

        const std::string& Expression() const noexcept { return expression_; }

        void Check(const Schema* schema);

    private:
        template <typename T>
        struct CatchFailedExpression;

        struct MatchIndexResult;
        struct Pos
        {
            std::size_t start = std::string::npos;
            std::size_t end = std::string::npos;
        };

        MatchIndexResult MatchIndex(std::size_t max);
        bool Match(std::string_view name);

        template <typename T>
        bool DispatchOp(T value);
        template <typename T>
        void Finish(T value);

        template <typename T>
        void Evaluate(T value)
            requires(!std::is_convertible_v<T, const Type*> && !std::is_convertible_v<T, const Value*>)
        {
            Finish(value);
        }

        void Evaluate(Span<const Annotation*> annotations);

        void Evaluate(const Annotation* annotation);
        void Evaluate(const Field* field);
        void Evaluate(const EnumItem* item);
        void Evaluate(const Argument* arg);
        void Evaluate(const Type* type);
        void Evaluate(const Value* value);
        void Evaluate(const Module* mod);
        void Evaluate(const Schema* schema);

        void Advance();

        // Source
        std::string filename_;
        std::size_t line_ = 1;

        // Inputs
        std::string expression_;
        Pos components_;
        Pos op_;
        Pos reference_;

        // State
        std::string_view next_;
        std::size_t nextPos_ = 0;
    };
} // namespace potato::schematic::test
