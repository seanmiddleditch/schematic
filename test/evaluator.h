// Schematic. Copyright (C) Sean Middleditch and contributors.

#pragma once

#include <string>
#include <string_view>

#include "schematic/schema.h"

namespace potato::schematic::test
{
    class CheckEvaluator
    {
    public:
        explicit CheckEvaluator(std::string_view test, std::string_view filename, std::size_t line);

        void Check(const Schema* schema);

    private:
        template <typename T>
        struct CatchFailedExpression;

        struct OpIs;
        struct OpIsA;

        bool Match(std::string_view name);
        bool IsEnd();

        void Fail();
        
        template <typename T>
        void Finish(T value);

        template <typename T>
        void Evaluate(T value);

        void Evaluate(const Field* field);
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
        std::string test_;
        std::string expression;
        std::string op;
        std::string reference;

        // State
        std::string_view remaining;
        std::string_view next;
    };
} // namespace potato::schematic::test
