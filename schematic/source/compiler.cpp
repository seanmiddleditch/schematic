// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"

#include "ast.h"
#include "ir_gen.h"
#include "lexer.h"
#include "location.h"
#include "parser.h"
#include "schema_gen.h"
#include "token.h"

#include "schematic/allocator.h"
#include "schematic/schema.h"
#include "schematic/utility.h"

#include <fmt/core.h>

#include <charconv>
#include <climits>
#include <cstdint>
#include <utility>

using namespace schematic;
using namespace schematic::compiler;

namespace
{
    class CompilerImpl final : public Compiler
    {
    public:
        CompilerImpl(ArenaAllocator& arena, Logger& logger, CompileContext& ctx)
            : arena_(arena)
            , logger_(logger)
            , ctx_(ctx)
        {
        }

        void AddStandardPreamble() override;
        void AddPreamble(std::string_view filename) override;
        const Schema* Compile(std::string_view filename) override;

    private:
        template <typename T>
        static T* CreateBuiltinType(ArenaAllocator& arena, IRModule* module, TypeKind kind, const char* name);
        IRModule* CreateStandardPreamble();

        ArenaAllocator& arena_;
        Logger& logger_;
        CompileContext& ctx_;

        Array<const char*> preambles_;
        bool useStandardPreamble_ = false;
    };
} // namespace

Compiler* schematic::NewCompiler(ArenaAllocator& arena, Logger& logger, CompileContext& ctx)
{
    return arena.New<CompilerImpl>(arena, logger, ctx);
}

void CompilerImpl::AddStandardPreamble()
{
    useStandardPreamble_ = true;
}

void CompilerImpl::AddPreamble(std::string_view filename)
{
    preambles_.PushBack(arena_, arena_.NewString(filename));
}

const Schema* CompilerImpl::Compile(std::string_view filename)
{
    IRState state;

    if (useStandardPreamble_)
        state.preambles.PushBack(arena_, CreateStandardPreamble());

    for (const char* const preamble : preambles_)
    {
        IRGenerator preambleCompiler(arena_, logger_, ctx_, state, preamble);
        if (!preambleCompiler.CompilePreamble())
            return nullptr;
    }

    const std::string_view contents = ctx_.ReadFileContents(arena_, filename);
    IRGenerator rootCompiler(arena_, logger_, ctx_, state, filename);
    IRSchema* const irSchema = rootCompiler.CompileRoot();
    if (irSchema == nullptr)
        return nullptr;

    SchemaGenerator schemaGen(arena_, logger_);
    const Schema* const schema = schemaGen.Compile(irSchema);

    return schema;
}

template <typename T>
T* CompilerImpl::CreateBuiltinType(ArenaAllocator& arena, IRModule* module, TypeKind kind, const char* name)
{
    T* const type = arena.New<T>();
    type->name = name;
    type->typeKind = kind;
    type->parent = module;

    module->types.PushBack(arena, type);
    return type;
};

IRModule* CompilerImpl::CreateStandardPreamble()
{
    constexpr std::size_t byteWidthInBits = 8; // we assume target uses expect 8-bit bytes

    IRModule* preamble = arena_.New<IRModule>();
    preamble->filename = "$builtins";

    auto AddInt = [this, preamble]<typename T>(const char* name, T)
    {
        IRTypeBuiltin* const type = CreateBuiltinType<IRTypeBuiltin>(arena_, preamble, TypeKind::Int, name);
        type->isSigned = std::is_signed_v<T>;
        type->width = byteWidthInBits * sizeof(T);
    };
    auto AddFloat = [this, preamble]<typename T>(const char* name, T)
    {
        IRTypeBuiltin* const type = CreateBuiltinType<IRTypeBuiltin>(arena_, preamble, TypeKind::Float, name);
        type->width = byteWidthInBits * sizeof(T);
    };

    CreateBuiltinType<IRTypeBuiltin>(arena_, preamble, TypeKind::Type, "type");
    CreateBuiltinType<IRTypeBuiltin>(arena_, preamble, TypeKind::Bool, "bool");
    CreateBuiltinType<IRTypeBuiltin>(arena_, preamble, TypeKind::String, "string");

    AddInt("int8", std::int8_t{});
    AddInt("uint8", std::uint8_t{});
    AddInt("int16", std::int16_t{});
    AddInt("uint16", std::uint16_t{});
    AddInt("int32", std::int32_t{});
    AddInt("uint32", std::uint32_t{});
    AddInt("int64", std::int64_t{});
    AddInt("uint64", std::uint64_t{});

    AddFloat("float32", float{});
    AddFloat("float64", double{});

    return preamble;
}
