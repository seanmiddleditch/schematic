// Schematic. Copyright (C) Sean Middleditch and contributors.

#include "schematic/compiler.h"

#include "compile.h"
#include "resolver.h"

using namespace potato::schematic;
using namespace potato::schematic::compiler;

struct potato::schematic::Compiler::Impl final : Logger
    , Resolver
{
    ArenaAllocator arena;
    Compiler* compiler = nullptr;
    bool useBuiltins = false;

    void Error(const LogLocation& location, std::string_view message) override;
    const Source* ResolveModule(std::string_view name, const Source* referrer) override;
};

potato::schematic::Compiler::Compiler()
    : impl_(new Impl)
{
    impl_->compiler = this;
}

potato::schematic::Compiler::~Compiler()
{
    delete impl_;
    impl_ = nullptr;
}

void potato::schematic::Compiler::AddBuiltins()
{
    impl_->useBuiltins = true;
}

const Schema* potato::schematic::Compiler::Compile(const std::filesystem::path& filename)
{
    const Source* const source = LoadModule(filename);
    return potato::schematic::compiler::Compile(*impl_, *impl_, impl_->arena, source, { .builtins = impl_->useBuiltins });
}

void potato::schematic::Compiler::Impl::Error(const LogLocation& location, std::string_view message)
{
    compiler->Error(location, message);
}

const Source* potato::schematic::Compiler::Impl::ResolveModule(std::string_view name, const Source* referrer)
{
    return compiler->ResolveModule(name, referrer);
}
