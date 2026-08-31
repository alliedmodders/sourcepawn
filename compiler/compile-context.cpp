// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006

#include "compile-context.h"

#include <assert.h>

#include "compile-options.h"
#include "errors.h"
#include "scopes.h"
#include "source-manager.h"
#include "symbols.h"
#include "types.h"

namespace sp {
namespace cc {

CompileContext* CompileContext::sInstance = nullptr;

CompileContext::CompileContext()
  : globals_(nullptr)
{
    assert(!sInstance);
    sInstance = this;
    default_include_ = sDEF_PREFIX;

    reports_ = std::make_unique<ReportManager>(*this);
    options_ = std::make_unique<CompileOptions>();
    sources_ = std::make_unique<SourceManager>(*this);
    types_ = std::make_unique<TypeManager>(*this);
    types_->init();
}

CompileContext::~CompileContext()
{
    sInstance = nullptr;
}

void
CompileContext::CreateGlobalScope()
{
    globals_ = new SymbolScope(nullptr, sGLOBAL);
}

void
CompileContext::InitLexer()
{
    lexer_ = std::make_shared<Lexer>(*this);
}

tr::unordered_map<Atom*, Decl*>* CompileContext::NewSymbolMap() {
    symbol_maps_.emplace_front();
    return &symbol_maps_.front();
}

tr::unordered_map<Atom*, Type*>* CompileContext::NewTypeMap() {
    type_maps_.emplace_front();
    return &type_maps_.front();
}

void CompileContext::TrackMalloc(size_t bytes) {
    malloc_bytes_ += bytes;
    malloc_bytes_peak_ = std::max(malloc_bytes_peak_, malloc_bytes_);
}

void CompileContext::TrackFree(size_t bytes) {
    malloc_bytes_ -= bytes;
}

void* NativeAllocator::Malloc(size_t n) {
    void* p = malloc(n);
    if (!p)
        return nullptr;
    if (auto* cc = CompileContext::maybe_get())
        cc->TrackMalloc(n);
    return p;
}

void NativeAllocator::Free(void* p, size_t n) {
    if (auto* cc = CompileContext::maybe_get())
        cc->TrackFree(n);
    free(p);
}

} // namespace cc
} // namespace sp
