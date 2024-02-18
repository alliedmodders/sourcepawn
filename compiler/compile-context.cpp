// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2021
//  Copyright (c) ITB CompuPhase, 1997-2006
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.

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

DefaultArrayData* CompileContext::NewDefaultArrayData() {
    default_array_data_objects_.emplace_front();
    return &default_array_data_objects_.front();
}

tr::vector<tr::string>* CompileContext::NewDebugStringList() {
    debug_strings_.emplace_front();
    return &debug_strings_.front();
}

tr::unordered_map<Atom*, Decl*>* CompileContext::NewSymbolMap() {
    symbol_maps_.emplace_front();
    return &symbol_maps_.front();
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
