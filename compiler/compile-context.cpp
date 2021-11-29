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
#include "scvars.h"
#include "symbols.h"

CompileContext* CompileContext::sInstance = nullptr;

CompileContext::CompileContext()
  : globals_(nullptr)
{
    assert(!sInstance);
    sInstance = this;
    default_include_ = sDEF_PREFIX;

    reports_ = std::make_unique<ReportManager>(*this);
    options_ = std::make_unique<CompileOptions>();
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
