// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2006-2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "code-stubs.h"
#include "environment.h"

using namespace sp;

CodeStubs::CodeStubs(Environment* env)
 : env_(env),
   return_stub_(nullptr)
{
}

bool CodeStubs::Initialize() {
#if defined(SP_JIT_V1)
    if (!CompileInvokeStubV1())
        return false;
#endif
#if defined(SP_JIT_V2)
    if (!CompileInvokeStubV2())
        return false;
    if (!CompileDeallocStub())
        return false;
#endif
    return true;
}
