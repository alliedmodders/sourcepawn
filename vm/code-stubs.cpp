// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
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
