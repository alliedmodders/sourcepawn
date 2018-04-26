// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include <sp_vm_api.h>
#include "code-stubs.h"
#include "environment.h"

using namespace sp;
using namespace SourcePawn;

bool
CodeStubs::InitializeFeatureDetection()
{
  return true;
}

bool
CodeStubs::CompileInvokeStub()
{
  return true;
}

SPVM_NATIVE_FUNC
CodeStubs::CreateFakeNativeStub(SPVM_FAKENATIVE_FUNC callback, void* pData)
{
  assert(false);
  return (SPVM_NATIVE_FUNC)nullptr;
}
