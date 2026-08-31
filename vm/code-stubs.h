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
#ifndef _include_sourcepawn_vm_code_stubs_h_
#define _include_sourcepawn_vm_code_stubs_h_

#include <stdint.h>

#include <sp_vm_api.h>
#include "code-allocator.h"
#include "linking.h"

namespace sp {

namespace v1 {
class PluginRuntime;
}
namespace v2 {
class Runtime;
}

class Environment;

typedef int (*InvokeStubV1Fn)(v1::PluginRuntime* cx, void* code, cell_t* rval);
typedef int (*InvokeStubV2Fn)(v2::Runtime* cx, void* code, cell_t* rval);

class CodeStubs
{
  public:
    CodeStubs(Environment* env);

  public:
    bool Initialize();

    InvokeStubV1Fn InvokeStubV1() const {
        return (InvokeStubV1Fn)invoke_stub_v1_.entry;
    }
    InvokeStubV2Fn InvokeStubV2() const {
        return (InvokeStubV2Fn)invoke_stub_v2_.entry;
    }
    void* ReturnStub() const {
        return return_stub_;
    }

  private:
#if defined(SP_HAS_JIT)
    bool CompileInvokeStubV1();
    bool CompileInvokeStubV2();
#endif

  private:
    Environment* env_;
    LinkedCode invoke_stub_v1_;
    LinkedCode invoke_stub_v2_;
    void* return_stub_; // Owned by invoke_stub_v1_.
};

} // namespace sp

#endif // _include_sourcepawn_vm_code_stubs_h_
