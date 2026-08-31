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
} // namespace v1

namespace v2 {
class Runtime;

struct ReturnStubs
{
    // Reads error code from register, reports it, then jumps to
    // return_reported_error.
    void* report_error = nullptr;

    // Reports a timeout, notifies watchdog, then returns.
    void* throw_timeout = nullptr;

    // Hardcoded entry paths that load a specific error code and jump to report_error.
    void* throw_error_code[SP_MAX_ERROR_CODES] = {};

    // Unwinds the stack after an error and returns from invoke.
    void* return_reported_error = nullptr;

    // Reports a detailed out-of-bounds error.
    void* bounds_error = nullptr;

    // Dispatches a deferred error with a proper exit frame.
    void* deferred_error = nullptr;
};
} // namespace v2

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
    const v2::ReturnStubs& return_stubs_v2() const {
        return return_stubs_v2_;
    }
    void* DeallocStub() const { return dealloc_stub_.entry; }

  private:
#if defined(SP_JIT_V1)
    bool CompileInvokeStubV1();
#endif
#if defined(SP_JIT_V2)
    bool CompileInvokeStubV2();
    bool CompileDeallocStub();
#endif

  private:
    Environment* env_;
    LinkedCode invoke_stub_v1_;
    LinkedCode invoke_stub_v2_;
    void* return_stub_; // Owned by invoke_stub_v1_.
    v2::ReturnStubs return_stubs_v2_;
    LinkedCode dealloc_stub_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_code_stubs_h_
