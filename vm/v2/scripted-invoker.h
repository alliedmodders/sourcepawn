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
#pragma once

#include <memory>
#include <string>

#include <amtl/am-refcounting.h>
#include <sp_vm_api.h>

namespace sp {
class CompiledFunction;
class SmxImage;
class Environment;
}
namespace sp::v2 {

using namespace ke;
using namespace SourcePawn;

class Runtime;
class MethodInfo;

class ScriptedInvoker : public IPluginFunction
{
  public:
    ScriptedInvoker(Runtime* pRuntime, uint32_t method_index);
    virtual ~ScriptedInvoker();

  public:
    int PushCell(cell_t cell) override;
    int PushCellByRef(cell_t* cell, int flags) override;
    int PushFloat(float number) override;
    int PushFloatByRef(float* number, int flags) override;
    int PushArray(cell_t* inarray, unsigned int cells, int copyback) override;
    int PushString(const char* string) override;
    int PushStringEx(char* buffer, size_t length, int sz_flags, int cp_flags) override;
    int PushInt64(int64_t value) override;
    int Execute(cell_t* result) override;
    void Cancel() override;
    IPluginContext* GetParentContext() override;
    IPluginRuntime* GetParentRuntime() override;
    bool Invoke(cell_t* result) override;
    bool IsRunnable() override;
    funcid_t GetFunctionID() override;
    const char* DebugName() override;
    bool Invoke(const sp::CallArgs& args, cell_t* rval = nullptr) override;

  public:

    // Helper for pRuntime->AcquireMethod that caches the result.
    RefPtr<MethodInfo> AcquireMethod();

  private:
    Environment* env_;
    Runtime* context_;
    uint32_t method_index_;
    CallArgs default_args_;
    std::string debug_name_;
    RefPtr<MethodInfo> method_;
};

} // namespace sp::v2
