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
#ifndef _INCLUDE_SOURCEMOD_BASEFUNCTION_H_
#define _INCLUDE_SOURCEMOD_BASEFUNCTION_H_

#include <memory>

#include <amtl/am-refcounting.h>
#include <sp_vm_api.h>

namespace sp {

using namespace ke;
using namespace SourcePawn;

class PluginRuntime;
typedef PluginRuntime PluginContext;
class CompiledFunction;
class MethodInfo;

struct ParamInfo {
    int flags;         /* Copy-back flags */
    bool marked;       /* Whether this is marked as being used */
    cell_t local_addr; /* Local address to free */
    cell_t* phys_addr; /* Physical address of our copy */
    cell_t* orig_addr; /* Original address to copy back to */
    ucell_t size;      /* Size of array in bytes */
    struct {
        bool is_sz;   /* is a string */
        int sz_flags; /* has sz flags */
    } str;
};

class ScriptedInvoker : public IPluginFunction
{
  public:
    ScriptedInvoker(PluginRuntime* pRuntime, funcid_t fnid, uint32_t pub_id);
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
    bool Invoke(cell_t* result) override;
    bool IsRunnable() override;
    funcid_t GetFunctionID() override;
    IPluginRuntime* GetParentRuntime() override;
    const char* DebugName() override {
        return full_name_.get();
    }

  public:
    sp_public_t* Public() const {
        return public_;
    }

    // Helper for pRuntime->AcquireMethod that caches the result.
    RefPtr<MethodInfo> AcquireMethod();

  private:
    int _PushString(const char* string, int sz_flags, int cp_flags, size_t len);
    int SetError(int err);

  private:
    Environment* env_;
    PluginContext* context_;
    cell_t m_params[SP_MAX_EXEC_PARAMS];
    ParamInfo m_info[SP_MAX_EXEC_PARAMS];
    unsigned int m_curparam;
    int m_errorstate;
    funcid_t m_FnId;
    std::unique_ptr<char[]> full_name_;
    sp_public_t* public_;
    RefPtr<MethodInfo> method_;
};

} // namespace sp

#endif //_INCLUDE_SOURCEMOD_BASEFUNCTION_H_
