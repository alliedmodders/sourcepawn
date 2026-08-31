// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#ifndef _INCLUDE_SOURCEMOD_BASEFUNCTION_H_
#define _INCLUDE_SOURCEMOD_BASEFUNCTION_H_

#include <memory>

#include <amtl/am-refcounting.h>
#include <sp_vm_api.h>

namespace sp {
class CompiledFunction;
class SmxImage;
}
namespace sp::v1 {

using namespace ke;
using namespace SourcePawn;

class PluginRuntime;
typedef PluginRuntime PluginContext;
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
    IPluginRuntime* GetParentRuntime() override;
    bool Invoke(cell_t* result) override;
    bool IsRunnable() override;
    funcid_t GetFunctionID() override;
    const char* DebugName() override { return full_name_.get(); }
    bool Invoke(const sp::CallArgs& args, cell_t* rval = nullptr) override;

  public:
    sp_public_t* Public() const {
        return public_;
    }

    // Helper for pRuntime->AcquireMethod that caches the result.
    RefPtr<MethodInfo> AcquireMethod();

  private:
    Environment* env_;
    PluginContext* context_;
    CallArgs default_args_;
    funcid_t m_FnId;
    std::unique_ptr<char[]> full_name_;
    sp_public_t* public_;
    RefPtr<MethodInfo> method_;
};

} // namespace sp

#endif //_INCLUDE_SOURCEMOD_BASEFUNCTION_H_
