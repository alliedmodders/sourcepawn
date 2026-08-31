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

#include <amtl/am-fixedarray.h>
#include <amtl/am-hashmap.h>
#include <amtl/am-inlinelist.h>
#include <amtl/am-refcounting.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <sp_vm_api.h>
#include "base-runtime.h"
#include "heap-defaults.h"
#include "smx-image.h"
#include "v2/scripted-invoker.h"

namespace sp {
namespace v2 {

using namespace ke;

class MethodInfo;

struct NativeEntry : public sp_native_t {
    NativeEntry() : legacy_fn(nullptr) {}
    SPVM_NATIVE_FUNC legacy_fn;
    RefPtr<SourcePawn::INativeCallback> callback;
};

static const size_t SP_MAX_RETURN_STACK = 1024;
static const cell_t STACK_MARGIN = 64; // 16 parameters of safety, I guess

class Runtime final : public BaseRuntime,
                      public ke::InlineListNode<Runtime>
{
  public:
    Runtime(SmxImage* image);
    ~Runtime();

    bool Initialize() override;

    bool CallGlobalCtor() override;

  public: // IPluginRuntime
    bool IsDebugging() override;
    int FindNativeByName(const char* name, uint32_t* index) override;
    int GetNativeByIndex(uint32_t index, sp_native_t** native);
    uint32_t GetNativesNum() override;
    int FindPublicByName(const char* name, uint32_t* index) override;
    int GetPublicByIndex(uint32_t index, sp_public_t** publicptr) override;
    uint32_t GetPublicsNum() override;
    int GetPubvarByIndex(uint32_t index, sp_pubvar_t** pubvar) override;
    int FindPubvarByName(const char* name, uint32_t* index) override;
    int GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr) override;
    uint32_t GetPubVarsNum() override;
    IPluginFunction* GetFunctionByName(const char* public_name) override;
    IPluginFunction* GetFunctionById(funcid_t func_id) override;
    size_t GetMemUsage() override;
    int UpdateNativeBinding(uint32_t index, SPVM_NATIVE_FUNC pfn, uint32_t flags,
                            void* data) override;
    int UpdateNativeBindingObject(uint32_t index, INativeCallback* callback, uint32_t flags,
                                  void* data) override;
    const sp_native_t* GetNative(uint32_t index) override;
    bool PerformFullValidation() override;
    bool UsesDirectArrays() override;
    void InstallBuiltinNatives() override {}

  public: // IPluginContext
    int LocalToPhysAddr(cell_t local_addr, cell_t** phys_addr) override;
    int LocalToString(cell_t local_addr, char** addr) override;
    int StringToLocal(cell_t local_addr, size_t chars, const char* source) override;
    int StringToLocalUTF8(cell_t local_addr, size_t maxbytes, const char* source,
                          size_t* wrtnbytes) override;
    cell_t* GetNullRef(SP_NULL_TYPE type) override;
    int LocalToStringNULL(cell_t local_addr, char** addr) override;
    int LocalToArrayPtr(cell_t base, ARRAY_PTR* out) override;
    void* GetArrayData(ARRAY_PTR handle, uint32_t* size = nullptr) override;
    IPluginRuntime* GetRuntime() override { return this; }
    cell_t* GetLocalParams() override;
    bool HeapAlloc2dArray(unsigned int length, unsigned int stride, cell_t* local_addr,
                          const cell_t* init) override;
    void EnterHeapScope() override;
    void LeaveHeapScope() override;
    cell_t GetNullFunctionValue() override;
    bool IsNullFunctionId(funcid_t func) override;
    bool GetFunctionByIdOrNull(funcid_t func, IPluginFunction** out) override;
    IPluginFunction* GetFunctionByIdOrError(funcid_t func_id) override;
    bool InvokeMethod(uint32_t method_index, const cell_t* params, unsigned int num_params, cell_t* result);
    bool IsInExec() override;

    int AllocArray(unsigned int cells, cell_t* local_addr, cell_t** phys_addr);
    bool Invoke(funcid_t fnid, const cell_t* params, unsigned int num_params, cell_t* result);

  public:
    ke::RefPtr<BaseMethodInfo> GetMethodFromFrameId(uint32_t frame_id) const override;
    ke::RefPtr<BaseMethodInfo> GetMethodByIndex(uint32_t method_index) const;
    RefPtr<MethodInfo> AcquireMethod(uint32_t method_index);
    const std::vector<RefPtr<MethodInfo>>& AllMethods() const;

    ScriptedInvoker* GetScriptedInvoker(funcid_t func_id);
    ScriptedInvoker* GetFunctionByMethodIndex(uint32_t method_index);
    bool GetNativeIndex(uint32_t method_index, uint32_t* index) const;

    NativeEntry* NativeAt(size_t index) { return &natives_[index]; }
    Runtime* context() const { return const_cast<Runtime*>(this); }
    Runtime* runtime() const { return const_cast<Runtime*>(this); }

    size_t HeapSize() const;
    size_t DataSize() const;

    static inline size_t offsetOfSp() { return offsetof(Runtime, sp_); }
    static inline size_t offsetOfRuntime() { return 0; /* Deprecated, Runtime is self */ }
    static inline size_t offsetOfMemory() { return offsetof(Runtime, memory_); }
    static inline size_t offsetOfHpScope() { return offsetof(Runtime, hp_scope_); }

    uint32_t& sp() { return sp_; }
    uint32_t& hp_scope() { return hp_scope_; }

    HeapImpl& heap() { return heap_; }

    struct HeapScope {
        HeapImpl::Position pos;
        uint32_t prev_hp_scope;
    };

    bool enterHeapScope();
    void leaveHeapScope();

    int generateArray(cell_t dims, cell_t* stk, bool autozero);
    int generateFullArray(uint32_t argc, cell_t* argv, int autozero);

    bool pushHeap(cell_t value);
    bool popHeap(cell_t* out);
    bool addStack(cell_t amount);
    bool getCellValue(cell_t address, cell_t* out);
    bool setCellValue(cell_t address, cell_t value);
    bool heapAlloc(cell_t amount, cell_t* out);
    cell_t* heapAllocEx(cell_t amount, cell_t* out);
    cell_t* acquireAddrRange(cell_t address, uint32_t bounds);
    bool initArray(cell_t array_addr, cell_t dat_addr, cell_t iv_size, cell_t data_copy_size,
                   cell_t data_fill_size, cell_t fill_value);

    int64_t* acquireInt64Addr(cell_t address) {
        cell_t* addr = acquireAddrRange(address, sizeof(int64_t));
        if (!addr)
            return nullptr;
        return reinterpret_cast<int64_t*>(addr);
    }

  private:
    bool InitializeContext();
    bool InitializeGlobals();

  private:
    std::vector<NativeEntry> natives_;
    std::unordered_map<uint32_t, uint32_t> native_map_;
    std::unique_ptr<sp_pubvar_t[]> pubvars_;
    std::vector<sp_public_t> publics_;
    std::vector<std::unique_ptr<ScriptedInvoker>> entrypoints_;
    std::vector<RefPtr<MethodInfo>> methods_;
    ke::FixedArray<uint32_t> global_addrs_;

    bool paused_ = false;
    bool computed_code_hash_ = false;
    bool computed_data_hash_ = false;
    unsigned char code_hash_[16];
    unsigned char data_hash_[16];

    HeapImpl heap_;
    uint8_t* memory_ = nullptr;
    uint32_t data_size_;
    cell_t* m_pNullVec = nullptr;
    cell_t* m_pNullString = nullptr;
    uint32_t sp_base_ = 0;
    uint32_t sp_top_ = 0;
    uint32_t sp_ = 0;
    uint32_t hp_scope_ = 0;
};

} // namespace v2
} // namespace sp
