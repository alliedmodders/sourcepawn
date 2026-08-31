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
#include "handle.h"
#include "heap.h"
#include "scripted-invoker.h"
#include "smx-image.h"
#include "type-cache.h"

namespace sp {

struct SpArray;

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
    Runtime(std::shared_ptr<SmxImage> image, bool data_only = false);
    ~Runtime();

    bool Initialize() override;
    Runtime* AsV2() override { return this; }

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
    int ParamToArrayPtr(cell_t base, ARRAY_PTR* out) override;
    void* GetArrayData(ARRAY_PTR handle, uint32_t* size = nullptr) override;
    int LocalToArrayPtr(cell_t addr, ARRAY_PTR* out) override;
    bool InvokeMethod(uint32_t method_index, const cell_t* params, unsigned int num_params,
                      cell_t* result);
    bool IsInExec() override;

  public:
    ke::RefPtr<BaseMethodInfo> GetMethodFromFrameId(uint32_t frame_id) const override;
    ke::RefPtr<BaseMethodInfo> GetMethodByIndex(uint32_t method_index) const;
    RefPtr<MethodInfo> AcquireMethod(uint32_t method_index);
    const TypeDesc* LoadMethodSignature(uint32_t method_index);
    const TypeDesc* LoadClosureType(uint32_t method_index);
    const TypeDesc* LoadFunctionSignature(FastRtti& parser, bool is_native);
    const std::vector<RefPtr<MethodInfo>>& AllMethods() const;

    ScriptedInvoker* GetScriptedInvoker(funcid_t func_id);
    ScriptedInvoker* GetFunctionByMethodIndex(uint32_t method_index);
    bool GetNativeIndex(uint32_t method_index, uint32_t* index) const;
    uint32_t GetGlobalAddr(uint16_t index) const { return global_vars_[index].addr; }
    const TypeDesc* GetTypeOfGlobal(uint16_t index);
    uint32_t GetStringAddr(uint16_t index) const { return string_addrs_[index]; }
    const TypeDesc* GetStringLitType(uint16_t index);

    const TypeDesc* LoadType(FastRtti& rtti);
    const TypeDesc* LoadArgType(FastRtti& rtti);
    const TypeDesc* LoadTypeFromId(uint32_t type_id);
    const TypeDesc* GetReferenceType(const TypeDesc* td);
    const TypeDesc* GetArrayType(const TypeDesc* elt);
    const TypeDesc* GetFixedArrayType(const TypeDesc* elt, uint32_t size);
    const TypeDesc* GetFlatArrayType(const TypeDesc* elt, uint32_t size);
    const TypeDesc* GetSliceType(const TypeDesc* elt);
    const TypeDesc* GetPrimitiveType(TypeKind kind);
    const TypeDesc* GetClassdefType(const smx_rtti_classdef* classdef);
    uint32_t AllocStringBlobFromData(uint32_t data_offset);
    uint32_t AllocateGlobal(const TypeDesc* td);

    Handle<SpFunction> CastFunctionId(funcid_t func_id, const TypeDesc* td);
    Handle<SpArray> NewArray(const TypeDesc* td, uint32_t size);
    Handle<SpObject> NewObject(const TypeDesc* td);
    Handle<SpArray> NewBulkArray(const TypeDesc* td, uint8_t dims, cell_t* sizes);
    void FillArray(SpArray* array, uint32_t data_offset);
    bool CopyArrayFlatA(cell_t src_addr, cell_t dest_addr, uint32_t count);
    bool CopyArrayOfObjects(cell_t src_addr, cell_t dest_addr);
    void FillFlatArray(cell_t local_addr, const TypeDesc* td, uint32_t data_offset);
    void* GetArrayElem(SpArray* array, uint32_t index);
    Handle<SpArray> NewSlice(SpArray* array, uint32_t index);
    Handle<SpArray> NewSliceEs(uint32_t data, uint32_t size);
    Handle<SpArray> NewFlatSlice(cell_t local_addr, const TypeDesc* td, uint32_t index);
    Handle<SpFunction> NewClosure(const TypeDesc* td, MethodInfo* method);

    NativeEntry* NativeAt(size_t index) { return &natives_[index]; }
    Runtime* context() const { return const_cast<Runtime*>(this); }
    Runtime* runtime() const { return const_cast<Runtime*>(this); }

    size_t HeapSize() const;
    size_t DataSize() const;

    static inline size_t offsetOfRuntime() { return 0; /* Deprecated, Runtime is self */ }
    static inline size_t offsetOfEnv() { return offsetof(Runtime, env_); }

    bool data_only() const { return data_only_; }

    Heap& heap() { return heap_; }

    Environment* env() const { return env_; }

  private:
    bool InitializeContext();
    bool InitializeGlobals();

  private:
    Environment* env_;
    // Must be declared before any RawHeapPtr members to ensure they are destroyed
    // before the heap itself is destroyed.
    Heap heap_;
    std::vector<NativeEntry> natives_;
    std::unordered_map<uint32_t, uint32_t> native_map_;
    struct PubvarEntry {
        sp_pubvar_t pubvar;
        uint32_t global_index;
        cell_t local_addr;
        bool resolved = false;
    };
    void ResolvePubvar(PubvarEntry& entry);
    std::vector<PubvarEntry> pubvars_;
    std::vector<sp_public_t> publics_;
    std::vector<std::unique_ptr<ScriptedInvoker>> entrypoints_;
    std::vector<RefPtr<MethodInfo>> methods_;

    struct GlobalDesc {
        const TypeDesc* td = nullptr;
        uint32_t addr = 0;
    };
    RawHeapPtr<uint8_t[]> global_buffer_;
    ke::FixedArray<GlobalDesc> global_vars_;
    ke::FixedArray<uint32_t> string_addrs_;

    // Keep objects alive while being allocated for legacy native calls.
    std::vector<std::vector<Handle<SpArray>>> heap_scopes_;

    bool paused_ = false;
    bool data_only_ = false;
    bool computed_code_hash_ = false;
    bool computed_data_hash_ = false;
    unsigned char code_hash_[16];
    unsigned char data_hash_[16];

    cell_t* m_pNullVec = nullptr;
    cell_t* m_pNullString = nullptr;
};

} // namespace v2
} // namespace sp
