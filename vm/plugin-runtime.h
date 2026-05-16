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
#ifndef _INCLUDE_SOURCEPAWN_JIT_RUNTIME_H_
#define _INCLUDE_SOURCEPAWN_JIT_RUNTIME_H_

#include <amtl/am-hashmap.h>
#include <amtl/am-inlinelist.h>
#include <amtl/am-refcounting.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <sp_vm_api.h>
#include "smx-image.h"
#include "scripted-invoker.h"

namespace sp {

static const cell_t STACK_MARGIN = 16 * sizeof(cell_t);

using namespace ke;

class PluginRuntime;
class MethodInfo;

typedef PluginRuntime PluginContext;

struct NativeEntry : public sp_native_t {
    NativeEntry()
     : legacy_fn(nullptr) {
    }
    SPVM_NATIVE_FUNC legacy_fn;
    RefPtr<SourcePawn::INativeCallback> callback;
};

/* Jit wants fast access to this so we expose things as public */
class PluginRuntime : public SourcePawn::IPluginRuntime,
                      public ke::InlineListNode<PluginRuntime>
{
  public:
    PluginRuntime(SmxImage* image);
    ~PluginRuntime();

    bool Initialize();

  public:
    bool IsDebugging() override;
    int FindNativeByName(const char* name, uint32_t* index) override;
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
    IPluginContext* GetDefaultContext() override;
    void SetPauseState(bool paused) override;
    bool IsPaused() override;
    size_t GetMemUsage() override;
    unsigned char* GetCodeHash() override;
    unsigned char* GetDataHash() override;
    void SetNames(const char* fullname, const char* name);
    unsigned GetNativeReplacement(size_t index);
    ScriptedInvoker* GetPublicFunction(size_t index);
    int UpdateNativeBinding(uint32_t index, SPVM_NATIVE_FUNC pfn, uint32_t flags,
                            void* data) override;
    int UpdateNativeBindingObject(uint32_t index, INativeCallback* callback, uint32_t flags,
                                  void* data) override;
    const sp_native_t* GetNative(uint32_t index) override;
    const char* GetFilename() override {
        return full_name_.c_str();
    }
    bool PerformFullValidation();
    bool UsesDirectArrays() override;
    bool UsesHeapScopes();

    // Mark builtin natives as bound.
    void InstallBuiltinNatives();

    // Return the method if it was previously analyzed; null otherwise.
    RefPtr<MethodInfo> GetMethod(cell_t pcode_offset) const;

    // If there is no method at the given offset, return null. If there is a
    // method, return it.
    RefPtr<MethodInfo> AcquireMethod(cell_t pcode_offset);

    // Return a list of all methods. The caller must own the environment lock.
    const std::vector<RefPtr<MethodInfo>>& AllMethods() const;

    NativeEntry* NativeAt(size_t index) {
        return &natives_[index];
    }

    PluginContext* GetBaseContext();

    const char* Name() const {
        return name_.c_str();
    }

  public: // IPluginContext
    int LocalToPhysAddr(cell_t local_addr, cell_t** phys_addr) override;
    int LocalToString(cell_t local_addr, char** addr) override;
    int StringToLocal(cell_t local_addr, size_t chars, const char* source) override;
    int StringToLocalUTF8(cell_t local_addr, size_t maxbytes, const char* source,
                          size_t* wrtnbytes) override;
    cell_t* GetNullRef(SP_NULL_TYPE type) override;
    int LocalToStringNULL(cell_t local_addr, char** addr) override;
    IPluginRuntime* GetRuntime() override;
    cell_t* GetLocalParams() override;
    bool HeapAlloc2dArray(unsigned int length, unsigned int stride, cell_t* local_addr,
                          const cell_t* init) override;
    void EnterHeapScope() override;
    void LeaveHeapScope() override;
    cell_t GetNullFunctionValue() override;
    bool IsNullFunctionId(funcid_t func) override;
    bool GetFunctionByIdOrNull(funcid_t func, IPluginFunction** out) override;
    IPluginFunction* GetFunctionByIdOrError(funcid_t func_id) override;
    bool IsInExec() override;

    // Generic API access.
    void SetKey(int k, void* value) override;
    bool GetKey(int k, void** value) override;
    SourcePawn::ISourcePawnEngine2* APIv2() override;
    void ReportError(const char* fmt, ...) override;
    void ReportErrorVA(const char* fmt, va_list ap) override;
    void ReportFatalError(const char* fmt, ...) override;
    void ReportFatalErrorVA(const char* fmt, va_list ap) override;
    void ReportErrorNumber(int error) override;
    cell_t ThrowNativeErrorEx(int error, const char* msg, ...) override;
    cell_t ThrowNativeError(const char* msg, ...) override;
    int GetLastNativeError() override;
    cell_t BlamePluginError(SourcePawn::IPluginFunction* pf, const char* msg, ...) override;
    IFrameIterator* CreateFrameIterator() override;
    void DestroyFrameIterator(IFrameIterator* it) override;
    int LocalToArrayPtr(cell_t base, ARRAY_PTR* out) override;
    void* GetArrayData(ARRAY_PTR handle, uint32_t* size = nullptr) override;

  public:
    bool Invoke(funcid_t fnid, const cell_t* params, unsigned int num_params, cell_t* result);

    int AllocArray(unsigned int cells, cell_t* local_addr, cell_t** phys_addr);

    size_t HeapSize() const {
        return mem_size_;
    }
    uint8_t* memory() const {
        return memory_;
    }
    size_t DataSize() const {
        return data_size_;
    }

    static inline size_t offsetOfSp() {
        return offsetof(PluginRuntime, sp_);
    }
    static inline size_t offsetOfHp() {
        return offsetof(PluginRuntime, hp_);
    }
    static inline size_t offsetOfFrm() {
        return offsetof(PluginRuntime, frm_);
    }
    static inline size_t offsetOfMemory() {
        return offsetof(PluginRuntime, memory_);
    }
    static inline size_t offsetOfHpScope() {
        return offsetof(PluginRuntime, hp_scope_);
    }

    int32_t* addressOfSp() {
        return &sp_;
    }
    cell_t* addressOfFrm() {
        return &frm_;
    }
    cell_t* addressOfHp() {
        return &hp_;
    }
    cell_t* addressOfHpScope() {
        return &hp_scope_;
    }

    cell_t frm() const {
        return frm_;
    }
    cell_t sp() const {
        return sp_;
    }
    cell_t hp() const {
        return hp_;
    }

    int popTrackerAndSetHeap();
    int pushTracker(uint32_t amount);

    // Note: this is allowed even in legacy plugins, since the underlying
    // mechanism doesn't actually require opcode support. The heap code
    // support bit only indicates that we should *not* use the tracker.
    bool enterHeapScope();
    bool leaveHeapScope();

    int generateArray(cell_t dims, cell_t* stk, bool autozero);
    int generateFullArray(uint32_t argc, cell_t* argv, int autozero);

    // These functions will report an error on failure.
    bool pushAmxFrame();
    bool popAmxFrame();
    bool pushStack(cell_t value);
    bool popStack(cell_t* out);
    bool pushHeap(cell_t value);
    bool popHeap(cell_t* out);
    bool addStack(cell_t amount);
    bool getFrameValue(cell_t offset, cell_t* out);
    bool setFrameValue(cell_t offset, cell_t value);
    bool getCellValue(cell_t address, cell_t* out);
    bool setCellValue(cell_t address, cell_t value);
    bool heapAlloc(cell_t amount, cell_t* out);
    cell_t* heapAllocEx(cell_t amount, cell_t* out);
    cell_t* acquireAddrRange(cell_t address, uint32_t bounds);
    bool initArray(cell_t array_addr, cell_t dat_addr, cell_t iv_size, cell_t data_copy_size,
                   cell_t data_fill_size, cell_t fill_value);

    cell_t* throwIfBadAddress(cell_t addr);

    int64_t* acquireInt64Addr(cell_t address) {
        cell_t* addr = acquireAddrRange(address, sizeof(int64_t));
        if (!addr)
            return nullptr;
        return reinterpret_cast<int64_t*>(addr);
    }

    int64_t* acquireInt64Slot(cell_t offset) {
        cell_t* addr = throwIfBadAddress(frm_ + offset);
        assert(addr);
        return reinterpret_cast<int64_t*>(addr);
    }

    PluginContext* context() {
        return this;
    }
    PluginRuntime* runtime() {
        return this;
    }

  public:
    typedef SmxImage::Code Code;
    typedef SmxImage::Data Data;

    const Code& code() const {
        return code_;
    }
    const Data& data() const {
        return data_;
    }
    SmxImage* image() const {
        return image_.get();
    }

  private:
    void SetupFloatNativeRemapping();

    struct floattbl_t {
        floattbl_t() {
            found = false;
            index = 0;
        }
        bool found;
        unsigned int index;
    };

  private:
    std::unique_ptr<sp::SmxImage> image_;
    std::unique_ptr<uint8_t[]> aligned_code_;
    std::unique_ptr<floattbl_t[]> float_table_;
    std::string name_;
    std::string full_name_;
    Code code_;
    Data data_;
    std::unique_ptr<NativeEntry[]> natives_;
    std::unique_ptr<sp_public_t[]> publics_;
    std::unique_ptr<sp_pubvar_t[]> pubvars_;
    std::unique_ptr<ScriptedInvoker*[]> entrypoints_;

    uint8_t* memory_;
    uint32_t data_size_;
    uint32_t mem_size_;

    cell_t* m_pNullVec;
    cell_t* m_pNullString;

    // "Stack top", for convenience.
    cell_t stp_;

    // Stack, heap, and frame pointer.
    cell_t sp_;
    cell_t hp_;
    cell_t frm_;
    cell_t hp_scope_;

    struct FunctionMapPolicy {
        static inline uint32_t hash(ucell_t value) {
            return ke::HashInteger<4>(value);
        }
        static inline bool matches(ucell_t a, ucell_t b) {
            return a == b;
        }
    };
    typedef ke::HashMap<ucell_t, RefPtr<MethodInfo>, FunctionMapPolicy> FunctionMap;

    FunctionMap function_map_;
    std::vector<RefPtr<MethodInfo>> methods_;

    // Pause state.
    bool paused_;

    // Checksumming.
    bool computed_code_hash_;
    bool computed_data_hash_;
    unsigned char code_hash_[16];
    unsigned char data_hash_[16];

    Environment* env_;
    void* m_keys[4];
    bool m_keys_set[4];
};

} // namespace sp

#endif //_INCLUDE_SOURCEPAWN_JIT_RUNTIME_H_
