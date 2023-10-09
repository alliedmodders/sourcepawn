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
#ifndef _INCLUDE_SOURCEPAWN_V1CONTEXT_H_
#define _INCLUDE_SOURCEPAWN_V1CONTEXT_H_

#include "base-context.h"
#include "scripted-invoker.h"
#include "plugin-runtime.h"

namespace sp {

static const size_t SP_MAX_RETURN_STACK = 1024;
static const cell_t STACK_MARGIN = 64; // 16 parameters of safety, I guess

class Environment;
class PluginContext;

class PluginContext final : public BasePluginContext
{
 public:
  PluginContext(PluginRuntime* pRuntime);
  ~PluginContext() override;

  bool Initialize();

 public: //IPluginContext
  int HeapAlloc(unsigned int cells, cell_t* local_addr, cell_t** phys_addr) override;
  int HeapPop(cell_t local_addr) override;
  int HeapRelease(cell_t local_addr) override;
  int FindNativeByName(const char* name, uint32_t* index) override;
  int GetNativeByIndex(uint32_t index, sp_native_t** native) override;
  uint32_t GetNativesNum() override;
  int FindPublicByName(const char* name, uint32_t* index) override;
  int GetPublicByIndex(uint32_t index, sp_public_t** publicptr) override;
  uint32_t GetPublicsNum() override;
  int GetPubvarByIndex(uint32_t index, sp_pubvar_t** pubvar) override;
  int FindPubvarByName(const char* name, uint32_t* index) override;
  int GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr) override;
  uint32_t GetPubVarsNum() override;
  int LocalToPhysAddr(cell_t local_addr, cell_t** phys_addr) override;
  int LocalToString(cell_t local_addr, char** addr) override;
  int StringToLocal(cell_t local_addr, size_t chars, const char* source) override;
  int StringToLocalUTF8(cell_t local_addr, size_t maxbytes, const char* source, size_t* wrtnbytes) override;
  IPluginFunction* GetFunctionByName(const char* public_name) override;
  IPluginFunction* GetFunctionById(funcid_t func_id) override;
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
  bool Invoke(funcid_t fnid, const cell_t* params, unsigned int num_params, cell_t* result);

  size_t HeapSize() const {
    return mem_size_;
  }
  uint8_t* memory() const {
    return memory_;
  }
  size_t DataSize() const {
    return data_size_;
  }
  PluginRuntime* runtime() const {
    return m_pRuntime;
  }

 public:
  bool IsInExec() override;

  static inline size_t offsetOfSp() {
    return offsetof(PluginContext, sp_);
  }
  static inline size_t offsetOfRuntime() {
    return offsetof(PluginContext, m_pRuntime);
  }
  static inline size_t offsetOfMemory() {
    return offsetof(PluginContext, memory_);
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
  cell_t* acquireAddrRange(cell_t address, uint32_t bounds);
  bool initArray(cell_t array_addr,
                 cell_t dat_addr,
                 cell_t iv_size,
                 cell_t data_copy_size,
                 cell_t data_fill_size,
                 cell_t fill_value);

  cell_t* throwIfBadAddress(cell_t addr);

 private:
  PluginRuntime* m_pRuntime;
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
};

} // namespace sp

#endif //_INCLUDE_SOURCEPAWN_V1CONTEXT_H_
