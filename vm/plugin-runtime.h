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

#include <sp_vm_api.h>
#include <amtl/am-vector.h>
#include <amtl/am-string.h>
#include <amtl/am-inlinelist.h>
#include <amtl/am-hashmap.h>
#include <amtl/am-refcounting.h>
#include "scripted-invoker.h"
#include "legacy-image.h"

namespace sp {

using namespace ke;

class PluginContext;
class MethodInfo;

struct NativeEntry : public sp_native_t
{
  NativeEntry()
   : legacy_fn(nullptr)
  {}
  SPVM_NATIVE_FUNC legacy_fn;
  RefPtr<SourcePawn::INativeCallback> callback;
};

/* Jit wants fast access to this so we expose things as public */
class PluginRuntime
  : public SourcePawn::IPluginRuntime,
    public SourcePawn::IPluginDebugInfo,
    public ke::InlineListNode<PluginRuntime>
{
 public:
  PluginRuntime(LegacyImage* image);
  ~PluginRuntime();

  bool Initialize();

 public:
  virtual bool IsDebugging() override;
  virtual IPluginDebugInfo* GetDebugInfo() override;
  virtual int FindNativeByName(const char* name, uint32_t* index) override;
  virtual int GetNativeByIndex(uint32_t index, sp_native_t** native) override;
  virtual uint32_t GetNativesNum() override;
  virtual int FindPublicByName(const char* name, uint32_t* index) override;
  virtual int GetPublicByIndex(uint32_t index, sp_public_t** publicptr) override;
  virtual uint32_t GetPublicsNum() override;
  virtual int GetPubvarByIndex(uint32_t index, sp_pubvar_t** pubvar) override;
  virtual int FindPubvarByName(const char* name, uint32_t* index) override;
  virtual int GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr) override;
  virtual uint32_t GetPubVarsNum() override;
  virtual IPluginFunction* GetFunctionByName(const char* public_name) override;
  virtual IPluginFunction* GetFunctionById(funcid_t func_id) override;
  virtual IPluginContext* GetDefaultContext() override;
  virtual int ApplyCompilationOptions(ICompilation* co) override;
  virtual void SetPauseState(bool paused) override;
  virtual bool IsPaused() override;
  virtual size_t GetMemUsage() override;
  virtual unsigned char* GetCodeHash() override;
  virtual unsigned char* GetDataHash() override;
  void SetNames(const char* fullname, const char* name);
  unsigned GetNativeReplacement(size_t index);
  ScriptedInvoker* GetPublicFunction(size_t index);
  int UpdateNativeBinding(uint32_t index, SPVM_NATIVE_FUNC pfn, uint32_t flags, void* data) override;
  int UpdateNativeBindingObject(uint32_t index, INativeCallback* callback, uint32_t flags,
                                void* data) override;
  const sp_native_t* GetNative(uint32_t index) override;
  int LookupLine(ucell_t addr, uint32_t* line) override;
  int LookupFunction(ucell_t addr, const char** name) override;
  int LookupFile(ucell_t addr, const char** filename) override;
  size_t NumFiles() override;
  const char* GetFileName(size_t index) override;
  int LookupFunctionAddress(const char* function, const char* file, ucell_t* addr) override;
  int LookupLineAddress(const uint32_t line, const char* file, ucell_t* addr) override;
  const char* GetFilename() override {
    return full_name_.c_str();
  }
  bool PerformFullValidation() override;
  bool UsesDirectArrays() override;
  bool UsesHeapScopes();

  // Mark builtin natives as bound.
  virtual void InstallBuiltinNatives();

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

  static PluginRuntime* FromAPI(IPluginRuntime* rt) {
    return static_cast<PluginRuntime*>(rt);
  }

 public:
  typedef LegacyImage::Code Code;
  typedef LegacyImage::Data Data;

  const Code& code() const {
    return code_;
  }
  const Data& data() const {
    return data_;
  }
  LegacyImage* image() const {
    return image_.get();
  }
  PluginContext* context() const {
    return context_.get();
  }

 private:
  void SetupFloatNativeRemapping();

  struct floattbl_t
  {
    floattbl_t() {
      found = false;
      index = 0;
    }
    bool found;
    unsigned int index;
  };

 private:
  std::unique_ptr<sp::LegacyImage> image_;
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
  std::unique_ptr<PluginContext> context_;

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
  std::vector<RefPtr<MethodInfo>> methods_;;

  // Pause state.
  bool paused_;

  // Checksumming.
  bool computed_code_hash_;
  bool computed_data_hash_;
  unsigned char code_hash_[16];
  unsigned char data_hash_[16];
};

} // sp

#endif //_INCLUDE_SOURCEPAWN_JIT_RUNTIME_H_

