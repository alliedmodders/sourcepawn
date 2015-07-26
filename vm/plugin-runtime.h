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
#include <am-vector.h>
#include <am-string.h>
#include <am-inlinelist.h>
#include <am-hashmap.h>
#include "compiled-function.h"
#include "scripted-invoker.h"
#include "legacy-image.h"
#include "native-group.h"
#include "native-registry.h"

namespace sp {

class PluginContext;

struct floattbl_t
{
  floattbl_t() {
    found = false;
    index = 0;
  }
  bool found;
  unsigned int index;
};

struct NativeEntry : public sp_native_t
{
  NativeEntry()
   : binding(nullptr)
  {}
  const NativeDef* binding;
  Ref<NativeGroup::WeakRef> weak_ref;
};

/* Jit wants fast access to this so we expose things as public */
class PluginRuntime final
  : public SourcePawn::IPluginRuntime,
    public SourcePawn::IPluginDebugInfo,
    public ke::InlineListNode<PluginRuntime>
{
 public:
  PluginRuntime(LegacyImage *image);
  ~PluginRuntime();

  bool Initialize();

 public:
  virtual bool IsDebugging();
  virtual IPluginDebugInfo *GetDebugInfo();
  virtual int FindNativeByName(const char *name, uint32_t *index);
  virtual int GetNativeByIndex(uint32_t index, sp_native_t **native);
  virtual uint32_t GetNativesNum();
  virtual int FindPublicByName(const char *name, uint32_t *index);
  virtual int GetPublicByIndex(uint32_t index, sp_public_t **publicptr);
  virtual uint32_t GetPublicsNum();
  virtual int GetPubvarByIndex(uint32_t index, sp_pubvar_t **pubvar);
  virtual int FindPubvarByName(const char *name, uint32_t *index);
  virtual int GetPubvarAddrs(uint32_t index, cell_t *local_addr, cell_t **phys_addr);
  virtual uint32_t GetPubVarsNum();
  virtual IPluginFunction *GetFunctionByName(const char *public_name);
  virtual IPluginFunction *GetFunctionById(funcid_t func_id);
  virtual IPluginContext *GetDefaultContext();
  virtual int ApplyCompilationOptions(ICompilation *co);
  virtual void SetPauseState(bool paused);
  virtual bool IsPaused();
  virtual size_t GetMemUsage();
  virtual unsigned char *GetCodeHash();
  virtual unsigned char *GetDataHash();
  CompiledFunction *GetJittedFunctionByOffset(cell_t pcode_offset);
  void AddJittedFunction(CompiledFunction *fn);
  void SetNames(const char *fullname, const char *name);
  unsigned GetNativeReplacement(size_t index);
  ScriptedInvoker *GetPublicFunction(size_t index);
  const sp_native_t *GetNative(uint32_t index) override;
  int LookupLine(ucell_t addr, uint32_t *line) override;
  int LookupFunction(ucell_t addr, const char **name) override;
  int LookupFile(ucell_t addr, const char **filename) override;
  const char *GetFilename() override {
    return full_name_.chars();
  }
  bool BindNatives(const ke::Ref<INativeRegistry>& registry) override;
  void MarkNativesOptional(const ke::Ref<INativeGroup>& group) override;
  void MarkNativeOptional(const char* name) override;

  void addDependency(NativeGroup::StrongRef* ref) {
    dependencies_.append(ref);
  }

  NativeEntry* NativeAt(size_t index) {
    return &natives_[index];
  }

  PluginContext *GetBaseContext();

  size_t NumJitFunctions() const {
    return m_JitFunctions.length();
  }
  CompiledFunction *GetJitFunction(size_t i) const {
    return m_JitFunctions[i];
  }
  const char *Name() const {
    return name_.chars();
  }

 public:
  typedef LegacyImage::Code Code;
  typedef LegacyImage::Data Data;

  const Code &code() const {
    return code_;
  }
  const Data &data() const {
    return data_;
  }
  LegacyImage *image() const {
    return image_;
  }

 private:
  void SetupFloatNativeRemapping();

 private:
  ke::AutoPtr<sp::LegacyImage> image_;
  ke::AutoArray<uint8_t> aligned_code_;
  ke::AutoArray<floattbl_t> float_table_;
  ke::AString name_;
  ke::AString full_name_;
  Code code_;
  Data data_;
  ke::AutoArray<NativeEntry> natives_;
  ke::AutoArray<sp_public_t> publics_;
  ke::AutoArray<sp_pubvar_t> pubvars_;
  ke::AutoArray<ScriptedInvoker *> entrypoints_;
  ke::AutoPtr<PluginContext> context_;

  struct FunctionMapPolicy {
    static inline uint32_t hash(ucell_t value) {
      return ke::HashInteger<4>(value);
    }
    static inline bool matches(ucell_t a, ucell_t b) {
      return a == b;
    }
  };
  typedef ke::HashMap<ucell_t, CompiledFunction *, FunctionMapPolicy> FunctionMap;

  FunctionMap function_map_;
  ke::Vector<CompiledFunction *> m_JitFunctions;

  ke::Vector<Ref<NativeGroup::StrongRef>> dependencies_;

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