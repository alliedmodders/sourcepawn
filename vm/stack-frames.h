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
#ifndef _include_sourcepawn_vm_stack_frames_h_
#define _include_sourcepawn_vm_stack_frames_h_

#include <sp_vm_api.h>
#include <assert.h>
#include <am-cxx.h>

namespace sp {

using namespace SourcePawn;

class PluginContext;
class PluginRuntime;
struct FrameLayout;

enum class FrameType : intptr_t
{
  None,
  Entry,
  Scripted,
  Exit
};
KE_DEFINE_ENUM_COMPARATORS(FrameType, intptr_t);

enum class ExitFrameType : uintptr_t
{
  Native,
  Helper
};
KE_DEFINE_ENUM_OPERATORS(ExitFrameType);
KE_DEFINE_ENUM_COMPARATORS(ExitFrameType, intptr_t);

static const uintptr_t kExitFrameTypeBits = 3;
static const uintptr_t kExitFramePayloadBits = (sizeof(void *) * 8) - kExitFrameTypeBits;
static inline uintptr_t EncodeExitFrameId(ExitFrameType type, uintptr_t payload)
{
  assert(payload < (uintptr_t(1) << kExitFramePayloadBits));
  return (payload << kExitFrameTypeBits) | uintptr_t(type);
}
static inline ExitFrameType GetExitFrameType(uintptr_t stack_val)
{
  return ExitFrameType(stack_val & ((1 << kExitFrameTypeBits) - 1));
}
static inline uintptr_t GetExitFramePayload(uintptr_t stack_val)
{
  return stack_val >> kExitFrameTypeBits;
}

// An InvokeFrame represents one activation of Execute2().
class InvokeFrame
{
 public:
  InvokeFrame(PluginContext *cx, ucell_t cip);
  ~InvokeFrame();

  InvokeFrame *prev() const {
    return prev_;
  }
  PluginContext *cx() const {
    return cx_;
  }

  intptr_t* prev_exit_fp() const {
    return prev_exit_fp_;
  }
  ucell_t entry_cip() const {
    return entry_cip_;
  }

 private:
  InvokeFrame *prev_;
  PluginContext *cx_;
  intptr_t* prev_exit_fp_;
  ucell_t entry_cip_;
};

class FrameIterator : public SourcePawn::IFrameIterator
{
 public:
  FrameIterator();

  bool Done() const override {
    return !ivk_;
  }
  void Next() override;
  void Reset() override;

  bool IsNativeFrame() const override;
  bool IsScriptedFrame() const override;
  const char *FunctionName() const override;
  const char *FilePath() const override;
  unsigned LineNumber() const override;
  IPluginContext *Context() const override;
  bool IsInternalFrame() const override;

  bool IsEntryFrame() const;
  FrameLayout* Frame() const;

 private:
  void nextInvokeFrame(intptr_t* exit_fp);
  cell_t findCip() const;
  cell_t function_cip() const;

 private:
  InvokeFrame* ivk_;
  FrameLayout* cur_frame_;
  PluginRuntime* runtime_;
  mutable ucell_t cip_;
  void* pc_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_stack_frames_h_
