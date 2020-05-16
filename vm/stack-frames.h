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

#include <assert.h>

#include <memory>

#include <sp_vm_api.h>
#include <am-cxx.h>
#include <amtl/am-platform.h>
#include <amtl/am-refcounting.h>
#include <amtl/am-enum.h>
#if defined(KE_ARCH_X86)
# include "x86/frames-x86.h"
#elif defined(KE_ARCH_X64)
# include "x64/frames-x64.h"
#endif

namespace sp {

using namespace SourcePawn;

class PluginContext;
class PluginRuntime;
class MethodInfo;
struct FrameLayout;

enum class FrameType
{
  Internal,
  Scripted,
  Native
};

// These are specific to the JIT.
enum class JitFrameType : intptr_t
{
  None,
  Entry,
  Scripted,
  Exit
};
KE_DEFINE_ENUM_COMPARATORS(JitFrameType, intptr_t);

// These are specific to the JIT.
enum class ExitFrameType : uintptr_t
{
  Native,
  Helper
};
KE_DEFINE_ENUM_OPERATORS(ExitFrameType);
KE_DEFINE_ENUM_COMPARATORS(ExitFrameType, intptr_t);

static const uintptr_t kExitFrameTypeBits = 3;
static const uintptr_t kExitFramePayloadBits = (sizeof(void*) * 8) - kExitFrameTypeBits;
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

class JitInvokeFrame;
class InterpInvokeFrame;

// An InvokeFrame represents one activation of Execute2().
class InvokeFrame
{
 protected:
  InvokeFrame(PluginContext* cx, ucell_t cip);
  ~InvokeFrame();

 public:
  InvokeFrame* prev() const {
    return prev_;
  }
  PluginContext* cx() const {
    return cx_;
  }

  ucell_t entry_cip() const {
    return entry_cip_;
  }

  virtual JitInvokeFrame* AsJitInvokeFrame() {
    return nullptr;
  }
  virtual InterpInvokeFrame* AsInterpInvokeFrame() {
    return nullptr;
  }

 protected:
  InvokeFrame* prev_;
  PluginContext* cx_;
  ucell_t entry_cip_;
};

// Created by the interpreter. These are 1:1 with interpreter frames, for now.
class InterpInvokeFrame final : public InvokeFrame
{
  friend class InterpFrameIterator;

 public:
  InterpInvokeFrame(PluginContext* cx,
                    MethodInfo* method,
                    const cell_t* const& cip);
  ~InterpInvokeFrame();

  void enterNativeCall(uint32_t native_index);
  void leaveNativeCall();

  InterpInvokeFrame* AsInterpInvokeFrame() override {
    return this;
  }

 private:
  ke::RefPtr<MethodInfo> method_;
  const cell_t* const& cip_;
  int native_index_;
};

// JIT frames are always contained within JitInvokeFrame.
class JitInvokeFrame final : public InvokeFrame
{
 public:
  JitInvokeFrame(PluginContext* cx, ucell_t cip);
  ~JitInvokeFrame();

  JitInvokeFrame* AsJitInvokeFrame() override {
    return this;
  }

  intptr_t* prev_exit_fp() const {
    return prev_exit_fp_;
  }

 private:
  intptr_t* prev_exit_fp_;
};

class InlineFrameIterator
{
 public:
  virtual ~InlineFrameIterator()
  {}

  // "done" should return true if, after the current frame, there are no more
  // frames to iterate.
  virtual bool done() const = 0;
  virtual void next() = 0;
  virtual FrameType type() const = 0;
  virtual cell_t function_cip() const = 0;
  virtual cell_t cip() const = 0;
  virtual uint32_t native_index() const = 0;
};

class InterpFrameIterator final : public InlineFrameIterator
{
 public:
  InterpFrameIterator(InterpInvokeFrame* ivk);

  bool done() const override;
  void next() override;
  FrameType type() const override;
  cell_t function_cip() const override;
  cell_t cip() const override;
  uint32_t native_index() const override;

 private:
  InterpInvokeFrame* ivk_;
  FrameType current_;
};

class JitFrameIterator final : public InlineFrameIterator
{
 public:
  explicit JitFrameIterator(Environment* env);
  JitFrameIterator(PluginRuntime* rt, intptr_t* exit_fp);

  bool done() const override;
  void next() override;
  FrameType type() const override;
  cell_t function_cip() const override;
  cell_t cip() const override;
  uint32_t native_index() const override;

  FrameLayout* frame() const {
    return cur_frame_;
  }

 private:
  PluginRuntime* rt_;
  FrameLayout* cur_frame_;
  mutable ucell_t cip_;
  void* pc_;
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
  const char* FunctionName() const override;
  const char* FilePath() const override;
  unsigned LineNumber() const override;
  IPluginContext* Context() const override;
  bool IsInternalFrame() const override;

  cell_t cip() const;

 private:
  void nextInvokeFrame();

 private:
  InvokeFrame* ivk_;
  PluginRuntime* runtime_;
  intptr_t* next_exit_fp_;
  std::unique_ptr<InlineFrameIterator> frame_cursor_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_stack_frames_h_
