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
#include "environment.h"
#include "plugin-runtime.h"
#include "plugin-context.h"
#include "stack-frames.h"
#include "x86/frames-x86.h"
#include "compiled-function.h"

using namespace ke;
using namespace sp;
using namespace SourcePawn;

InvokeFrame::InvokeFrame(PluginContext *cx, ucell_t entry_cip)
 : prev_(Environment::get()->top()),
   cx_(cx),
   prev_exit_frame_(Environment::get()->exit_frame()),
   entry_cip_(0),
   entry_sp_(nullptr)
{
  Environment::get()->enterInvoke(this);
}

InvokeFrame::~InvokeFrame()
{
  assert(Environment::get()->top() == this);
  Environment::get()->leaveInvoke();
}

FrameIterator::FrameIterator()
 : ivk_(Environment::get()->top()),
   exit_frame_(Environment::get()->exit_frame()),
   runtime_(nullptr),
   sp_iter_(nullptr),
   sp_stop_(nullptr),
   function_cip_(kInvalidCip),
   cip_(kInvalidCip),
   pc_(nullptr)
{
  if (!ivk_)
    return;

  nextInvokeFrame();
}

void
FrameIterator::nextInvokeFrame()
{
  assert(exit_frame_.exit_sp());
  assert(exit_frame_.frame_type() != FrameType::None);
  sp_iter_ = exit_frame_.exit_sp();

  // Inside an exit frame, the stack looks like this:
  //    .. C++ ..
  //    ----------- <-- entry_sp
  //    return addr to C++
  //    entry cip for frame #0
  //    alignment
  //    -----------
  //    return addr to frame #0
  //    entry cip for frame #1
  //    alignment
  //    ----------- <-- exit sp
  //    saved regs
  //    return addr
  //    .. InvokeNativeBoundHelper() ..
  //
  // We are guaranteed to always have one frame. We subtract one frame from
  // the entry sp so we hit the stopping point correctly.
  assert(ivk_->entry_sp());
  assert(ke::IsAligned(sizeof(JitFrame), sizeof(intptr_t)));
  sp_stop_ = ivk_->entry_sp() - (sizeof(JitFrame) / sizeof(intptr_t));
  assert(sp_stop_ >= sp_iter_);

  runtime_ = ivk_->cx()->runtime();
  function_cip_ = kInvalidCip;
  pc_ = nullptr;
  cip_ = kInvalidCip;

  if (exit_frame_.frame_type() == FrameType::Helper) {
    // We have an exit frame, but it's not for natives. automatically advance
    // to the most recent scripted frame.
    const JitExitFrameForHelper *exit =
      JitExitFrameForHelper::FromExitSp(exit_frame_.exit_sp());
    exit_frame_ = ExitFrame();

    // If we haven't compiled the function yet, but threw an error, then the
    // return address will be null.
    pc_ = exit->return_address;
    assert(pc_ || exit->isCompileFunction());

    // The function owning pc_ is in the previous frame.
    const JitFrame *frame = exit->prev();
    function_cip_ = frame->function_cip;
    sp_iter_ = reinterpret_cast<const intptr_t *>(frame);
    return;
  }
}

void
FrameIterator::Next()
{
  switch (exit_frame_.frame_type()) {
    case FrameType::LegacyNative:
    {
      // If we're at an exit frame, the return address will yield the current pc.
      const JitExitFrameForLegacyNative *exit =
        JitExitFrameForLegacyNative::FromExitSp(exit_frame_.exit_sp());
      exit_frame_ = ExitFrame();

      pc_ = nullptr;
      cip_ = kInvalidCip;

      // The function owning pc_ is in the previous frame.
      const JitFrame *frame = JitFrame::FromSp(sp_iter_);
      function_cip_ = frame->function_cip;
      return;
    }
    case FrameType::NewNative:
    {
      // If we're at an exit frame, the return address will yield the current pc.
      const JitExitFrameForNewNative *exit =
        JitExitFrameForNewNative::FromExitSp(exit_frame_.exit_sp());
      exit_frame_ = ExitFrame();

      pc_ = nullptr;
      cip_ = kInvalidCip;

      // The function owning pc_ is in the previous frame.
      const JitFrame *frame = JitFrame::FromSp(sp_iter_);
      function_cip_ = frame->function_cip;
      return;
    }
    default:
      assert(exit_frame_.frame_type() == FrameType::Helper ||
             exit_frame_.frame_type() == FrameType::None);
      break;
  }

  if (sp_iter_ >= sp_stop_) {
    // Jump to the next invoke frame.
    exit_frame_ = ivk_->prev_exit_frame();
    ivk_ = ivk_->prev();
    if (!ivk_)
      return;

    nextInvokeFrame();
    return;
  }

  pc_ = JitFrame::FromSp(sp_iter_)->return_address;
  assert(pc_);

  // Advance, and find the function cip the pc belongs to.
  sp_iter_ = reinterpret_cast<const intptr_t *>(JitFrame::FromSp(sp_iter_) + 1);
  function_cip_ = JitFrame::FromSp(sp_iter_)->function_cip;
  cip_ = kInvalidCip;
}

void
FrameIterator::Reset()
{
  *this = FrameIterator();
}

cell_t
FrameIterator::findCip() const
{
  CompiledFunction *fn = runtime_->GetJittedFunctionByOffset(function_cip_);
  if (!fn)
    return 0;

  if (cip_ == kInvalidCip) {
    if (pc_)
      cip_ = fn->FindCipByPc(pc_);
    else
      cip_ = function_cip_;
  }
  return cip_;
}

unsigned
FrameIterator::LineNumber() const
{
  ucell_t cip = findCip();
  if (cip == kInvalidCip)
    return 0;

  uint32_t line;
  if (!runtime_->image()->LookupLine(cip, &line))
    return 0;

  return line;
}

const char *
FrameIterator::FilePath() const
{
  ucell_t cip = findCip();
  if (cip == kInvalidCip)
    return runtime_->image()->LookupFile(function_cip_);

  return runtime_->image()->LookupFile(cip);
}

const char *
FrameIterator::FunctionName() const
{
  assert(ivk_);
  switch (exit_frame_.frame_type()) {
    case FrameType::LegacyNative:
    {
      const JitExitFrameForLegacyNative *exit =
        JitExitFrameForLegacyNative::FromExitSp(exit_frame_.exit_sp());
      const sp_native_t *native = runtime_->GetNative(exit->native_index);
      if (!native)
        return nullptr;
      return native->name;
    }
    case FrameType::NewNative:
    {
      const JitExitFrameForNewNative *exit =
        JitExitFrameForNewNative::FromExitSp(exit_frame_.exit_sp());
      const sp_native_t *native = runtime_->GetNative(exit->native_index);
      if (!native)
        return nullptr;
      return native->name;
    }
    default:
      assert(exit_frame_.frame_type() == FrameType::Helper ||
             exit_frame_.frame_type() == FrameType::None);
      break;
  }

  return runtime_->image()->LookupFunction(function_cip_);
}

bool
FrameIterator::IsNativeFrame() const
{
  return exit_frame_.frame_type() == FrameType::LegacyNative ||
         exit_frame_.frame_type() == FrameType::NewNative;
}

bool
FrameIterator::IsScriptedFrame() const
{
  return !IsNativeFrame() && ivk_;
}

IPluginContext *
FrameIterator::Context() const
{
  if (!ivk_)
    return nullptr;
  return ivk_->cx();
}
