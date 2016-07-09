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
#include "method-info.h"

using namespace ke;
using namespace sp;
using namespace SourcePawn;

InvokeFrame::InvokeFrame(PluginContext *cx, ucell_t entry_cip)
 : prev_(Environment::get()->top()),
   cx_(cx),
   prev_exit_fp_(Environment::get()->exit_fp()),
   entry_cip_(0)
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
   cur_frame_(nullptr),
   runtime_(nullptr),
   cip_(kInvalidCip),
   pc_(nullptr)
{
  if (!ivk_)
    return;

  nextInvokeFrame(Environment::get()->exit_fp());
}

void
FrameIterator::nextInvokeFrame(intptr_t* exit_fp)
{
  cur_frame_ = FrameLayout::FromFp(exit_fp);
  assert(cur_frame_->frame_type == FrameType::Exit);
  assert(cur_frame_->return_address);
  assert(cur_frame_->prev_fp);

  runtime_ = ivk_->cx()->runtime();
  pc_ = nullptr;
  cip_ = kInvalidCip;
}

void
FrameIterator::Next()
{
  if (cur_frame_->frame_type == FrameType::Entry) {
    // Done with this InvokeFrame, so jump to the next.
    intptr_t* next_fp = ivk_->prev_exit_fp();
    ivk_ = ivk_->prev();
    if (!ivk_)
      return;
    nextInvokeFrame(next_fp);
    return;
  }

  pc_ = cur_frame_->return_address;
  cip_ = kInvalidCip;
  cur_frame_ = FrameLayout::FromFp(cur_frame_->prev_fp);
}

void
FrameIterator::Reset()
{
  *this = FrameIterator();
}

cell_t
FrameIterator::function_cip() const
{
  assert(cur_frame_->frame_type == FrameType::Scripted);
  return cur_frame_->function_id;
}

cell_t
FrameIterator::findCip() const
{
  RefPtr<MethodInfo> method = runtime_->GetMethod(function_cip());
  if (!method)
    return 0;

  CompiledFunction *fn = method->jit();
  if (!fn)
    return 0;

  if (cip_ == kInvalidCip) {
    if (pc_)
      cip_ = fn->FindCipByPc(pc_);
    else
      cip_ = function_cip();
  }
  return cip_;
}

unsigned
FrameIterator::LineNumber() const
{
  if (!IsScriptedFrame())
    return 0;

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
  if (!IsScriptedFrame())
    return nullptr;

  ucell_t cip = findCip();
  if (cip == kInvalidCip)
    return runtime_->image()->LookupFile(function_cip());

  return runtime_->image()->LookupFile(cip);
}

const char *
FrameIterator::FunctionName() const
{
  assert(ivk_);
  if (IsNativeFrame()) {
    uint32_t native_index = GetExitFramePayload(cur_frame_->function_id);
    const sp_native_t *native = runtime_->GetNative(native_index);
    if (!native)
      return nullptr;
    return native->name;
  }

  if (IsScriptedFrame())
    return runtime_->image()->LookupFunction(function_cip());

  return nullptr;
}

bool
FrameIterator::IsNativeFrame() const
{
  return cur_frame_->frame_type == FrameType::Exit &&
         GetExitFrameType(cur_frame_->function_id) == ExitFrameType::Native;
}

bool
FrameIterator::IsScriptedFrame() const
{
  return cur_frame_->frame_type == FrameType::Scripted;
}

bool
FrameIterator::IsEntryFrame() const
{
  return cur_frame_->frame_type == FrameType::Entry;
}

FrameLayout*
FrameIterator::Frame() const
{
  return cur_frame_;
}

IPluginContext *
FrameIterator::Context() const
{
  if (!ivk_)
    return nullptr;
  return ivk_->cx();
}

bool
FrameIterator::IsInternalFrame() const
{
  return !(IsScriptedFrame() || IsNativeFrame());
}
