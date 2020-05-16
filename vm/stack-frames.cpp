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
#include "compiled-function.h"
#include "method-info.h"
#if defined(KE_ARCH_X86)
# include "x86/frames-x86.h"
#elif defined(KE_ARCH_X64)
# include "x64/frames-x64.h"
#elif !defined(SP_HAS_JIT)
# include "null-frame-layout.h"
#endif

using namespace ke;
using namespace sp;
using namespace SourcePawn;

InvokeFrame::InvokeFrame(PluginContext* cx, ucell_t entry_cip)
 : prev_(Environment::get()->top()),
   cx_(cx),
   entry_cip_(0)
{
  Environment::get()->enterInvoke(this);
}

InvokeFrame::~InvokeFrame()
{
  assert(Environment::get()->top() == this);
  Environment::get()->leaveInvoke();
}

InterpInvokeFrame::InterpInvokeFrame(PluginContext* cx,
                                     MethodInfo* method,
                                     const cell_t* const& cip)
 : InvokeFrame(cx, method->pcode_offset()),
   method_(method),
   cip_(cip),
   native_index_(-1)
{
}

InterpInvokeFrame::~InterpInvokeFrame()
{
  assert(native_index_ == -1);
}

void
InterpInvokeFrame::enterNativeCall(uint32_t native_index)
{
  assert(native_index_ == -1);
  native_index_ = native_index;
}

void
InterpInvokeFrame::leaveNativeCall()
{
  assert(native_index_ != -1);
  native_index_ = -1;
}

JitInvokeFrame::JitInvokeFrame(PluginContext* cx, ucell_t entry_cip)
 : InvokeFrame(cx, entry_cip),
   prev_exit_fp_(Environment::get()->exit_fp())
{
}

JitInvokeFrame::~JitInvokeFrame()
{
  Environment::get()->leaveJitInvoke(this);
}

InterpFrameIterator::InterpFrameIterator(InterpInvokeFrame* ivk)
 : ivk_(ivk)
{
  if (ivk_->native_index_ != -1)
    current_ = FrameType::Native;
  else
    current_ = FrameType::Scripted;
}

bool
InterpFrameIterator::done() const
{
  return current_ == FrameType::Scripted;
}

void
InterpFrameIterator::next()
{
  assert(!done());
  current_ = FrameType::Scripted;
}

FrameType
InterpFrameIterator::type() const
{
  return current_;
}

cell_t
InterpFrameIterator::function_cip() const
{
  assert(current_ == FrameType::Scripted);
  return ivk_->method_->pcode_offset();
}

cell_t
InterpFrameIterator::cip() const
{
  assert(current_ == FrameType::Scripted);
  auto& code = ivk_->cx()->runtime()->code();

  const uint8_t* ptr = reinterpret_cast<const uint8_t*>(ivk_->cip_);
  assert(ptr >= code.bytes && ptr < code.bytes + code.length);

  return ptr - code.bytes;
}

uint32_t
InterpFrameIterator::native_index() const
{
  assert(current_ == FrameType::Native);
  return ivk_->native_index_;
}

// This constructor is for find_entry_fp() in the JIT.
JitFrameIterator::JitFrameIterator(Environment* env)
 : JitFrameIterator(env->top()->cx()->runtime(), env->exit_fp())
{
}

JitFrameIterator::JitFrameIterator(PluginRuntime* rt, intptr_t* exit_fp)
 : rt_(rt),
   cur_frame_(FrameLayout::FromFp(exit_fp))
{
  assert(cur_frame_->frame_type == JitFrameType::Exit);
  assert(cur_frame_->return_address);
  assert(cur_frame_->prev_fp);

  pc_ = nullptr;
  cip_ = kInvalidCip;
}

bool
JitFrameIterator::done() const
{
  return cur_frame_->frame_type == JitFrameType::Entry;
}

void
JitFrameIterator::next()
{
  assert(!done());

  pc_ = cur_frame_->return_address;
  cip_ = kInvalidCip;
  cur_frame_ = FrameLayout::FromFp(cur_frame_->prev_fp);
}

FrameType
JitFrameIterator::type() const
{
  switch ((JitFrameType)cur_frame_->frame_type) {
  case JitFrameType::Scripted:
    return FrameType::Scripted;
  case JitFrameType::Exit:
    if (GetExitFrameType(cur_frame_->function_id) == ExitFrameType::Native)
      return FrameType::Native;
    return FrameType::Internal;
  default:
    return FrameType::Internal;
  }
}

cell_t
JitFrameIterator::function_cip() const
{
  assert(cur_frame_->frame_type == JitFrameType::Scripted);
  return cur_frame_->function_id;
}

cell_t
JitFrameIterator::cip() const
{
  RefPtr<MethodInfo> method = rt_->GetMethod(function_cip());
  if (!method)
    return 0;

  CompiledFunction* fn = method->jit();
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

uint32_t
JitFrameIterator::native_index() const
{
  assert(type() == FrameType::Native);
  return GetExitFramePayload(cur_frame_->function_id);
}

FrameIterator::FrameIterator()
 : ivk_(nullptr),
   runtime_(nullptr),
   next_exit_fp_(nullptr)
{
  Reset();
}

void
FrameIterator::nextInvokeFrame()
{
  runtime_ = ivk_->cx()->runtime();
  if (JitInvokeFrame* jvk = ivk_->AsJitInvokeFrame()) {
    frame_cursor_ = std::make_unique<JitFrameIterator>(runtime_, next_exit_fp_);
    next_exit_fp_ = jvk->prev_exit_fp();
    return;
  }
  if (InterpInvokeFrame* ivk = ivk_->AsInterpInvokeFrame()) {
    frame_cursor_ = std::make_unique<InterpFrameIterator>(ivk);
    return;
  }
}

cell_t
FrameIterator::cip() const
{
  return frame_cursor_->cip();
}

void
FrameIterator::Next()
{
  if (frame_cursor_->done()) {
    frame_cursor_ = nullptr;

    ivk_ = ivk_->prev();
    if (ivk_)
      nextInvokeFrame();
    return;
  }

  frame_cursor_->next();
}

void
FrameIterator::Reset()
{
  ivk_ = Environment::get()->top();
  runtime_ = nullptr;
  next_exit_fp_ = Environment::get()->exit_fp();
  frame_cursor_ = nullptr;

  if (ivk_)
    nextInvokeFrame();
}

unsigned
FrameIterator::LineNumber() const
{
  if (!IsScriptedFrame())
    return 0;

  ucell_t cip = frame_cursor_->cip();
  if (cip == kInvalidCip)
    return 0;

  uint32_t line;
  if (!runtime_->image()->LookupLine(cip, &line))
    return 0;

  return line;
}

const char*
FrameIterator::FilePath() const
{
  if (!IsScriptedFrame())
    return nullptr;

  ucell_t cip = frame_cursor_->cip();
  if (cip == kInvalidCip)
    return runtime_->image()->LookupFile(frame_cursor_->function_cip());

  return runtime_->image()->LookupFile(cip);
}

const char*
FrameIterator::FunctionName() const
{
  assert(ivk_);
  if (IsNativeFrame()) {
    uint32_t native_index = frame_cursor_->native_index();
    const sp_native_t* native = runtime_->GetNative(native_index);
    if (!native)
      return nullptr;
    return native->name;
  }

  if (IsScriptedFrame()) {
    cell_t function_cip = frame_cursor_->function_cip();
    return runtime_->image()->LookupFunction(function_cip);
  }

  return nullptr;
}

bool
FrameIterator::IsNativeFrame() const
{
  return frame_cursor_->type() == FrameType::Native;
}

bool
FrameIterator::IsScriptedFrame() const
{
  return frame_cursor_->type() == FrameType::Scripted;
}

IPluginContext*
FrameIterator::Context() const
{
  if (!ivk_)
    return nullptr;
  return ivk_->cx();
}

bool
FrameIterator::IsInternalFrame() const
{
  return frame_cursor_->type() == FrameType::Internal;
}
