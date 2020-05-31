// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// SourcePawn is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
//
#include "jit.h"
#include "environment.h"
#include "linking.h"
#include "method-info.h"
#include "opcodes.h"
#include "outofline-asm.h"
#include "pcode-reader.h"
#include "plugin-runtime.h"
#include "stack-frames.h"
#include "watchdog_timer.h"
#if defined(KE_ARCH_X86)
# include "x86/jit_x86.h"
#endif

namespace sp {

using namespace SourcePawn;

#define __ masm.

CompilerBase::CompilerBase(PluginRuntime* rt, MethodInfo* method)
 : env_(Environment::get()),
   rt_(rt),
   context_(rt->GetBaseContext()),
   image_(rt_->image()),
   method_info_(method),
   error_(SP_ERROR_NONE),
   pcode_start_(0),
   code_start_(nullptr),
   op_cip_(nullptr)
{
}

CompilerBase::~CompilerBase()
{
}

CompiledFunction*
CompilerBase::Compile(PluginContext* cx, RefPtr<MethodInfo> method, int* err)
{
  Compiler cc(cx->runtime(), method);

  CompiledFunction* fun = cc.emit();
  if (!fun) {
    *err = cc.error();
    return nullptr;
  }

  method->setCompiledFunction(fun);
  return fun;
}

CompiledFunction*
CompilerBase::emit()
{
  graph_ = method_info_->ValidateWithGraph();
  if (!graph_) {
    reportError(method_info_->validationError());
    return nullptr;
  }

  pcode_start_ = method_info_->pcode_offset();
  code_start_ = reinterpret_cast<const cell_t*>(rt_->code().bytes + pcode_start_);

#if defined JIT_SPEW
  Environment::get()->debugger()->OnDebugSpew(
      "Compiling function %s::%s\n",
      rt_->Name(),
      rt_->image()->LookupFunction(pcode_start_));

  SpewOpcode(stdout, rt_, code_start_, reader.cip());
#endif

  emitPrologue();

  for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
    block_ = *iter;
    __ bind(block_->label());

    PcodeReader<CompilerBase> reader(rt_, block_, this);
    reader.begin();

    while (reader.more()) {
#if defined JIT_SPEW
      SpewOpcode(rt_, code_start_, reader.cip());
#endif

      // Save the start of the opcode for emitCipMap().
      op_cip_ = reader.cip();

      if (!reader.visitNext() || error_)
        return nullptr;
    }

    // Note: the offset is ignored.
    if (block_->endType() == BlockEnd::Jump)
      visitJUMP(0);
  }

  for (size_t i = 0; i < ool_paths_.size(); i++) {
    OutOfLinePath* path = ool_paths_[i];
    __ bind(path->label());
    if (!path->emit(static_cast<Compiler*>(this)))
      return nullptr;
  }

  // For each backward jump, emit a little thunk so we can exit from a timeout.
  // Track the offset of where the thunk is, so the watchdog timer can patch it.
  for (size_t i = 0; i < backward_jumps_.size(); i++) {
    BackwardJump& jump = backward_jumps_[i];
    jump.timeout_offset = masm.pc();
    __ call(&throw_timeout_);
    emitCipMapping(jump.cip);
  }

  // These have to come last.
  emitThrowPathIfNeeded(SP_ERROR_DIVIDE_BY_ZERO);
  emitThrowPathIfNeeded(SP_ERROR_STACKLOW);
  emitThrowPathIfNeeded(SP_ERROR_STACKMIN);
  emitThrowPathIfNeeded(SP_ERROR_ARRAY_BOUNDS);
  emitThrowPathIfNeeded(SP_ERROR_MEMACCESS);
  emitThrowPathIfNeeded(SP_ERROR_HEAPLOW);
  emitThrowPathIfNeeded(SP_ERROR_HEAPMIN);
  emitThrowPathIfNeeded(SP_ERROR_INTEGER_OVERFLOW);
  emitThrowPathIfNeeded(SP_ERROR_INVALID_NATIVE);

  // Common path for invoking line debugger.
  emitDebugBreakHandler();

  // This has to come very, very last, since it checks whether return paths
  // are used.
  emitErrorHandlers();

  if (error_)
    return nullptr;

  CodeChunk code = LinkCode(env_, masm);
  if (!code.address()) {
    reportError(SP_ERROR_OUT_OF_MEMORY);
    return nullptr;
  }

  std::unique_ptr<FixedArray<LoopEdge>> edges(
    new FixedArray<LoopEdge>(backward_jumps_.size()));
  for (size_t i = 0; i < backward_jumps_.size(); i++) {
    const BackwardJump& jump = backward_jumps_[i];
    edges->at(i).offset = jump.pc;
    edges->at(i).disp32 = int32_t(jump.timeout_offset) - int32_t(jump.pc);
  }

  std::unique_ptr<FixedArray<CipMapEntry>> cipmap(
    new FixedArray<CipMapEntry>(cip_map_.size()));
  memcpy(cipmap->buffer(), cip_map_.data(), cip_map_.size() * sizeof(CipMapEntry));

  assert(error_ == SP_ERROR_NONE);
  return new CompiledFunction(code, pcode_start_, edges.release(), cipmap.release());
}

void
CompilerBase::emitErrorPath(ErrorPath* path)
{
  // For each path that had an error check, bind it to an error routine and
  // add it to the cip map. What we'll get is something like:
  //
  //   compare dividend, 0
  //   jump-if-equal error_thunk_0
  //
  // error_thunk_0:
  //   call integer_overflow
  //
  // integer_overflow:
  //   mov error-code-reg, SP_ERROR_DIVIDE_BY_ZERO
  //   jump report_error
  //
  // report_error:
  //   create exit frame
  //   push error-code-reg
  //   call InvokeReportError(int err)
  //

  // If there's no error code, it should be in eax. Otherwise we'll jump to
  // a path that sets eax to a hardcoded value.
  __ alignStack();
  if (path->err == 0)
    __ call(&report_error_);
  else
    __ call(&throw_error_code_[path->err]);

  emitCipMapping(path->cip);
}

void
CompilerBase::emitThrowPathIfNeeded(int err)
{
  assert(err < SP_MAX_ERROR_CODES);

  if (!throw_error_code_[err].used())
    return;

  __ bind(&throw_error_code_[err]);
  emitThrowPath(err);
}

void
CompilerBase::reportError(int err)
{
  // Break here to get an error report stack.
  error_ = err;
}

int
CompilerBase::CompileFromThunk(PluginContext* cx, cell_t pcode_offs, void** addrp, uint8_t* pc)
{
  // If the watchdog timer has declared a timeout, we must process it now,
  // and possibly refuse to compile, since otherwise we will compile a
  // function that is not patched for timeouts.
  if (!Environment::get()->watchdog()->HandleInterrupt())
    return SP_ERROR_TIMEOUT;

  RefPtr<MethodInfo> method = cx->runtime()->AcquireMethod(pcode_offs);
  if (!method)
    return SP_ERROR_INVALID_ADDRESS;

  CompiledFunction* fn = method->jit();
  if (!fn) {
    int err;
    fn = Compile(cx, method, &err);
    if (!fn)
      return err;
  }

#if defined JIT_SPEW
  Environment::get()->debugger()->OnDebugSpew(
      "Patching thunk to %s::%s\n",
      cx->runtime()->Name(),
      cx->runtime()->image()->LookupFunction(pcode_offs));
#endif

  *addrp = fn->GetEntryAddress();

  /* Right now, we always keep the code RWE */
  PatchCallThunk(pc, fn->GetEntryAddress());
  return SP_ERROR_NONE;
}

// Find the |ebp| associated with the entry frame. We use this to drop out of
// the entire scripted call stack.
void*
CompilerBase::find_entry_fp()
{
  void* fp = nullptr;

  for (JitFrameIterator iter(Environment::get()); !iter.done(); iter.next()) {
    FrameLayout* frame = iter.frame();
    if (frame->frame_type == JitFrameType::Entry)
      break;
    fp = frame->prev_fp;
  }

  assert(fp);
  return fp;
}

// Exit frame is a JitExitFrameForHelper.
void
CompilerBase::InvokeReportError(int err)
{
  Environment::get()->ReportError(err);
}

// Exit frame is a JitExitFrameForHelper. This is a special function since we
// have to notify the watchdog timer that we're unblocked.
void
CompilerBase::InvokeReportTimeout()
{
  Environment::get()->watchdog()->NotifyTimeoutReceived();
  InvokeReportError(SP_ERROR_TIMEOUT);
}

bool
ErrorPath::emit(Compiler* cc)
{
  cc->emitErrorPath(this);
  return true;
}

bool
OutOfBoundsErrorPath::emit(Compiler* cc)
{
  cc->emitOutOfBoundsErrorPath(this);
  return true;
}

} // namespace sp
