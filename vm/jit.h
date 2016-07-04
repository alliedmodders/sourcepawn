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
#ifndef _include_sourcepawn_jit_h_
#define _include_sourcepawn_jit_h_

#include <sp_vm_types.h>
#include <sp_vm_api.h>
#include <am-vector.h>
#include "macro-assembler.h"
#include "opcodes.h"
#include "pool-allocator.h"
#include "outofline-asm.h"

namespace sp {

using namespace SourcePawn;

class PluginRuntime;
class PluginContext;
class LegacyImage;

struct BackwardJump {
  // The pc at the jump instruction (i.e. after it).
  uint32_t pc;
  // The cip of the jump.
  const cell_t *cip;
  // The offset of the timeout thunk. This is filled in at the end.
  uint32_t timeout_offset;

  BackwardJump()
  {}
  BackwardJump(uint32_t pc, const cell_t *cip)
   : pc(pc),
     cip(cip)
  {}
};

class ErrorPath : public OutOfLinePath
{
 public:
  ErrorPath(const cell_t* cip, int err)
  : cip(cip),
    err(err)
  {}

  bool emit(Compiler* cc) override;

  const cell_t *cip;
  int err;
};

class CompilerBase
{
  friend class ErrorPath;

 public:
  CompilerBase(PluginRuntime *rt, cell_t pcode_offs);
  virtual ~CompilerBase();

  static CompiledFunction *Compile(PluginRuntime *prt, cell_t pcode_offs, int *err);

 protected:
  CompiledFunction* emit(int* errp);

  virtual bool emitOp(sp::OPCODE op) = 0;
  virtual void emitThrowPath(int err) = 0;
  virtual void emitErrorHandlers() = 0;

  // Helpers.
  static int CompileFromThunk(PluginRuntime *runtime, cell_t pcode_offs, void **addrp, uint8_t* pc);
  static void* find_entry_fp();
  static void InvokeReportError(int err);
  static void InvokeReportTimeout();
  static void PatchCallThunk(uint8_t* pc, void* target);

 protected:
  cell_t readCell();

  // Map a return address (i.e. an exit point from a function) to its source
  // cip. This lets us avoid tracking the cip during runtime. These are
  // sorted by definition since we assemble and emit in forward order.
  void emitCipMapping(const cell_t *cip) {
    CipMapEntry entry;
    entry.cipoffs = uintptr_t(cip) - uintptr_t(code_start_);
    entry.pcoffs = masm.pc();
    cip_map_.append(entry);
  }

 protected:
  void emitErrorPath(ErrorPath* path);
  void emitThrowPathIfNeeded(int err);

 protected:
  Environment *env_;
  PluginRuntime *rt_;
  PluginContext *context_;
  LegacyImage *image_;
  PoolScope scope_;
  int error_;
  uint32_t pcode_start_;
  const cell_t *code_start_;
  const cell_t *cip_;
  const cell_t *op_cip_;
  const cell_t *code_end_;

  MacroAssembler masm;

  Label *jump_map_;

  ke::Vector<OutOfLinePath*> ool_paths_;

  Label throw_timeout_;
  Label throw_error_code_[SP_MAX_ERROR_CODES];
  Label report_error_;
  Label return_reported_error_;

  ke::Vector<BackwardJump> backward_jumps_;
  ke::Vector<CipMapEntry> cip_map_;
};

} // namespace sp

#endif // _include_sourcepawn_jit_h_
