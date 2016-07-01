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
#ifndef _INCLUDE_SOURCEPAWN_JIT_X64_H_
#define _INCLUDE_SOURCEPAWN_JIT_X64_H_

#include <sp_vm_types.h>
#include <sp_vm_api.h>
#include <am-vector.h>
#include "jit.h"

using namespace SourcePawn;

namespace sp {

struct CallThunk
{
  SilentCodeLabel target;
  PatchLabel patch;
  cell_t pcode_offset;

  CallThunk(cell_t pcode_offset)
    : pcode_offset(pcode_offset)
  {
  }
};

class Compiler : public CompilerBase
{
 public:
  Compiler(PluginRuntime *rt, cell_t pcode_offs);
  ~Compiler();

 protected:
  bool emitOp(sp::OPCODE op) override;
  void emitCallThunks() override;
  void emitErrorHandlers() override;
  void emitThrowPath(int err) override;

  bool emitCall();
  bool emitSysreqN();
  bool emitLegacyNativeCall(uint32_t native_index, NativeEntry* native);
  void emitCheckAddress(Register reg);

  void jumpOnError(ConditionCode cc, int err = 0);

  void emitThrowPathIfNeeded(int err);

 private:
  ke::Vector<CallThunk> call_thunks_;
};

}

#endif //_INCLUDE_SOURCEPAWN_JIT_X64_H_
