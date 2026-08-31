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
#pragma once

#include <assert.h>

#include <amtl/am-refcounting.h>
#include "binary-reader.h"
#include "heap-defaults.h"
#include <sp_vm_types.h>
#include <span>
#include "stack-frames.h"

namespace sp {
class Environment;
class SmxImage;
}
namespace sp::v2 {

using namespace ke;

class Runtime;
class MethodInfo;

struct InterpFrame {
    InterpInvokeFrame ivk;
    MethodInfo* caller_method;

    const uint8_t* saved_cip;

    uint32_t dest_reg;
    uint32_t prev_frame;
    uint32_t hp_scope;
    sp::HeapImpl::Position heap_pos;
};

class Interpreter final
{
  public:
    static bool Run(Runtime* cx, RefPtr<MethodInfo> method, cell_t* rval);

  private:
    Interpreter(Runtime* cx, RefPtr<MethodInfo> method);

    bool run_internal(std::span<cell_t> args);
    bool CheckTimeout();
    cell_t return_value() const { return return_value_; }

  private:
    Environment* env_;
    Runtime* rt_;
    SmxImage* smx_;
    HeapImpl& heap_;
    RefPtr<MethodInfo> method_;
    const uint8_t* code_;
    BinaryReader reader_;
    bool has_returned_;
    cell_t return_value_;
    cell_t frm_;
    cell_t* phys_frm_;
    InterpInvokeFrame* ivk_;

    std::span<cell_t> vregs_;
};

} // namespace sp::v2
