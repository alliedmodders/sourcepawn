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

#include <span>

#include <amtl/am-refcounting.h>
#include <sp_vm_types.h>
#include "binary-reader.h"
#include "heap.h"
#include "objects.h"
#include "stack-frames.h"

namespace sp {
class Environment;
class SmxImage;
} // namespace sp

namespace sp::v2 {

using namespace ke;

class Runtime;
class MethodInfo;

class InterpFrame final : public InterpInvokeFrame {
  public:
    InterpFrame(SpFunction* sp_fn, BaseRuntime* cx, const uint8_t** initial_cip,
                const uint8_t* saved_cip, uint32_t dest_reg, uint32_t prev_frame);
    ~InterpFrame();

    BaseMethodInfo* method() const override;

    const uint8_t* saved_cip;
    uint32_t dest_reg;
    uint32_t prev_frame;

    static size_t AlignedSize() {
        return ke::Align(sizeof(InterpFrame), alignof(std::max_align_t));
    }

    cell_t* vregs() {
        return reinterpret_cast<cell_t*>(reinterpret_cast<uint8_t*>(this) + AlignedSize());
    }
};

class Interpreter final
{
  public:
    static bool Run(Runtime* cx, Handle<SpFunction> fn, uint32_t frm, cell_t* rval);

  private:
    Interpreter(Runtime* cx, Handle<SpFunction> fn, uint32_t frm);

    bool run_internal();
    bool CheckTimeout();
    void UnwindStack(InterpInvokeFrame* root_ivk, MethodInfo* root_method,
                     std::span<cell_t> root_vregs);
    cell_t return_value() const { return return_value_; }

  private:
    Environment* env_;
    Runtime* rt_;
    SmxImage* smx_;
    Heap& heap_;
    const uint8_t* code_;
    BinaryReader reader_;
    bool has_returned_;
    cell_t return_value_;
    cell_t frm_;
    cell_t* phys_frm_;
    InterpFrame* ivk_;
    Handle<SpFunction> entry_fn_;

    std::span<cell_t> vregs_;
};

} // namespace sp::v2
