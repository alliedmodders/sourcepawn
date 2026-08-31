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

#include <amtl/am-refcounting.h>
#include <assert.h>
#include <sp_vm_types.h>
#include "binary-reader.h"
#include "heap-defaults.h"
#include "stack-frames.h"

namespace sp {
class Environment;
}
namespace sp::v2 {

using namespace ke;

class Runtime;
class MethodInfo;

class Interpreter final
{
  public:
    static bool Run(Runtime* cx, RefPtr<MethodInfo> method, cell_t* rval);

  private:
    Interpreter(Runtime* cx, RefPtr<MethodInfo> method);

    bool run();
    cell_t return_value() const { return return_value_; }

    int32_t CalcLocalsSize();
    bool InitLocals();

  private:
    cell_t StackOffset(cell_t offset);
    cell_t getLocalCell(int32_t slot);
    void setLocalCell(int32_t slot, cell_t value);
    int64_t& getLocalInt64(int32_t slot);

  private:
    enum class StackType : uint8_t {
        Cell,
        Int64
    };

    struct StackValue {
        StackType type;
        union {
          cell_t cell;
          int64_t i64;
        } u;
    };

    void pushCell(cell_t value);
    cell_t popCell();
    void pushInt64(int64_t value);
    int64_t popInt64();
    void popStack();
    StackValue popValue();
    void pushValue(const StackValue& v);

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

    uint8_t* stack_types_top_ = nullptr;
    uint8_t* stack_types_limit_ = nullptr;
    uint8_t* stack_types_ptr_ = nullptr;
    cell_t* eval_stack_top_ = nullptr;
    cell_t* eval_stack_limit_ = nullptr;
    cell_t* eval_stack_ptr_ = nullptr;
};

} // namespace sp::v2
