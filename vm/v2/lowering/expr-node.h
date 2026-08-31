// vim: set ts=8 sw=4 tw=99 sts=4 et:
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
#pragma once

#include <stdint.h>

#include <span>

#include <sp_vm_types.h>
#include "type-desc.h"
#include "v2/lowering/ll-op.h"

namespace sp::v2 {

struct VReg {
    uint16_t index;
    uint16_t cells;
    bool owned;

    VReg() : index(kInvalidReg), cells(0), owned(false) {}
    VReg(uint16_t index, uint16_t cells, bool owned) : index(index), cells(cells), owned(owned) {}

    bool valid() const { return index != kInvalidReg; }
    bool operator==(const VReg& other) const { return index == other.index; }
    bool operator!=(const VReg& other) const { return index != other.index; }

    static constexpr uint16_t kInvalidReg = 0xffff;
};

struct ExprNode {
    enum Kind {
        kInvalid,
        kReg,
        kConstant,
        kSimpleOp,
        kSlotOp,
        kLoadElem,
        kCall,
        kLoadFn,
        kLoadUpvar,
        kLoadField,
        kNewObj,
    };

    ExprNode() : kind(kInvalid), type(nullptr) {}

    ExprNode(Kind kind, const TypeDesc* type, VReg reg, bool owns_reg = true)
      : kind(kind), type(type)
    {
        this->reg = reg;
        this->reg.owned = owns_reg;
    }

    ExprNode(const TypeDesc* type, cell_t value)
      : kind(kConstant), type(type)
    {
        this->constval.value = value;
    }

    ExprNode(const TypeDesc* type, int64_t value64)
      : kind(kConstant), type(type)
    {
        this->constval.value64 = value64;
    }

    bool IsInvariant() const {
        return kind == kReg || kind == kConstant || kind == kLoadFn;
    }

    bool IsSafeForDirectGcObjStore() const {
        // Returns true if the node evaluation does not rely on any existing local state
        return kind == kConstant || kind == kLoadFn;
    }

    Kind kind;
    const TypeDesc* type;

    union {
        VReg reg;
        struct {
            cell_t value;
            int64_t value64;
        } constval;
        struct {
            LLOp opcode;
            ExprNode* left;
            ExprNode* right;
        } op;
        struct {
            LLOp opcode;
            ExprNode* base;
            ExprNode* index;
        } load_elem;
        struct {
            LLOp opcode;
            VReg reg;
        } slot_op;
        struct {
            uint32_t method_index;
            std::span<VReg> argv;
            std::span<VReg> args_to_free;
            VReg spread_reg;
            VReg fn_reg;
        } call;
        struct {
            uint32_t fn_id;
        } load_fn;
        struct {
            uint32_t index;
        } load_upvar;
        struct {
            ExprNode* base;
            uint32_t offset;
        } load_field;
    };
};

} // namespace sp::v2
