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

#include <assert.h>
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
        kUnaryOp,
        kBinaryOp,
        kSlotOp,
        kLoadElem,
        kCall,
        kLoadFn,
        kLoadUpvar,
        kLoadField,
        kNewObj,
    };

    ExprNode() : kind(kInvalid), type(nullptr) {}

    ExprNode(Kind kind) : kind(kind), type(nullptr) {}

    ExprNode(Kind kind, const TypeDesc* type) : kind(kind), type(type) {}

    bool IsInvariant() const {
        return kind == kReg || kind == kConstant || kind == kLoadFn;
    }

    bool IsSafeForDirectGcObjStore() const {
        // Returns true if the node evaluation does not rely on any existing local state
        return kind == kConstant || kind == kLoadFn;
    }

    template <class T>
    T* as() {
        if (T::is_a(this))
            return reinterpret_cast<T*>(this);
        return nullptr;
    }

    template <class T>
    T* to() {
        assert(T::is_a(this));
        return reinterpret_cast<T*>(this);
    }

    Kind kind;
    const TypeDesc* type;
    VReg reg;
};

struct CallExprNode : ExprNode {
    CallExprNode()
      : ExprNode(ExprNode::kCall),
        method_index(0)
    {}

    CallExprNode(const TypeDesc* type, std::span<VReg> argv, std::span<VReg> args_to_free)
      : ExprNode(ExprNode::kCall, type),
        method_index(0),
        argv(std::move(argv)),
        args_to_free(std::move(args_to_free))
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kCall; }

    uint32_t method_index;
    std::span<VReg> argv;
    std::span<VReg> args_to_free;
    VReg spread_reg;
    VReg fn_reg;
};

struct ConstExprNode : ExprNode {
    ConstExprNode(const TypeDesc* type, cell_t value)
      : ExprNode(ExprNode::kConstant, type), value(value)
    {}

    ConstExprNode(const TypeDesc* type, int64_t value64)
      : ExprNode(ExprNode::kConstant, type), value64(value64)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kConstant; }

    union {
        cell_t value;
        int64_t value64;
    };
};

struct LoadElemExprNode : ExprNode {
    LoadElemExprNode(const TypeDesc* type, LLOp opcode, ExprNode* base, ExprNode* index)
      : ExprNode(ExprNode::kLoadElem, type),
        opcode(opcode),
        base(base),
        index(index)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kLoadElem; }

    LLOp opcode;
    ExprNode* base;
    ExprNode* index;
};

struct SlotOpExprNode : ExprNode {
    SlotOpExprNode(const TypeDesc* type, LLOp opcode, VReg reg)
      : ExprNode(ExprNode::kSlotOp, type),
        opcode(opcode),
        reg(reg)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kSlotOp; }

    LLOp opcode;
    VReg reg;
};

struct LoadUpvarExprNode : ExprNode {
    LoadUpvarExprNode(const TypeDesc* type, uint32_t index)
      : ExprNode(ExprNode::kLoadUpvar, type),
        index(index)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kLoadUpvar; }

    uint32_t index;
};

struct LoadFieldExprNode : ExprNode {
    LoadFieldExprNode(const TypeDesc* type, ExprNode* base, uint32_t offset)
      : ExprNode(ExprNode::kLoadField, type),
        base(base),
        offset(offset)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kLoadField; }

    ExprNode* base;
    uint32_t offset;
};

struct LoadFnExprNode : ExprNode {
    LoadFnExprNode(const TypeDesc* type, uint32_t fn_id)
      : ExprNode(ExprNode::kLoadFn, type),
        fn_id(fn_id)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kLoadFn; }

    uint32_t fn_id;
};

struct UnaryExprNode : ExprNode {
    UnaryExprNode(const TypeDesc* type, LLOp opcode, ExprNode* operand)
      : ExprNode(ExprNode::kUnaryOp, type),
        opcode(opcode),
        operand(operand)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kUnaryOp; }

    LLOp opcode;
    ExprNode* operand;
};

struct BinaryExprNode : ExprNode {
    BinaryExprNode(const TypeDesc* type, LLOp opcode, ExprNode* left, ExprNode* right)
      : ExprNode(ExprNode::kBinaryOp, type),
        opcode(opcode),
        left(left),
        right(right)
    {}

    static bool is_a(ExprNode* node) { return node->kind == ExprNode::kBinaryOp; }

    LLOp opcode;
    ExprNode* left;
    ExprNode* right;
};

} // namespace sp::v2
