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
#include "v2/lowering/lowering.h"

#include <assert.h>
#include <string.h>

#include <list>
#include <memory>
#include <span>
#include <vector>

#include <amtl/am-vector.h>
#include "binary-reader.h"
#include "type-desc.h"
#include "v2/control-flow.h"
#include "v2/lowering/llcode.h"
#include "v2/lowering/ll-op.h"
#include "v2/lowering/lowering-assembler.h"
#include "v2/method-info.h"
#include "v2/opcodes.h"
#include "v2/runtime.h"
#include "utils/bitset.h"
#include "utils/pool-allocator.h"

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
        kLoadElem,
        kCall
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
        return kind == kReg || kind == kConstant;
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
            const smx_rtti_method* method;
            std::span<VReg> argv;
            std::span<VReg> args_to_free;
        } call;
    };
};

struct LoweringData : public IBlockData {
    LoweringData() : propagated(false) {}
    explicit LoweringData(const std::vector<ExprNode*>& stack)
     : stack(stack),
       propagated(true)
    {}
    std::vector<ExprNode*> stack;
    bool propagated;
};

class MethodLowerer
{
  public:
    MethodLowerer(ControlFlowGraph* graph, MethodInfo* method)
     : graph_(graph),
       method_(method),
       rt_(graph->rt()),
       image_(graph->rt()->image()),
       cell_type_(graph->rt()->GetPrimitiveType(TypeKind::Int32)),
       int64_type_(graph->rt()->GetPrimitiveType(TypeKind::Int64)),
       float32_type_(graph->rt()->GetPrimitiveType(TypeKind::Float32)),
       null_type_(graph->rt()->GetPrimitiveType(TypeKind::Null)),
       reader_(nullptr, nullptr)
    {}

    std::unique_ptr<InterpCode> Lower();

  private:
    void LowerBlock(Block* next_block);
    void LowerInstruction(OPCODE op);
    void EmitJumpTarget(Block* target_block);
    void PatchJumps();

    void emitOp(LLOp op) {
        masm_.emit<uint16_t>((uint16_t)op);
    }

    template <typename T>
    void emitVal(T val) {
        masm_.emit<T>(val);
    }

    void emitVal(VReg val) {
        assert(val.valid());
        masm_.emit<uint16_t>(val.index);
    }

    template <typename... Args>
    void emit(LLOp op, Args... args) {
        emitOp(op);
        (emitVal(args), ...);
    }

    ExprNode* popStack() {
        return ke::PopBack(&stack_);
    }

    void pushStack(ExprNode* node) {
        stack_.push_back(node);
    }

    VReg AllocateTemp(const TypeDesc* type);
    VReg AllocateTempCells(uint16_t cells, bool is_gcobj = false);
    void FreeReg(VReg reg);

    ExprNode* CreateLocalNode(const TypeDesc* type, VReg reg) {
        return pool_.make<ExprNode>(ExprNode::kReg, type, reg, false);
    }

    ExprNode* CreateConstNode(const TypeDesc* type, cell_t value) {
        return pool_.make<ExprNode>(type, value);
    }

    ExprNode* CreateConstNode64(const TypeDesc* type, int64_t value) {
        return pool_.make<ExprNode>(type, value);
    }

    ExprNode* CreateOpNode(const TypeDesc* type, LLOp op, ExprNode* left, ExprNode* right) {
        ExprNode* node = pool_.make<ExprNode>();
        node->kind = ExprNode::kSimpleOp;
        node->type = type;
        node->op.opcode = op;
        node->op.left = left;
        node->op.right = right;
        return node;
    }

    ExprNode* CreateLoadElemNode(const TypeDesc* type, LLOp opcode, ExprNode* base, ExprNode* index) {
        ExprNode* node = pool_.make<ExprNode>();
        node->kind = ExprNode::kLoadElem;
        node->type = type;
        node->load_elem.opcode = opcode;
        node->load_elem.base = base;
        node->load_elem.index = index;
        return node;
    }

    uint16_t GetCellCount(const TypeDesc* type) const {
        uint32_t cells = type->slot_size() / 4;
        assert(cells <= UINT16_MAX);
        return cells;
    }

    ExprNode* CreateTempNode(const TypeDesc* type, VReg reg, bool owns_reg = true) {
        return pool_.make<ExprNode>(ExprNode::kReg, type, reg, owns_reg);
    }

    ExprNode* CreateCallNode(const TypeDesc* type, const smx_rtti_method* method, std::span<VReg>&& argv, std::span<VReg>&& args_to_free) {
        ExprNode* node = pool_.make<ExprNode>(ExprNode::kCall, type, VReg(), false);
        node->call.method = method;
        node->call.argv = std::move(argv);
        node->call.args_to_free = std::move(args_to_free);
        return node;
    }

    void InitializeRegisters();

    VReg OffsetToVReg(int32_t offset) const {
        auto it = offset_to_vreg_.find(offset);
        assert(it != offset_to_vreg_.end());

        return it->second;
    }

    void FlushEmitStack();
    void FlushEmit(ExprNode* node);
    void ReconcileStack(Block* target);
    void EmitMove(VReg src, VReg dest, const TypeDesc* type);
    VReg EmitNode(ExprNode* node, VReg target_reg = VReg());
    void EmitCall(const smx_rtti_method* method, VReg dest_reg, const std::span<VReg>& argv);

    void LowerCall(uint32_t method_index, std::optional<uint8_t> argc);
    void LowerBinary(LLOp op_i32, LLOp op_f32, LLOp op_i64,
                     const TypeDesc* force_result_type = nullptr);
    void LowerUnary(LLOp op_i32, const TypeDesc* force_result_type = nullptr);
    void LowerUnary(LLOp op_i32, LLOp op_f32, LLOp op_i64, const TypeDesc* force_result_type = nullptr);

  private:
    ControlFlowGraph* graph_;
    MethodInfo* method_;
    Runtime* rt_;
    SmxImage* image_;
    LoweringAssembler masm_;
    std::vector<Block*> blocks_by_id_;
    std::vector<size_t> jumps_to_patch_;
    std::vector<InterpCode::OffsetMapping> mappings_;
    const TypeDesc* cell_type_ = nullptr;
    const TypeDesc* int64_type_ = nullptr;
    const TypeDesc* float32_type_ = nullptr;
    const TypeDesc* null_type_ = nullptr;
    BinaryReader reader_;

    Block* block_ = nullptr;
    Block* next_block_ = nullptr;
    std::vector<ExprNode*> stack_;
    uint32_t base_temp_reg_ = 0;
    uint32_t num_temp_regs_ = 0;
    PoolAllocator pool_;
    std::unordered_map<int32_t, VReg> offset_to_vreg_;
    BitSet temp_regs_used_;
    BitSet gcobj_regs_;
};

std::unique_ptr<InterpCode> MethodLowerer::Lower() {
    InitializeRegisters();
    AutoClearBlockData<LoweringData> clear_block_data(graph_);

    // :TODO: the last block has the max ID.
    uint32_t max_id = 0;
    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
        if ((*iter)->id() > max_id)
            max_id = (*iter)->id();
    }
    blocks_by_id_.resize(max_id + 1, nullptr);

    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++)
        blocks_by_id_[(*iter)->id()] = *iter;

    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
        block_ = *iter;
        auto next_iter = iter;
        next_iter++;
        Block* next_block = (next_iter != graph_->rpoEnd()) ? *next_iter : nullptr;
        LowerBlock(next_block);
    }
    block_ = nullptr;

    PatchJumps();

    if (num_temp_regs_ >= UINT16_MAX) {
        rt_->ReportErrorNumber(SP_ERROR_STACKLOW);
        return nullptr;
    }

    auto bytes = std::make_unique<uint8_t[]>(masm_.code_size());
    memcpy(bytes.get(), masm_.bytes(), masm_.code_size());
    return std::make_unique<InterpCode>(std::move(bytes), masm_.code_size(), num_temp_regs_,
                                        std::move(mappings_), std::move(gcobj_regs_));
}

void MethodLowerer::InitializeRegisters() {
    uint32_t current_reg = 0;

    const auto& arg_types = method_->arg_types();
    const BitSet& mutated_args = method_->mutated_args();
    std::vector<int32_t> shadow_args;

    for (size_t i = 0; i < arg_types.size(); i++) {
        int32_t offset = -(int32_t)(i + 1);
        offset_to_vreg_[offset] = VReg(current_reg, 1, false);

        if (mutated_args.test(i) && arg_types[i]->IsHeapItem())
            shadow_args.push_back(offset);

        // Arguments are always passed in a single cell, for backward
        // compatibility with OP_SYSREQ_N natives.
        current_reg += 1;
    }

    const auto& local_types = method_->local_types();
    for (size_t i = 0; i < local_types.size(); i++) {
        uint16_t cells = GetCellCount(local_types[i]);
        if (local_types[i]->IsHeapItem())
            gcobj_regs_.set(current_reg);
        offset_to_vreg_[i] = VReg(current_reg, cells, false);
        current_reg += cells;
    }

    // If a HeapItem argument is mutated, we cannot reuse its register because the
    // frame cleanup would Release() the caller's borrowed reference. Instead, we
    // allocate a shadow register and take ownership of a new reference.
    for (int32_t offset : shadow_args) {
        VReg orig_reg = offset_to_vreg_[offset];

        VReg shadow_reg(current_reg, 1, false);
        gcobj_regs_.set(current_reg);
        current_reg += 1;

        emit(LL_MOVE, orig_reg, shadow_reg);
        emit(LL_ADDREF, shadow_reg);

        offset_to_vreg_[offset] = shadow_reg;
    }

    base_temp_reg_ = current_reg;
    num_temp_regs_ = current_reg;
}

void MethodLowerer::LowerBlock(Block* next_block) {
    next_block_ = next_block;
    block_->label()->bind(masm_.pc());

    const uint8_t* stop_at = block_->end();
    reader_ = BinaryReader(block_->start(), stop_at);

    LoweringData* data = block_->data<LoweringData>();
    if (data)
        stack_ = data->stack;
    else
        stack_.clear();

    while (reader_.cursor() < stop_at) {
        uint32_t high_offset = (uint32_t)(reader_.cursor() - graph_->rt()->code().bytes);
        mappings_.push_back({(uint32_t)masm_.pc(), high_offset});

        OPCODE op = (OPCODE)reader_.read<uint8_t>();
        LowerInstruction(op);
    }

    FlushEmitStack();

    if (block_->endType() == BlockEnd::Jump) {
        Block* target = block_->successors()[0];
        ReconcileStack(target);
        if (target != next_block) {
            emitOp(LL_JUMP);
            EmitJumpTarget(target);
        }
    }

    // Propagate stack state.
    for (Block* succ : block_->successors()) {
        if (succ->id() > block_->id()) {
            LoweringData* succ_data = succ->data<LoweringData>();
            if (!succ_data || !succ_data->propagated)
                succ->setData(new LoweringData(stack_));
        }
    }
}

void MethodLowerer::LowerInstruction(OPCODE op) {
    switch (op) {
        case OP_NOP:
            emit(LL_NOP);
            break;

        case OP_LOAD_I_I32:
        case OP_LOAD_I_F32: {
            LowerUnary(op == OP_LOAD_I_I32 ? LL_LOAD_I_I32 : LL_LOAD_I_F32,
                       op == OP_LOAD_I_I32 ? cell_type_ : float32_type_);
            break;
        }

        case OP_LOAD_I_I64: {
            LowerUnary(LL_LOAD_I_I64, int64_type_);
            break;
        }

        case OP_LOAD_I_U8: {
            LowerUnary(LL_LOAD_I_U8, cell_type_);
            break;
        }

        case OP_LOAD_ELEM_I32:
        case OP_LOAD_ELEM_F32:
        case OP_LOAD_ELEM_I64:
        case OP_LOAD_ELEM_U8:
        case OP_LOAD_ELEM_A: {
            ExprNode* index = popStack();
            ExprNode* base_node = popStack();
            const TypeDesc* base = base_node->type;

            const TypeDesc* elt = base->array_elt();
            const TypeDesc* result_type = (op == OP_LOAD_ELEM_U8) ? cell_type_ : elt;

            LLOp llop = LL_NOP;
            if (base->IsFlatArray()) {
                assert(op != OP_LOAD_ELEM_A);
                switch (op) {
                    case OP_LOAD_ELEM_I32: llop = LL_LOAD_I_I32; break;
                    case OP_LOAD_ELEM_F32: llop = LL_LOAD_I_F32; break;
                    case OP_LOAD_ELEM_I64: llop = LL_LOAD_I_I64; break;
                    case OP_LOAD_ELEM_U8:  llop = LL_LOAD_I_U8; break;
                    default: assert(false); break;
                }
            } else {
                switch (op) {
                    case OP_LOAD_ELEM_I32: llop = LL_LOAD_ELEM_I32; break;
                    case OP_LOAD_ELEM_F32: llop = LL_LOAD_ELEM_F32; break;
                    case OP_LOAD_ELEM_I64: llop = LL_LOAD_ELEM_I64; break;
                    case OP_LOAD_ELEM_U8:  llop = LL_LOAD_ELEM_U8; break;
                    case OP_LOAD_ELEM_A:   llop = LL_LOAD_ELEM_A; break;
                    default: assert(false); break;
                }
            }

            pushStack(CreateLoadElemNode(result_type, llop, base_node, index));
            break;
        }

        case OP_STOR_I_I32:
        case OP_STOR_I_F32:
        case OP_STOR_I_I64:
        case OP_STOR_I_U8:
        case OP_STOR_I_A: {
            LLOp llop = LL_NOP;
            switch (op) {
                case OP_STOR_I_I32: llop = LL_STOR_I_I32; break;
                case OP_STOR_I_F32: llop = LL_STOR_I_F32; break;
                case OP_STOR_I_I64: llop = LL_STOR_I_I64; break;
                case OP_STOR_I_U8:  llop = LL_STOR_I_U8; break;
                case OP_STOR_I_A:   llop = LL_STOR_I_A; break;
                default: assert(false); break;
            }
            ExprNode* val = popStack();
            ExprNode* addr = popStack();

            FlushEmitStack();

            VReg val_reg = EmitNode(val);
            VReg addr_reg = EmitNode(addr);

            emit(llop, addr_reg, val_reg);

            FreeReg(val_reg);
            FreeReg(addr_reg);
            break;
        }

        case OP_STOR_ELEM_I32:
        case OP_STOR_ELEM_F32:
        case OP_STOR_ELEM_I64:
        case OP_STOR_ELEM_U8:
        case OP_STOR_ELEM_A: {
            ExprNode* val = popStack();
            ExprNode* index = popStack();
            ExprNode* base_node = popStack();
            const TypeDesc* base = base_node->type;

            FlushEmitStack();

            VReg val_reg = EmitNode(val);
            VReg index_reg = EmitNode(index);
            VReg base_reg = EmitNode(base_node);

            if (base->IsFlatArray()) {
                LLOp llop = LL_NOP;
                switch (op) {
                    case OP_STOR_ELEM_I32: llop = LL_STOR_ELEM_FLAT_I32; break;
                    case OP_STOR_ELEM_F32: llop = LL_STOR_ELEM_FLAT_F32; break;
                    case OP_STOR_ELEM_I64: llop = LL_STOR_ELEM_FLAT_I64; break;
                    case OP_STOR_ELEM_U8:  llop = LL_STOR_ELEM_FLAT_U8; break;
                    default: assert(false); break;
                }
                emit(llop, (uint32_t)base->array_size(), (uint16_t)base->array_elt()->element_size(), base_reg, index_reg, val_reg);
            } else {
                LLOp llop = LL_NOP;
                switch (op) {
                    case OP_STOR_ELEM_I32: llop = LL_STOR_ELEM_I32; break;
                    case OP_STOR_ELEM_F32: llop = LL_STOR_ELEM_F32; break;
                    case OP_STOR_ELEM_I64: llop = LL_STOR_ELEM_I64; break;
                    case OP_STOR_ELEM_U8:  llop = LL_STOR_ELEM_U8; break;
                    case OP_STOR_ELEM_A:   llop = LL_STOR_ELEM_A; break;
                    default: assert(false); break;
                }
                emit(llop, base_reg, index_reg, val_reg);
            }

            FreeReg(val_reg);
            FreeReg(index_reg);
            FreeReg(base_reg);
            break;
        }

        case OP_POP: {
            ExprNode* val = stack_.back();
            if (!val->IsInvariant()) {
                VReg reg = EmitNode(val);
                if (reg.valid())
                    FreeReg(reg);
            } else if (val->kind == ExprNode::kReg) {
                FreeReg(val->reg);
            }
            popStack();
            break;
        }

        case OP_DUP: {
            FlushEmitStack();
            ExprNode* top = stack_.back();
            if (top->kind == ExprNode::kReg) {
                ExprNode* dup = CreateTempNode(top->type, top->reg, false);
                pushStack(dup);
            } else {
                assert(top->kind != ExprNode::kSimpleOp && top->kind != ExprNode::kLoadElem);
                pushStack(top);
            }
            break;
        }

        case OP_SWAP: {
            FlushEmitStack();
            ExprNode* val1 = popStack();
            ExprNode* val2 = popStack();
            pushStack(val1);
            pushStack(val2);
            break;
        }

        case OP_RETN: {
            ExprNode* val = popStack();

            FlushEmitStack();

            VReg val_reg = EmitNode(val);
            if (val->type->IsHeapItem())
                emit(LL_RETN_A, val_reg);
            else
                emit(LL_RETN, val_reg);

            FreeReg(val_reg);
            break;
        }

        case OP_RETV: {
            FlushEmitStack();
            emit(LL_RETV);
            break;
        }

        case OP_SHL:
            LowerBinary(LL_SHL,  LL_NOP, LL_SHL_I64);
            break;
        case OP_SHR:
            LowerBinary(LL_SHR,  LL_NOP, LL_SHR_I64);
            break;
        case OP_SSHR:
            LowerBinary(LL_SSHR, LL_NOP, LL_SSHR_I64);
            break;
        case OP_AND:
            LowerBinary(LL_AND,  LL_NOP, LL_AND_I64);
            break;
        case OP_OR:
            LowerBinary(LL_OR,   LL_NOP, LL_OR_I64);
            break;
        case OP_XOR:
            LowerBinary(LL_XOR,  LL_NOP, LL_XOR_I64);
            break;
        case OP_SUB:
            LowerBinary(LL_SUB_I32, LL_SUB_F32, LL_SUB_I64);
            break;
        case OP_SMUL:
            LowerBinary(LL_SMUL_I32, LL_MUL_F32, LL_SMUL_I64);
            break;
        case OP_SDIV:
            LowerBinary(LL_SDIV_I32, LL_DIV_F32, LL_SDIV_I64);
            break;
        case OP_SMOD:
            LowerBinary(LL_SMOD_I32, LL_MOD_F32, LL_SMOD_I64);
            break;
        case OP_ADD:
            LowerBinary(LL_ADD_I32, LL_ADD_F32, LL_ADD_I64);
            break;
        case OP_NOT:
            LowerUnary(LL_NOT, cell_type_);
            break;

        case OP_INC:
        case OP_DEC: {
            ExprNode* a = popStack();
            pushStack(a);
            if (a->type->kind() == TypeKind::Float32) {
                pushStack(CreateConstNode(float32_type_, sp_ftoc(op == OP_INC ? 1.0f : -1.0f)));
                LowerBinary(LL_ADD_I32, LL_ADD_F32, LL_ADD_I64, float32_type_);
            } else if (a->type->kind() == TypeKind::Int64) {
                pushStack(CreateConstNode64(int64_type_, op == OP_INC ? 1 : -1));
                LowerBinary(LL_ADD_I32, LL_ADD_F32, LL_ADD_I64, int64_type_);
            } else {
                pushStack(CreateConstNode(cell_type_, op == OP_INC ? 1 : -1));
                LowerBinary(LL_ADD_I32, LL_ADD_F32, LL_ADD_I64, cell_type_);
            }
            break;
        }

        case OP_NEG:
            LowerUnary(LL_NEG, LL_NEG_F32, LL_NEG_I64);
            break;
        case OP_INVERT:
            LowerUnary(LL_INVERT, LL_NOP, LL_INVERT_I64);
            break;
        case OP_TEST:
            LowerUnary(LL_TEST_I32, LL_TEST_F32, LL_TEST_I64, cell_type_);
            break;
        case OP_CVT_F32:
            LowerUnary(LL_CVT_F32, float32_type_);
            break;

        case OP_COPYARRAY: {
            ExprNode* src_node = popStack();
            ExprNode* dest_node = popStack();

            FlushEmitStack();

            VReg src_reg = EmitNode(src_node);
            VReg dest_reg = EmitNode(dest_node);

            const TypeDesc* src = src_node->type;
            const TypeDesc* dest = dest_node->type;
            if (dest->IsFlatArray() || src->IsFlatArray()) {
                VReg flat_src_reg = src_reg;
                VReg flat_dest_reg = dest_reg;

                if (!src->IsFlatArray()) {
                    flat_src_reg = AllocateTemp(cell_type_);
                    emit(LL_ARRAY_TO_FLAT, src_reg, flat_src_reg);
                } else if (!dest->IsFlatArray()) {
                    flat_dest_reg = AllocateTemp(cell_type_);
                    emit(LL_ARRAY_TO_FLAT, dest_reg, flat_dest_reg);
                }

                uint32_t elements = dest->array_size();
                if (src->kind() == TypeKind::FixedArray || src->kind() == TypeKind::FlatArray) {
                    elements = src->array_size();
                }
                if (dest->array_elt()->IsHeapItem()) {
                    emit(LL_COPYARRAY_FLAT_A, elements, flat_src_reg, flat_dest_reg);
                } else {
                    uint32_t bytes = elements * dest->array_elt()->element_size();
                    emit(LL_COPYARRAY_FLAT, bytes, flat_src_reg, flat_dest_reg);
                }

                if (!src->IsFlatArray()) {
                    FreeReg(flat_src_reg);
                }
                if (!dest->IsFlatArray()) {
                    FreeReg(flat_dest_reg);
                }
            } else {
                emit(LL_COPYARRAY, src_reg, dest_reg);
            }
            FreeReg(src_reg);
            FreeReg(dest_reg);
            break;
        }

        case OP_SLICE:
        case OP_SLICE_AS: {
            const TypeDesc* result_type;
            ExprNode* index_node;
            ExprNode* base_node;

            if (op == OP_SLICE_AS) {
                uint32_t type_id = reader_.read<uint32_t>();
                result_type = rt_->LoadTypeFromId(type_id);
                index_node = CreateConstNode(cell_type_, 0);
                base_node = popStack();
            } else {
                index_node = popStack();
                base_node = popStack();
                assert(base_node->type->IsArrayish());
                result_type = graph_->rt()->GetSliceType(base_node->type->array_elt());
            }

            const TypeDesc* base = base_node->type;
            FlushEmitStack();
            VReg index_reg = EmitNode(index_node);
            VReg base_reg = EmitNode(base_node);

            VReg dest = AllocateTemp(result_type);

            if (base->IsFlatArray()) {
                emit(LL_SLICE_FLAT, base, base_reg, index_reg, dest);
            } else {
                emit(LL_SLICE, base_reg, index_reg, dest);
            }

            FreeReg(index_reg);
            FreeReg(base_reg);
            pushStack(CreateTempNode(result_type, dest));
            break;
        }

        case OP_CVT_I64: {
            LowerUnary(LL_CVT_I64, int64_type_);
            break;
        }

        case OP_TRUNCATE_I64: {
            LowerUnary(LL_TRUNCATE_I64, cell_type_);
            break;
        }

        case OP_EQ:
            LowerBinary(LL_EQ_I32, LL_EQ_F32, LL_EQ_I64, cell_type_);
            break;
        case OP_NEQ:
            LowerBinary(LL_NEQ_I32, LL_NEQ_F32, LL_NEQ_I64, cell_type_);
            break;
        case OP_SLESS:
            LowerBinary(LL_SLESS_I32, LL_LESS_F32, LL_SLESS_I64, cell_type_);
            break;
        case OP_SLEQ:
            LowerBinary(LL_SLEQ_I32, LL_LEQ_F32, LL_SLEQ_I64, cell_type_);
            break;
        case OP_SGRTR:
            LowerBinary(LL_SGRTR_I32, LL_GRTR_F32, LL_SGRTR_I64, cell_type_);
            break;
        case OP_SGEQ:
            LowerBinary(LL_SGEQ_I32, LL_GEQ_F32, LL_SGEQ_I64, cell_type_);
            break;

        case OP_LOAD_GLB: {
            uint16_t index = reader_.read<uint16_t>();
            const TypeDesc* type = graph_->rt()->GetTypeOfGlobal(index);
            VReg dest = AllocateTemp(type);
            LLOp llop = LL_LOAD_GLB_X32;
            if (type->IsHeapItem())
                llop = LL_LOAD_GLB_A;
            else if (type->IsInt64())
                llop = LL_LOAD_GLB_X64;
            emit(llop, index, dest);
            pushStack(CreateTempNode(type, dest));
            break;
        }

        case OP_STOR_GLB: {
            uint16_t index = reader_.read<uint16_t>();
            ExprNode* val = popStack();
            FlushEmitStack();
            VReg val_reg = EmitNode(val);

            LLOp llop = LL_STOR_GLB_X32;
            if (val->type->IsHeapItem())
                llop = LL_STOR_GLB_A;
            else if (val->type->IsInt64())
                llop = LL_STOR_GLB_X64;

            emit(llop, index, val_reg);
            FreeReg(val_reg);
            break;
        }

        case OP_ADDR_GLB: {
            uint16_t index = reader_.read<uint16_t>();
            const TypeDesc* td = graph_->rt()->GetTypeOfGlobal(index);
            const TypeDesc* ptr_type = td->IsCompositeValue() ? td : graph_->rt()->GetReferenceType(td);
            VReg dest = AllocateTemp(ptr_type);
            emit(LL_ADDR_GLB, index, dest);
            pushStack(CreateTempNode(ptr_type, dest));
            break;
        }

        case OP_LOAD_STR: {
            uint16_t index = reader_.read<uint16_t>();
            const TypeDesc* type = graph_->rt()->GetStringLitType(index);
            VReg dest = AllocateTemp(type);
            emit(LL_LOAD_STR, index, dest);
            pushStack(CreateTempNode(type, dest));
            break;
        }

        case OP_LOAD_S: {
            int16_t offset = reader_.read<int16_t>();
            const TypeDesc* type = method_->GetTypeOfLocal(offset);
            pushStack(CreateLocalNode(type, OffsetToVReg(offset)));
            break;
        }

        case OP_STOR_S: {
            int16_t offset = reader_.read<int16_t>();
            const TypeDesc* type = method_->GetTypeOfLocal(offset);
            ExprNode* val = popStack();

            FlushEmitStack();
            if (type->IsHeapItem()) {
                // We must evaluate the RHS into a temporary register first to prevent
                // use-after-free bugs if the RHS expression references the target local itself.
                VReg src = EmitNode(val);
                emit(LL_STOR_S_A, OffsetToVReg(offset), src);
                FreeReg(src);
            } else {
                VReg target = OffsetToVReg(offset);
                EmitNode(val, target);
            }
            break;
        }

        case OP_ADDR_S: {
            int16_t offset = reader_.read<int16_t>();
            const TypeDesc* td = method_->GetTypeOfLocal(offset);
            const TypeDesc* ptr_type = td->IsCompositeValue() ? td : graph_->rt()->GetReferenceType(td);
            VReg dest = AllocateTemp(ptr_type);
            emit(LL_ADDR_S, OffsetToVReg(offset), dest);
            pushStack(CreateTempNode(ptr_type, dest));
            break;
        }

        case OP_STOR_S_C: {
            int16_t offset = reader_.read<int16_t>();
            cell_t value = reader_.read<cell_t>();
            FlushEmitStack();
            VReg dest = OffsetToVReg(offset);
            emit(LL_LOAD_CONST, value, dest);
            break;
        }

        case OP_IDXADDR: {
            ExprNode* index = popStack();
            ExprNode* base_node = popStack();
            const TypeDesc* base = base_node->type;
            FlushEmitStack();
            VReg index_reg = EmitNode(index);
            VReg base_reg = EmitNode(base_node);

            const TypeDesc* result_type = graph_->rt()->GetReferenceType(base->array_elt());
            VReg dest = AllocateTemp(result_type);

            if (base->IsFlatArray()) {
                emit(LL_IDXADDR_FLAT, (uint32_t)base->array_size(), (uint16_t)base->array_elt()->element_size(), base_reg, index_reg, dest);
            } else {
                emit(LL_IDXADDR, base_reg, index_reg, dest);
            }

            FreeReg(index_reg);
            FreeReg(base_reg);
            pushStack(CreateTempNode(result_type, dest));
            break;
        }

        case OP_PUSH_C: {
            cell_t value = reader_.read<cell_t>();
            pushStack(CreateConstNode(cell_type_, value));
            break;
        }

        case OP_PUSH_C_I8: {
            int8_t value = reader_.read<int8_t>();
            pushStack(CreateConstNode(cell_type_, (cell_t)value));
            break;
        }

        case OP_PUSH_C_I64: {
            int64_t value = reader_.read<int64_t>();
            pushStack(CreateConstNode64(int64_type_, value));
            break;
        }

        case OP_PUSH_C_F32: {
            float value = reader_.read<float>();
            pushStack(CreateConstNode(float32_type_, sp_ftoc(value)));
            break;
        }

        case OP_LOAD_NULL: {
            pushStack(CreateConstNode(null_type_, 0));
            break;
        }

        case OP_LOAD_FN: {
            uint32_t fn_id = reader_.read<uint32_t>();
            VReg dest = AllocateTemp(cell_type_);
            emit(LL_LOAD_FN, fn_id, dest);
            pushStack(CreateTempNode(cell_type_, dest));
            break;
        }

        case OP_LOAD_FLD: {
            uint32_t ref_index = reader_.read<uint32_t>();
            auto ref = image_->getFieldRef(ref_index);
            auto classdef = image_->getClassdef(ref->cls_index);
            auto field = image_->getField(ref->field_index);
            const TypeDesc* field_td = rt_->LoadTypeFromId(field->type_id);
            const TypeDesc* class_td = rt_->GetEnumStructType(classdef);

            uint32_t relative_field_index = ref->field_index - classdef->first_field;
            uint32_t offset = class_td->cls_offsets()[relative_field_index];

            ExprNode* base_node = popStack();
            FlushEmitStack();
            VReg base_reg = EmitNode(base_node);

            VReg dest = AllocateTemp(field_td);

            LLOp llop = LL_LOAD_FLD_X32;
            if (field_td->IsHeapItem())
                llop = LL_LOAD_FLD_A;
            else if (field_td->IsInt64())
                llop = LL_LOAD_FLD_X64;
            emit(llop, offset, base_reg, dest);

            FreeReg(base_reg);
            pushStack(CreateTempNode(field_td, dest));
            break;
        }

        case OP_ADDR_FLD: {
            uint32_t ref_index = reader_.read<uint32_t>();
            auto ref = image_->getFieldRef(ref_index);
            auto classdef = image_->getClassdef(ref->cls_index);
            auto field = image_->getField(ref->field_index);
            const TypeDesc* field_td = rt_->LoadTypeFromId(field->type_id);
            const TypeDesc* class_td = rt_->GetEnumStructType(classdef);
            const TypeDesc* pushed_td = field_td->IsCompositeValue() ? field_td : rt_->GetReferenceType(field_td);

            uint32_t relative_field_index = ref->field_index - classdef->first_field;
            uint32_t offset = class_td->cls_offsets()[relative_field_index];

            ExprNode* base_node = popStack();
            FlushEmitStack();
            VReg base_reg = EmitNode(base_node);

            VReg dest = AllocateTemp(pushed_td);

            emit(LL_ADDR_FLD, offset, base_reg, dest);

            FreeReg(base_reg);
            pushStack(CreateTempNode(pushed_td, dest));
            break;
        }

        case OP_STOR_FLD: {
            uint32_t ref_index = reader_.read<uint32_t>();
            auto ref = image_->getFieldRef(ref_index);
            auto classdef = image_->getClassdef(ref->cls_index);
            auto field = image_->getField(ref->field_index);
            const TypeDesc* field_td = rt_->LoadTypeFromId(field->type_id);
            const TypeDesc* class_td = rt_->GetEnumStructType(classdef);

            uint32_t relative_field_index = ref->field_index - classdef->first_field;
            uint32_t offset = class_td->cls_offsets()[relative_field_index];

            ExprNode* val_node = popStack();
            ExprNode* base_node = popStack();

            FlushEmitStack();

            VReg val_reg = EmitNode(val_node);
            VReg base_reg = EmitNode(base_node);

            LLOp llop = LL_STOR_FLD_X32;
            if (field_td->IsHeapItem())
                llop = LL_STOR_FLD_A;
            else if (field_td->IsInt64())
                llop = LL_STOR_FLD_X64;

            emit(llop, offset, base_reg, val_reg);

            FreeReg(val_reg);
            FreeReg(base_reg);
            break;
        }

        case OP_LOAD_FLD_OFFSET: {
            uint32_t ref_index = reader_.read<uint32_t>();
            auto ref = image_->getFieldRef(ref_index);
            auto classdef = image_->getClassdef(ref->cls_index);
            const TypeDesc* class_td = rt_->GetEnumStructType(classdef);

            uint32_t relative_field_index = ref->field_index - classdef->first_field;
            uint32_t offset = class_td->cls_offsets()[relative_field_index];
            uint32_t cell_offset = offset / sizeof(cell_t);

            pushStack(CreateConstNode(cell_type_, (cell_t)cell_offset));
            break;
        }

        case OP_LOAD_ES_SIZE: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = rt_->LoadTypeFromId(type_id);
            uint32_t cell_size = td->slot_size() / sizeof(cell_t);

            pushStack(CreateConstNode(cell_type_, (cell_t)cell_size));
            break;
        }

        case OP_COPYOBJ: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = rt_->LoadTypeFromId(type_id);

            ExprNode* src_node = popStack();
            ExprNode* dest_node = popStack();

            FlushEmitStack();

            VReg src_reg = EmitNode(src_node);
            VReg dest_reg = EmitNode(dest_node);

            emit(LL_COPYOBJ, (uint32_t)td->slot_size(), src_reg, dest_reg);

            FreeReg(src_reg);
            FreeReg(dest_reg);
            break;
        }

        case OP_SLICE_ES: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = rt_->LoadTypeFromId(type_id);
            uint32_t cell_size = td->slot_size() / sizeof(cell_t);

            ExprNode* base_node = popStack();

            FlushEmitStack();

            const TypeDesc* any_type = graph_->rt()->GetPrimitiveType(TypeKind::Any);
            const TypeDesc* slice_type = graph_->rt()->GetSliceType(any_type);

            VReg base_reg = EmitNode(base_node);
            VReg dest = AllocateTemp(slice_type);

            emit(LL_SLICE_ES, cell_size, base_reg, dest);

            FreeReg(base_reg);
            pushStack(CreateTempNode(slice_type, dest));
            break;
        }

        case OP_CALL: {
            uint32_t method_id = reader_.read<uint32_t>();
            LowerCall(method_id, {});
            break;
        }

        case OP_CALLN: {
            uint32_t method_id = reader_.read<uint32_t>();
            uint8_t nargs = reader_.read<uint8_t>();
            LowerCall(method_id, {nargs});
            break;
        }

        case OP_NEWARRAY: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = graph_->rt()->LoadTypeFromId(type_id);

            FlushEmitStack();

            VReg size_reg;
            ExprNode* size_node = nullptr;
            if (td->kind() == TypeKind::Array) {
                size_node = popStack();
                size_reg = EmitNode(size_node);
            }

            VReg dest = AllocateTemp(td);
            if (td->kind() == TypeKind::Array)
                emit(LL_NEWARRAY, td, size_reg, dest);
            else
                emit(LL_NEWFIXEDARRAY, td, dest);

            if (size_node)
                FreeReg(size_reg);
            pushStack(CreateTempNode(td, dest));
            break;
        }

        case OP_NEWBULKARRAY: {
            uint8_t ndims = reader_.read<uint8_t>();
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = graph_->rt()->LoadTypeFromId(type_id);

            FlushEmitStack();

            VReg base_dim_reg = AllocateTempCells(ndims);
            for (uint8_t i = 0; i < ndims; i++) {
                ExprNode* dim = popStack();
                VReg target_reg(base_dim_reg.index + i, 1, false);
                EmitNode(dim, target_reg);
            }

            VReg dest = AllocateTemp(td);
            emit(LL_NEWBULKARRAY, ndims, td, base_dim_reg, dest);

            FreeReg(base_dim_reg);

            pushStack(CreateTempNode(td, dest));
            break;
        }

        case OP_FILLARRAY: {
            ExprNode* base_node = popStack();
            uint32_t data_offs = reader_.read<uint32_t>();

            FlushEmitStack();

            VReg base_reg = EmitNode(base_node);

            if (base_node->type->IsFlatArray())
                emit(LL_FILLARRAY_FLAT, data_offs, base_node->type, base_reg);
            else
                emit(LL_FILLARRAY, data_offs, base_reg);
            FreeReg(base_reg);
            break;
        }

        case OP_JUMP: {
            FlushEmitStack();

            // Skip past jump target.
            reader_.read<cell_t>();

            Block* target = block_->successors()[0];
            ReconcileStack(target);
            if (target != next_block_) {
                emit(LL_JUMP);
                EmitJumpTarget(target);
            }
            break;
        }

        case OP_JZER:
        case OP_JNZ: {
            ExprNode* val = popStack();

            FlushEmitStack();

            // Skip past jump target.
            reader_.read<cell_t>();

            VReg val_reg = EmitNode(val);
            emit(op == OP_JZER ? LL_JZER : LL_JNZ, val_reg);

            Block* target_block = block_->successors()[1];
            EmitJumpTarget(target_block);

            Block* fallthrough_block = block_->successors()[0];
            ReconcileStack(fallthrough_block);
            if (fallthrough_block != next_block_) {
                emit(LL_JUMP);
                EmitJumpTarget(fallthrough_block);
            }

            FreeReg(val_reg);
            break;
        }

        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSGRTR:
        case OP_JSGEQ:
        case OP_JSLEQ: {
            LLOp llop = LL_NOP;
            switch (op) {
                case OP_JEQ:    llop = LL_JEQ; break;
                case OP_JNEQ:   llop = LL_JNEQ; break;
                case OP_JSLESS: llop = LL_JSLESS; break;
                case OP_JSGRTR: llop = LL_JSGRTR; break;
                case OP_JSGEQ:  llop = LL_JSGEQ; break;
                case OP_JSLEQ:  llop = LL_JSLEQ; break;
                default: assert(false); break;
            }
            // Skip past jump target.
            reader_.read<cell_t>();

            ExprNode* right = popStack();
            ExprNode* left = popStack();

            FlushEmitStack();

            VReg left_reg = EmitNode(left);
            VReg right_reg = EmitNode(right);

            emit(llop, left_reg, right_reg);
            EmitJumpTarget(block_->successors()[1]);

            Block* fallthrough_block = block_->successors()[0];
            ReconcileStack(fallthrough_block);
            if (fallthrough_block != next_block_) {
                emit(LL_JUMP);
                EmitJumpTarget(fallthrough_block);
            }

            FreeReg(left_reg);
            FreeReg(right_reg);
            break;
        }

        case OP_SWITCH: {
            ExprNode* val = popStack();

            FlushEmitStack();

            VReg val_reg = EmitNode(val);

            emit(LL_SWITCH, val_reg);

            cell_t ncases = reader_.read<cell_t>();
            // Skip default value.
            reader_.read<cell_t>();

            emitVal<cell_t>(ncases);

            Block* default_block = block_->successors()[0];
            EmitJumpTarget(default_block);

            for (cell_t i = 0; i < ncases; i++) {
                cell_t case_value = reader_.read<cell_t>();
                reader_.read<cell_t>();
                emitVal<cell_t>(case_value);
                Block* target_block = block_->successors()[1 + i];
                EmitJumpTarget(target_block);
            }
            FreeReg(val_reg);
            break;
        }

        default:
            assert(false);
            break;
    }
}

void MethodLowerer::FlushEmitStack() {
    for (ExprNode* node : stack_)
        FlushEmit(node);
}

void MethodLowerer::FlushEmit(ExprNode* node) {
    // Only skip flushing if the register is owned by the stack. Otherwise, a
    // LOAD_S later followed by DUP would result in an aliasing problem, where
    // an assignment to the local would affect both the local and the value on
    // the expression stack.
    if (node->kind == ExprNode::kReg && node->reg.owned)
        return;

    VReg temp = AllocateTemp(node->type);
    EmitNode(node, temp);

    *node = ExprNode(ExprNode::kReg, node->type, temp, true);
}

void MethodLowerer::EmitMove(VReg src, VReg dest, const TypeDesc* type) {
    // Although we can allocate sequential runs for large local variables, we
    // never move those around on the stack.
    assert(src.cells == dest.cells);
    assert(src.cells == 1 || src.cells == 2);

    if (src == dest)
        return;

    if (type->IsHeapItem()) {
        if (gcobj_regs_.test(dest.index)) {
            emit(LL_RELEASE, dest);
            emit(LL_ADDREF, src);
        }
        emit(LL_MOVE, src, dest);
    } else {
        if (type->kind() == TypeKind::Int64)
            emit(LL_MOVE_I64, src, dest);
        else
            emit(LL_MOVE, src, dest);
    }
}

void MethodLowerer::ReconcileStack(Block* target) {
    LoweringData* data = target->data<LoweringData>();
    if (!data || !data->propagated)
        return;

    const auto& target_stack = data->stack;
    assert(stack_.size() == target_stack.size());

    struct Move {
        VReg src;
        VReg dest;
        const TypeDesc* type;
    };

    std::list<Move> moves;
    for (size_t i = 0; i < stack_.size(); i++) {
        assert(stack_[i]->kind == ExprNode::kReg);
        assert(target_stack[i]->kind == ExprNode::kReg);

        VReg src = stack_[i]->reg;
        VReg dest = target_stack[i]->reg;
        if (src != dest)
            moves.push_back({src, dest, stack_[i]->type});
    }

    std::vector<VReg> temps_to_free;

    while (!moves.empty()) {
        size_t size_before = moves.size();

        auto it = moves.begin();
        while (it != moves.end()) {
            bool blocked = false;
            for (const auto& other : moves) {
                if (&other != &*it && other.src == it->dest) {
                    blocked = true;
                    break;
                }
            }

            if (!blocked) {
                EmitMove(it->src, it->dest, it->type);
                it = moves.erase(it);
                continue;
            }
            it++;
        }

        if (moves.size() < size_before)
            continue;

        auto cycle_it = moves.begin();
        VReg temp = AllocateTemp(cycle_it->type);
        temps_to_free.push_back(temp);

        EmitMove(cycle_it->src, temp, cycle_it->type);

        VReg old_src = cycle_it->src;
        for (auto& m : moves) {
            if (m.src == old_src)
                m.src = temp;
        }
    }

    for (VReg temp : temps_to_free)
        FreeReg(temp);
}

void MethodLowerer::EmitJumpTarget(Block* target_block) {
    if (target_block->label()->bound()) {
        emitVal<cell_t>(target_block->label()->offset());
    } else {
        jumps_to_patch_.push_back(masm_.pc());
        emitVal<cell_t>(target_block->id());
    }
}

void MethodLowerer::PatchJumps() {
    for (size_t patch_offset : jumps_to_patch_) {
        cell_t* patch_ptr = reinterpret_cast<cell_t*>(masm_.bytes() + patch_offset);
        cell_t target_block_id = *patch_ptr;
        Block* target_block = blocks_by_id_[target_block_id];
        assert(target_block != nullptr);
        *patch_ptr = (cell_t)target_block->label()->offset();
    }
}

void MethodLowerer::LowerBinary(LLOp op_i32, LLOp op_f32, LLOp op_i64,
                                const TypeDesc* force_result_type)
{
    ExprNode* right = popStack();
    ExprNode* left = popStack();
    LLOp llop;
    const TypeDesc* type;

    if (op_f32 != LL_NOP && left->type->kind() == TypeKind::Float32 && right->type->kind() == TypeKind::Float32) {
        llop = op_f32;
        type = force_result_type ? force_result_type : float32_type_;
    } else if (op_i64 != LL_NOP && left->type->kind() == TypeKind::Int64 && right->type->kind() == TypeKind::Int64) {
        llop = op_i64;
        type = force_result_type ? force_result_type : int64_type_;
    } else {
        llop = op_i32;
        type = force_result_type ? force_result_type : cell_type_;
    }
    pushStack(CreateOpNode(type, llop, left, right));
}

void MethodLowerer::LowerUnary(LLOp op_i32, const TypeDesc* force_result_type) {
    ExprNode* val = popStack();
    const TypeDesc* type = force_result_type ? force_result_type : val->type;
    pushStack(CreateOpNode(type, op_i32, val, nullptr));
}

void MethodLowerer::LowerUnary(LLOp op_i32, LLOp op_f32, LLOp op_i64,
                               const TypeDesc* force_result_type)
{
    ExprNode* a = popStack();
    LLOp op = LL_NOP;
    if (a->type->kind() == TypeKind::Float32) {
        op = op_f32;
    } else if (a->type->kind() == TypeKind::Int64) {
        op = op_i64;
    } else {
        op = op_i32;
    }
    const TypeDesc* type = force_result_type ? force_result_type : a->type;
    pushStack(CreateOpNode(type, op, a, nullptr));
}

void MethodLowerer::LowerCall(uint32_t method_index, std::optional<uint8_t> argc) {
    const smx_rtti_method* method = graph_->rt()->image()->GetMethod(method_index);

    uint32_t arg_count = 0;
    if (argc) {
        arg_count = *argc;
    } else {
        auto parser = graph_->rt()->image()->GetTypeParser(method->signature);
        [[maybe_unused]] bool success = parser.ReadFunctionSignatureArgCount(&arg_count);
        assert(success);
    }

    for (uint32_t i = 0; i < stack_.size() - arg_count; i++)
        FlushEmit(stack_[i]);

    std::vector<VReg> argv(arg_count);
    std::vector<VReg> args_to_free;

    for (uint32_t i = 0; i < arg_count; i++) {
        ExprNode* node = popStack();
        VReg arg_reg = EmitNode(node);

        if (node->type->IsNonFlatArray() && (method->flags & kRttiMethod_Native)) {
            VReg dest = AllocateTempCells(GetCellCount(cell_type_), false);
            emit(LL_ARRAY_TO_NATIVE, arg_reg, dest);
            args_to_free.push_back(arg_reg);
            argv[i] = dest;
        } else {
            argv[i] = arg_reg;
        }
    }

    bool is_void = graph_->rt()->image()->IsVoidMethod(method);
    const TypeDesc* return_td = nullptr;

    if (is_void) {
        EmitCall(method, VReg(), std::span<VReg>(argv));
        for (VReg reg : args_to_free)
            FreeReg(reg);
        return;
    }

    if (!is_void) {
        // :TODO: make a helper function for this
        auto parser = graph_->rt()->image()->GetTypeParser(method->signature);
        uint32_t unused_argc;
        parser.ReadFunctionSignatureArgCount(&unused_argc);
        uint8_t variadic;
        parser.GetByte(&variadic);
        if (variadic == cb::kLegacyVariadic) {
            parser.NextByte();
        }
        return_td = graph_->rt()->LoadType(parser);
        assert(return_td != nullptr);
    }

    if (return_td) {
        std::span<VReg> call_argv;
        std::span<VReg> call_args_to_free;

        if (argv.size() > 0) {
            std::span<VReg> arr = pool_.make_n<VReg>(argv.size());
            for (size_t i = 0; i < argv.size(); i++)
                arr[i] = argv[i];
            call_argv = arr;
        }

        if (args_to_free.size() > 0) {
            std::span<VReg> arr = pool_.make_n<VReg>(args_to_free.size());
            for (size_t i = 0; i < args_to_free.size(); i++)
                arr[i] = args_to_free[i];
            call_args_to_free = arr;
        }

        pushStack(CreateCallNode(return_td, method, std::move(call_argv), std::move(call_args_to_free)));
    }
}

void MethodLowerer::EmitCall(const smx_rtti_method* method, VReg dest_reg,
                             const std::span<VReg>& argv)
{
    emit(LL_CALL, method, (uint8_t)argv.size(), (uint16_t)dest_reg.index);
    for (uint32_t i = 0; i < argv.size(); i++) {
        emitVal(argv[i]);
    }
    for (uint32_t i = 0; i < argv.size(); i++) {
        FreeReg(argv[i]);
    }
}

VReg MethodLowerer::EmitNode(ExprNode* node, VReg target_reg) {
    switch (node->kind) {
        case ExprNode::kReg:
            if (target_reg.valid() && target_reg != node->reg) {
                EmitMove(node->reg, target_reg, node->type);
                return target_reg;
            }
            return node->reg;

        case ExprNode::kCall: {
            if (target_reg.valid() && node->type->IsHeapItem()) {
                // We cannot emit an LL_RELEASE(target_reg) before EmitCall because
                // doing so would destroy the old array before returning the new one,
                // causing a crash if they share the same physical array instance
                // (e.g. self-assignment like x = get_array(x)).
                VReg temp = AllocateTemp(node->type);
                EmitCall(node->call.method, temp, node->call.argv);
                for (VReg reg : node->call.args_to_free)
                    FreeReg(reg);
                emit(LL_RELEASE, target_reg);
                EmitMove(temp, target_reg, node->type);
                FreeReg(temp);
                return target_reg;
            }
            VReg dest = target_reg.valid() ? target_reg : AllocateTemp(node->type);
            EmitCall(node->call.method, dest, node->call.argv);
            for (VReg reg : node->call.args_to_free)
                FreeReg(reg);
            return dest;
        }

        case ExprNode::kConstant: {
            VReg dest = target_reg.valid() ? target_reg : AllocateTemp(node->type);
            if (node->type->kind() != TypeKind::Int64)
                emit(LL_LOAD_CONST, node->constval.value, dest);
            else
                emit(LL_LOAD_CONST_I64, node->constval.value64, dest);
            return dest;
        }

        case ExprNode::kSimpleOp: {
            VReg left_reg = EmitNode(node->op.left);
            VReg right_reg;
            if (node->op.right)
                right_reg = EmitNode(node->op.right);

            VReg dest = target_reg.valid() ? target_reg : AllocateTemp(node->type);

            emitOp(node->op.opcode);
            if (left_reg.valid())
                emitVal(left_reg);
            if (right_reg.valid())
                emitVal(right_reg);
            emitVal(dest);

            if (node->op.left)
                FreeReg(left_reg);
            if (node->op.right)
                FreeReg(right_reg);

            return dest;
        }

        case ExprNode::kLoadElem: {
            VReg index_reg = EmitNode(node->load_elem.index);
            VReg base_reg = EmitNode(node->load_elem.base);

            if (target_reg.valid() && node->type->IsHeapItem())
                emit(LL_RELEASE, target_reg);

            VReg dest = target_reg.valid() ? target_reg : AllocateTemp(node->type);
            const TypeDesc* base = node->load_elem.base->type;
            LLOp op = node->load_elem.opcode;

            if (base->IsFlatArray()) {
                VReg addr_dest = AllocateTemp(cell_type_);
                emit(LL_IDXADDR_FLAT, (uint32_t)base->array_size(), (uint16_t)base->array_elt()->element_size(), base_reg, index_reg, addr_dest);
                emit(op, addr_dest, dest);
                FreeReg(addr_dest);
            } else {
                emit(op, base_reg, index_reg, dest);
            }

            FreeReg(index_reg);
            FreeReg(base_reg);
            return dest;
        }

        default:
            assert(false);
            return target_reg;
    }
}


VReg MethodLowerer::AllocateTemp(const TypeDesc* type) {
    return AllocateTempCells(GetCellCount(type), type->IsHeapItem());
}

VReg MethodLowerer::AllocateTempCells(uint16_t cells, bool is_gcobj) {
    uint32_t search_start = base_temp_reg_;
    if (num_temp_regs_ >= search_start + cells) {
        for (uint32_t i = search_start; i <= num_temp_regs_ - cells; i++) {
            if (gcobj_regs_.test(i) != is_gcobj)
                continue;

            bool fits = true;
            for (uint32_t j = 0; j < cells; j++) {
                if (temp_regs_used_.test(i + j) || gcobj_regs_.test(i + j) != is_gcobj) {
                    fits = false;
                    break;
                }
            }
            if (fits) {
                for (uint32_t j = 0; j < cells; j++)
                    temp_regs_used_.set(i + j);
                return VReg(i, cells, true);
            }
        }
    }

    uint32_t reg = num_temp_regs_;
    for (uint32_t j = 0; j < cells; j++) {
        temp_regs_used_.set(reg + j);
        if (is_gcobj)
            gcobj_regs_.set(reg + j);
    }

    num_temp_regs_ += cells;
    return VReg(reg, cells, true);
}

void MethodLowerer::FreeReg(VReg reg) {
    if (!reg.valid() || !reg.owned)
        return;

    if (gcobj_regs_.test(reg.index))
        emit(LL_RELEASE, reg);

    assert(reg.index >= base_temp_reg_);
    for (uint32_t i = 0; i < reg.cells; i++) {
        assert(temp_regs_used_.test(reg.index + i));
        temp_regs_used_.unset(reg.index + i);
    }
}

std::unique_ptr<InterpCode> LowerMethod(ControlFlowGraph* graph, MethodInfo* method) {
    MethodLowerer lowerer(graph, method);
    return lowerer.Lower();
}

} // namespace sp::v2
