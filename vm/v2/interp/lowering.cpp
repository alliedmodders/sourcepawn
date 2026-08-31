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
#include "v2/interp/lowering.h"

#include <assert.h>
#include <string.h>

#include <memory>
#include <vector>

#include <amtl/am-vector.h>
#include "binary-reader.h"
#include "v2/control-flow.h"
#include "v2/interp/interp-code.h"
#include "v2/interp/ll-op.h"
#include "v2/interp/lowering-assembler.h"
#include "v2/method-info.h"
#include "v2/opcodes.h"
#include "v2/pcode-visitor.h"
#include "v2/runtime.h"

namespace sp::v2 {

struct LoweringData : public IBlockData {
    LoweringData() {}
    explicit LoweringData(const std::vector<const TypeDesc*>& stack)
     : stack(stack)
    {}
    std::vector<const TypeDesc*> stack;
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

    const TypeDesc* popStack() {
        return ke::PopBack(&stack_);
    }

    void pushStack(const TypeDesc* td) {
        stack_.push_back(td);
    }

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
    BinaryReader reader_;

    Block* block_ = nullptr;
    std::vector<const TypeDesc*> stack_;
};

std::unique_ptr<InterpCode> MethodLowerer::Lower() {
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

    auto bytes = std::make_unique<uint8_t[]>(masm_.code_size());
    memcpy(bytes.get(), masm_.bytes(), masm_.code_size());
    return std::make_unique<InterpCode>(std::move(bytes), masm_.code_size(), std::move(mappings_));
}

void MethodLowerer::LowerBlock(Block* next_block) {
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

    if (block_->endType() == BlockEnd::Jump) {
        Block* target = block_->successors()[0];
        if (target != next_block) {
            emitOp(LL_JUMP);
            EmitJumpTarget(target);
        }
    }

    // Propagate stack state.
    for (Block* succ : block_->successors()) {
        if (succ->id() > block_->id())
            succ->setData(new LoweringData(stack_));
    }
}

void MethodLowerer::LowerInstruction(OPCODE op) {
    switch (op) {
        case OP_NOP:
            emitOp(LL_NOP);
            break;

        case OP_LOAD_I_I32:
        case OP_LOAD_I_F32: {
            emitOp(op == OP_LOAD_I_I32 ? LL_LOAD_I_I32 : LL_LOAD_I_F32);
            const TypeDesc* addr = popStack();
            pushStack(addr->ref_type());
            break;
        }

        case OP_LOAD_I_I64: {
            emitOp(LL_LOAD_I_I64);
            popStack();
            pushStack(int64_type_);
            break;
        }

        case OP_LOAD_I_U8: {
            emitOp(LL_LOAD_I_U8);
            popStack();
            pushStack(cell_type_);
            break;
        }

        case OP_LOAD_ELEM_I32:
        case OP_LOAD_ELEM_F32:
        case OP_LOAD_ELEM_I64:
        case OP_LOAD_ELEM_U8:
        case OP_LOAD_ELEM_A: {
            popStack();
            const TypeDesc* base = popStack();
            if (base->IsFlatArray()) {
                assert(op != OP_LOAD_ELEM_A);
                emitOp(LL_IDXADDR_FLAT);
                emitVal<uint32_t>(base->array_size());
                emitVal<uint32_t>(base->array_elt()->element_size());
                const TypeDesc* elt = base->array_elt();
                LLOp llop = LL_NOP;
                switch (op) {
                    case OP_LOAD_ELEM_I32: llop = LL_LOAD_I_I32; break;
                    case OP_LOAD_ELEM_F32: llop = LL_LOAD_I_F32; break;
                    case OP_LOAD_ELEM_I64: llop = LL_LOAD_I_I64; break;
                    case OP_LOAD_ELEM_U8:  llop = LL_LOAD_I_U8; break;
                    default: assert(false); break;
                }
                emitOp(llop);
                pushStack(op == OP_LOAD_ELEM_U8 ? cell_type_ : elt);
            } else {
                LLOp llop = LL_NOP;
                switch (op) {
                    case OP_LOAD_ELEM_I32: llop = LL_LOAD_ELEM_I32; break;
                    case OP_LOAD_ELEM_F32: llop = LL_LOAD_ELEM_F32; break;
                    case OP_LOAD_ELEM_I64: llop = LL_LOAD_ELEM_I64; break;
                    case OP_LOAD_ELEM_U8:  llop = LL_LOAD_ELEM_U8; break;
                    case OP_LOAD_ELEM_A:   llop = LL_LOAD_ELEM_A; break;
                    default: assert(false); break;
                }
                emitOp(llop);
                const TypeDesc* elt = base->array_elt();
                pushStack(op == OP_LOAD_ELEM_U8 ? cell_type_ : elt);
            }
            break;
        }

        case OP_STOR_I_I32:
        case OP_STOR_I_F32:
        case OP_STOR_I_I64:
        case OP_STOR_I_U8: {
            LLOp llop = LL_NOP;
            switch (op) {
                case OP_STOR_I_I32: llop = LL_STOR_I_I32; break;
                case OP_STOR_I_F32: llop = LL_STOR_I_F32; break;
                case OP_STOR_I_I64: llop = LL_STOR_I_I64; break;
                case OP_STOR_I_U8:  llop = LL_STOR_I_U8; break;
                default: assert(false); break;
            }
            emitOp(llop);
            popStack();
            popStack();
            break;
        }

        case OP_STOR_ELEM_I32:
        case OP_STOR_ELEM_F32:
        case OP_STOR_ELEM_I64:
        case OP_STOR_ELEM_U8: {
            popStack();
            popStack();
            const TypeDesc* base = popStack();
            if (base->IsFlatArray()) {
                LLOp llop = LL_NOP;
                switch (op) {
                    case OP_STOR_ELEM_I32: llop = LL_STOR_ELEM_FLAT_I32; break;
                    case OP_STOR_ELEM_F32: llop = LL_STOR_ELEM_FLAT_F32; break;
                    case OP_STOR_ELEM_I64: llop = LL_STOR_ELEM_FLAT_I64; break;
                    case OP_STOR_ELEM_U8:  llop = LL_STOR_ELEM_FLAT_U8; break;
                    default: assert(false); break;
                }
                emitOp(llop);
                emitVal<uint32_t>(base->array_size());
                emitVal<uint32_t>(base->array_elt()->element_size());
            } else {
                LLOp llop = LL_NOP;
                switch (op) {
                    case OP_STOR_ELEM_I32: llop = LL_STOR_ELEM_I32; break;
                    case OP_STOR_ELEM_F32: llop = LL_STOR_ELEM_F32; break;
                    case OP_STOR_ELEM_I64: llop = LL_STOR_ELEM_I64; break;
                    case OP_STOR_ELEM_U8:  llop = LL_STOR_ELEM_U8; break;
                    default: assert(false); break;
                }
                emitOp(llop);
            }
            break;
        }

        case OP_POP: {
            emitOp(LL_POP);
            popStack();
            break;
        }

        case OP_DUP: {
            emitOp(LL_DUP);
            const TypeDesc* val = stack_.back();
            pushStack(val);
            break;
        }

        case OP_SWAP: {
            emitOp(LL_SWAP);
            const TypeDesc* val1 = popStack();
            const TypeDesc* val2 = popStack();
            pushStack(val1);
            pushStack(val2);
            break;
        }

        case OP_RETN: {
            emitOp(LL_RETN);
            popStack();
            break;
        }

        case OP_RETV: {
            emitOp(LL_RETV);
            break;
        }

        case OP_SHL:
        case OP_SHR:
        case OP_SSHR:
        case OP_AND:
        case OP_OR:
        case OP_XOR: {
            const TypeDesc* b = popStack();
            const TypeDesc* a = popStack();
            LLOp llop = LL_NOP;
            if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                switch (op) {
                    case OP_SHL:  llop = LL_SHL_I64; break;
                    case OP_SHR:  llop = LL_SHR_I64; break;
                    case OP_SSHR: llop = LL_SSHR_I64; break;
                    case OP_AND:  llop = LL_AND_I64; break;
                    case OP_OR:   llop = LL_OR_I64; break;
                    case OP_XOR:  llop = LL_XOR_I64; break;
                    default: assert(false); break;
                }
                emitOp(llop);
                pushStack(int64_type_);
            } else {
                switch (op) {
                    case OP_SHL:  llop = LL_SHL; break;
                    case OP_SHR:  llop = LL_SHR; break;
                    case OP_SSHR: llop = LL_SSHR; break;
                    case OP_AND:  llop = LL_AND; break;
                    case OP_OR:   llop = LL_OR; break;
                    case OP_XOR:  llop = LL_XOR; break;
                    default: assert(false); break;
                }
                emitOp(llop);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_SUB: {
            const TypeDesc* b = popStack();
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                emitOp(LL_SUB_F32);
                pushStack(float32_type_);
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                emitOp(LL_SUB_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_SUB_I32);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_SMUL: {
            const TypeDesc* b = popStack();
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                emitOp(LL_MUL_F32);
                pushStack(float32_type_);
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                emitOp(LL_SMUL_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_SMUL_I32);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_SDIV: {
            const TypeDesc* b = popStack();
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                emitOp(LL_DIV_F32);
                pushStack(float32_type_);
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                emitOp(LL_SDIV_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_SDIV_I32);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_SMOD: {
            const TypeDesc* b = popStack();
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                emitOp(LL_MOD_F32);
                pushStack(float32_type_);
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                emitOp(LL_SMOD_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_SMOD_I32);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_ADD: {
            const TypeDesc* b = popStack();
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                emitOp(LL_ADD_F32);
                pushStack(float32_type_);
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                emitOp(LL_ADD_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_ADD_I32);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_NOT: {
            emitOp(LL_NOT);
            popStack();
            pushStack(cell_type_);
            break;
        }

        case OP_INC:
        case OP_DEC: {
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32) {
                emitOp(LL_PUSH_C);
                emitVal<float>(op == OP_INC ? 1.0f : -1.0f);
                emitOp(LL_ADD_F32);
                pushStack(float32_type_);
            } else if (a->kind() == TypeKind::Int64) {
                emitOp(LL_PUSH_C_I64);
                emitVal<int64_t>(op == OP_INC ? 1 : -1);
                emitOp(LL_ADD_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_PUSH_C);
                emitVal<cell_t>(op == OP_INC ? 1 : -1);
                emitOp(LL_ADD_I32);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_NEG: {
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32) {
                emitOp(LL_NEG_F32);
                pushStack(float32_type_);
            } else if (a->kind() == TypeKind::Int64) {
                emitOp(LL_NEG_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_NEG);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_INVERT: {
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Int64) {
                emitOp(LL_INVERT_I64);
                pushStack(int64_type_);
            } else {
                emitOp(LL_INVERT);
                pushStack(cell_type_);
            }
            break;
        }

        case OP_TEST: {
            const TypeDesc* a = popStack();
            if (a->kind() == TypeKind::Float32)
                emitOp(LL_TEST_F32);
            else if (a->kind() == TypeKind::Int64)
                emitOp(LL_TEST_I64);
            else
                assert(false);
            pushStack(cell_type_);
            break;
        }

        case OP_CVT_F32: {
            emitOp(LL_CVT_F32);
            popStack();
            pushStack(float32_type_);
            break;
        }

        case OP_ARRAY_TO_NATIVE: {
            emitOp(LL_ARRAY_TO_NATIVE);
            break;
        }

        case OP_COPYARRAY: {
            const TypeDesc* src = popStack();
            const TypeDesc* dest = popStack();
            if (dest->IsFlatArray() || src->IsFlatArray()) {
                if (!src->IsFlatArray()) {
                    emitOp(LL_ARRAY_TO_FLAT);
                } else if (!dest->IsFlatArray()) {
                    emitOp(LL_SWAP);
                    emitOp(LL_ARRAY_TO_FLAT);
                    emitOp(LL_SWAP);
                }
                // If the source has a statically known size, copy that size.
                // This is safe because the verifier guarantees that the source
                // size is less than or equal to the destination size.
                uint32_t elements = dest->array_size();
                if (src->kind() == TypeKind::FixedArray || src->kind() == TypeKind::FlatArray)
                    elements = src->array_size();
                uint32_t bytes = elements * dest->array_elt()->element_size();
                emitOp(LL_COPYARRAY_FLAT);
                emitVal<uint32_t>(bytes);
            } else {
                emitOp(LL_COPYARRAY);
            }
            break;
        }

        case OP_SLICE: {
            popStack();
            const TypeDesc* base = stack_.back();
            if (base->IsFlatArray()) {
                emitOp(LL_SLICE_FLAT);
                emitVal<const TypeDesc*>(base);
            } else {
                emitOp(LL_SLICE);
            }
            break;
        }

        case OP_SLICE_AS: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = rt_->LoadTypeFromId(type_id);
            const TypeDesc* base = stack_.back();
            popStack(); // pop base
            pushStack(td);
            emitOp(LL_PUSH_C);
            emitVal<cell_t>(0);
            if (base->IsFlatArray()) {
                emitOp(LL_SLICE_FLAT);
                emitVal<const TypeDesc*>(base);
            } else {
                emitOp(LL_SLICE);
            }
            break;
        }

        case OP_CVT_I64: {
            emitOp(LL_CVT_I64);
            popStack();
            pushStack(int64_type_);
            break;
        }

        case OP_TRUNCATE_I64: {
            emitOp(LL_TRUNCATE_I64);
            popStack();
            pushStack(cell_type_);
            break;
        }

        case OP_EQ:
        case OP_NEQ:
        case OP_SLESS:
        case OP_SLEQ:
        case OP_SGRTR:
        case OP_SGEQ: {
            const TypeDesc* b = popStack();
            const TypeDesc* a = popStack();
            LLOp llop = LL_NOP;
            if (a->kind() == TypeKind::Float32 && b->kind() == TypeKind::Float32) {
                switch (op) {
                    case OP_EQ:    llop = LL_EQ_F32; break;
                    case OP_NEQ:   llop = LL_NEQ_F32; break;
                    case OP_SLESS: llop = LL_LESS_F32; break;
                    case OP_SLEQ:  llop = LL_LEQ_F32; break;
                    case OP_SGRTR: llop = LL_GRTR_F32; break;
                    case OP_SGEQ:  llop = LL_GEQ_F32; break;
                    default: assert(false); break;
                }
            } else if (a->kind() == TypeKind::Int64 && b->kind() == TypeKind::Int64) {
                switch (op) {
                    case OP_EQ:    llop = LL_EQ_I64; break;
                    case OP_NEQ:   llop = LL_NEQ_I64; break;
                    case OP_SLESS: llop = LL_SLESS_I64; break;
                    case OP_SLEQ:  llop = LL_SLEQ_I64; break;
                    case OP_SGRTR: llop = LL_SGRTR_I64; break;
                    case OP_SGEQ:  llop = LL_SGEQ_I64; break;
                    default: assert(false); break;
                }
            } else {
                switch (op) {
                    case OP_EQ:    llop = LL_EQ_I32; break;
                    case OP_NEQ:   llop = LL_NEQ_I32; break;
                    case OP_SLESS: llop = LL_SLESS_I32; break;
                    case OP_SLEQ:  llop = LL_SLEQ_I32; break;
                    case OP_SGRTR: llop = LL_SGRTR_I32; break;
                    case OP_SGEQ:  llop = LL_SGEQ_I32; break;
                    default: assert(false); break;
                }
            }
            emitOp(llop);
            pushStack(cell_type_);
            break;
        }

        case OP_LOAD_GLB: {
            emitOp(LL_LOAD_GLB);
            uint16_t index = reader_.read<uint16_t>();
            emitVal<uint16_t>(index);
            pushStack(graph_->rt()->GetTypeOfGlobal(index));
            break;
        }

        case OP_STOR_GLB: {
            emitOp(LL_STOR_GLB);
            uint16_t index = reader_.read<uint16_t>();
            emitVal<uint16_t>(index);
            popStack();
            break;
        }

        case OP_ADDR_GLB: {
            emitOp(LL_ADDR_GLB);
            uint16_t index = reader_.read<uint16_t>();
            emitVal<uint16_t>(index);
            const TypeDesc* td = graph_->rt()->GetTypeOfGlobal(index);
            if (td->IsCompositeValue())
                pushStack(td);
            else
                pushStack(graph_->rt()->GetReferenceType(td));
            break;
        }

        case OP_LOAD_STR: {
            emitOp(LL_LOAD_STR);
            uint16_t index = reader_.read<uint16_t>();
            emitVal<uint16_t>(index);
            pushStack(graph_->rt()->GetStringLitType(index));
            break;
        }

        case OP_LOAD_S: {
            emitOp(LL_LOAD_S);
            int16_t offset = reader_.read<int16_t>();
            emitVal<int16_t>(offset);
            pushStack(method_->GetTypeOfLocal(offset));
            break;
        }

        case OP_STOR_S: {
            emitOp(LL_STOR_S);
            int16_t offset = reader_.read<int16_t>();
            emitVal<int16_t>(offset);
            popStack();
            break;
        }

        case OP_ADDR_S: {
            emitOp(LL_ADDR_S);
            int16_t offset = reader_.read<int16_t>();
            emitVal<int16_t>(offset);
            const TypeDesc* td = method_->GetTypeOfLocal(offset);
            if (td->IsCompositeValue())
                pushStack(td);
            else
                pushStack(graph_->rt()->GetReferenceType(td));
            break;
        }

        case OP_STOR_S_C: {
            emitOp(LL_STOR_S_C);
            int16_t offset = reader_.read<int16_t>();
            cell_t value = reader_.read<cell_t>();
            emitVal<int16_t>(offset);
            emitVal<cell_t>(value);
            break;
        }

        case OP_IDXADDR: {
            popStack();
            const TypeDesc* base = popStack();
            if (base->IsFlatArray()) {
                emitOp(LL_IDXADDR_FLAT);
                emitVal<uint32_t>(base->array_size());
                emitVal<uint32_t>(base->array_elt()->element_size());
            } else {
                emitOp(LL_IDXADDR);
            }
            pushStack(graph_->rt()->GetReferenceType(base->array_elt()));
            break;
        }

        case OP_PUSH_C: {
            emitOp(LL_PUSH_C);
            emitVal<cell_t>(reader_.read<cell_t>());
            pushStack(cell_type_);
            break;
        }

        case OP_PUSH_C_I8: {
            emitOp(LL_PUSH_C_I8);
            emitVal<int8_t>(reader_.read<int8_t>());
            pushStack(cell_type_);
            break;
        }

        case OP_PUSH_C_I64: {
            emitOp(LL_PUSH_C_I64);
            emitVal<int64_t>(reader_.read<int64_t>());
            pushStack(int64_type_);
            break;
        }

        case OP_PUSH_C_F32: {
            emitOp(LL_PUSH_C);
            emitVal<float>(reader_.read<float>());
            pushStack(float32_type_);
            break;
        }

        case OP_LOAD_FN: {
            emitOp(LL_LOAD_FN);
            emitVal<uint32_t>(reader_.read<uint32_t>());
            pushStack(cell_type_);
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
            popStack();

            LLOp llop = field_td->IsInt64() ? LL_LOAD_FLD_X64 : LL_LOAD_FLD_X32;
            emitOp(llop);
            emitVal<uint32_t>(offset);
            pushStack(field_td);
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
            popStack();

            emitOp(LL_ADDR_FLD);
            emitVal<uint32_t>(offset);
            pushStack(pushed_td);
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
            popStack();
            popStack();

            LLOp llop = field_td->IsInt64() ? LL_STOR_FLD_X64 : LL_STOR_FLD_X32;
            emitOp(llop);
            emitVal<uint32_t>(offset);
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

            emitOp(LL_PUSH_C);
            emitVal<cell_t>(cell_offset);
            pushStack(cell_type_);
            break;
        }

        case OP_LOAD_ES_SIZE: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = rt_->LoadTypeFromId(type_id);
            uint32_t cell_size = td->slot_size() / sizeof(cell_t);

            emitOp(LL_PUSH_C);
            emitVal<cell_t>(cell_size);
            pushStack(cell_type_);
            break;
        }

        case OP_COPYOBJ: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = rt_->LoadTypeFromId(type_id);
            popStack();
            popStack();
            emitOp(LL_COPYOBJ);
            emitVal<uint32_t>(td->slot_size());
            break;
        }

        case OP_SLICE_ES: {
            uint32_t type_id = reader_.read<uint32_t>();
            const TypeDesc* td = rt_->LoadTypeFromId(type_id);
            uint32_t cell_size = td->slot_size() / sizeof(cell_t);
            emitOp(LL_SLICE_ES);
            emitVal<uint32_t>(cell_size);
            popStack();
            const TypeDesc* any_type = graph_->rt()->GetPrimitiveType(TypeKind::Any);
            const TypeDesc* slice_type = graph_->rt()->GetSliceType(any_type);
            pushStack(slice_type);
            break;
        }

        case OP_CALL:
        case OP_CALLN: {
            emitOp(op == OP_CALL ? LL_CALL : LL_CALLN);
            uint32_t method_index = reader_.read<uint32_t>();
            emitVal<uint32_t>(method_index);

            uint32_t arg_count = 0;
            if (op == OP_CALLN) {
                uint8_t nargs = reader_.read<uint8_t>();
                emitVal<uint8_t>(nargs);
                arg_count = nargs;
            } else {
                const smx_rtti_method* method = graph_->rt()->image()->GetMethod(method_index);
                assert(method != nullptr);
                auto parser = graph_->rt()->image()->GetTypeParser(method->signature);
                [[maybe_unused]] bool success = parser.ReadFunctionSignatureArgCount(&arg_count);
                assert(success);
            }

            for (uint32_t i = 0; i < arg_count; i++) {
                popStack();
            }

            const smx_rtti_method* method = graph_->rt()->image()->GetMethod(method_index);
            assert(method != nullptr);
            if (!graph_->rt()->image()->IsVoidMethod(method)) {
                auto parser = graph_->rt()->image()->GetTypeParser(method->signature);
                uint32_t unused_argc;
                parser.ReadFunctionSignatureArgCount(&unused_argc);
                uint8_t variadic;
                parser.GetByte(&variadic);
                if (variadic == cb::kLegacyVariadic) {
                    parser.NextByte();
                }
                const TypeDesc* td = graph_->rt()->LoadType(parser);
                assert(td != nullptr);
                pushStack(td);
            }
            break;
        }

        case OP_NEWARRAY: {
            emitOp(LL_NEWARRAY);
            uint32_t type_id = reader_.read<uint32_t>();
            emitVal<uint32_t>(type_id);
            const TypeDesc* td = graph_->rt()->LoadTypeFromId(type_id);
            assert(td != nullptr);
            if (td->kind() == TypeKind::Array) {
                popStack();
            }
            pushStack(td);
            break;
        }

        case OP_NEWBULKARRAY: {
            emitOp(LL_NEWBULKARRAY);
            uint8_t ndims = reader_.read<uint8_t>();
            uint32_t type_id = reader_.read<uint32_t>();
            emitVal<uint8_t>(ndims);
            emitVal<uint32_t>(type_id);
            const TypeDesc* td = graph_->rt()->LoadTypeFromId(type_id);
            assert(td != nullptr);
            for (uint8_t i = 0; i < ndims; i++) {
                popStack();
            }
            pushStack(td);
            break;
        }

        case OP_FILLARRAY: {
            const TypeDesc* base = popStack();
            uint32_t data_offs = reader_.read<uint32_t>();
            if (base->IsFlatArray()) {
                emitOp(LL_FILLARRAY_FLAT);
                emitVal<uint32_t>(data_offs);
                emitVal<const TypeDesc*>(base);
            } else {
                emitOp(LL_FILLARRAY);
                emitVal<uint32_t>(data_offs);
            }
            break;
        }

        case OP_HEAP_SAVE:
        case OP_HEAP_RESTORE: {
            emitOp(op == OP_HEAP_SAVE ? LL_HEAP_SAVE : LL_HEAP_RESTORE);
            break;
        }

        case OP_JUMP: {
            emitOp(LL_JUMP);
            reader_.read<cell_t>();
            Block* target_block = block_->successors()[0];
            EmitJumpTarget(target_block);
            break;
        }

        case OP_JZER:
        case OP_JNZ: {
            emitOp(op == OP_JZER ? LL_JZER : LL_JNZ);
            reader_.read<cell_t>();
            Block* target_block = block_->successors()[1];
            EmitJumpTarget(target_block);

            emitOp(LL_JUMP);
            EmitJumpTarget(block_->successors()[0]);

            popStack();
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
            emitOp(llop);
            reader_.read<cell_t>();
            Block* target_block = block_->successors()[1];
            EmitJumpTarget(target_block);

            emitOp(LL_JUMP);
            EmitJumpTarget(block_->successors()[0]);

            popStack();
            popStack();
            break;
        }

        case OP_SWITCH: {
            emitOp(LL_SWITCH);
            cell_t ncases = reader_.read<cell_t>();
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
            popStack();
            break;
        }

        default:
            assert(false);
            break;
    }
}

void MethodLowerer::EmitJumpTarget(Block* target_block) {
    if (target_block->label()->bound())
        emitVal<cell_t>(target_block->label()->offset());
    else {
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

std::unique_ptr<InterpCode> LowerMethod(ControlFlowGraph* graph, MethodInfo* method) {
    MethodLowerer lowerer(graph, method);
    return lowerer.Lower();
}

} // namespace sp::v2
