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

#include "binary-reader.h"
#include "v2/control-flow.h"
#include "v2/interp/interp-code.h"
#include "v2/interp/ll-op.h"
#include "v2/interp/lowering-assembler.h"
#include "v2/opcodes.h"
#include "v2/pcode-visitor.h"
#include "v2/runtime.h"

namespace sp::v2 {

class MethodLowerer
{
  public:
    MethodLowerer(ControlFlowGraph* graph)
     : graph_(graph)
    {}

    std::unique_ptr<InterpCode> Lower();

  private:
    void LowerBlock(Block* block);
    void LowerInstruction(OPCODE op, BinaryReader& reader, Block* block);
    void EmitJumpTarget(Block* target_block);
    void PatchJumps();

    void emitOp(LLOp op) {
        masm_.emit<uint16_t>((uint16_t)op);
    }

    template <typename T>
    void emitVal(T val) {
        masm_.emit<T>(val);
    }

  private:
    ControlFlowGraph* graph_;
    LoweringAssembler masm_;
    std::vector<Block*> blocks_by_id_;
    std::vector<size_t> jumps_to_patch_;
    std::vector<InterpCode::OffsetMapping> mappings_;
};

std::unique_ptr<InterpCode> MethodLowerer::Lower() {
    uint32_t max_id = 0;
    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++) {
        if ((*iter)->id() > max_id)
            max_id = (*iter)->id();
    }
    blocks_by_id_.resize(max_id + 1, nullptr);
    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++)
        blocks_by_id_[(*iter)->id()] = *iter;

    for (auto iter = graph_->rpoBegin(); iter != graph_->rpoEnd(); iter++)
        LowerBlock(*iter);

    PatchJumps();

    auto bytes = std::make_unique<uint8_t[]>(masm_.code_size());
    memcpy(bytes.get(), masm_.bytes(), masm_.code_size());
    return std::make_unique<InterpCode>(std::move(bytes), masm_.code_size(), std::move(mappings_));
}

void MethodLowerer::LowerBlock(Block* block) {
    block->label()->bind(masm_.pc());

    const uint8_t* stop_at = block->end();
    BinaryReader reader(block->start());

    while (reader.cursor() < stop_at) {
        uint32_t high_offset = (uint32_t)(reader.cursor() - graph_->rt()->code().bytes);
        mappings_.push_back({(uint32_t)masm_.pc(), high_offset});

        OPCODE op = (OPCODE)reader.read<uint8_t>();
        LowerInstruction(op, reader, block);
    }

    if (block->endType() == BlockEnd::Jump) {
        assert(block->successors().size() == 1);
        emitOp(LL_JUMP);
        EmitJumpTarget(block->successors()[0]);
    }
}

void MethodLowerer::LowerInstruction(OPCODE op, BinaryReader& reader, Block* block) {
    LLOp llop = (LLOp)op;
    emitOp(llop);

    switch (op) {
        case OP_NOP:
        case OP_LOAD_I_I32:
        case OP_LOAD_I_F32:
        case OP_LOAD_I_I64:
        case OP_LOAD_I_U8:
        case OP_LOAD_ELEM_A:
        case OP_LOAD_ELEM_I32:
        case OP_LOAD_ELEM_F32:
        case OP_LOAD_ELEM_I64:
        case OP_LOAD_ELEM_U8:
        case OP_STOR_I_I32:
        case OP_STOR_I_F32:
        case OP_STOR_I_I64:
        case OP_STOR_I_U8:
        case OP_STOR_ELEM_I32:
        case OP_STOR_ELEM_F32:
        case OP_STOR_ELEM_I64:
        case OP_STOR_ELEM_U8:
        case OP_POP:
        case OP_DUP:
        case OP_SWAP:
        case OP_TRUNCATE_I64:
        case OP_TEST_I64:
        case OP_RETN:
        case OP_RETV:
        case OP_SHL:
        case OP_SHR:
        case OP_SSHR:
        case OP_SMUL:
        case OP_SDIV_I32:
        case OP_SMOD_I32:
        case OP_ADD:
        case OP_SUB:
        case OP_AND:
        case OP_OR:
        case OP_XOR:
        case OP_NOT:
        case OP_NEG:
        case OP_INVERT:
        case OP_EQ:
        case OP_NEQ:
        case OP_SLESS:
        case OP_SLEQ:
        case OP_SGRTR:
        case OP_SGEQ:
        case OP_EQ_I64:
        case OP_NEQ_I64:
        case OP_SLESS_I64:
        case OP_SLEQ_I64:
        case OP_SGRTR_I64:
        case OP_SGEQ_I64:
        case OP_TEST_F32:
        case OP_NEG_F32:
        case OP_MUL_F32:
        case OP_DIV_F32:
        case OP_ADD_F32:
        case OP_SUB_F32:
        case OP_CVT_F32:
        case OP_MOD_F32:
        case OP_EQ_F32:
        case OP_NEQ_F32:
        case OP_LESS_F32:
        case OP_LEQ_F32:
        case OP_GRTR_F32:
        case OP_GEQ_F32:
        case OP_INC:
        case OP_DEC:
        case OP_HEAP_SAVE:
        case OP_HEAP_RESTORE:
        case OP_ARRAY_TO_NATIVE:
        case OP_COPYARRAY:
        case OP_SLICE:
        case OP_CVT_I64:
        case OP_INVERT_I64:
        case OP_NEG_I64:
        case OP_SMUL_I64:
        case OP_SDIV_I64:
        case OP_SMOD_I64:
        case OP_ADD_I64:
        case OP_SUB_I64:
        case OP_SHL_I64:
        case OP_SSHR_I64:
        case OP_SHR_I64:
        case OP_OR_I64:
        case OP_AND_I64:
        case OP_XOR_I64:
            break;

        case OP_LOAD_GLB:
        case OP_STOR_GLB:
        case OP_ADDR_GLB:
        case OP_LOAD_STR:
            emitVal<uint16_t>(reader.read<uint16_t>());
            break;

        case OP_LOAD_S:
        case OP_STOR_S:
        case OP_ADDR_S:
            emitVal<int16_t>(reader.read<int16_t>());
            break;

        case OP_STOR_S_C: {
            int16_t offset = reader.read<int16_t>();
            cell_t value = reader.read<cell_t>();
            emitVal<int16_t>(offset);
            emitVal<cell_t>(value);
            break;
        }

        case OP_IDXADDR: {
            break;
        }


        case OP_PUSH_C:
            emitVal<cell_t>(reader.read<cell_t>());
            break;

        case OP_PUSH_C_I8:
            emitVal<int8_t>(reader.read<int8_t>());
            break;

        case OP_PUSH_C_I64:
            emitVal<int64_t>(reader.read<int64_t>());
            break;

        case OP_LOAD_FN:
        case OP_CALL:
        case OP_LOAD_FLD:
        case OP_ADDR_FLD:
        case OP_NEWARRAY:
        case OP_FILLARRAY:
            emitVal<uint32_t>(reader.read<uint32_t>());
            break;

        case OP_NEWBULKARRAY: {
            uint8_t ndims = reader.read<uint8_t>();
            uint32_t type_id = reader.read<uint32_t>();
            emitVal<uint8_t>(ndims);
            emitVal<uint32_t>(type_id);
            break;
        }

        case OP_CALLN: {
            uint32_t method_index = reader.read<uint32_t>();
            uint8_t nargs = reader.read<uint8_t>();
            emitVal<uint32_t>(method_index);
            emitVal<uint8_t>(nargs);
            break;
        }

        case OP_JUMP: {
            reader.read<cell_t>();
            Block* target_block = block->successors()[0];
            EmitJumpTarget(target_block);
            break;
        }

        case OP_JZER:
        case OP_JNZ:
        case OP_JEQ:
        case OP_JNEQ:
        case OP_JSLESS:
        case OP_JSGRTR:
        case OP_JSGEQ:
        case OP_JSLEQ: {
            reader.read<cell_t>();
            Block* target_block = block->successors()[1];
            EmitJumpTarget(target_block);

            // Emit explicit fallthrough jump.
            emitOp(LL_JUMP);
            EmitJumpTarget(block->successors()[0]);
            break;
        }

        case OP_SWITCH: {
            cell_t ncases = reader.read<cell_t>();
            reader.read<cell_t>(); // skip default offset in original stream

            emitVal<cell_t>(ncases);

            Block* default_block = block->successors()[0];
            EmitJumpTarget(default_block);

            for (cell_t i = 0; i < ncases; i++) {
                cell_t case_value = reader.read<cell_t>();
                reader.read<cell_t>(); // skip case offset in original stream
                emitVal<cell_t>(case_value);
                Block* target_block = block->successors()[1 + i];
                EmitJumpTarget(target_block);
            }
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

std::unique_ptr<InterpCode> LowerMethod(ControlFlowGraph* graph) {
    MethodLowerer lowerer(graph);
    return lowerer.Lower();
}

} // namespace sp::v2
