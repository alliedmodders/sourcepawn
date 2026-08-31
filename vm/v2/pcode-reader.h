// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2006-2015 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#pragma once

#include <amtl/am-refcounting.h>
#include <limits.h>
#include <smx/smx-v2-opcodes.h>
#include <sp_vm_types.h>
#include "control-flow.h"
#include "v2/opcodes.h"
#include "v2/plugin-runtime.h"
#include "v2/pcode-visitor.h"

namespace sp::v2 {

class PluginRuntime;
class PcodeVisitor;

template <typename T>
class PcodeReader
{
  public:
    PcodeReader(PluginRuntime* rt, uint32_t startOffset, T* visitor)
     : rt_(rt)
     , visitor_(visitor)
     , code_(nullptr)
     , cip_(nullptr)
     , stop_at_(nullptr) {
        assert(ke::IsAligned(startOffset, sizeof(cell_t)));

        auto& code = rt->code();
        code_ = reinterpret_cast<const cell_t*>(code.bytes);
        cip_ = code_ + (startOffset / sizeof(cell_t));
        insn_begin_ = cip_;
        stop_at_ = reinterpret_cast<const cell_t*>(code.bytes + code.length);
    }
    PcodeReader(PluginRuntime* rt, Block* block, T* visitor)
     : rt_(rt)
     , visitor_(visitor)
     , code_(nullptr)
     , cip_(nullptr)
     , stop_at_(nullptr) {
        auto& code = rt->code();
        code_ = reinterpret_cast<const cell_t*>(code.bytes);
        cip_ = reinterpret_cast<const cell_t*>(block->start());
        insn_begin_ = cip_;
        stop_at_ = reinterpret_cast<const cell_t*>(block->end());
    }

    // We skip the first OP_PROC; it should be handled before parsing bytecode.
    void begin() {
        if (peekOpcode() == OP_PROC)
            readCell();
    }

    // Read the next opcode, return true on success, false otherwise.
    bool visitNext() {
        insn_begin_ = cip_;
        OPCODE op = (OPCODE)readCell();
        return visitOp(op);
    }

    // Peek at the next opcode.
    OPCODE peekOpcode() const {
        assert(more());
        return (OPCODE)*cip_;
    }

    const cell_t* start() const {
        return code_;
    }

    // Return whether or not there is more to decode.
    bool more() const {
        return cip_ < stop_at_;
    }

    // Return the current position in the code stream.
    const cell_t* cip() const {
        return cip_;
    }

    // Return the start of the current instruction.
    const cell_t* const& insn_begin() const {
        return insn_begin_;
    }
    cell_t cip_offset() const {
        return (cip_ - code_) * sizeof(cell_t);
    }

    void jump(cell_t offset) {
        assert(offset >= 0);
        assert(ke::IsAligned(offset, sizeof(cell_t)));

        cip_ = code_ + (offset / sizeof(cell_t));
        assert(cip_ >= code_ && cip_ < stop_at_);
    }

  private:
    bool visitOp(OPCODE op) {
        if (!visitor_->beforeVisitOp(op))
            return false;
        switch (op) {
            case OP_NOP:
                return true;

            // This opcode is used to note where line breaks occur.
            case OP_BREAK:
                return visitor_->visitBREAK();

            case OP_LOAD_PRI:
            case OP_LOAD_ALT: {
                PawnReg reg = (op == OP_LOAD_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t offset = readCell();
                return visitor_->visitLOAD(reg, offset);
            }

            case OP_LOAD_S_PRI:
            case OP_LOAD_S_ALT: {
                PawnReg reg = (op == OP_LOAD_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t offset = readCell();
                return visitor_->visitLOAD_S(reg, offset);
            }

            case OP_LREF_S_PRI:
            case OP_LREF_S_ALT: {
                PawnReg reg = (op == OP_LREF_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t offset = readCell();
                return visitor_->visitLREF_S(reg, offset);
            }

            case OP_LOAD_I:
                return visitor_->visitLOAD_I();

            case OP_LODB_I: {
                cell_t val = readCell();
                return visitor_->visitLODB_I(val);
            }

            case OP_CONST_PRI:
            case OP_CONST_ALT: {
                PawnReg reg = (op == OP_CONST_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t val = readCell();
                return visitor_->visitCONST(reg, val);
            }

            case OP_ADDR_PRI:
            case OP_ADDR_ALT: {
                PawnReg reg = (op == OP_ADDR_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t offset = readCell();
                return visitor_->visitADDR(reg, offset);
            }

            case OP_STOR_PRI:
            case OP_STOR_ALT: {
                PawnReg reg = (op == OP_STOR_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t offset = readCell();
                return visitor_->visitSTOR(offset, reg);
            }

            case OP_STOR_S_PRI:
            case OP_STOR_S_ALT: {
                PawnReg reg = (op == OP_STOR_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t offset = readCell();
                return visitor_->visitSTOR_S(offset, reg);
            }

            case OP_STOR_S_C: {
                cell_t offset = readCell();
                cell_t value = readCell();
                return visitor_->visitSTOR_S_C(offset, value);
            }

            case OP_SREF_S_PRI:
            case OP_SREF_S_ALT: {
                PawnReg reg = (op == OP_SREF_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t offset = readCell();
                return visitor_->visitSREF_S(offset, reg);
            }

            case OP_STOR_I:
                return visitor_->visitSTOR_I();

            case OP_STRB_I: {
                cell_t val = readCell();
                return visitor_->visitSTRB_I(val);
            }

            case OP_IDXADDR:
                return visitor_->visitIDXADDR();

            case OP_MOVE_PRI:
                return visitor_->visitMOVE(PawnReg::Pri);

            case OP_MOVE_ALT:
                return visitor_->visitMOVE(PawnReg::Alt);

            case OP_XCHG:
                return visitor_->visitXCHG();

            case OP_PUSH_PRI:
            case OP_PUSH_ALT: {
                PawnReg reg = (op == OP_PUSH_PRI) ? PawnReg::Pri : PawnReg::Alt;
                return visitor_->visitPUSH(reg);
            }

            case OP_PUSH_C:
            {
                cell_t value = readCell();
                return visitor_->visitPUSH_C(value);
            }

            case OP_PUSH_S:
            {
                cell_t slot = readCell();
                return visitor_->visitPUSH_S(slot);
            }

            case OP_PUSH_I_I64:
                return visitor_->visitPUSH_I_I64();

            case OP_POP_PRI:
            case OP_POP_ALT: {
                PawnReg reg = (op == OP_POP_PRI) ? PawnReg::Pri : PawnReg::Alt;
                return visitor_->visitPOP(reg);
            }

            case OP_HEAP: {
                cell_t amount = readCell();
                return visitor_->visitHEAP(amount);
            }

            case OP_CVT_I64: {
                cell_t slot = readCell();
                return visitor_->visitCVT_I64(slot);
            }
            case OP_TRUNCATE_I64:
                return visitor_->visitTRUNCATE_I64();
            case OP_TEST_I64:
                return visitor_->visitTEST_I64();

            case OP_INVERT_I64: {
                cell_t slot = readCell();
                return visitor_->visitINVERT_I64(slot);
            }
            case OP_NEG_I64: {
                cell_t slot = readCell();
                return visitor_->visitNEG_I64(slot);
            }
            case OP_SMUL_I64: {
                cell_t slot = readCell();
                return visitor_->visitSMUL_I64(slot);
            }
            case OP_SDIV_ALT_I64: {
                cell_t pri_slot = readCell();
                return visitor_->visitSDIV_ALT_I64(pri_slot);
            }
            case OP_SMOD_ALT_I64: {
                cell_t pri_slot = readCell();
                return visitor_->visitSMOD_ALT_I64(pri_slot);
            }
            case OP_ADD_I64: {
                cell_t slot = readCell();
                return visitor_->visitADD_I64(slot);
            }
            case OP_SUB_ALT_I64: {
                cell_t slot = readCell();
                return visitor_->visitSUB_ALT_I64(slot);
            }
            case OP_SHL_I64: {
                cell_t slot = readCell();
                return visitor_->visitSHL_I64(slot);
            }
            case OP_SSHR_I64: {
                cell_t slot = readCell();
                return visitor_->visitSSHR_I64(slot);
            }
            case OP_SHR_I64: {
                cell_t slot = readCell();
                return visitor_->visitSHR_I64(slot);
            }
            case OP_OR_I64: {
                cell_t slot = readCell();
                return visitor_->visitOR_I64(slot);
            }
            case OP_AND_I64: {
                cell_t slot = readCell();
                return visitor_->visitAND_I64(slot);
            }
            case OP_XOR_I64: {
                cell_t slot = readCell();
                return visitor_->visitXOR_I64(slot);
            }
            case OP_STOR_S_C_I64: {
                cell_t slot = readCell();
                cell_t cell0 = readCell();
                cell_t cell1 = readCell();
                return visitor_->visitSTOR_S_C_I64(slot, cell0, cell1);
            }
            case OP_STOR_S_PRI_I64: {
                cell_t slot = readCell();
                return visitor_->visitSTOR_S_PRI_I64(slot);
            }

            case OP_RETN:
                return visitor_->visitRETN();

            case OP_CALL: {
                cell_t offset = readCell();
                return visitor_->visitCALL(offset);
            }

            case OP_JUMP: {
                cell_t offset = readCell();
                return visitor_->visitJUMP(offset);
            }

#define JCMP_CASE(op, cmpop)                       \
    case op: {                                     \
        cell_t offset = readCell();                \
        return visitor_->visitJcmp(cmpop, offset); \
    }

                JCMP_CASE(OP_JZER, CompareOp::Zero)
                JCMP_CASE(OP_JNZ, CompareOp::NotZero)
                JCMP_CASE(OP_JEQ, CompareOp::Eq)
                JCMP_CASE(OP_JNEQ, CompareOp::Neq)
                JCMP_CASE(OP_JSLESS, CompareOp::Sless)
                JCMP_CASE(OP_JSLEQ, CompareOp::Sleq)
                JCMP_CASE(OP_JSGRTR, CompareOp::Sgrtr)
                JCMP_CASE(OP_JSGEQ, CompareOp::Sgeq)

#undef JCMP_CASE

            case OP_SHL:
                return visitor_->visitSHL();
            case OP_SHR:
                return visitor_->visitSHR();
            case OP_SSHR:
                return visitor_->visitSSHR();

            case OP_SHL_C_PRI:
            case OP_SHL_C_ALT: {
                PawnReg reg = (op == OP_SHL_C_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t val = readCell();
                return visitor_->visitSHL_C(reg, val);
            }

            case OP_SMUL:
                return visitor_->visitSMUL();
            case OP_SDIV:
                return visitor_->visitSDIV(PawnReg::Pri);
            case OP_SDIV_ALT:
                return visitor_->visitSDIV(PawnReg::Alt);
            case OP_SDIV_ALT_I32:
                return visitor_->visitSDIV_ALT_I32();
            case OP_SMOD_ALT_I32:
                return visitor_->visitSMOD_ALT_I32();
            case OP_ADD:
                return visitor_->visitADD();
            case OP_SUB_ALT:
                return visitor_->visitSUB_ALT();
            case OP_AND:
                return visitor_->visitAND();
            case OP_OR:
                return visitor_->visitOR();
            case OP_XOR:
                return visitor_->visitXOR();
            case OP_NOT:
                return visitor_->visitNOT();
            case OP_NEG:
                return visitor_->visitNEG();
            case OP_INVERT:
                return visitor_->visitINVERT();

            case OP_ADD_C: {
                cell_t val = readCell();
                return visitor_->visitADD_C(val);
            }

            case OP_SMUL_C: {
                cell_t val = readCell();
                return visitor_->visitSMUL_C(val);
            }

            case OP_ZERO_PRI:
                return visitor_->visitZERO(PawnReg::Pri);

            case OP_ZERO_ALT:
                return visitor_->visitZERO(PawnReg::Alt);

            case OP_ZERO_S: {
                cell_t offset = readCell();
                return visitor_->visitZERO_S(offset);
            }
            case OP_ZERO_S_I64: {
                cell_t offset = readCell();
                return visitor_->visitZERO_S_I64(offset);
            }

            case OP_EQ:
                return visitor_->visitCompareOp(CompareOp::Eq);
            case OP_NEQ:
                return visitor_->visitCompareOp(CompareOp::Neq);
            case OP_SLESS:
                return visitor_->visitCompareOp(CompareOp::Sless);
            case OP_SLEQ:
                return visitor_->visitCompareOp(CompareOp::Sleq);
            case OP_SGRTR:
                return visitor_->visitCompareOp(CompareOp::Sgrtr);
            case OP_SGEQ:
                return visitor_->visitCompareOp(CompareOp::Sgeq);

            case OP_EQ_I64:
                return visitor_->visitCompareOp64(CompareOp::Eq);
            case OP_NEQ_I64:
                return visitor_->visitCompareOp64(CompareOp::Neq);
            case OP_SLESS_I64:
                return visitor_->visitCompareOp64(CompareOp::Sless);
            case OP_SLEQ_I64:
                return visitor_->visitCompareOp64(CompareOp::Sleq);
            case OP_SGRTR_I64:
                return visitor_->visitCompareOp64(CompareOp::Sgrtr);
            case OP_SGEQ_I64:
                return visitor_->visitCompareOp64(CompareOp::Sgeq);

            case OP_TEST_F32:
                return visitor_->visitTEST_F32();
            case OP_NEG_F32:
                return visitor_->visitNEG_F32();
            case OP_MUL_F32:
                return visitor_->visitMUL_F32();
            case OP_DIV_ALT_F32:
                return visitor_->visitDIV_ALT_F32();
            case OP_ADD_F32:
                return visitor_->visitADD_F32();
            case OP_SUB_ALT_F32:
                return visitor_->visitSUB_ALT_F32();
            case OP_CVT_F32:
                return visitor_->visitCVT_F32();
            case OP_MOD_ALT_F32:
                return visitor_->visitMOD_ALT_F32();
            case OP_EQ_F32:
                return visitor_->visitCompareOpF32(CompareOp::Eq);
            case OP_NEQ_F32:
                return visitor_->visitCompareOpF32(CompareOp::Neq);
            case OP_LESS_F32:
                return visitor_->visitCompareOpF32(CompareOp::Sless);
            case OP_LEQ_F32:
                return visitor_->visitCompareOpF32(CompareOp::Sleq);
            case OP_GRTR_F32:
                return visitor_->visitCompareOpF32(CompareOp::Sgrtr);
            case OP_GEQ_F32:
                return visitor_->visitCompareOpF32(CompareOp::Sgeq);

            case OP_INC_PRI:
                return visitor_->visitINC(PawnReg::Pri);
            case OP_INC_ALT:
                return visitor_->visitINC(PawnReg::Alt);

            case OP_DEC_PRI:
                return visitor_->visitDEC(PawnReg::Pri);
            case OP_DEC_ALT:
                return visitor_->visitDEC(PawnReg::Alt);

            case OP_MOVS: {
                cell_t val = readCell();
                return visitor_->visitMOVS(val);
            }

            case OP_MOVE_I64:
                return visitor_->visitMOVE_I64();

            case OP_FILL: {
                cell_t val = readCell();
                return visitor_->visitFILL(val);
            }

            case OP_BOUNDS: {
                cell_t value = readCell();
                return visitor_->visitBOUNDS(value);
            }

            case OP_SWAP_PRI:
            case OP_SWAP_ALT: {
                PawnReg reg = (op == OP_SWAP_PRI) ? PawnReg::Pri : PawnReg::Alt;
                return visitor_->visitSWAP(reg);
            }

            case OP_PUSH_ADR:
            {
                cell_t slot = readCell();
                return visitor_->visitPUSH_ADR(slot);
            }

            case OP_SYSREQ_N: {
                cell_t index = readCell();
                cell_t nparams = readCell();
                return visitor_->visitSYSREQ_N(index, nparams);
            }

            case OP_GENARRAY:
            case OP_GENARRAY_Z: {
                cell_t val = readCell();
                return visitor_->visitGENARRAY(val, (op == OP_GENARRAY_Z));
            }

            case OP_STRADJUST_PRI:
                return visitor_->visitSTRADJUST_PRI();

            case OP_SWITCH: {
                cell_t tableOffset = readCell();

                const cell_t* casetbl = code_ + (tableOffset / sizeof(cell_t));

                const cell_t* table;
                cell_t ncases, defaultOffset;
                {
                    ke::SaveAndSet<const cell_t*> saved_pos(&cip_, casetbl);

                    assert((OPCODE)*cip_ == OP_CASETBL);
                    cip_++;

                    ncases = *cip_++;
                    defaultOffset = *cip_++;
                    table = cip_;
                    cip_ += ncases * 2;
                }

                return visitor_->visitSWITCH(
                    defaultOffset, reinterpret_cast<const CaseTableEntry*>(table), ncases);
            }

            case OP_CASETBL: {
                cell_t ncases = readCell();

                getCells((ncases * 2) + 1);

                // Nothing to do here. This is handled in OP_SWITCH.
                return true;
            }

            case OP_INITARRAY_PRI:
            case OP_INITARRAY_ALT: {
                PawnReg reg = (op == OP_INITARRAY_PRI) ? PawnReg::Pri : PawnReg::Alt;
                cell_t addr = readCell();
                cell_t iv_size = readCell();
                cell_t data_copy_size = readCell();
                cell_t data_fill_size = readCell();
                cell_t fill_value = readCell();
                return visitor_->visitINITARRAY(reg, addr, iv_size, data_copy_size, data_fill_size,
                                                fill_value);
            }

            case OP_HEAP_SAVE:
                return visitor_->visitHEAP_SAVE();
            case OP_HEAP_RESTORE:
                return visitor_->visitHEAP_RESTORE();

            default:
                assert(false);
                return false;
        }
    }

    cell_t readCell() {
        assert(cip_ < stop_at_);
        return *cip_++;
    }
    const cell_t* getCells(size_t n) {
        assert(cip_ + n <= stop_at_);
        const cell_t* result = cip_;
        cip_ += n;
        return result;
    }

  private:
    PluginRuntime* rt_;
    T* visitor_;
    const cell_t* code_;
    const cell_t* insn_begin_;
    const cell_t* cip_;
    const cell_t* stop_at_;
};

} // namespace sp::v2
