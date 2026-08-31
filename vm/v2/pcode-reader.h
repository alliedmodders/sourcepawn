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
#include "v2/runtime.h"
#include "v2/pcode-visitor.h"

namespace sp::v2 {

class Runtime;
class PcodeVisitor;

template <typename T>
class PcodeReader
{
  public:
    PcodeReader(Runtime* rt, uint32_t startOffset, T* visitor)
     : rt_(rt),
       visitor_(visitor),
       code_(nullptr),
       cip_(nullptr),
       stop_at_(nullptr)
    {
        auto& code = rt->code();
        code_ = code.bytes;
        cip_ = code_ + startOffset;
        insn_begin_ = cip_;
        stop_at_ = code.bytes + code.length;
    }
    PcodeReader(Runtime* rt, Block* block, T* visitor)
     : rt_(rt),
       visitor_(visitor),
       code_(nullptr),
       cip_(nullptr),
       stop_at_(nullptr)
    {
        auto& code = rt->code();
        code_ = code.bytes;
        cip_ = block->start();
        insn_begin_ = cip_;
        stop_at_ = block->end();
    }

    // Read the next opcode, return true on success, false otherwise.
    bool visitNext() {
        insn_begin_ = cip_;
        OPCODE op = (OPCODE)*cip_++;
        return visitOp(op);
    }

    // Peek at the next opcode.
    OPCODE peekOpcode() const {
        assert(more());
        return (OPCODE)*cip_;
    }

    const uint8_t* start() const {
        return code_;
    }

    // Return whether or not there is more to decode.
    bool more() const {
        return cip_ < stop_at_;
    }

    // Return the current position in the code stream.
    const uint8_t* cip() const {
        return cip_;
    }

    // Return the start of the current instruction.
    const uint8_t* const& insn_begin() const {
        return insn_begin_;
    }
    cell_t cip_offset() const {
        return (cell_t)(cip_ - code_);
    }

    void jump(cell_t offset) {
        assert(offset >= 0);

        cip_ = code_ + offset;
        assert(cip_ >= code_ && cip_ < stop_at_);
    }

  private:
    bool visitOp(OPCODE op) {
        if (!visitor_->beforeVisitOp(op))
            return false;
        switch (op) {
            case OP_NOP:
                return true;

            case OP_LOAD_GLB: {
                cell_t srcaddr = readCell();
                return visitor_->visitLOAD_GLB(srcaddr);
            }

            case OP_LOAD_S: {
                cell_t offset = readInt16();
                return visitor_->visitLOAD_S(offset);
            }

            case OP_LOAD_I_I32:
                return visitor_->visitLOAD_I_I32();

            case OP_LOAD_I_U8:
                return visitor_->visitLOAD_I_U8();

            case OP_STOR_GLB: {
                cell_t address = readCell();
                return visitor_->visitSTOR_GLB(address);
            }

            case OP_STOR_S: {
                cell_t offset = readInt16();
                return visitor_->visitSTOR_S(offset);
            }

            case OP_STOR_S_C: {
                cell_t offset = readInt16();
                cell_t value = readCell();
                return visitor_->visitSTOR_S_C(offset, value);
            }

            case OP_STOR_I_I32:
                return visitor_->visitSTOR_I_I32();

            case OP_STOR_I_U8:
                return visitor_->visitSTOR_I_U8();

            case OP_LOAD_FN: {
                uint32_t method_index = (uint32_t)readCell();
                return visitor_->visitLOAD_FN(method_index);
            }

            case OP_IDXADDR: {
                uint8_t rank_size = read<uint8_t>();
                int32_t bounds = read<int32_t>();
                return visitor_->visitIDXADDR(rank_size, bounds);
            }

            case OP_POP:
                return visitor_->visitPOP();

            case OP_DUP:
                return visitor_->visitDUP();

            case OP_SWAP:
                return visitor_->visitSWAP();

            case OP_PUSH_C:
            {
                cell_t value = readCell();
                return visitor_->visitPUSH_C(value);
            }

            case OP_PUSH_C_I8:
            {
                int8_t value = read<int8_t>();
                return visitor_->visitPUSH_C_I8(value);
            }

            case OP_PUSH_C_I64:
            {
                int64_t value = read<int64_t>();
                return visitor_->visitPUSH_C_I64(value);
            }

            case OP_PUSH_C_F32:
            {
                float value = read<float>();
                return visitor_->visitPUSH_C_F32(value);
            }

            case OP_CVT_I64: {
                cell_t slot = readInt16();
                return visitor_->visitCVT_I64(slot);
            }
            case OP_TRUNCATE_I64:
                return visitor_->visitTRUNCATE_I64();
            case OP_TEST:
                return visitor_->visitTEST();



            case OP_RETN:
                return visitor_->visitRETN();

            case OP_RETV:
                return visitor_->visitRETV();

            case OP_CALL: {
                uint32_t method_index = (uint32_t)readCell();
                return visitor_->visitCALL(method_index);
            }

            case OP_CALLN: {
                uint32_t method_index = (uint32_t)readCell();
                uint8_t nargs = read<uint8_t>();
                return visitor_->visitCALLN(method_index, nargs);
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

            case OP_SMUL:
                return visitor_->visitSMUL();
            case OP_SDIV:
                return visitor_->visitSDIV();
            case OP_SMOD:
                return visitor_->visitSMOD();
            case OP_ADD:
                return visitor_->visitADD();
            case OP_SUB:
                return visitor_->visitSUB();
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






            case OP_CVT_F32:
                return visitor_->visitCVT_F32();


            case OP_INC:
                return visitor_->visitINC();

            case OP_DEC:
                return visitor_->visitDEC();

            case OP_ADDR_S:
            {
                cell_t slot = readInt16();
                return visitor_->visitADDR_S(slot);
            }

            case OP_SWITCH: {
                cell_t ncases = readCell();
                cell_t defaultOffset = readCell();
                const uint8_t* table = cip_;
                cip_ += ncases * sizeof(cell_t) * 2;

                return visitor_->visitSWITCH(
                    defaultOffset, reinterpret_cast<const CaseTableEntry*>(table), ncases);
            }

            default:
                assert(false);
                return false;
        }
    }

    cell_t readCell() {
        return read<cell_t>();
    }
    int16_t readInt16() {
        return read<int16_t>();
    }
    template <typename U> U read() {
        assert(cip_ + sizeof(U) <= stop_at_);
        U val = *reinterpret_cast<const U*>(cip_);
        cip_ += sizeof(U);
        return val;
    }
    const uint8_t* getBytes(size_t n) {
        assert(cip_ + n <= stop_at_);
        const uint8_t* result = cip_;
        cip_ += n;
        return result;
    }

  private:
    Runtime* rt_;
    T* visitor_;
    const uint8_t* code_;
    const uint8_t* insn_begin_;
    const uint8_t* cip_;
    const uint8_t* stop_at_;
};

} // namespace sp::v2
