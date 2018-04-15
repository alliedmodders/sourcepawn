// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#ifndef _include_sourcepawn_vm_pcode_reader_h_
#define _include_sourcepawn_vm_pcode_reader_h_

#include <assert.h>
#include <sp_vm_types.h>
#include <smx/smx-v1-opcodes.h>
#include <limits.h>
#include "plugin-runtime.h"
#include "control-flow.h"
#include "opcodes.h"

namespace sp {

class PluginRuntime;
class PcodeVisitor;

template <typename T>
class PcodeReader
{
 public:
  PcodeReader(PluginRuntime* rt, uint32_t startOffset, T* visitor)
   : rt_(rt),
     visitor_(visitor),
     code_(nullptr),
     cip_(nullptr),
     stop_at_(nullptr)
  {
    assert(ke::IsAligned(startOffset, sizeof(cell_t)));
  
    auto& code = rt->code();
    code_ = reinterpret_cast<const cell_t*>(code.bytes);
    cip_ = code_ + (startOffset / sizeof(cell_t));
    stop_at_ = reinterpret_cast<const cell_t*>(code.bytes + code.length);
  }
  PcodeReader(PluginRuntime* rt, Block* block, T* visitor)
   : rt_(rt),
     visitor_(visitor),
     code_(nullptr),
     cip_(nullptr),
     stop_at_(nullptr)
  {
    auto& code = rt->code();
    code_ = reinterpret_cast<const cell_t*>(code.bytes);
    cip_ = reinterpret_cast<const cell_t*>(block->start());

    const uint8_t* end = block->end();
    if (block->endType() == BlockEnd::Insn)
      end = NextInstruction(end);
    stop_at_ = reinterpret_cast<const cell_t*>(end);
  }

  // We skip the first OP_PROC; it should be handled before parsing bytecode.
  void begin() {
    if (peekOpcode() == OP_PROC)
      readCell();
  }

  // Read the next opcode, return true on success, false otherwise.
  bool visitNext() {
    OPCODE op = (OPCODE)readCell();
    return visitOp(op);
  }

  // Peek at the next opcode.
  OPCODE peekOpcode() const {
    assert(more());
    return (OPCODE)*cip_;
  }

  // Return whether or not there is more to decode.
  bool more() const {
    return cip_ < stop_at_;
  }

  // Return the current position in the code stream.
  const cell_t* const& cip() const {
    return cip_;
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
    switch (op) {
    case OP_NOP:
      return true;

    // This opcode is used to note where line breaks occur.
    case OP_BREAK:
      return visitor_->visitBREAK();

    case OP_LOAD_PRI:
    case OP_LOAD_ALT:
    {
      PawnReg reg = (op == OP_LOAD_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t offset = readCell();
      return visitor_->visitLOAD(reg, offset);
    }

    case OP_LOAD_S_PRI:
    case OP_LOAD_S_ALT:
    {
      PawnReg reg = (op == OP_LOAD_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t offset = readCell();
      return visitor_->visitLOAD_S(reg, offset);
    }

    case OP_LREF_S_PRI:
    case OP_LREF_S_ALT:
    {
      PawnReg reg = (op == OP_LREF_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t offset = readCell();
      return visitor_->visitLREF_S(reg, offset);
    }

    case OP_LOAD_I:
      return visitor_->visitLOAD_I();

    case OP_LODB_I:
    {
      cell_t val = readCell();
      return visitor_->visitLODB_I(val);
    }

    case OP_CONST_PRI:
    case OP_CONST_ALT:
    {
      PawnReg reg = (op == OP_CONST_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t val = readCell();
      return visitor_->visitCONST(reg, val);
    }

    case OP_ADDR_PRI:
    case OP_ADDR_ALT:
    {
      PawnReg reg = (op == OP_ADDR_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t offset = readCell();
      return visitor_->visitADDR(reg, offset);
    }

    case OP_STOR_PRI:
    case OP_STOR_ALT:
    {
      PawnReg reg = (op == OP_STOR_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t offset = readCell();
      return visitor_->visitSTOR(offset, reg);
    }

    case OP_STOR_S_PRI:
    case OP_STOR_S_ALT:
    {
      PawnReg reg = (op == OP_STOR_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t offset = readCell();
      return visitor_->visitSTOR_S(offset, reg);
    }

    case OP_SREF_S_PRI:
    case OP_SREF_S_ALT:
    {
      PawnReg reg = (op == OP_SREF_S_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t offset = readCell();
      return visitor_->visitSREF_S(offset, reg);
    }

    case OP_STOR_I:
      return visitor_->visitSTOR_I();

    case OP_STRB_I:
    {
      cell_t val = readCell();
      return visitor_->visitSTRB_I(val);
    }

    case OP_LIDX:
      return visitor_->visitLIDX();

    case OP_IDXADDR:
      return visitor_->visitIDXADDR();

    case OP_MOVE_PRI:
      return visitor_->visitMOVE(PawnReg::Pri);

    case OP_MOVE_ALT:
      return visitor_->visitMOVE(PawnReg::Alt);

    case OP_XCHG:
      return visitor_->visitXCHG();

    case OP_PUSH_PRI:
    case OP_PUSH_ALT:
    {
      PawnReg reg = (op == OP_PUSH_PRI) ? PawnReg::Pri : PawnReg::Alt;
      return visitor_->visitPUSH(reg);
    }

    case OP_PUSH_C:
    case OP_PUSH2_C:
    case OP_PUSH3_C:
    case OP_PUSH4_C:
    case OP_PUSH5_C:
    {
      size_t n = 1;
      if (op >= OP_PUSH2_C)
        n = ((op - OP_PUSH2_C) / 4) + 2;

      const cell_t* vec = getCells(n);
      return visitor_->visitPUSH_C(vec, n);
    }

    case OP_PUSH:
    case OP_PUSH2:
    case OP_PUSH3:
    case OP_PUSH4:
    case OP_PUSH5:
    {
      size_t n = 1;
      if (op >= OP_PUSH2)
        n = ((op - OP_PUSH2) / 4) + 2;

      const cell_t* vec = getCells(n);
      return visitor_->visitPUSH(vec, n);
    }

    case OP_PUSH_S:
    case OP_PUSH2_S:
    case OP_PUSH3_S:
    case OP_PUSH4_S:
    case OP_PUSH5_S:
    {
      size_t n = 1;
      if (op >= OP_PUSH2_S)
        n = ((op - OP_PUSH2_S) / 4) + 2;

      const cell_t* vec = getCells(n);
      return visitor_->visitPUSH_S(vec, n);
    }

    case OP_POP_PRI:
    case OP_POP_ALT:
    {
      PawnReg reg = (op == OP_POP_PRI) ? PawnReg::Pri : PawnReg::Alt;
      return visitor_->visitPOP(reg);
    }

    case OP_STACK:
    {
      cell_t amount = readCell();
      return visitor_->visitSTACK(amount);
    }

    case OP_HEAP:
    {
      cell_t amount = readCell();
      return visitor_->visitHEAP(amount);
    }

    case OP_RETN:
      return visitor_->visitRETN();

    case OP_CALL:
    {
      cell_t offset = readCell();
      return visitor_->visitCALL(offset);
    }

    case OP_JUMP:
    {
      cell_t offset = readCell();
      return visitor_->visitJUMP(offset);
    }

#define JCMP_CASE(op, cmpop)                  \
    case op: {                                  \
      cell_t offset = readCell();               \
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
    case OP_SHL_C_ALT:
    {
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

    case OP_ADD:
      return visitor_->visitADD();
    case OP_SUB:
      return visitor_->visitSUB();
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

    case OP_ADD_C:
    {
      cell_t val = readCell();
      return visitor_->visitADD_C(val);
    }

    case OP_SMUL_C:
    {
      cell_t val = readCell();
      return visitor_->visitSMUL_C(val);
    }

    case OP_ZERO_PRI:
      return visitor_->visitZERO(PawnReg::Pri);

    case OP_ZERO_ALT:
      return visitor_->visitZERO(PawnReg::Alt);

    case OP_ZERO:
    {
      cell_t offset = readCell();
      return visitor_->visitZERO(offset);
    }

    case OP_ZERO_S:
    {
      cell_t offset = readCell();
      return visitor_->visitZERO_S(offset);
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

    case OP_EQ_C_PRI:
    case OP_EQ_C_ALT:
    {
      PawnReg reg = (op == OP_EQ_C_PRI) ? PawnReg::Pri : PawnReg::Alt;
      cell_t val = readCell();
      return visitor_->visitEQ_C(reg, val);
    }

    case OP_INC_PRI:
      return visitor_->visitINC(PawnReg::Pri);
    case OP_INC_ALT:
      return visitor_->visitINC(PawnReg::Alt);

    case OP_INC:
    {
      cell_t offset = readCell();
      return visitor_->visitINC(offset);
    }

    case OP_INC_S:
    {
      cell_t offset = readCell();
      return visitor_->visitINC_S(offset);
    }

    case OP_INC_I:
      return visitor_->visitINC_I();

    case OP_DEC_PRI:
      return visitor_->visitDEC(PawnReg::Pri);
    case OP_DEC_ALT:
      return visitor_->visitDEC(PawnReg::Alt);

    case OP_DEC:
    {
      cell_t offset = readCell();
      return visitor_->visitDEC(offset);
    }

    case OP_DEC_S:
    {
      cell_t offset = readCell();
      return visitor_->visitDEC_S(offset);
    }

    case OP_DEC_I:
      return visitor_->visitDEC_I();

    case OP_MOVS:
    {
      cell_t val = readCell();
      return visitor_->visitMOVS(val);
    }

    case OP_FILL:
    {
      cell_t val = readCell();
      return visitor_->visitFILL(val);
    }

    case OP_BOUNDS:
    {
      cell_t value = readCell();
      return visitor_->visitBOUNDS(value);
    }

    case OP_SYSREQ_C:
    {
      cell_t index = readCell();
      return visitor_->visitSYSREQ_C(index);
    }

    case OP_SWAP_PRI:
    case OP_SWAP_ALT:
    {
      PawnReg reg = (op == OP_SWAP_PRI) ? PawnReg::Pri : PawnReg::Alt;
      return visitor_->visitSWAP(reg);
    }

    case OP_PUSH_ADR:
    case OP_PUSH2_ADR:
    case OP_PUSH3_ADR:
    case OP_PUSH4_ADR:
    case OP_PUSH5_ADR:
    {
      size_t n = 1;
      if (op >= OP_PUSH2_ADR)
        n = ((op - OP_PUSH2_ADR) / 4) + 2;

      const cell_t* vec = getCells(n);
      return visitor_->visitPUSH_ADR(vec, n);
    }

    case OP_SYSREQ_N:
    {
      cell_t index = readCell();
      cell_t nparams = readCell();

      NativeEntry* native = rt_->NativeAt(index);
      if (native->status == SP_NATIVE_BOUND &&
          !(native->flags & (SP_NTVFLAG_EPHEMERAL|SP_NTVFLAG_OPTIONAL)))
      {
        uint32_t replacement = rt_->GetNativeReplacement(index);
        if (replacement != OP_NOP)
          return visitOp((OPCODE)replacement);
      }

      return visitor_->visitSYSREQ_N(index, nparams);
    }

    case OP_LOAD_BOTH:
    {
      cell_t offs1 = readCell();
      cell_t offs2 = readCell();
      return visitor_->visitLOAD_BOTH(offs1, offs2);
    }

    case OP_LOAD_S_BOTH:
    {
      cell_t offs1 = readCell();
      cell_t offs2 = readCell();
      return visitor_->visitLOAD_S_BOTH(offs1, offs2);
    }

    case OP_CONST:
    {
      cell_t offset = readCell();
      cell_t value = readCell();
      return visitor_->visitCONST(offset, value);
    }

    case OP_CONST_S:
    {
      cell_t offset = readCell();
      cell_t value = readCell();
      return visitor_->visitCONST_S(offset, value);
    }

    case OP_TRACKER_PUSH_C:
    {
      cell_t amount = readCell();
      return visitor_->visitTRACKER_PUSH_C(amount);
    }

    case OP_TRACKER_POP_SETHEAP:
      return visitor_->visitTRACKER_POP_SETHEAP();

    case OP_GENARRAY:
    case OP_GENARRAY_Z:
    {
      cell_t val = readCell();
      return visitor_->visitGENARRAY(val, (op == OP_GENARRAY_Z));
    }

    case OP_STRADJUST_PRI:
      return visitor_->visitSTRADJUST_PRI();

    case OP_FABS:
      return visitor_->visitFABS();
    case OP_FLOAT:
      return visitor_->visitFLOAT();
    case OP_FLOATADD:
      return visitor_->visitFLOATADD();
    case OP_FLOATSUB:
      return visitor_->visitFLOATSUB();
    case OP_FLOATMUL:
      return visitor_->visitFLOATMUL();
    case OP_FLOATDIV:
      return visitor_->visitFLOATDIV();

    case OP_RND_TO_NEAREST:
      return visitor_->visitRND_TO_NEAREST();

    case OP_RND_TO_CEIL:
      return visitor_->visitRND_TO_CEIL();

    case OP_RND_TO_ZERO:
      return visitor_->visitRND_TO_ZERO();

    case OP_RND_TO_FLOOR:
      return visitor_->visitRND_TO_FLOOR();

    // This is the old float cmp, which returns ordered results. In newly
    // compiled code it should not be used or generated.
    //
    // Note that the checks here are inverted: the test is |rhs OP lhs|.
    case OP_FLOATCMP:
      return visitor_->visitFLOATCMP();

    case OP_FLOAT_GT:
      return visitor_->visitFLOAT_CMP_OP(CompareOp::Sgrtr);
    case OP_FLOAT_GE:
      return visitor_->visitFLOAT_CMP_OP(CompareOp::Sgeq);
    case OP_FLOAT_LE:
      return visitor_->visitFLOAT_CMP_OP(CompareOp::Sleq);
    case OP_FLOAT_LT:
      return visitor_->visitFLOAT_CMP_OP(CompareOp::Sless);
    case OP_FLOAT_EQ:
      return visitor_->visitFLOAT_CMP_OP(CompareOp::Eq);
    case OP_FLOAT_NE:
      return visitor_->visitFLOAT_CMP_OP(CompareOp::Neq);
    case OP_FLOAT_NOT:
      return visitor_->visitFLOAT_NOT();

    case OP_HALT:
    {
      cell_t value = readCell();
      return visitor_->visitHALT(value);
    }

    case OP_SWITCH:
    {
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
        defaultOffset,
        reinterpret_cast<const CaseTableEntry*>(table),
        ncases);
    }

    case OP_CASETBL:
    {
      cell_t ncases = readCell();

      getCells((ncases * 2) + 1);

      // Nothing to do here. This is handled in OP_SWITCH.
      return true;
    }

    case OP_REBASE:
    {
      cell_t addr = readCell();
      cell_t iv_size = readCell();
      cell_t data_size = readCell();
      return visitor_->visitREBASE(addr, iv_size, data_size);
    }

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
  const cell_t* cip_;
  const cell_t* stop_at_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_pcode_reader_h_
