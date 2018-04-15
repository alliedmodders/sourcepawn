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
#ifndef _include_sourcepawn_vm_pcode_visitor_h_
#define _include_sourcepawn_vm_pcode_visitor_h_

#include <sp_vm_types.h>
#include <smx/smx-v1-opcodes.h>

namespace sp {

class LegacyImage;

enum class PawnReg {
  Pri,
  Alt
};

enum class CompareOp {
  Zero,
  NotZero,
  Eq,
  Neq,
  Sless,
  Sleq,
  Sgrtr,
  Sgeq
};

struct CaseTableEntry {
  cell_t value;
  cell_t address;
};
static_assert(sizeof(CaseTableEntry) == sizeof(cell_t) * 2,
              "CaseTableEntry must be two cells wide");

class PcodeVisitor
{
 public:
  virtual bool visitBREAK() = 0;
  virtual bool visitLOAD(PawnReg dest, cell_t srcaddr) = 0;
  virtual bool visitLOAD_S(PawnReg dest, cell_t srcoffs) = 0;
  virtual bool visitLREF_S(PawnReg dest, cell_t srcoffs) = 0;
  virtual bool visitLOAD_I() = 0;
  virtual bool visitLODB_I(cell_t width) = 0;
  virtual bool visitCONST(PawnReg dest, cell_t imm) = 0;
  virtual bool visitADDR(PawnReg dest, cell_t offset) = 0;
  virtual bool visitSTOR(cell_t address, PawnReg src) = 0;
  virtual bool visitSTOR_S(cell_t offset, PawnReg src) = 0;
  virtual bool visitSREF_S(cell_t offset, PawnReg src) = 0;
  virtual bool visitSTOR_I() = 0;
  virtual bool visitSTRB_I(cell_t width) = 0;
  virtual bool visitLIDX() = 0;
  virtual bool visitIDXADDR() = 0;
  virtual bool visitMOVE(PawnReg reg) = 0;
  virtual bool visitXCHG() = 0;
  virtual bool visitPUSH(PawnReg src) = 0;
  virtual bool visitPUSH_C(const cell_t* vals, size_t nvals) = 0;
  virtual bool visitPUSH(const cell_t* addresses, size_t nvals) = 0;
  virtual bool visitPUSH_S(const cell_t* offsets, size_t nvals) = 0;
  virtual bool visitPOP(PawnReg dest) = 0;
  virtual bool visitSTACK(cell_t amount) = 0;
  virtual bool visitHEAP(cell_t amount) = 0;
  virtual bool visitRETN() = 0;
  virtual bool visitCALL(cell_t offset) = 0;
  virtual bool visitJUMP(cell_t offset) = 0;
  virtual bool visitJcmp(CompareOp op, cell_t offset) = 0;
  virtual bool visitSHL() = 0;
  virtual bool visitSHR() = 0;
  virtual bool visitSSHR() = 0;
  virtual bool visitSHL_C(PawnReg dest, cell_t amount) = 0;
  virtual bool visitSMUL() = 0;
  virtual bool visitSDIV(PawnReg dest) = 0;
  virtual bool visitADD() = 0;
  virtual bool visitSUB() = 0;
  virtual bool visitSUB_ALT() = 0;
  virtual bool visitAND() = 0;
  virtual bool visitOR() = 0;
  virtual bool visitXOR() = 0;
  virtual bool visitNOT() = 0;
  virtual bool visitNEG() = 0;
  virtual bool visitINVERT() = 0;
  virtual bool visitADD_C(cell_t value) = 0;
  virtual bool visitSMUL_C(cell_t value) = 0;
  virtual bool visitZERO(PawnReg dest) = 0;
  virtual bool visitZERO(cell_t address) = 0;
  virtual bool visitZERO_S(cell_t offset) = 0;
  virtual bool visitCompareOp(CompareOp op) = 0;
  virtual bool visitEQ_C(PawnReg src, cell_t value) = 0;
  virtual bool visitINC(PawnReg dest) = 0;
  virtual bool visitINC(cell_t address) = 0;
  virtual bool visitINC_S(cell_t offset) = 0;
  virtual bool visitINC_I() = 0;
  virtual bool visitDEC(PawnReg dest) = 0;
  virtual bool visitDEC(cell_t address) = 0;
  virtual bool visitDEC_S(cell_t offset) = 0;
  virtual bool visitDEC_I() = 0;
  virtual bool visitMOVS(uint32_t amount) = 0;
  virtual bool visitFILL(uint32_t amount) = 0;
  virtual bool visitBOUNDS(uint32_t limit) = 0;
  virtual bool visitSYSREQ_C(uint32_t native_index) = 0;
  virtual bool visitSWAP(PawnReg dest) = 0;
  virtual bool visitPUSH_ADR(const cell_t* offsets, size_t nvals) = 0;
  virtual bool visitSYSREQ_N(uint32_t native_index, uint32_t nparams) = 0;
  virtual bool visitLOAD_BOTH(cell_t addressForPri, cell_t addressForAlt) = 0;
  virtual bool visitLOAD_S_BOTH(cell_t offsetForPri, cell_t offsetForAlt) = 0;
  virtual bool visitCONST(cell_t address, cell_t value) = 0;
  virtual bool visitCONST_S(cell_t offset, cell_t value) = 0;
  virtual bool visitTRACKER_PUSH_C(cell_t amount) = 0;
  virtual bool visitTRACKER_POP_SETHEAP() = 0;
  virtual bool visitGENARRAY(uint32_t dims, bool autozero) = 0;
  virtual bool visitSTRADJUST_PRI() = 0;
  virtual bool visitFABS() = 0;
  virtual bool visitFLOAT() = 0;
  virtual bool visitFLOATADD() = 0;
  virtual bool visitFLOATSUB() = 0;
  virtual bool visitFLOATMUL() = 0;
  virtual bool visitFLOATDIV() = 0;
  virtual bool visitRND_TO_NEAREST() = 0;
  virtual bool visitRND_TO_FLOOR() = 0;
  virtual bool visitRND_TO_CEIL() = 0;
  virtual bool visitRND_TO_ZERO() = 0;
  virtual bool visitFLOATCMP() = 0;
  virtual bool visitFLOAT_CMP_OP(CompareOp op) = 0;
  virtual bool visitFLOAT_NOT() = 0;
  virtual bool visitHALT(cell_t value) = 0;
  virtual bool visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) = 0;
  virtual bool visitREBASE(cell_t addr, cell_t iv_size, cell_t data_size) = 0;
};

class IncompletePcodeVisitor : public PcodeVisitor
{
 public:
  virtual bool visitBREAK() override {
    assert(false);
    return false;
  }
  virtual bool visitLOAD(PawnReg dest, cell_t srcaddr) override {
    assert(false);
    return false;
  }
  virtual bool visitLOAD_S(PawnReg dest, cell_t srcoffs) override {
    assert(false);
    return false;
  }
  virtual bool visitLREF_S(PawnReg dest, cell_t srcoffs) override {
    assert(false);
    return false;
  }
  virtual bool visitLOAD_I() override {
    assert(false);
    return false;
  }
  virtual bool visitLODB_I(cell_t width) override {
    assert(false);
    return false;
  }
  virtual bool visitCONST(PawnReg dest, cell_t imm) override {
    assert(false);
    return false;
  }
  virtual bool visitADDR(PawnReg dest, cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitSTOR(cell_t offset, PawnReg src) override {
    assert(false);
    return false;
  }
  virtual bool visitSTOR_S(cell_t offset, PawnReg src) override {
    assert(false);
    return false;
  }
  virtual bool visitSREF_S(cell_t offset, PawnReg src) override {
    assert(false);
    return false;
  }
  virtual bool visitSTOR_I() override {
    assert(false);
    return false;
  }
  virtual bool visitSTRB_I(cell_t width) override {
    assert(false);
    return false;
  }
  virtual bool visitLIDX() override {
    assert(false);
    return false;
  }
  virtual bool visitIDXADDR() override {
    assert(false);
    return false;
  }
  virtual bool visitMOVE(PawnReg reg) override {
    assert(false);
    return false;
  }
  virtual bool visitXCHG() override {
    assert(false);
    return false;
  }
  virtual bool visitPUSH(PawnReg src) override {
    assert(false);
    return false;
  }
  virtual bool visitPUSH_C(const cell_t* vals, size_t nvals) override {
    assert(false);
    return false;
  }
  virtual bool visitPUSH(const cell_t* offsets, size_t nvals) override {
    assert(false);
    return false;
  }
  virtual bool visitPUSH_S(const cell_t* offsets, size_t nvals) override {
    assert(false);
    return false;
  }
  virtual bool visitPOP(PawnReg dest) override {
    assert(false);
    return false;
  }
  virtual bool visitSTACK(cell_t amount) override {
    assert(false);
    return false;
  }
  virtual bool visitHEAP(cell_t amount) override {
    assert(false);
    return false;
  }
  virtual bool visitRETN() override {
    assert(false);
    return false;
  }
  virtual bool visitCALL(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitJUMP(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitJcmp(CompareOp op, cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitSHL() override {
    assert(false);
    return false;
  }
  virtual bool visitSHR() override {
    assert(false);
    return false;
  }
  virtual bool visitSSHR() override {
    assert(false);
    return false;
  }
  virtual bool visitSHL_C(PawnReg dest, cell_t amount) override {
    assert(false);
    return false;
  }
  virtual bool visitSMUL() override {
    assert(false);
    return false;
  }
  virtual bool visitSDIV(PawnReg dest) override {
    assert(false);
    return false;
  }
  virtual bool visitADD() override {
    assert(false);
    return false;
  }
  virtual bool visitSUB() override {
    assert(false);
    return false;
  }
  virtual bool visitSUB_ALT() override {
    assert(false);
    return false;
  }
  virtual bool visitAND() override {
    assert(false);
    return false;
  }
  virtual bool visitOR() override {
    assert(false);
    return false;
  }
  virtual bool visitXOR() override {
    assert(false);
    return false;
  }
  virtual bool visitNOT() override {
    assert(false);
    return false;
  }
  virtual bool visitNEG() override {
    assert(false);
    return false;
  }
  virtual bool visitINVERT() override {
    assert(false);
    return false;
  }
  virtual bool visitADD_C(cell_t value) override {
    assert(false);
    return false;
  }
  virtual bool visitSMUL_C(cell_t value) override {
    assert(false);
    return false;
  }
  virtual bool visitZERO(PawnReg dest) override {
    assert(false);
    return false;
  }
  virtual bool visitZERO(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitZERO_S(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitCompareOp(CompareOp op) override {
    assert(false);
    return false;
  }
  virtual bool visitEQ_C(PawnReg src, cell_t value) override {
    assert(false);
    return false;
  }
  virtual bool visitINC(PawnReg dest) override {
    assert(false);
    return false;
  }
  virtual bool visitINC(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitINC_S(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitINC_I() override {
    assert(false);
    return false;
  }
  virtual bool visitDEC(PawnReg dest) override {
    assert(false);
    return false;
  }
  virtual bool visitDEC(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitDEC_S(cell_t offset) override {
    assert(false);
    return false;
  }
  virtual bool visitDEC_I() override {
    assert(false);
    return false;
  }
  virtual bool visitMOVS(uint32_t amount) override {
    assert(false);
    return false;
  }
  virtual bool visitFILL(uint32_t amount) override {
    assert(false);
    return false;
  }
  virtual bool visitBOUNDS(uint32_t limit) override {
    assert(false);
    return false;
  }
  virtual bool visitSYSREQ_C(uint32_t native_index) override {
    assert(false);
    return false;
  }
  virtual bool visitSWAP(PawnReg dest) override {
    assert(false);
    return false;
  }
  virtual bool visitPUSH_ADR(const cell_t* offsets, size_t nvals) override {
    assert(false);
    return false;
  }
  virtual bool visitSYSREQ_N(uint32_t native_index, uint32_t nparams) override {
    assert(false);
    return false;
  }
  virtual bool visitLOAD_BOTH(cell_t offsetForPri, cell_t offsetForAlt) override {
    assert(false);
    return false;
  }
  virtual bool visitLOAD_S_BOTH(cell_t offsetForPri, cell_t offsetForAlt) override {
    assert(false);
    return false;
  }
  virtual bool visitCONST(cell_t offset, cell_t value) override {
    assert(false);
    return false;
  }
  virtual bool visitCONST_S(cell_t offset, cell_t value) override {
    assert(false);
    return false;
  }
  virtual bool visitTRACKER_PUSH_C(cell_t amount) override {
    assert(false);
    return false;
  }
  virtual bool visitTRACKER_POP_SETHEAP() override {
    assert(false);
    return false;
  }
  virtual bool visitGENARRAY(uint32_t dims, bool autozero) override {
    assert(false);
    return false;
  }
  virtual bool visitSTRADJUST_PRI() override {
    assert(false);
    return false;
  }
  virtual bool visitFABS() override {
    assert(false);
    return false;
  }
  virtual bool visitFLOAT() override {
    assert(false);
    return false;
  }
  virtual bool visitFLOATADD() override {
    assert(false);
    return false;
  }
  virtual bool visitFLOATSUB() override {
    assert(false);
    return false;
  }
  virtual bool visitFLOATMUL() override {
    assert(false);
    return false;
  }
  virtual bool visitFLOATDIV() override {
    assert(false);
    return false;
  }
  virtual bool visitRND_TO_NEAREST() override {
    assert(false);
    return false;
  }
  virtual bool visitRND_TO_FLOOR() override {
    assert(false);
    return false;
  }
  virtual bool visitRND_TO_CEIL() override {
    assert(false);
    return false;
  }
  virtual bool visitRND_TO_ZERO() override {
    assert(false);
    return false;
  }
  virtual bool visitFLOATCMP() override {
    assert(false);
    return false;
  }
  virtual bool visitFLOAT_CMP_OP(CompareOp op) override {
    assert(false);
    return false;
  }
  virtual bool visitFLOAT_NOT() override {
    assert(false);
    return false;
  }
  virtual bool visitHALT(cell_t value) override {
    assert(false);
    return false;
  }
  virtual bool visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) override {
    assert(false);
    return false;
  }
  virtual bool visitREBASE(cell_t addr, cell_t iv_size, cell_t data_size) override {
    assert(false);
    return false;
  }
};

} // namespace sp

#endif // _include_sourcepawn_vm_pcode_visitor_h_

