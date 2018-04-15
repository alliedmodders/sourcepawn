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
#ifndef _include_sourcepawn_vm_interpreter_h_
#define _include_sourcepawn_vm_interpreter_h_

#include <assert.h>
#include <amtl/am-refcounting.h>
#include <sp_vm_types.h>
#include "pcode-visitor.h"
#include "pcode-reader.h"
#include "stack-frames.h"

namespace sp {

using namespace ke;

class Environment;
class PluginContext;
class PluginRuntime;
class MethodInfo;

class InterpRegs
{
public:
  InterpRegs()
   : regs_()
  {}

  cell_t& operator[](PawnReg reg) {
    assert(reg == PawnReg::Pri || reg == PawnReg::Alt);
    return regs_[unsigned(reg)];
  }
  cell_t& pri() {
    return regs_[0];
  }
  cell_t& alt() {
    return regs_[1];
  }

private:
  cell_t regs_[2];
};

class Interpreter final : public PcodeVisitor
{
 public:
  static bool Run(PluginContext* cx, RefPtr<MethodInfo> method, cell_t* rval);

 public:
  bool visitPUSH_C(const cell_t* vals, size_t nvals) override;
  bool visitPUSH_ADR(const cell_t* offsets, size_t nvals) override;
  bool visitCALL(cell_t offset) override;
  bool visitHEAP(cell_t amount) override;
  bool visitLOAD_I() override;
  bool visitSTOR_I() override;
  bool visitPUSH(PawnReg src) override;
  bool visitPUSH(const cell_t* offsets, size_t nvals) override;
  bool visitPOP(PawnReg dest) override;
  bool visitSYSREQ_C(uint32_t native_index) override;
  bool visitSYSREQ_N(uint32_t native_index, uint32_t nparams) override;
  bool visitZERO(PawnReg dest) override;
  bool visitZERO(cell_t offset) override;
  bool visitZERO_S(cell_t offset) override;
  bool visitRETN() override;
  bool visitSTACK(cell_t amount) override;
  bool visitPUSH_S(const cell_t* offsets, size_t nvals) override;
  bool visitCONST(PawnReg dest, cell_t imm) override;
  bool visitCONST(cell_t offset, cell_t value) override;
  bool visitCONST_S(cell_t offset, cell_t value) override;
  bool visitJUMP(cell_t offset) override;
  bool visitJcmp(CompareOp op, cell_t offset) override;
  bool visitLOAD_S(PawnReg dest, cell_t srcoffs) override;
  bool visitSTOR_S(cell_t offset, PawnReg src) override;
  bool visitLREF_S(PawnReg dest, cell_t srcoffs) override;
  bool visitSREF_S(cell_t destoffs, PawnReg src) override;
  bool visitADD_C(cell_t value) override;
  bool visitSMUL_C(cell_t value) override;
  bool visitADD() override;
  bool visitINC(PawnReg dest) override;
  bool visitINC(cell_t offset) override;
  bool visitINC_S(cell_t offset) override;
  bool visitINC_I() override;
  bool visitDEC(PawnReg dest) override;
  bool visitDEC(cell_t address) override;
  bool visitDEC_S(cell_t offset) override;
  bool visitDEC_I() override;
  bool visitLOAD_BOTH(cell_t offsetForPri, cell_t offsetForAlt) override;
  bool visitLOAD_S_BOTH(cell_t offsetForPri, cell_t offsetForAlt) override;
  bool visitAND() override;
  bool visitOR() override;
  bool visitXOR() override;
  bool visitSHL() override;
  bool visitSHR() override;
  bool visitSSHR() override;
  bool visitSHL_C(PawnReg dest, cell_t amount) override;
  bool visitSUB() override;
  bool visitSUB_ALT() override;
  bool visitSMUL() override;
  bool visitSDIV(PawnReg dest) override;
  bool visitNOT() override;
  bool visitNEG() override;
  bool visitINVERT() override;
  bool visitEQ_C(PawnReg src, cell_t value) override;
  bool visitCompareOp(CompareOp op) override;
  bool visitADDR(PawnReg dest, cell_t offset) override;
  bool visitMOVS(uint32_t amount) override;
  bool visitFILL(uint32_t amount) override;
  bool visitIDXADDR() override;
  bool visitLIDX() override;
  bool visitLODB_I(cell_t width) override;
  bool visitSTRB_I(cell_t width) override;
  bool visitLOAD(PawnReg dest, cell_t srcaddr) override;
  bool visitSTOR(cell_t offset, PawnReg src) override;
  bool visitMOVE(PawnReg reg) override;
  bool visitXCHG() override;
  bool visitSWAP(PawnReg dest) override;
  bool visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) override;
  bool visitFABS() override;
  bool visitFLOAT() override;
  bool visitFLOATADD() override;
  bool visitFLOATSUB() override;
  bool visitFLOATMUL() override;
  bool visitFLOATDIV() override;
  bool visitRND_TO_NEAREST() override;
  bool visitRND_TO_FLOOR() override;
  bool visitRND_TO_CEIL() override;
  bool visitRND_TO_ZERO() override;
  bool visitFLOATCMP() override;
  bool visitFLOAT_CMP_OP(CompareOp op) override;
  bool visitFLOAT_NOT() override;
  bool visitBOUNDS(uint32_t limit) override;
  bool visitGENARRAY(uint32_t dims, bool autozero) override;
  bool visitTRACKER_PUSH_C(cell_t amount) override;
  bool visitTRACKER_POP_SETHEAP() override;
  bool visitSTRADJUST_PRI() override;
  bool visitBREAK() override;
  bool visitHALT(cell_t value) override;
  bool visitREBASE(cell_t addr, cell_t iv_size, cell_t data_size) override;

 private:
  Interpreter(PluginContext* cx, RefPtr<MethodInfo> method);

  bool run();

  cell_t return_value() const {
    return return_value_;
  }

 private:
  bool invokeNative(uint32_t native_index);

 private:
  Environment* env_;
  PluginRuntime* rt_;
  PluginContext* cx_;
  PcodeReader<Interpreter> reader_;
  RefPtr<MethodInfo> method_;
  bool has_returned_;
  cell_t return_value_;
  InterpRegs regs_;
  InterpInvokeFrame* ivk_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_interpreter_h_
