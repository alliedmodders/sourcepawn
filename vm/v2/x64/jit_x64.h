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
#pragma once

#include "v2/jit.h"
#include "x64/constants-x64.h"
#include "v2/runtime.h"

namespace sp {
class CompiledFunction;
class SmxImage;
}
namespace sp::v2 {

using namespace SourcePawn;

class CallThunk;

class Compiler : public CompilerBase
{
    friend class OutOfBoundsErrorPath;

  public:
    Compiler(Runtime* rt, MethodInfo* method);
    ~Compiler();

    bool beforeVisitOp(OPCODE op) override;
    bool visitBREAK() override;
    bool visitLOAD_PRI(cell_t srcaddr) override;
    bool visitLOAD_S(PawnReg dest, cell_t srcoffs) override;
    bool visitLOAD_I() override;
    bool visitLODB_I() override;
    bool visitCONST(PawnReg dest, cell_t imm) override;
    bool visitADDR(PawnReg dest, cell_t offset) override;
    bool visitSTOR_PRI(cell_t offset) override;
    bool visitSTOR_S(cell_t offset, PawnReg src) override;
    bool visitSTOR_I() override;
    bool visitSTRB_I() override;
    bool visitIDXADDR() override;
    bool visitMOVE(PawnReg reg) override;
    bool visitXCHG() override;
    bool visitPUSH(PawnReg src) override;
    bool visitPUSH_C(cell_t value) override;
    bool visitPUSH_S(cell_t offset) override;
    bool visitPOP(PawnReg dest) override;
    bool visitHEAP(cell_t amount) override;
    bool visitRETN() override;
    bool visitCALL(uint32_t method_index) override;
    bool visitJcmp(CompareOp op, cell_t offset) override;
    bool visitSHL() override;
    bool visitSHR() override;
    bool visitSSHR() override;
    bool visitSMUL() override;
    bool visitSDIV_ALT_I32() override;
    bool visitSMOD_ALT_I32() override;
    bool visitADD() override;
    bool visitSUB_ALT() override;
    bool visitAND() override;
    bool visitOR() override;
    bool visitXOR() override;
    bool visitNOT() override;
    bool visitNEG() override;
    bool visitINVERT() override;
    bool visitSMUL_C(cell_t value) override;
    bool visitZERO(PawnReg dest) override;
    bool visitCompareOp(CompareOp op) override;
    bool visitINC_PRI() override;
    bool visitDEC_PRI() override;
    bool visitMOVS(uint32_t amount) override;
    bool visitFILL(uint32_t amount) override;
    bool visitBOUNDS(uint32_t limit) override;
    bool visitSWAP_ALT() override;
    bool visitPUSH_ADR(cell_t slot) override;
    bool visitSYSREQ_N(uint32_t native_index, uint32_t nparams) override;
    bool visitGENARRAY(uint32_t dims, bool autozero) override;
    bool visitSTRADJUST_PRI() override;
    bool visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) override;
    bool visitMOVE_I64() override;
    bool visitCVT_I64(cell_t slot) override;
    bool visitTRUNCATE_I64() override;
    bool visitTEST_I64() override;
    bool visitINVERT_I64(cell_t slot) override;
    bool visitNEG_I64(cell_t slot) override;
    bool visitSMUL_I64(cell_t slot) override;
    bool visitSDIV_ALT_I64(cell_t pri_slot) override;
    bool visitSMOD_ALT_I64(cell_t pri_slot) override;
    bool visitADD_I64(cell_t slot) override;
    bool visitSUB_ALT_I64(cell_t slot) override;
    bool visitSHL_I64(cell_t slot) override;
    bool visitSSHR_I64(cell_t slot) override;
    bool visitSHR_I64(cell_t slot) override;
    bool visitOR_I64(cell_t slot) override;
    bool visitAND_I64(cell_t slot) override;
    bool visitXOR_I64(cell_t slot) override;
    bool visitCompareOp64(CompareOp op) override;
    bool visitTEST_F32() override;
    bool visitNEG_F32() override;
    bool visitMUL_F32() override;
    bool visitDIV_ALT_F32() override;
    bool visitADD_F32() override;
    bool visitSUB_ALT_F32() override;
    bool visitCompareOpF32(CompareOp op) override;
    bool visitCVT_F32() override;
    bool visitMOD_ALT_F32() override;
    bool visitSTOR_S_PRI_I64(cell_t slot) override;
    bool visitSTOR_S_C(cell_t slot, cell_t value) override;

  private:
    bool setup(cell_t pcode_offs);

  private:
    void emitPrologue() override;
    void emitOutOfBoundsError(OutOfBoundsError* path) override;
    void emitDebugBreakHandler() override;
    void emitCallThunk(CallThunk* thunk) override;

    void emitLegacyNativeCall(uint32_t native_index, NativeEntry* native);
    void emitGenArray(bool autozero);
    void emitCheckAddress(Register reg, size_t read_size = 4);
    void emitFloatCmp(ConditionCode cc);
    void jumpOnError(ConditionCode cc, int err = 0);

    Operand hpAddr() { return Operand(context_reg, Runtime::offsetOfHp()); }
    Operand frmAddr() { return Operand(context_reg, Runtime::offsetOfFrm()); }
    Operand spAddr() { return Operand(context_reg, Runtime::offsetOfSp()); }
    Operand hpScopeAddr() { return Operand(context_reg, Runtime::offsetOfHpScope()); }
};

} // namespace sp::v2
