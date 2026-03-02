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
#ifndef _INCLUDE_SOURCEPAWN_JIT_X86_H_
#define _INCLUDE_SOURCEPAWN_JIT_X86_H_

#include <amtl/am-vector.h>
#include <sp_vm_api.h>
#include <sp_vm_types.h>
#include "compiled-function.h"
#include "jit.h"
#include "macro-assembler.h"
#include "opcodes.h"
#include "plugin-context.h"
#include "plugin-runtime.h"

using namespace SourcePawn;

namespace sp {
class LegacyImage;
class Environment;
class CompiledFunction;
class CallThunk;

class Compiler : public CompilerBase
{
    friend class CallThunk;
    friend class OutOfBoundsErrorPath;

  public:
    Compiler(PluginRuntime* rt, MethodInfo* method);
    ~Compiler();

    bool visitBREAK() override;
    bool visitLOAD(PawnReg dest, cell_t srcaddr) override;
    bool visitLOAD_S(PawnReg dest, cell_t srcoffs) override;
    bool visitLREF_S(PawnReg dest, cell_t srcoffs) override;
    bool visitLOAD_I() override;
    bool visitLODB_I(cell_t width) override;
    bool visitCONST(PawnReg dest, cell_t imm) override;
    bool visitADDR(PawnReg dest, cell_t offset) override;
    bool visitSTOR(cell_t offset, PawnReg src) override;
    bool visitSTOR_S(cell_t offset, PawnReg src) override;
    bool visitSREF_S(cell_t offset, PawnReg src) override;
    bool visitSTOR_I() override;
    bool visitSTRB_I(cell_t width) override;
    bool visitLIDX() override;
    bool visitIDXADDR() override;
    bool visitMOVE(PawnReg reg) override;
    bool visitXCHG() override;
    bool visitPUSH(PawnReg src) override;
    bool visitPUSH_C(const cell_t* val, size_t nvals) override;
    bool visitPUSH(const cell_t* offsets, size_t nvals) override;
    bool visitPUSH_S(const cell_t* offsets, size_t nvals) override;
    bool visitPOP(PawnReg dest) override;
    bool visitSTACK(cell_t amount) override;
    bool visitHEAP(cell_t amount) override;
    bool visitRETN() override;
    bool visitCALL(cell_t offset) override;
    bool visitJUMP(cell_t offset) override;
    bool visitJcmp(CompareOp op, cell_t offset) override;
    bool visitSHL() override;
    bool visitSHR() override;
    bool visitSSHR() override;
    bool visitSHL_C(PawnReg dest, cell_t amount) override;
    bool visitSMUL() override;
    bool visitSDIV(PawnReg dest) override;
    bool visitSDIV_ALT_I32() override;
    bool visitSMOD_ALT_I32() override;
    bool visitADD() override;
    bool visitSUB() override;
    bool visitSUB_ALT() override;
    bool visitAND() override;
    bool visitOR() override;
    bool visitXOR() override;
    bool visitNOT() override;
    bool visitNEG() override;
    bool visitINVERT() override;
    bool visitADD_C(cell_t value) override;
    bool visitSMUL_C(cell_t value) override;
    bool visitZERO(PawnReg dest) override;
    bool visitZERO(cell_t offset) override;
    bool visitZERO_S(cell_t offset) override;
    bool visitCompareOp(CompareOp op) override;
    bool visitEQ_C(PawnReg src, cell_t value) override;
    bool visitINC(PawnReg dest) override;
    bool visitINC(cell_t offset) override;
    bool visitINC_S(cell_t offset) override;
    bool visitINC_I() override;
    bool visitDEC(PawnReg dest) override;
    bool visitDEC(cell_t offset) override;
    bool visitDEC_S(cell_t offset) override;
    bool visitDEC_I() override;
    bool visitMOVS(uint32_t amount) override;
    bool visitFILL(uint32_t amount) override;
    bool visitBOUNDS(uint32_t limit) override;
    bool visitSYSREQ_C(uint32_t native_index) override;
    bool visitSWAP(PawnReg dest) override;
    bool visitPUSH_ADR(const cell_t* offsets, size_t nvals) override;
    bool visitSYSREQ_N(uint32_t native_index, uint32_t nparams) override;
    bool visitLOAD_BOTH(cell_t offsetForPri, cell_t offsetForAlt) override;
    bool visitLOAD_S_BOTH(cell_t offsetForPri, cell_t offsetForAlt) override;
    bool visitCONST(cell_t offset, cell_t value) override;
    bool visitCONST_S(cell_t offset, cell_t value) override;
    bool visitTRACKER_PUSH_C(cell_t amount) override;
    bool visitTRACKER_POP_SETHEAP() override;
    bool visitGENARRAY(uint32_t dims, bool autozero) override;
    bool visitSTRADJUST_PRI() override;
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
    bool visitHALT(cell_t value) override;
    bool visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) override;
    bool visitINITARRAY(PawnReg reg, cell_t addr, cell_t iv_size, cell_t data_copy_size,
                        cell_t data_fill_size, cell_t fill_value) override;
    bool visitHEAP_SAVE() override;
    bool visitHEAP_RESTORE() override;
    bool visitPUSH_I_I64() override;
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
    bool visitSTOR_S_I64_C(cell_t slot, cell_t cell0, cell_t cell1) override;
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

  private:
    bool setup(cell_t pcode_offs);

  private:
    void emitPrologue() override;
    void emitThrowPath(int err) override;
    void emitErrorHandlers() override;
    void emitOutOfBoundsErrorPath(OutOfBoundsErrorPath* path) override;
    void emitDebugBreakHandler() override;

    void emitLegacyNativeCall(uint32_t native_index, NativeEntry* native);
    void emitGenArray(bool autozero);
    void emitCheckAddress(Register reg, size_t read_size = 4);
    void emitFloatCmp(ConditionCode cc);
    void emitCallThunk(CallThunk* thunk);
    void jumpOnError(ConditionCode cc, int err = 0);

    ExternalAddress hpAddr() {
        return ExternalAddress(context_->addressOfHp());
    }
    ExternalAddress frmAddr() {
        return ExternalAddress(context_->addressOfFrm());
    }
    ExternalAddress spAddr() {
        return ExternalAddress(context_->addressOfSp());
    }
    ExternalAddress hpScopeAddr() {
        return ExternalAddress(context_->addressOfHpScope());
    }
};

const Register pri = eax;
const Register alt = edx;
const Register stk = edi;
const Register dat = esi;
const Register tmp = ecx;
const Register frm = ebx;

} // namespace sp

#endif //_INCLUDE_SOURCEPAWN_JIT_X86_H_
