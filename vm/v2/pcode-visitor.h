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
#pragma once

#include <smx/smx-v2-opcodes.h>
#include <sp_vm_types.h>

namespace sp::v2 {

class LegacyImage;

enum class PawnReg { Pri, Alt };

enum class CompareOp { Zero, NotZero, Eq, Neq, Sless, Sleq, Sgrtr, Sgeq };

struct CaseTableEntry {
    cell_t value;
    cell_t address;
};
static_assert(sizeof(CaseTableEntry) == sizeof(cell_t) * 2,
              "CaseTableEntry must be two cells wide");

class PcodeVisitor
{
  public:
    virtual bool beforeVisitOp(OPCODE op) { return true; }
    virtual bool visitBREAK() = 0;
    virtual bool visitLOAD_GLB(cell_t srcaddr) = 0;
    virtual bool visitLOAD_S(cell_t srcoffs) = 0;
    virtual bool visitLREF_S(cell_t srcoffs) = 0;
    virtual bool visitLOAD_I() = 0;
    virtual bool visitLODB_I() = 0;
    virtual bool visitADDR_S(cell_t offset) = 0;
    virtual bool visitSTOR_GLB(cell_t address) = 0;
    virtual bool visitSTOR_S(cell_t offset) = 0;
    virtual bool visitSREF_S(cell_t offset) = 0;
    virtual bool visitSTOR_I() = 0;
    virtual bool visitSTRB_I() = 0;
    virtual bool visitLOAD_FN(uint32_t method_index) = 0;
    virtual bool visitIDXADDR(uint8_t rank_size, int32_t bounds) = 0;
    virtual bool visitPUSH_C(cell_t value) = 0;
    virtual bool visitPUSH_C_I8(int8_t value) = 0;
    virtual bool visitPUSH_C_I64(int64_t value) = 0;
    virtual bool visitPOP() = 0;
    virtual bool visitDUP() = 0;
    virtual bool visitSWAP() = 0;
    virtual bool visitDUP_ROTATE() = 0;
    virtual bool visitHEAP(cell_t amount) = 0;
    virtual bool visitRETN() = 0;
    virtual bool visitRETV() = 0;
    virtual bool visitCALL(uint32_t method_index) = 0;
    virtual bool visitCALLN(uint32_t method_index, uint8_t nargs) = 0;
    virtual bool visitJUMP(cell_t offset) = 0;
    virtual bool visitJcmp(CompareOp op, cell_t offset) = 0;
    virtual bool visitSHL() = 0;
    virtual bool visitSHR() = 0;
    virtual bool visitSSHR() = 0;
    virtual bool visitSMUL() = 0;
    virtual bool visitSDIV_I32() = 0;
    virtual bool visitSMOD_I32() = 0;
    virtual bool visitADD() = 0;
    virtual bool visitSUB() = 0;
    virtual bool visitAND() = 0;
    virtual bool visitOR() = 0;
    virtual bool visitXOR() = 0;
    virtual bool visitNOT() = 0;
    virtual bool visitNEG() = 0;
    virtual bool visitINVERT() = 0;
    virtual bool visitADD_C(cell_t value) = 0;
    virtual bool visitSMUL_C(cell_t value) = 0;
    virtual bool visitZERO_S(cell_t offset) = 0;
    virtual bool visitZERO_S_I64(cell_t offset) = 0;
    virtual bool visitCompareOp(CompareOp op) = 0;
    virtual bool visitINC() = 0;
    virtual bool visitDEC() = 0;
    virtual bool visitMOVS(uint32_t amount) = 0;
    virtual bool visitMOVE_I64() = 0;
    virtual bool visitFILL(uint32_t amount) = 0;
    virtual bool visitSWITCH(cell_t defaultOffset, const CaseTableEntry* cases, size_t ncases) = 0;
    virtual bool visitHEAP_SAVE() = 0;
    virtual bool visitHEAP_RESTORE() = 0;
    virtual bool visitCVT_I64(cell_t slot) = 0;
    virtual bool visitTRUNCATE_I64() = 0;
    virtual bool visitTEST_I64() = 0;
    virtual bool visitINVERT_I64(cell_t slot) = 0;
    virtual bool visitNEG_I64(cell_t slot) = 0;
    virtual bool visitSMUL_I64(cell_t slot) = 0;
    virtual bool visitSDIV_I64(cell_t pri_slot) = 0;
    virtual bool visitSMOD_I64(cell_t pri_slot) = 0;
    virtual bool visitADD_I64(cell_t slot) = 0;
    virtual bool visitSUB_I64(cell_t slot) = 0;
    virtual bool visitSHL_I64(cell_t slot) = 0;
    virtual bool visitSSHR_I64(cell_t slot) = 0;
    virtual bool visitSHR_I64(cell_t slot) = 0;
    virtual bool visitOR_I64(cell_t slot) = 0;
    virtual bool visitAND_I64(cell_t slot) = 0;
    virtual bool visitXOR_I64(cell_t slot) = 0;
    virtual bool visitSTOR_S_C(cell_t slot, cell_t value) = 0;
    virtual bool visitSTOR_S_C_I64(cell_t slot, cell_t cell0, cell_t cell1) = 0;
    virtual bool visitSTOR_S_I64(cell_t slot) = 0;
    virtual bool visitCompareOp64(CompareOp op) = 0;
    virtual bool visitTEST_F32() = 0;
    virtual bool visitNEG_F32() = 0;
    virtual bool visitMUL_F32() = 0;
    virtual bool visitDIV_F32() = 0;
    virtual bool visitADD_F32() = 0;
    virtual bool visitSUB_F32() = 0;
    virtual bool visitCVT_F32() = 0;
    virtual bool visitMOD_F32() = 0;
    virtual bool visitCompareOpF32(CompareOp op) = 0;
};

} // namespace sp::v2
