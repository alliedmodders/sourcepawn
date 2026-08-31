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
#pragma once

#include <stdint.h>

namespace sp::v2 {

#define LL_OPCODE_LIST(FOR_EACH) \
    FOR_EACH(NOP, 0, "nop") \
    FOR_EACH(LOAD_GLB, 1, "load.glb") \
    FOR_EACH(LOAD_S, 2, "load.s") \
    FOR_EACH(LOAD_I_I32, 3, "load.i.i32") \
    FOR_EACH(LOAD_I_U8, 4, "load.i.u8") \
    FOR_EACH(POP, 5, "pop") \
    FOR_EACH(STOR_GLB, 6, "stor.glb") \
    FOR_EACH(STOR_S, 7, "stor.s") \
    FOR_EACH(DUP, 8, "dup") \
    FOR_EACH(STOR_S_C, 9, "stor.s.c") \
    FOR_EACH(STOR_I_I32, 10, "stor.i.i32") \
    FOR_EACH(STOR_I_U8, 11, "stor.i.u8") \
    FOR_EACH(IDXADDR, 12, "idxaddr") \
    FOR_EACH(PUSH_C, 13, "push.c") \
    FOR_EACH(RETN, 14, "retn") \
    FOR_EACH(CALL, 15, "call") \
    FOR_EACH(JUMP, 16, "jump") \
    FOR_EACH(JZER, 17, "jzer") \
    FOR_EACH(JNZ, 18, "jnz") \
    FOR_EACH(JEQ, 19, "jeq") \
    FOR_EACH(JNEQ, 20, "jneq") \
    FOR_EACH(JSLESS, 21, "jsless") \
    FOR_EACH(JSLEQ, 22, "jsleq") \
    FOR_EACH(JSGRTR, 23, "jsgrtr") \
    FOR_EACH(JSGEQ, 24, "jsgeq") \
    FOR_EACH(SHL, 25, "shl") \
    FOR_EACH(SHR, 26, "shr") \
    FOR_EACH(SSHR, 27, "sshr") \
    FOR_EACH(SMUL_I32, 28, "smul.i32") \
    FOR_EACH(SDIV_I32, 29, "sdiv.i32") \
    FOR_EACH(SMOD_I32, 30, "smod.i32") \
    FOR_EACH(ADD_I32, 31, "add.i32") \
    FOR_EACH(SUB_I32, 32, "sub.i32") \
    FOR_EACH(AND, 33, "and") \
    FOR_EACH(OR, 34, "or") \
    FOR_EACH(XOR, 35, "xor") \
    FOR_EACH(NOT, 36, "not") \
    FOR_EACH(NEG, 37, "neg") \
    FOR_EACH(INVERT, 38, "invert") \
    FOR_EACH(EQ_I32, 39, "eq.i32") \
    FOR_EACH(NEQ_I32, 40, "neq.i32") \
    FOR_EACH(SLESS_I32, 41, "sless.i32") \
    FOR_EACH(SLEQ_I32, 42, "sleq.i32") \
    FOR_EACH(SGRTR_I32, 43, "sgrtr.i32") \
    FOR_EACH(SGEQ_I32, 44, "sgeq.i32") \
    FOR_EACH(INC, 45, "inc") \
    FOR_EACH(DEC, 46, "dec") \
    FOR_EACH(COPYARRAY, 47, "copyarray") \
    FOR_EACH(SWITCH, 48, "switch") \
    FOR_EACH(ADDR_S, 49, "addr.s") \
    FOR_EACH(HEAP_SAVE, 50, "heap.save") \
    FOR_EACH(HEAP_RESTORE, 51, "heap.restore") \
    FOR_EACH(TEST_F32, 52, "test.f32") \
    FOR_EACH(NEG_F32, 53, "neg.f32") \
    FOR_EACH(MUL_F32, 54, "mul.f32") \
    FOR_EACH(DIV_F32, 55, "div.f32") \
    FOR_EACH(ADD_F32, 56, "add.f32") \
    FOR_EACH(SUB_F32, 57, "sub.f32") \
    FOR_EACH(EQ_F32, 58, "eq.f32") \
    FOR_EACH(NEQ_F32, 59, "neq.f32") \
    FOR_EACH(LESS_F32, 60, "less.f32") \
    FOR_EACH(LEQ_F32, 61, "leq.f32") \
    FOR_EACH(GRTR_F32, 62, "grtr.f32") \
    FOR_EACH(GEQ_F32, 63, "geq.f32") \
    FOR_EACH(CVT_F32, 64, "cvt.f32") \
    FOR_EACH(MOD_F32, 65, "mod.f32") \
    FOR_EACH(CVT_I64, 66, "cvt.i64") \
    FOR_EACH(TRUNCATE_I64, 67, "truncate.i64") \
    FOR_EACH(TEST_I64, 68, "test.i64") \
    FOR_EACH(INVERT_I64, 69, "invert.i64") \
    FOR_EACH(NEG_I64, 70, "neg.i64") \
    FOR_EACH(SMUL_I64, 71, "smul.i64") \
    FOR_EACH(SDIV_I64, 72, "sdiv.i64") \
    FOR_EACH(ADD_I64, 73, "add.i64") \
    FOR_EACH(SUB_I64, 74, "sub.i64") \
    FOR_EACH(SHL_I64, 75, "shl.i64") \
    FOR_EACH(SSHR_I64, 76, "sshr.i64") \
    FOR_EACH(SHR_I64, 77, "shr.i64") \
    FOR_EACH(EQ_I64, 78, "eq.i64") \
    FOR_EACH(NEQ_I64, 79, "neq.i64") \
    FOR_EACH(OR_I64, 80, "or.i64") \
    FOR_EACH(AND_I64, 81, "and.i64") \
    FOR_EACH(XOR_I64, 82, "xor.i64") \
    FOR_EACH(SLESS_I64, 83, "sless.i64") \
    FOR_EACH(SLEQ_I64, 84, "sleq.i64") \
    FOR_EACH(SGRTR_I64, 85, "sgrtr.i64") \
    FOR_EACH(SGEQ_I64, 86, "sgeq.i64") \
    FOR_EACH(SMOD_I64, 87, "smod.i64") \
    FOR_EACH(SWAP, 88, "swap") \
    FOR_EACH(LOAD_FN, 89, "load.fn") \
    FOR_EACH(LOAD_I_I64, 90, "load.i.i64") \
    FOR_EACH(STOR_I_I64, 91, "stor.i.i64") \
    FOR_EACH(RETV, 92, "retv") \
    FOR_EACH(PUSH_C_I8, 93, "push.c.i8") \
    FOR_EACH(CALLN, 94, "calln") \
    FOR_EACH(PUSH_C_I64, 95, "push.c.i64") \
    FOR_EACH(ADDR_GLB, 96, "addr.glb") \
    FOR_EACH(LOAD_STR, 97, "load.str") \
    FOR_EACH(NEWARRAY, 98, "newarray") \
    FOR_EACH(NEWBULKARRAY, 99, "newbulkarray") \
    FOR_EACH(FILLARRAY, 100, "fillarray") \
    FOR_EACH(ARRAY_TO_NATIVE, 101, "array2native") \
    FOR_EACH(SLICE, 102, "slice") \
    FOR_EACH(LOAD_FLD_X32, 103, "load.fld.x32") \
    FOR_EACH(ADDR_FLD, 104, "addr.fld") \
    FOR_EACH(LOAD_ELEM_I32, 105, "load.elem.i32") \
    FOR_EACH(LOAD_ELEM_F32, 106, "load.elem.f32") \
    FOR_EACH(LOAD_ELEM_I64, 107, "load.elem.i64") \
    FOR_EACH(LOAD_ELEM_U8, 108, "load.elem.i8") \
    FOR_EACH(STOR_ELEM_I32, 109, "stor.elem.i32") \
    FOR_EACH(STOR_ELEM_F32, 110, "stor.elem.f32") \
    FOR_EACH(STOR_ELEM_I64, 111, "stor.elem.i64") \
    FOR_EACH(STOR_ELEM_U8, 112, "stor.elem.i8") \
    FOR_EACH(LOAD_I_F32, 113, "load.i.f32") \
    FOR_EACH(STOR_I_F32, 114, "stor.i.f32") \
    FOR_EACH(LOAD_ELEM_A, 115, "load.elem.a") \
    FOR_EACH(IDXADDR_FLAT, 116, "idxaddr.flat") \
    FOR_EACH(ARRAY_TO_FLAT, 117, "array2flat") \
    FOR_EACH(COPYARRAY_FLAT, 118, "copyarray.flat") \
    FOR_EACH(FILLARRAY_FLAT, 119, "fillarray.flat") \
    FOR_EACH(SLICE_FLAT, 120, "slice.flat") \
    FOR_EACH(STOR_ELEM_FLAT_I32, 121, "stor.elem.flat.i32") \
    FOR_EACH(STOR_ELEM_FLAT_F32, 122, "stor.elem.flat.f32") \
    FOR_EACH(STOR_ELEM_FLAT_I64, 123, "stor.elem.flat.i64") \
    FOR_EACH(STOR_ELEM_FLAT_U8, 124, "stor.elem.flat.i8") \
    FOR_EACH(LOAD_FLD_X64, 125, "load.fld.x64") \
    FOR_EACH(STOR_FLD_X32, 126, "stor.fld.x32") \
    FOR_EACH(STOR_FLD_X64, 127, "stor.fld.x64") \
    FOR_EACH(COPYOBJ, 128, "copyobj") \
    FOR_EACH(SLICE_ES, 129, "slice.es")

enum LLOp : uint16_t {
#define FOR_EACH_OPCODE(op, val, text) LL_##op = val,
    LL_OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    LLOp_Max
};

inline const char* GetLLOpName(LLOp op) {
    if (op >= LLOp_Max)
        return "unknown";
    switch (op) {
#define FOR_EACH_OPCODE(op, val, text) case LL_##op: return text;
        LL_OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
        default: return "unknown";
    }
}

} // namespace sp::v2
