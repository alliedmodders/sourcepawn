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

enum LLArgFmt : uint8_t {
    // 0xffff is an invalid register.
    LL_FMT_REG,

    // const TypeDesc*
    LL_FMT_TYPEDESC,

    // smx_rtti_method*
    LL_FMT_METHOD_PTR,
    LL_FMT_METHOD_ID,

    // string table index
    LL_FMT_STR_ID,

    // global table index
    LL_FMT_GLB_ID,

    // stack offset / variable index
    LL_FMT_STACK_ID,

    // Arbitrary constants.
    LL_FMT_U8,
    LL_FMT_U16,
    LL_FMT_I32,
    LL_FMT_U32,
    LL_FMT_CELL,
    LL_FMT_I64,

    // Jump target.
    LL_FMT_TARGET,

    // Specialized switch encoding.
    LL_FMT_SWITCH,

    // One register for each argument.
    LL_FMT_CALL,
};

#define LL_OPCODE_LIST(FOR_EACH) \
    FOR_EACH(NOP, 0, "nop", {}) \
    FOR_EACH(LOAD_GLB_X32, 1, "load.glb.x32", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_S, 2, "load.s", {LL_FMT_STACK_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_I32, 3, "load.i.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_U8, 4, "load.i.u8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_X32, 5, "stor.glb.x32", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_I_I32, 6, "stor.i.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_U8, 7, "stor.i.u8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(IDXADDR, 8, "idxaddr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(RETN, 9, "retn", {LL_FMT_REG}) \
    FOR_EACH(CALL, 10, "call", {LL_FMT_METHOD_PTR, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(JUMP, 11, "jump", {LL_FMT_TARGET}) \
    FOR_EACH(JZER, 12, "jzer", {LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JNZ, 13, "jnz", {LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JEQ, 14, "jeq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JNEQ, 15, "jneq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSLESS, 16, "jsless", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSLEQ, 17, "jsleq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSGRTR, 18, "jsgrtr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSGEQ, 19, "jsgeq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(SHL, 20, "shl", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHR, 21, "shr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SSHR, 22, "sshr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMUL_I32, 23, "smul.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SDIV_I32, 24, "sdiv.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMOD_I32, 25, "smod.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_I32, 26, "add.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_I32, 27, "sub.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(AND, 28, "and", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(OR, 29, "or", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(XOR, 30, "xor", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NOT, 31, "not", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG, 32, "neg", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(INVERT, 33, "invert", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_I32, 34, "eq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_I32, 35, "neq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLESS_I32, 36, "sless.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLEQ_I32, 37, "sleq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGRTR_I32, 38, "sgrtr.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGEQ_I32, 39, "sgeq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY, 40, "copyarray", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SWITCH, 41, "switch", {LL_FMT_REG, LL_FMT_SWITCH}) \
    FOR_EACH(ADDR_S, 42, "addr.s", {LL_FMT_STACK_ID, LL_FMT_REG}) \
    FOR_EACH(HEAP_SAVE, 43, "heap.save", {}) \
    FOR_EACH(HEAP_RESTORE, 44, "heap.restore", {}) \
    FOR_EACH(TEST_F32, 45, "test.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_F32, 46, "neg.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MUL_F32, 47, "mul.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(DIV_F32, 48, "div.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_F32, 49, "add.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_F32, 50, "sub.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_F32, 51, "eq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_F32, 52, "neq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LESS_F32, 53, "less.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LEQ_F32, 54, "leq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GRTR_F32, 55, "grtr.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GEQ_F32, 56, "geq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_F32, 57, "cvt.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MOD_F32, 58, "mod.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_I64, 59, "cvt.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TRUNCATE_I64, 60, "truncate.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TEST_I64, 61, "test.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(INVERT_I64, 62, "invert.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_I64, 63, "neg.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMUL_I64, 64, "smul.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SDIV_I64, 65, "sdiv.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_I64, 66, "add.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_I64, 67, "sub.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHL_I64, 68, "shl.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SSHR_I64, 69, "sshr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHR_I64, 70, "shr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_I64, 71, "eq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_I64, 72, "neq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(OR_I64, 73, "or.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(AND_I64, 74, "and.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(XOR_I64, 75, "xor.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLESS_I64, 76, "sless.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLEQ_I64, 77, "sleq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGRTR_I64, 78, "sgrtr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGEQ_I64, 79, "sgeq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMOD_I64, 80, "smod.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FN, 81, "load.fn", {LL_FMT_METHOD_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_I64, 82, "load.i.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_I64, 83, "stor.i.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(RETV, 84, "retv", {}) \
    FOR_EACH(ADDR_GLB, 85, "addr.glb", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_STR, 86, "load.str", {LL_FMT_STR_ID, LL_FMT_REG}) \
    FOR_EACH(NEWARRAY, 87, "newarray", {LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEWBULKARRAY, 88, "newbulkarray", {LL_FMT_U8, LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(FILLARRAY, 89, "fillarray", {LL_FMT_U32, LL_FMT_REG}) \
    FOR_EACH(ARRAY_TO_NATIVE, 90, "array2native", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLICE, 91, "slice", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_X32, 92, "load.fld.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADDR_FLD, 93, "addr.fld", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I32, 94, "load.elem.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_F32, 95, "load.elem.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I64, 96, "load.elem.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_U8, 97, "load.elem.i8", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I32, 98, "stor.elem.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_F32, 99, "stor.elem.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I64, 100, "stor.elem.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_U8, 101, "stor.elem.i8", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_F32, 102, "load.i.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_F32, 103, "stor.i.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_A, 104, "load.elem.a", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(IDXADDR_FLAT, 105, "idxaddr.flat", {LL_FMT_U32, LL_FMT_U16, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ARRAY_TO_FLAT, 106, "array2flat", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEWFIXEDARRAY, 107, "newfixedarray", {LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY_FLAT, 108, "copyarray.flat", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(FILLARRAY_FLAT, 109, "fillarray.flat", {LL_FMT_U32, LL_FMT_REG}) \
    FOR_EACH(SLICE_FLAT, 110, "slice.flat", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I32, 111, "stor.elem.flat.i32", {LL_FMT_U32, LL_FMT_U16, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_F32, 112, "stor.elem.flat.f32", {LL_FMT_U32, LL_FMT_U16, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I64, 113, "stor.elem.flat.i64", {LL_FMT_U32, LL_FMT_U16, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_U8, 114, "stor.elem.flat.i8", {LL_FMT_U32, LL_FMT_U16, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_X64, 115, "load.fld.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_X32, 116, "stor.fld.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_X64, 117, "stor.fld.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYOBJ, 118, "copyobj", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLICE_ES, 119, "slice.es", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MOVE, 120, "move", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_CONST, 121, "load.const", {LL_FMT_CELL, LL_FMT_REG}) \
    FOR_EACH(TEST_I32, 122, "test.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_CONST_I64, 123, "load.const.i64", {LL_FMT_I64, LL_FMT_REG}) \
    FOR_EACH(MOVE_I64, 124, "move.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_GLB_X64, 125, "load.glb.x64", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_X64, 126, "stor.glb.x64", {LL_FMT_GLB_ID, LL_FMT_REG})

enum LLOp : uint16_t {
#define FOR_EACH_OPCODE(op, val, text, ...) LL_##op = val,
    LL_OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    LLOp_Max
};

inline const char* GetLLOpName(LLOp op) {
    if (op >= LLOp_Max)
        return "unknown";
    switch (op) {
#define FOR_EACH_OPCODE(op, val, text, ...) case LL_##op: return text;
        LL_OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
        default: return "unknown";
    }
}

} // namespace sp::v2
