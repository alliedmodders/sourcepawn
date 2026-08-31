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

#pragma pack(push, 1)
struct IdxAddrFlatArgs {
    uint32_t size;
    uint16_t elt_size;
    uint16_t base_reg;
    uint16_t index_reg;
    uint16_t dest_reg;
};

struct LoadElemFlatArgs {
    uint32_t array_size;
    uint16_t base_reg;
    uint16_t index_reg;
    uint16_t dest_reg;
};

struct StorElemFlatArgs {
    uint32_t array_size;
    uint16_t base_reg;
    uint16_t index_reg;
    uint16_t val_reg;
};

struct SliceFlatArgs {
    const TypeDesc* td;
    uint16_t base_reg;
    uint16_t index_reg;
    uint16_t dest_reg;
};

struct IdxAddrArgs {
    uint16_t base_reg;
    uint16_t index_reg;
    uint16_t elt_size;
    uint16_t dest_reg;
};

struct UpvarArgs {
    uint32_t slot;
    uint16_t closure_reg;
    uint16_t reg;
};

struct SwitchCaseEntry {
    cell_t value;
    uint32_t offset;
};
#pragma pack(pop)

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
    FOR_EACH(LOAD_I_I32, 2, "load.i.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_U8, 3, "load.i.u8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_X32, 4, "stor.glb.x32", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_I_I32, 5, "stor.i.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_U8, 6, "stor.i.u8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(IDXADDR, 7, "idxaddr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_U16, LL_FMT_REG}) \
    FOR_EACH(RETN, 8, "retn", {LL_FMT_REG}) \
    FOR_EACH(CALL, 9, "call", {LL_FMT_METHOD_PTR, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(JUMP, 10, "jump", {LL_FMT_TARGET}) \
    FOR_EACH(JZER, 11, "jzer", {LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JNZ, 12, "jnz", {LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JEQ, 13, "jeq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JNEQ, 14, "jneq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSLESS, 15, "jsless", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSLEQ, 16, "jsleq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSGRTR, 17, "jsgrtr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSGEQ, 18, "jsgeq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(SHL_I32, 19, "shl.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHR_I32, 20, "shr.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SSHR_I32, 21, "sshr.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMUL_I32, 22, "smul.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SDIV_I32, 23, "sdiv.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMOD_I32, 24, "smod.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_I32, 25, "add.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_I32, 26, "sub.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(AND_I32, 27, "and.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(OR_I32, 28, "or.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(XOR_I32, 29, "xor.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NOT_I32, 30, "not.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_I32, 31, "neg.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(INVERT_I32, 32, "invert.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_I32, 33, "eq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_I32, 34, "neq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLESS_I32, 35, "sless.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLEQ_I32, 36, "sleq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGRTR_I32, 37, "sgrtr.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGEQ_I32, 38, "sgeq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY, 39, "copyarray", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY_FLAT_A, 153, "copyarray.flat.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY_A, 154, "copyarray.a", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SWITCH, 40, "switch", {LL_FMT_REG, LL_FMT_SWITCH}) \
    FOR_EACH(ADDR_S, 41, "addr.s", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TEST_F32, 42, "test.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_F32, 43, "neg.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MUL_F32, 44, "mul.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(DIV_F32, 45, "div.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_F32, 46, "add.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_F32, 47, "sub.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_F32, 48, "eq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_F32, 49, "neq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LESS_F32, 50, "less.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LEQ_F32, 51, "leq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GRTR_F32, 52, "grtr.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GEQ_F32, 53, "geq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_F32, 54, "cvt.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MOD_F32, 55, "mod.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_I64, 56, "cvt.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TRUNCATE_I64, 57, "truncate.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TEST_I64, 58, "test.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(INVERT_I64, 59, "invert.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_I64, 60, "neg.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMUL_I64, 61, "smul.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SDIV_I64, 62, "sdiv.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_I64, 63, "add.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_I64, 64, "sub.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHL_I64, 65, "shl.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SSHR_I64, 66, "sshr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHR_I64, 67, "shr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_I64, 68, "eq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_I64, 69, "neq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(OR_I64, 70, "or.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(AND_I64, 71, "and.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(XOR_I64, 72, "xor.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLESS_I64, 73, "sless.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLEQ_I64, 74, "sleq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGRTR_I64, 75, "sgrtr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGEQ_I64, 76, "sgeq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMOD_I64, 77, "smod.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FN, 78, "load.fn", {LL_FMT_METHOD_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_I64, 79, "load.i.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_I64, 80, "stor.i.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(RETV, 81, "retv", {}) \
    FOR_EACH(ADDR_GLB, 82, "addr.glb", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_STR, 83, "load.str", {LL_FMT_STR_ID, LL_FMT_REG}) \
    FOR_EACH(NEWARRAY, 84, "newarray", {LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEWBULKARRAY, 85, "newbulkarray", {LL_FMT_U8, LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(FILLARRAY, 86, "fillarray", {LL_FMT_U32, LL_FMT_REG}) \
    FOR_EACH(SLICE, 88, "slice", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_X32, 89, "load.fld.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADDR_FLD, 90, "addr.fld", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I32, 91, "load.elem.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_F32, 92, "load.elem.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I64, 93, "load.elem.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_U8, 94, "load.elem.i8", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I32, 95, "stor.elem.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_F32, 96, "stor.elem.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I64, 97, "stor.elem.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_U8, 98, "stor.elem.i8", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_F32, 99, "load.i.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_F32, 100, "stor.i.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_A, 101, "load.elem.a", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(IDXADDR_FLAT, 102, "idxaddr.flat", {LL_FMT_U32, LL_FMT_U16, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ARRAY_TO_FLAT, 103, "array2flat", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEWFIXEDARRAY, 104, "newfixedarray", {LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY_FLAT, 105, "copyarray.flat", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(FILLARRAY_FLAT, 106, "fillarray.flat", {LL_FMT_U32, LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(SLICE_FLAT, 107, "slice.flat", {LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I32, 108, "stor.elem.flat.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_F32, 109, "stor.elem.flat.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I64, 110, "stor.elem.flat.i64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_U8, 111, "stor.elem.flat.i8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_X64, 112, "load.fld.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_X32, 113, "stor.fld.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_X64, 114, "stor.fld.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYOBJ, 115, "copyobj", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLICE_ES, 116, "slice.es", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MOVE, 117, "move", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_CONST, 118, "load.const", {LL_FMT_CELL, LL_FMT_REG}) \
    FOR_EACH(TEST_I32, 119, "test.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_CONST_I64, 120, "load.const.i64", {LL_FMT_I64, LL_FMT_REG}) \
    FOR_EACH(MOVE_I64, 121, "move.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_GLB_X64, 122, "load.glb.x64", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_X64, 123, "stor.glb.x64", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(RELEASE, 124, "release", {LL_FMT_REG}) \
    FOR_EACH(ADDREF, 125, "addref", {LL_FMT_REG}) \
    FOR_EACH(STOR_I_A, 126, "stor.i.a", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_A, 127, "stor.elem.a", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_GLB_A, 128, "load.glb.a", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_A, 129, "load.fld.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_A, 130, "stor.glb.a", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_A, 131, "stor.fld.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_S_A, 133, "stor.s.a", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(RETN_A, 134, "retn.a", {LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I32, 135, "load.elem.flat.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_F32, 136, "load.elem.flat.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I64, 137, "load.elem.flat.i64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_U8, 138, "load.elem.flat.i8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NTVCALL_VA, 139, "ntvcall.va", {LL_FMT_U32, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(NTVCALL, 140, "ntvcall", {LL_FMT_U32, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_I32, 141, "load.elem.flat.i.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_F32, 142, "load.elem.flat.i.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_I64, 143, "load.elem.flat.i.i64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_U8, 144, "load.elem.flat.i.u8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_I32, 145, "stor.elem.flat.i.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_F32, 146, "stor.elem.flat.i.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_I64, 147, "stor.elem.flat.i.i64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_U8, 148, "stor.elem.flat.i.u8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GETFNOBJ, 149, "getfnobj", {LL_FMT_REG, LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(GETFUNCID, 150, "getfuncid", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CALLI, 151, "calli", {LL_FMT_REG, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(NEWOBJ, 152, "newobj", {LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(NEWCLOSURE, 155, "newclosure", {LL_FMT_TYPEDESC, LL_FMT_METHOD_ID, LL_FMT_REG}) \
    FOR_EACH(CALLEE, 156, "callee", {LL_FMT_REG}) \
    FOR_EACH(LOAD_UPVAR_X32, 157, "load.upvar.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_UPVAR_X64, 158, "load.upvar.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_UPVAR_A, 159, "load.upvar.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADDR_UPVAR, 160, "addr.upvar", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_UPVAR_X32, 161, "stor.upvar.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_UPVAR_X64, 162, "stor.upvar.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_UPVAR_A, 163, "stor.upvar.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_I16, 164, "load.i.i16", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_I16, 165, "stor.i.i16", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I16, 166, "load.elem.i16", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I16, 167, "stor.elem.i16", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I16, 168, "stor.elem.flat.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I16, 169, "load.elem.flat.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_I16, 170, "load.elem.flat.i.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_I16, 171, "stor.elem.flat.i.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_I16, 172, "cvt.i16", {LL_FMT_REG, LL_FMT_REG})

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
