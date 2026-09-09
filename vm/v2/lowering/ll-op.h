// vim: set ts=8 sw=4 tw=99 sts=4 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
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
    // Opcode has no operands.
    LL_FMT_NONE,

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
    FOR_EACH(NOP, "nop", {LL_FMT_NONE}) \
    FOR_EACH(LOAD_GLB_X32, "load.glb.x32", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_X32, "load.i.x32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_U8, "load.i.u8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_X32, "stor.glb.x32", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_I_X32, "stor.i.x32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_I8, "stor.i.i8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(IDXADDR, "idxaddr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_U16, LL_FMT_REG}) \
    FOR_EACH(RETN, "retn", {LL_FMT_REG}) \
    FOR_EACH(CALL, "call", {LL_FMT_METHOD_PTR, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(JUMP, "jump", {LL_FMT_TARGET}) \
    FOR_EACH(JZER, "jzer", {LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JNZ, "jnz", {LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JEQ, "jeq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JNEQ, "jneq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSLESS, "jsless", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSLEQ, "jsleq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSGRTR, "jsgrtr", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(JSGEQ, "jsgeq", {LL_FMT_REG, LL_FMT_REG, LL_FMT_TARGET}) \
    FOR_EACH(SHL_I32, "shl.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHL_I64, "shl.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHR_I32, "shr.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SHR_I64, "shr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SSHR_I32, "sshr.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SSHR_I64, "sshr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(AND_I32, "and.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(AND_I64, "and.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(OR_I32, "or.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(OR_I64, "or.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(XOR_I32, "xor.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(XOR_I64, "xor.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(INVERT_I32, "invert.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(INVERT_I64, "invert.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NOT_I32, "not.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_I32, "add.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_I32, "sub.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMUL_I32, "smul.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SDIV_I32, "sdiv.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_I32, "eq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_I32, "neq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLESS_I32, "sless.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLEQ_I32, "sleq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGRTR_I32, "sgrtr.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGEQ_I32, "sgeq.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMOD_I32, "smod.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_F32, "add.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_F32, "sub.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MUL_F32, "mul.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(DIV_F32, "div.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_F32, "eq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_F32, "neq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LESS_F32, "less.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LEQ_F32, "leq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GRTR_F32, "grtr.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GEQ_F32, "geq.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MOD_F32, "mod.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_I64, "add.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_I64, "sub.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMUL_I64, "smul.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SDIV_I64, "sdiv.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_I64, "eq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_I64, "neq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLESS_I64, "sless.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLEQ_I64, "sleq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGRTR_I64, "sgrtr.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SGEQ_I64, "sgeq.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SMOD_I64, "smod.i64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADD_F64, "add.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SUB_F64, "sub.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MUL_F64, "mul.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(DIV_F64, "div.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(EQ_F64, "eq.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEQ_F64, "neq.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LESS_F64, "less.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LEQ_F64, "leq.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GRTR_F64, "grtr.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GEQ_F64, "geq.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MOD_F64, "mod.f64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_I32, "neg.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TEST_I32, "test.i32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_F32, "neg.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TEST_F32, "test.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_I64, "neg.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TEST_I64, "test.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEG_F64, "neg.f64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TEST_F64, "test.f64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FN, "load.fn", {LL_FMT_METHOD_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_X64, "load.i.x64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_X64, "stor.i.x64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(RETV, "retv", {LL_FMT_NONE}) \
    FOR_EACH(ADDR_GLB, "addr.glb", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_STR, "load.str", {LL_FMT_STR_ID, LL_FMT_REG}) \
    FOR_EACH(NEWARRAY, "newarray", {LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEWBULKARRAY, "newbulkarray", {LL_FMT_U8, LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(FILLARRAY, "fillarray", {LL_FMT_U32, LL_FMT_U32, LL_FMT_REG}) \
    FOR_EACH(SLICE, "slice", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_X32, "load.fld.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADDR_FLD, "addr.fld", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I32, "load.elem.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_F32, "load.elem.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_X64, "load.elem.x64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_U8, "load.elem.u8", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I32, "stor.elem.i32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_F32, "stor.elem.f32", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_X64, "stor.elem.x64", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I8, "stor.elem.i8", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_A, "load.elem.a", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(IDXADDR_FLAT, "idxaddr.flat", {LL_FMT_U32, LL_FMT_U16, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ARRAY_TO_FLAT, "array2flat", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEWFIXEDARRAY, "newfixedarray", {LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY_FLAT, "copyarray.flat", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(FILLARRAY_FLAT, "fillarray.flat", {LL_FMT_U32, LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(SLICE_FLAT, "slice.flat", {LL_FMT_TYPEDESC, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I32, "stor.elem.flat.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_F32, "stor.elem.flat.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_X64, "stor.elem.flat.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I8, "stor.elem.flat.i8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_X64, "load.fld.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_X32, "stor.fld.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_X64, "stor.fld.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYOBJ, "copyobj", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SLICE_ES, "slice.es", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(MOVE, "move", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_CONST, "load.const", {LL_FMT_CELL, LL_FMT_REG}) \
    FOR_EACH(LOAD_CONST64, "load.const64", {LL_FMT_I64, LL_FMT_REG}) \
    FOR_EACH(MOVE64, "move64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_GLB_X64, "load.glb.x64", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_X64, "stor.glb.x64", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(RELEASE, "release", {LL_FMT_REG}) \
    FOR_EACH(ADDREF, "addref", {LL_FMT_REG}) \
    FOR_EACH(STOR_I_A, "stor.i.a", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_A, "stor.elem.a", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_GLB_A, "load.glb.a", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(LOAD_FLD_A, "load.fld.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_GLB_A, "stor.glb.a", {LL_FMT_GLB_ID, LL_FMT_REG}) \
    FOR_EACH(STOR_FLD_A, "stor.fld.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_S_A, "stor.s.a", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(RETN_A, "retn.a", {LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I32, "load.elem.flat.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_F32, "load.elem.flat.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_X64, "load.elem.flat.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_U8, "load.elem.flat.u8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NTVCALL_VA, "ntvcall.va", {LL_FMT_U32, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(NTVCALL, "ntvcall", {LL_FMT_U32, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_I32, "load.elem.flat.i.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_F32, "load.elem.flat.i.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_X64, "load.elem.flat.i.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_U8, "load.elem.flat.i.u8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_I32, "stor.elem.flat.i.i32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_F32, "stor.elem.flat.i.f32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_X64, "stor.elem.flat.i.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_I8, "stor.elem.flat.i.i8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(GETFNOBJ, "getfnobj", {LL_FMT_REG, LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(GETFUNCID, "getfuncid", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CALLI, "calli", {LL_FMT_REG, LL_FMT_U8, LL_FMT_REG, LL_FMT_CALL}) \
    FOR_EACH(NEWOBJ, "newobj", {LL_FMT_TYPEDESC, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY, "copyarray", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY_FLAT_A, "copyarray.flat.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(COPYARRAY_A, "copyarray.a", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SWITCH, "switch", {LL_FMT_REG, LL_FMT_SWITCH}) \
    FOR_EACH(ADDR_S, "addr.s", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_F32, "cvt.f32", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_I64, "cvt.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(TRUNCATE_I64, "truncate.i64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(NEWCLOSURE, "newclosure", {LL_FMT_TYPEDESC, LL_FMT_METHOD_ID, LL_FMT_REG}) \
    FOR_EACH(CALLEE, "callee", {LL_FMT_REG}) \
    FOR_EACH(LOAD_UPVAR_X32, "load.upvar.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_UPVAR_X64, "load.upvar.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_UPVAR_A, "load.upvar.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(ADDR_UPVAR, "addr.upvar", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_UPVAR_X32, "stor.upvar.x32", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_UPVAR_X64, "stor.upvar.x64", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_UPVAR_A, "stor.upvar.a", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_I16, "load.i.i16", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_I_I16, "stor.i.i16", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I16, "load.elem.i16", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_I16, "stor.elem.i16", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I16, "stor.elem.flat.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I16, "load.elem.flat.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_I16, "load.elem.flat.i.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(STOR_ELEM_FLAT_I_I16, "stor.elem.flat.i.i16", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_I16, "cvt.i16", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_I_I8, "load.i.i8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_I8, "load.elem.i8", {LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I8, "load.elem.flat.i8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(LOAD_ELEM_FLAT_I_I8, "load.elem.flat.i.i8", {LL_FMT_U32, LL_FMT_REG, LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_I8, "cvt.i8", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_F64, "cvt.f64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(CVT_F32_F64, "cvt.f32.f64", {LL_FMT_REG, LL_FMT_REG}) \
    FOR_EACH(SIZEOF_ARRAY, "sizeofarray", {LL_FMT_REG, LL_FMT_REG}) \


enum LLOp : uint16_t {
#define FOR_EACH_OPCODE(op, text, ...) LL_##op,
    LL_OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    LLOp_Max
};

inline const char* GetLLOpName(LLOp op) {
    if (op >= LLOp_Max)
        return "unknown";
    switch (op) {
#define FOR_EACH_OPCODE(op, text, ...) case LL_##op: return text;
        LL_OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
        default: return "unknown";
    }
}

} // namespace sp::v2
