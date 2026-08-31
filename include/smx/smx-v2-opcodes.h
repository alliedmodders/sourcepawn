// vim: set sts=2 ts=8 sw=2 tw=99 et:
// =============================================================================
// SourcePawn
// Copyright (C) 2004-2014 AlliedModders LLC.  All rights reserved.
// =============================================================================
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License, version 3.0, as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program.  If not, see <http://www.gnu.org/licenses/>.
//
// As a special exception, AlliedModders LLC gives you permission to link the
// code of this program (as well as its derivative works) to "Half-Life 2," the
// "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
// by the Valve Corporation.  You must obey the GNU General Public License in
// all respects for all other code used.  Additionally, AlliedModders LLC grants
// this exception to all derivative works.  AlliedModders LLC defines further
// exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
// or <http://www.sourcemod.net/license.php>.
#pragma once

#include <stddef.h>
#include <stdint.h>

namespace sp {
namespace v2 {

#define OPCODE_LIST_V2(FOR_EACH) \
    FOR_EACH(NOP, 0, "nop", 1) \
    FOR_EACH(LOAD_GLB, 1, "load.glb", 5) \
    FOR_EACH(LOAD_S, 2, "load.s", 3) \
    /* Stack transition: A B -> B A B */ \
    FOR_EACH(DUP_ROTATE, 3, "dup.rotate", 1) \
    FOR_EACH(LREF_S, 4, "lref.s", 3) \
    FOR_EACH(LOAD_I, 5, "load.i", 1) \
    FOR_EACH(LODB_I, 6, "lodb.i", 1) \
    FOR_EACH(POP, 7, "pop", 1) \
    FOR_EACH(STOR_GLB, 8, "stor.glb", 5) \
    FOR_EACH(STOR_S, 9, "stor.s", 3) \
    FOR_EACH(DUP, 10, "dup", 1) \
    FOR_EACH(STOR_S_C, 11, "stor.s.c", 7) \
    FOR_EACH(SREF_S, 12, "sref.s", 3) \
    FOR_EACH(STOR_I, 13, "stor.i", 1) \
    FOR_EACH(STRB_I, 14, "strb.i", 1) \
    /* rank_size(uint8_t), bounds(uint32_t) */ \
    FOR_EACH(IDXADDR, 15, "idxaddr", 6) \
    FOR_EACH(PUSH_C, 16, "push.c", 5) \
    FOR_EACH(HEAP, 17, "heap", 5) \
    FOR_EACH(RETN, 18, "retn", 1) \
    FOR_EACH(CALL, 19, "call", 5) \
    FOR_EACH(JUMP, 20, "jump", 5) \
    FOR_EACH(JZER, 21, "jzer", 5) \
    FOR_EACH(JNZ, 22, "jnz", 5) \
    FOR_EACH(JEQ, 23, "jeq", 5) \
    FOR_EACH(JNEQ, 24, "jneq", 5) \
    FOR_EACH(JSLESS, 25, "jsless", 5) \
    FOR_EACH(JSLEQ, 26, "jsleq", 5) \
    FOR_EACH(JSGRTR, 27, "jsgrtr", 5) \
    FOR_EACH(JSGEQ, 28, "jsgeq", 5) \
    FOR_EACH(SHL, 29, "shl", 1) \
    FOR_EACH(SHR, 30, "shr", 1) \
    FOR_EACH(SSHR, 31, "sshr", 1) \
    FOR_EACH(SMUL, 32, "smul", 1) \
    FOR_EACH(SDIV_I32, 33, "sdiv.i32", 1) \
    FOR_EACH(SMOD_I32, 34, "smod.i32", 1) \
    FOR_EACH(ADD, 35, "add", 1) \
    FOR_EACH(SUB, 36, "sub", 1) \
    FOR_EACH(AND, 37, "and", 1) \
    FOR_EACH(OR, 38, "or", 1) \
    FOR_EACH(XOR, 39, "xor", 1) \
    FOR_EACH(NOT, 40, "not", 1) \
    FOR_EACH(NEG, 41, "neg", 1) \
    FOR_EACH(INVERT, 42, "invert", 1) \
    FOR_EACH(ADD_C, 43, "add.c", 5) \
    FOR_EACH(SMUL_C, 44, "smul.c", 5) \
    FOR_EACH(ZERO_S, 45, "zero.s", 3) \
    FOR_EACH(ZERO_S_I64, 46, "zero.s.i64", 3) \
    FOR_EACH(EQ, 47, "eq", 1) \
    FOR_EACH(NEQ, 48, "neq", 1) \
    FOR_EACH(SLESS, 49, "sless", 1) \
    FOR_EACH(SLEQ, 50, "sleq", 1) \
    FOR_EACH(SGRTR, 51, "sgrtr", 1) \
    FOR_EACH(SGEQ, 52, "sgeq", 1) \
    FOR_EACH(INC, 53, "inc", 1) \
    FOR_EACH(DEC, 54, "dec", 1) \
    FOR_EACH(MOVS, 55, "movs", 5) \
    FOR_EACH(FILL, 56, "fill", 5) \
    FOR_EACH(SWITCH, 57, "switch", 5) \
    FOR_EACH(CASETBL, 58, "casetbl", -1) \
    FOR_EACH(ADDR_S, 59, "addr.s", 3) \
    FOR_EACH(GENARRAY, 60, "genarray", 5) \
    FOR_EACH(GENARRAY_Z, 61, "genarray.z", 5) \
    FOR_EACH(STRADJUST, 62, "stradjust", 1) \
    FOR_EACH(INITARRAY, 63, "initarray", 21) \
    FOR_EACH(HEAP_SAVE, 64, "heap.save", 1) \
    FOR_EACH(HEAP_RESTORE, 65, "heap.restore", 1) \
    FOR_EACH(TEST_F32, 66, "test.f32", 1) \
    FOR_EACH(NEG_F32, 67, "neg.f32", 1) \
    FOR_EACH(MUL_F32, 68, "mul.f32", 1) \
    FOR_EACH(DIV_F32, 69, "div.f32", 1) \
    FOR_EACH(ADD_F32, 70, "add.f32", 1) \
    FOR_EACH(SUB_F32, 71, "sub.f32", 1) \
    FOR_EACH(EQ_F32, 72, "eq.f32", 1) \
    FOR_EACH(NEQ_F32, 73, "neq.f32", 1) \
    FOR_EACH(LESS_F32, 74, "less.f32", 1) \
    FOR_EACH(LEQ_F32, 75, "leq.f32", 1) \
    FOR_EACH(GRTR_F32, 76, "grtr.f32", 1) \
    FOR_EACH(GEQ_F32, 77, "geq.f32", 1) \
    FOR_EACH(CVT_F32, 78, "cvt.f32", 1) \
    FOR_EACH(MOD_F32, 79, "mod.f32", 1) \
    FOR_EACH(CVT_I64, 80, "cvt.i64", 1) \
    FOR_EACH(TRUNCATE_I64, 81, "truncate.i64", 1) \
    FOR_EACH(TEST_I64, 82, "test.i64", 1) \
    FOR_EACH(INVERT_I64, 83, "invert.i64", 1) \
    FOR_EACH(NEG_I64, 84, "neg.i64", 1) \
    FOR_EACH(SMUL_I64, 85, "smul.i64", 1) \
    FOR_EACH(SDIV_I64, 86, "sdiv.i64", 1) \
    FOR_EACH(ADD_I64, 87, "add.i64", 1) \
    FOR_EACH(SUB_I64, 88, "sub.i64", 1) \
    FOR_EACH(SHL_I64, 89, "shl.i64", 1) \
    FOR_EACH(SSHR_I64, 90, "sshr.i64", 1) \
    FOR_EACH(SHR_I64, 91, "shr.i64", 1) \
    FOR_EACH(EQ_I64, 92, "eq.i64", 1) \
    FOR_EACH(NEQ_I64, 93, "neq.i64", 1) \
    FOR_EACH(OR_I64, 94, "or.i64", 1) \
    FOR_EACH(AND_I64, 95, "and.i64", 1) \
    FOR_EACH(XOR_I64, 96, "xor.i64", 1) \
    FOR_EACH(STOR_S_C_I64, 97, "stor.s.c.i64", 11) \
    FOR_EACH(SLESS_I64, 98, "sless.i64", 1) \
    FOR_EACH(SLEQ_I64, 99, "sleq.i64", 1) \
    FOR_EACH(SGRTR_I64, 100, "sgrtr.i64", 1) \
    FOR_EACH(SGEQ_I64, 101, "sgeq.i64", 1) \
    FOR_EACH(SMOD_I64, 102, "smod.i64", 1) \
    FOR_EACH(STOR_S_I64, 103, "stor.s.i64", 3) \
    FOR_EACH(SWAP, 104, "swap", 1) \
    FOR_EACH(LOAD_FN, 105, "load.fn", 5) \
    FOR_EACH(LOAD_S_I64, 106, "load.s.i64", 3) \
    FOR_EACH(STOR_GLB_I64, 107, "stor.glb.i64", 5) \
    FOR_EACH(LOAD_GLB_I64, 108, "load.glb.i64", 5) \
    FOR_EACH(LOAD_I_I64, 109, "load.i.i64", 1) \
    FOR_EACH(STOR_I_I64, 110, "stor.i.i64", 1) \
    FOR_EACH(RETV, 111, "retv", 1) \
    FOR_EACH(PUSH_C_I8, 112, "push.c.i8", 2) \
    FOR_EACH(CALLN, 113, "calln", 6) \


enum OPCODE {
#define FOR_EACH_OPCODE(op, val, text, cells) OP_##op = val,
    OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    OPCODES_LAST
};

} // namespace v2
} // namespace sp
