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
    FOR_EACH(NONE, 0, "none", 1) \
    FOR_EACH(LOAD_PRI, 1, "load.pri", 5) \
    FOR_EACH(LOAD_S_PRI, 2, "load.s.pri", 3) \
    FOR_EACH(LOAD_S_ALT, 3, "load.s.alt", 3) \
    FOR_EACH(LREF_S_PRI, 4, "lref.s.pri", 3) \
    FOR_EACH(LOAD_I, 5, "load.i", 1) \
    FOR_EACH(LODB_I, 6, "lodb.i", 5) \
    FOR_EACH(CONST_PRI, 7, "const.pri", 5) \
    FOR_EACH(CONST_ALT, 8, "const.alt", 5) \
    FOR_EACH(ADDR_PRI, 9, "addr.pri", 3) \
    FOR_EACH(ADDR_ALT, 10, "addr.alt", 3) \
    FOR_EACH(STOR_PRI, 11, "stor.pri", 5) \
    FOR_EACH(STOR_S_PRI, 12, "stor.s.pri", 3) \
    FOR_EACH(STOR_S_ALT, 13, "stor.s.alt", 3) \
    FOR_EACH(STOR_S_C, 14, "stor.s.c", 7) \
    FOR_EACH(SREF_S_PRI, 15, "sref.s.pri", 3) \
    FOR_EACH(STOR_I, 16, "stor.i", 1) \
    FOR_EACH(STRB_I, 17, "strb.i", 5) \
    FOR_EACH(IDXADDR, 18, "idxaddr", 1) \
    FOR_EACH(MOVE_PRI, 19, "move.pri", 1) \
    FOR_EACH(MOVE_ALT, 20, "move.alt", 1) \
    FOR_EACH(XCHG, 21, "xchg", 1) \
    FOR_EACH(PUSH_PRI, 22, "push.pri", 1) \
    FOR_EACH(PUSH_ALT, 23, "push.alt", 1) \
    FOR_EACH(PUSH_C, 24, "push.c", 5) \
    FOR_EACH(PUSH_S, 25, "push.s", 3) \
    FOR_EACH(POP_PRI, 26, "pop.pri", 1) \
    FOR_EACH(POP_ALT, 27, "pop.alt", 1) \
    FOR_EACH(HEAP, 28, "heap", 5) \
    FOR_EACH(PROC, 29, "proc", 1) \
    FOR_EACH(RETN, 30, "retn", 1) \
    FOR_EACH(CALL, 31, "call", 5) \
    FOR_EACH(JUMP, 32, "jump", 5) \
    FOR_EACH(JZER, 33, "jzer", 5) \
    FOR_EACH(JNZ, 34, "jnz", 5) \
    FOR_EACH(JEQ, 35, "jeq", 5) \
    FOR_EACH(JNEQ, 36, "jneq", 5) \
    FOR_EACH(JSLESS, 37, "jsless", 5) \
    FOR_EACH(JSLEQ, 38, "jsleq", 5) \
    FOR_EACH(JSGRTR, 39, "jsgrtr", 5) \
    FOR_EACH(JSGEQ, 40, "jsgeq", 5) \
    FOR_EACH(SHL, 41, "shl", 1) \
    FOR_EACH(SHR, 42, "shr", 1) \
    FOR_EACH(SSHR, 43, "sshr", 1) \
    FOR_EACH(SMUL, 44, "smul", 1) \
    FOR_EACH(SDIV_ALT_I32, 45, "sdiv.i32", 1) \
    FOR_EACH(SMOD_ALT_I32, 46, "smod.i32", 1) \
    FOR_EACH(ADD, 47, "add", 1) \
    FOR_EACH(SUB_ALT, 48, "sub.alt", 1) \
    FOR_EACH(AND, 49, "and", 1) \
    FOR_EACH(OR, 50, "or", 1) \
    FOR_EACH(XOR, 51, "xor", 1) \
    FOR_EACH(NOT, 52, "not", 1) \
    FOR_EACH(NEG, 53, "neg", 1) \
    FOR_EACH(INVERT, 54, "invert", 1) \
    FOR_EACH(ADD_C, 55, "add.c", 5) \
    FOR_EACH(SMUL_C, 56, "smul.c", 5) \
    FOR_EACH(ZERO_PRI, 57, "zero.pri", 1) \
    FOR_EACH(ZERO_ALT, 58, "zero.alt", 1) \
    FOR_EACH(ZERO_S, 59, "zero.s", 3) \
    FOR_EACH(ZERO_S_I64, 60, "zero.s.i64", 3) \
    FOR_EACH(EQ, 61, "eq", 1) \
    FOR_EACH(NEQ, 62, "neq", 1) \
    FOR_EACH(SLESS, 63, "sless", 1) \
    FOR_EACH(SLEQ, 64, "sleq", 1) \
    FOR_EACH(SGRTR, 65, "sgrtr", 1) \
    FOR_EACH(SGEQ, 66, "sgeq", 1) \
    FOR_EACH(INC_PRI, 67, "inc.pri", 1) \
    FOR_EACH(DEC_PRI, 68, "dec.pri", 1) \
    FOR_EACH(MOVS, 69, "movs", 5) \
    FOR_EACH(FILL, 70, "fill", 5) \
    FOR_EACH(BOUNDS, 71, "bounds", 5) \
    FOR_EACH(SWITCH, 72, "switch", 5) \
    FOR_EACH(CASETBL, 73, "casetbl", -1) \
    FOR_EACH(SWAP_PRI, 74, "swap.pri", 1) \
    FOR_EACH(SWAP_ALT, 75, "swap.alt", 1) \
    FOR_EACH(PUSH_ADR, 76, "push.adr", 3) \
    FOR_EACH(NOP, 77, "nop", 1) \
    FOR_EACH(SYSREQ_N, 78, "sysreq.n", 9) \
    FOR_EACH(BREAK, 79, "break", 1) \
    FOR_EACH(GENARRAY, 80, "genarray", 5) \
    FOR_EACH(GENARRAY_Z, 81, "genarray.z", 5) \
    FOR_EACH(STRADJUST_PRI, 82, "stradjust.pri", 1) \
    FOR_EACH(ENDPROC, 83, "endproc", 1) \
    FOR_EACH(INITARRAY_ALT, 84, "initarray.alt", 21) \
    FOR_EACH(HEAP_SAVE, 85, "heap.save", 1) \
    FOR_EACH(HEAP_RESTORE, 86, "heap.restore", 1) \
    FOR_EACH(TEST_F32, 87, "test.f32", 1) \
    FOR_EACH(NEG_F32, 88, "neg.f32", 1) \
    FOR_EACH(MUL_F32, 89, "mul.f32", 1) \
    FOR_EACH(DIV_ALT_F32, 90, "div.alt.f32", 1) \
    FOR_EACH(ADD_F32, 91, "add.f32", 1) \
    FOR_EACH(SUB_ALT_F32, 92, "sub_alt.f32", 1) \
    FOR_EACH(EQ_F32, 93, "eq.f32", 1) \
    FOR_EACH(NEQ_F32, 94, "neq.f32", 1) \
    FOR_EACH(LESS_F32, 95, "less.f32", 1) \
    FOR_EACH(LEQ_F32, 96, "leq.f32", 1) \
    FOR_EACH(GRTR_F32, 97, "grtr.f32", 1) \
    FOR_EACH(GEQ_F32, 98, "geq.f32", 1) \
    FOR_EACH(CVT_F32, 99, "cvt.f32", 1) \
    FOR_EACH(MOD_ALT_F32, 100, "mod.alt.f32", 1) \
    FOR_EACH(MOVE_I64, 101, "move.i64", 1) \
    FOR_EACH(CVT_I64, 102, "cvt.i64", 3) \
    FOR_EACH(TRUNCATE_I64, 103, "truncate.i64", 1) \
    FOR_EACH(TEST_I64, 104, "test.i64", 1) \
    FOR_EACH(INVERT_I64, 105, "invert.i64", 3) \
    FOR_EACH(NEG_I64, 106, "neg.i64", 3) \
    FOR_EACH(SMUL_I64, 107, "smul.i64", 3) \
    FOR_EACH(SDIV_ALT_I64, 108, "sdiv.alt.i64", 3) \
    FOR_EACH(ADD_I64, 109, "add.i64", 3) \
    FOR_EACH(SUB_ALT_I64, 110, "sub_alt.i64", 3) \
    FOR_EACH(SHL_I64, 111, "shl.i64", 3) \
    FOR_EACH(SSHR_I64, 112, "sshr.i64", 3) \
    FOR_EACH(SHR_I64, 113, "shr.i64", 3) \
    FOR_EACH(EQ_I64, 114, "eq.i64", 1) \
    FOR_EACH(NEQ_I64, 115, "neq.i64", 1) \
    FOR_EACH(OR_I64, 116, "or.i64", 3) \
    FOR_EACH(AND_I64, 117, "and.i64", 3) \
    FOR_EACH(XOR_I64, 118, "neq.i64", 3) \
    FOR_EACH(STOR_S_C_I64, 119, "stor.s.c.i64", 11) \
    FOR_EACH(SLESS_I64, 120, "sless.i64", 1) \
    FOR_EACH(SLEQ_I64, 121, "sleq.i64", 1) \
    FOR_EACH(SGRTR_I64, 122, "sgrtr.i64", 1) \
    FOR_EACH(SGEQ_I64, 123, "sgeq.i64", 1) \
    FOR_EACH(SMOD_ALT_I64, 124, "smod.alt.i64", 3) \
    FOR_EACH(STOR_S_PRI_I64, 125, "stor.s.pri.i64", 3) \

enum OPCODE {
#define FOR_EACH_OPCODE(op, val, text, cells) OP_##op = val,
    OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    OPCODES_LAST
};

} // namespace v2
} // namespace sp
