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
    FOR_EACH(LOAD_S_PRI, 3, "load.s.pri", 3) \
    FOR_EACH(LOAD_S_ALT, 4, "load.s.alt", 3) \
    FOR_EACH(LREF_S_PRI, 5, "lref.s.pri", 3) \
    FOR_EACH(LOAD_I, 7, "load.i", 1) \
    FOR_EACH(LODB_I, 8, "lodb.i", 5) \
    FOR_EACH(CONST_PRI, 9, "const.pri", 5) \
    FOR_EACH(CONST_ALT, 10, "const.alt", 5) \
    FOR_EACH(ADDR_PRI, 11, "addr.pri", 3) \
    FOR_EACH(ADDR_ALT, 12, "addr.alt", 3) \
    FOR_EACH(STOR_PRI, 13, "stor.pri", 5) \
    FOR_EACH(STOR_S_PRI, 15, "stor.s.pri", 3) \
    FOR_EACH(STOR_S_ALT, 16, "stor.s.alt", 3) \
    FOR_EACH(STOR_S_C, 17, "stor.s.c", 7) \
    FOR_EACH(SREF_S_PRI, 18, "sref.s.pri", 3) \
    FOR_EACH(STOR_I, 20, "stor.i", 1) \
    FOR_EACH(STRB_I, 21, "strb.i", 5) \
    FOR_EACH(IDXADDR, 22, "idxaddr", 1) \
    FOR_EACH(MOVE_PRI, 23, "move.pri", 1) \
    FOR_EACH(MOVE_ALT, 24, "move.alt", 1) \
    FOR_EACH(XCHG, 25, "xchg", 1) \
    FOR_EACH(PUSH_PRI, 26, "push.pri", 1) \
    FOR_EACH(PUSH_ALT, 27, "push.alt", 1) \
    FOR_EACH(PUSH_C, 28, "push.c", 5) \
    FOR_EACH(PUSH_S, 29, "push.s", 3) \
    FOR_EACH(POP_PRI, 30, "pop.pri", 1) \
    FOR_EACH(POP_ALT, 31, "pop.alt", 1) \
    FOR_EACH(HEAP, 32, "heap", 5) \
    FOR_EACH(PROC, 33, "proc", 1) \
    FOR_EACH(RETN, 34, "retn", 1) \
    FOR_EACH(CALL, 35, "call", 5) \
    FOR_EACH(JUMP, 36, "jump", 5) \
    FOR_EACH(JZER, 37, "jzer", 5) \
    FOR_EACH(JNZ, 38, "jnz", 5) \
    FOR_EACH(JEQ, 39, "jeq", 5) \
    FOR_EACH(JNEQ, 40, "jneq", 5) \
    FOR_EACH(JSLESS, 41, "jsless", 5) \
    FOR_EACH(JSLEQ, 42, "jsleq", 5) \
    FOR_EACH(JSGRTR, 43, "jsgrtr", 5) \
    FOR_EACH(JSGEQ, 44, "jsgeq", 5) \
    FOR_EACH(SHL, 45, "shl", 1) \
    FOR_EACH(SHR, 46, "shr", 1) \
    FOR_EACH(SSHR, 47, "sshr", 1) \
    FOR_EACH(SMUL, 50, "smul", 1) \
    FOR_EACH(SDIV_ALT_I32, 53, "sdiv.i32", 1) \
    FOR_EACH(SMOD_ALT_I32, 54, "smod.i32", 1) \
    FOR_EACH(ADD, 55, "add", 1) \
    FOR_EACH(SUB_ALT, 56, "sub.alt", 1) \
    FOR_EACH(AND, 57, "and", 1) \
    FOR_EACH(OR, 58, "or", 1) \
    FOR_EACH(XOR, 59, "xor", 1) \
    FOR_EACH(NOT, 60, "not", 1) \
    FOR_EACH(NEG, 61, "neg", 1) \
    FOR_EACH(INVERT, 62, "invert", 1) \
    FOR_EACH(ADD_C, 63, "add.c", 5) \
    FOR_EACH(SMUL_C, 64, "smul.c", 5) \
    FOR_EACH(ZERO_PRI, 65, "zero.pri", 1) \
    FOR_EACH(ZERO_ALT, 66, "zero.alt", 1) \
    FOR_EACH(ZERO_S, 67, "zero.s", 3) \
    FOR_EACH(ZERO_S_I64, 68, "zero.s.i64", 3) \
    FOR_EACH(EQ, 69, "eq", 1) \
    FOR_EACH(NEQ, 70, "neq", 1) \
    FOR_EACH(SLESS, 71, "sless", 1) \
    FOR_EACH(SLEQ, 72, "sleq", 1) \
    FOR_EACH(SGRTR, 73, "sgrtr", 1) \
    FOR_EACH(SGEQ, 74, "sgeq", 1) \
    FOR_EACH(INC_PRI, 77, "inc.pri", 1) \
    FOR_EACH(DEC_PRI, 79, "dec.pri", 1) \
    FOR_EACH(MOVS, 81, "movs", 5) \
    FOR_EACH(FILL, 82, "fill", 5) \
    FOR_EACH(BOUNDS, 83, "bounds", 5) \
    FOR_EACH(SWITCH, 84, "switch", 5) \
    FOR_EACH(CASETBL, 85, "casetbl", -1) \
    FOR_EACH(SWAP_PRI, 86, "swap.pri", 1) \
    FOR_EACH(SWAP_ALT, 87, "swap.alt", 1) \
    FOR_EACH(PUSH_ADR, 88, "push.adr", 3) \
    FOR_EACH(NOP, 89, "nop", 1) \
    FOR_EACH(SYSREQ_N, 90, "sysreq.n", 9) \
    FOR_EACH(BREAK, 91, "break", 1) \
    FOR_EACH(GENARRAY, 92, "genarray", 5) \
    FOR_EACH(GENARRAY_Z, 93, "genarray.z", 5) \
    FOR_EACH(STRADJUST_PRI, 94, "stradjust.pri", 1) \
    FOR_EACH(ENDPROC, 95, "endproc", 1) \
    FOR_EACH(INITARRAY_ALT, 97, "initarray.alt", 21) \
    FOR_EACH(HEAP_SAVE, 98, "heap.save", 1) \
    FOR_EACH(HEAP_RESTORE, 99, "heap.restore", 1) \
    FOR_EACH(TEST_F32, 100, "test.f32", 1) \
    FOR_EACH(NEG_F32, 101, "neg.f32", 1) \
    FOR_EACH(MUL_F32, 102, "mul.f32", 1) \
    FOR_EACH(DIV_ALT_F32, 103, "div.alt.f32", 1) \
    FOR_EACH(ADD_F32, 104, "add.f32", 1) \
    FOR_EACH(SUB_ALT_F32, 105, "sub_alt.f32", 1) \
    FOR_EACH(EQ_F32, 106, "eq.f32", 1) \
    FOR_EACH(NEQ_F32, 107, "neq.f32", 1) \
    FOR_EACH(LESS_F32, 108, "less.f32", 1) \
    FOR_EACH(LEQ_F32, 109, "leq.f32", 1) \
    FOR_EACH(GRTR_F32, 110, "grtr.f32", 1) \
    FOR_EACH(GEQ_F32, 111, "geq.f32", 1) \
    FOR_EACH(CVT_F32, 112, "cvt.f32", 1) \
    FOR_EACH(MOD_ALT_F32, 113, "mod.alt.f32", 1) \
    FOR_EACH(MOVE_I64, 114, "move.i64", 1) \
    FOR_EACH(CVT_I64, 116, "cvt.i64", 3) \
    FOR_EACH(TRUNCATE_I64, 117, "truncate.i64", 1) \
    FOR_EACH(TEST_I64, 118, "test.i64", 1) \
    FOR_EACH(INVERT_I64, 119, "invert.i64", 3) \
    FOR_EACH(NEG_I64, 120, "neg.i64", 3) \
    FOR_EACH(SMUL_I64, 121, "smul.i64", 3) \
    FOR_EACH(SDIV_ALT_I64, 122, "sdiv.alt.i64", 3) \
    FOR_EACH(ADD_I64, 123, "add.i64", 3) \
    FOR_EACH(SUB_ALT_I64, 124, "sub_alt.i64", 3) \
    FOR_EACH(SHL_I64, 125, "shl.i64", 3) \
    FOR_EACH(SSHR_I64, 126, "sshr.i64", 3) \
    FOR_EACH(SHR_I64, 127, "shr.i64", 3) \
    FOR_EACH(EQ_I64, 128, "eq.i64", 1) \
    FOR_EACH(NEQ_I64, 129, "neq.i64", 1) \
    FOR_EACH(OR_I64, 130, "or.i64", 3) \
    FOR_EACH(AND_I64, 131, "and.i64", 3) \
    FOR_EACH(XOR_I64, 132, "neq.i64", 3) \
    FOR_EACH(STOR_S_C_I64, 133, "stor.s.c.i64", 11) \
    FOR_EACH(SLESS_I64, 134, "sless.i64", 1) \
    FOR_EACH(SLEQ_I64, 135, "sleq.i64", 1) \
    FOR_EACH(SGRTR_I64, 136, "sgrtr.i64", 1) \
    FOR_EACH(SGEQ_I64, 137, "sgeq.i64", 1) \
    FOR_EACH(SMOD_ALT_I64, 138, "smod.alt.i64", 3) \
    FOR_EACH(STOR_S_PRI_I64, 139, "stor.s.pri.i64", 3) \

enum OPCODE {
#define FOR_EACH_OPCODE(op, val, text, cells) OP_##op = val,
    OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    OPCODES_LAST
};

} // namespace v2
} // namespace sp
