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

#ifndef _INCLUDE_SPFILE_HEADERS_v1_opcodes_H
#define _INCLUDE_SPFILE_HEADERS_v1_opcodes_H

#include <stddef.h>
#include <stdint.h>

namespace sp {

#define OPCODE_LIST(FOR_EACH) \
    FOR_EACH(NONE, 0, "none", 1) \
    FOR_EACH(LOAD_PRI, 1, "load.pri", 2) \
    FOR_EACH(LOAD_ALT, 2, "load.alt", 2) \
    FOR_EACH(LOAD_S_PRI, 3, "load.s.pri", 2) \
    FOR_EACH(LOAD_S_ALT, 4, "load.s.alt", 2) \
    FOR_EACH(LREF_S_PRI, 7, "lref.s.pri", 2) \
    FOR_EACH(LREF_S_ALT, 8, "lref.s.alt", 2) \
    FOR_EACH(LOAD_I, 9, "load.i", 1) \
    FOR_EACH(LODB_I, 10, "lodb.i", 2) \
    FOR_EACH(CONST_PRI, 11, "const.pri", 2) \
    FOR_EACH(CONST_ALT, 12, "const.alt", 2) \
    FOR_EACH(ADDR_PRI, 13, "addr.pri", 2) \
    FOR_EACH(ADDR_ALT, 14, "addr.alt", 2) \
    FOR_EACH(STOR_PRI, 15, "stor.pri", 2) \
    FOR_EACH(STOR_ALT, 16, "stor.alt", 2) \
    FOR_EACH(STOR_S_PRI, 17, "stor.s.pri", 2) \
    FOR_EACH(STOR_S_ALT, 18, "stor.s.alt", 2) \
    FOR_EACH(SREF_S_PRI, 21, "sref.s.pri", 2) \
    FOR_EACH(SREF_S_ALT, 22, "sref.s.alt", 2) \
    FOR_EACH(STOR_I, 23, "stor.i", 1) \
    FOR_EACH(STRB_I, 24, "strb.i", 2) \
    FOR_EACH(LIDX, 25, "lidx", 1) \
    FOR_EACH(IDXADDR, 27, "idxaddr", 1) \
    FOR_EACH(MOVE_PRI, 33, "move.pri", 1) \
    FOR_EACH(MOVE_ALT, 34, "move.alt", 1) \
    FOR_EACH(XCHG, 35, "xchg", 1) \
    FOR_EACH(PUSH_PRI, 36, "push.pri", 1) \
    FOR_EACH(PUSH_ALT, 37, "push.alt", 1) \
    FOR_EACH(PUSH_C, 39, "push.c", 2) \
    FOR_EACH(PUSH, 40, "push", 2) \
    FOR_EACH(PUSH_S, 41, "push.s", 2) \
    FOR_EACH(POP_PRI, 42, "pop.pri", 1) \
    FOR_EACH(POP_ALT, 43, "pop.alt", 1) \
    FOR_EACH(STACK, 44, "stack", 2) \
    FOR_EACH(HEAP, 45, "heap", 2) \
    FOR_EACH(PROC, 46, "proc", 1) \
    FOR_EACH(RETN, 48, "retn", 1) \
    FOR_EACH(CALL, 49, "call", 2) \
    FOR_EACH(JUMP, 51, "jump", 2) \
    FOR_EACH(JZER, 53, "jzer", 2) \
    FOR_EACH(JNZ, 54, "jnz", 2) \
    FOR_EACH(JEQ, 55, "jeq", 2) \
    FOR_EACH(JNEQ, 56, "jneq", 2) \
    FOR_EACH(JSLESS, 61, "jsless", 2) \
    FOR_EACH(JSLEQ, 62, "jsleq", 2) \
    FOR_EACH(JSGRTR, 63, "jsgrtr", 2) \
    FOR_EACH(JSGEQ, 64, "jsgeq", 2) \
    FOR_EACH(SHL, 65, "shl", 1) \
    FOR_EACH(SHR, 66, "shr", 1) \
    FOR_EACH(SSHR, 67, "sshr", 1) \
    FOR_EACH(SHL_C_PRI, 68, "shl.c.pri", 2) \
    FOR_EACH(SHL_C_ALT, 69, "shl.c.alt", 2) \
    FOR_EACH(SMUL, 72, "smul", 1) \
    FOR_EACH(SDIV, 73, "sdiv", 1) \
    FOR_EACH(SDIV_ALT, 74, "sdiv.alt", 1) \
    FOR_EACH(ADD, 78, "add", 1) \
    FOR_EACH(SUB, 79, "sub", 1) \
    FOR_EACH(SUB_ALT, 80, "sub.alt", 1) \
    FOR_EACH(AND, 81, "and", 1) \
    FOR_EACH(OR, 82, "or", 1) \
    FOR_EACH(XOR, 83, "xor", 1) \
    FOR_EACH(NOT, 84, "not", 1) \
    FOR_EACH(NEG, 85, "neg", 1) \
    FOR_EACH(INVERT, 86, "invert", 1) \
    FOR_EACH(ADD_C, 87, "add.c", 2) \
    FOR_EACH(SMUL_C, 88, "smul.c", 2) \
    FOR_EACH(ZERO_PRI, 89, "zero.pri", 1) \
    FOR_EACH(ZERO_ALT, 90, "zero.alt", 1) \
    FOR_EACH(ZERO, 91, "zero", 2) \
    FOR_EACH(ZERO_S, 92, "zero.s", 2) \
    FOR_EACH(EQ, 95, "eq", 1) \
    FOR_EACH(NEQ, 96, "neq", 1) \
    FOR_EACH(SLESS, 101, "sless", 1) \
    FOR_EACH(SLEQ, 102, "sleq", 1) \
    FOR_EACH(SGRTR, 103, "sgrtr", 1) \
    FOR_EACH(SGEQ, 104, "sgeq", 1) \
    FOR_EACH(EQ_C_PRI, 105, "eq.c.pri", 2) \
    FOR_EACH(EQ_C_ALT, 106, "eq.c.alt", 2) \
    FOR_EACH(INC_PRI, 107, "inc.pri", 1) \
    FOR_EACH(INC_ALT, 108, "inc.alt", 1) \
    FOR_EACH(INC, 109, "inc", 2) \
    FOR_EACH(INC_S, 110, "inc.s", 2) \
    FOR_EACH(INC_I, 111, "inc.i", 1) \
    FOR_EACH(DEC_PRI, 112, "dec.pri", 1) \
    FOR_EACH(DEC_ALT, 113, "dec.alt", 1) \
    FOR_EACH(DEC, 114, "dec", 2) \
    FOR_EACH(DEC_S, 115, "dec.s", 2) \
    FOR_EACH(DEC_I, 116, "dec.i", 1) \
    FOR_EACH(MOVS, 117, "movs", 2) \
    FOR_EACH(FILL, 119, "fill", 2) \
    FOR_EACH(HALT, 120, "halt", 2) \
    FOR_EACH(BOUNDS, 121, "bounds", 2) \
    FOR_EACH(SYSREQ_C, 123, "sysreq.c", 2) \
    FOR_EACH(SWITCH, 129, "switch", 2) \
    FOR_EACH(CASETBL, 130, "casetbl", -1) \
    FOR_EACH(SWAP_PRI, 131, "swap.pri", 1) \
    FOR_EACH(SWAP_ALT, 132, "swap.alt", 1) \
    FOR_EACH(PUSH_ADR, 133, "push.adr", 2) \
    FOR_EACH(NOP, 134, "nop", 1) \
    FOR_EACH(SYSREQ_N, 135, "sysreq.n", 3) \
    FOR_EACH(BREAK, 137, "break", 1) \
    FOR_EACH(PUSH2_C, 138, "push2.c", 3) \
    FOR_EACH(PUSH2, 139, "push2", 3) \
    FOR_EACH(PUSH2_S, 140, "push2.s", 3) \
    FOR_EACH(PUSH2_ADR, 141, "push2.adr", 3) \
    FOR_EACH(PUSH3_C, 142, "push3.c", 4) \
    FOR_EACH(PUSH3, 143, "push3", 4) \
    FOR_EACH(PUSH3_S, 144, "push3.s", 4) \
    FOR_EACH(PUSH3_ADR, 145, "push3.adr", 4) \
    FOR_EACH(PUSH4_C, 146, "push4.c", 5) \
    FOR_EACH(PUSH4, 147, "push4", 5) \
    FOR_EACH(PUSH4_S, 148, "push4.s", 5) \
    FOR_EACH(PUSH4_ADR, 149, "push4.adr", 5) \
    FOR_EACH(PUSH5_C, 150, "push5.c", 6) \
    FOR_EACH(PUSH5, 151, "push5", 6) \
    FOR_EACH(PUSH5_S, 152, "push5.s", 6) \
    FOR_EACH(PUSH5_ADR, 153, "push5.adr", 6) \
    FOR_EACH(LOAD_BOTH, 154, "load.both", 3) \
    FOR_EACH(LOAD_S_BOTH, 155, "load.s.both", 3) \
    FOR_EACH(CONST, 156, "const", 3) \
    FOR_EACH(CONST_S, 157, "const.s", 3) \
    FOR_EACH(TRACKER_PUSH_C, 160, "trk.push.c", 2) \
    FOR_EACH(TRACKER_POP_SETHEAP, 161, "trk.pop", 1) \
    FOR_EACH(GENARRAY, 162, "genarray", 2) \
    FOR_EACH(GENARRAY_Z, 163, "genarray.z", 2) \
    FOR_EACH(STRADJUST_PRI, 164, "stradjust.pri", 1) \
    FOR_EACH(ENDPROC, 166, "endproc", 1) \
    FOR_EACH(INITARRAY_PRI, 169, "initarray.pri", 6) \
    FOR_EACH(INITARRAY_ALT, 170, "initarray.alt", 6) \
    FOR_EACH(HEAP_SAVE, 171, "heap.save", 1) \
    FOR_EACH(HEAP_RESTORE, 172, "heap.restore", 1) \
    /* The _I64 opcodes assume registers are pointers to at least two cells \
     * of memory representing an int64. Since the stack and registers are \
     * not designed to accomodate 64-bit temporaries, some of these opcodes \
     * take a stack slot parameter. This must be a 64-bit entry in the stack. \
     * As a convenience, these opcodes set PRI to the address of the given \
     * stack slot (as if ADDR.PRI were used).
     */ \
    FOR_EACH(MOVE_I64, 192, "move.i64", 1) \
    FOR_EACH(PUSH_I_I64, 193, "push.i.i64", 1) \
    FOR_EACH(CVT_I64, 194, "cvt.i64", 2) \
    FOR_EACH(TRUNCATE_I64, 195, "truncate.i64", 1) \
    FOR_EACH(TEST_I64, 196, "test.i64", 1) \
    FOR_EACH(INVERT_I64, 197, "invert.i64", 2) \
    FOR_EACH(NEG_I64, 198, "neg.i64", 2) \
    FOR_EACH(SMUL_I64, 199, "smul.i64", 2) \
    FOR_EACH(SDIV_ALT_I64, 200, "sdiv.alt.i64", 3) \
    FOR_EACH(ADD_I64, 201, "add.i64", 2) \
    FOR_EACH(SUB_ALT_I64, 202, "sub_alt.i64", 2) \
    FOR_EACH(SHL_I64, 203, "shl.i64", 2) \
    FOR_EACH(SSHR_I64, 204, "sshr.i64", 2) \
    FOR_EACH(SHR_I64, 205, "shr.i64", 2) \
    FOR_EACH(EQ_I64, 206, "eq.i64", 1) \
    FOR_EACH(NEQ_I64, 207, "neq.i64", 1) \
    FOR_EACH(OR_I64, 208, "or.i64", 2) \
    FOR_EACH(AND_I64, 209, "and.i64", 2) \
    FOR_EACH(XOR_I64, 210, "neq.i64", 2) \
    FOR_EACH(STOR_S_I64_C, 211, "stor.s.i64.c", 4) \
    FOR_EACH(SLESS_I64, 212, "sless.i64", 1) \
    FOR_EACH(SLEQ_I64, 213, "sleq.i64", 1) \
    FOR_EACH(SGRTR_I64, 214, "sgrtr.i64", 1) \
    FOR_EACH(SGEQ_I64, 215, "sgeq.i64", 1) \


enum OPCODE {
#define FOR_EACH_OPCODE(op, val, text, cells) OP_##op = val,
    OPCODE_LIST(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    OP_FABS,
    OP_FLOAT,
    OP_FLOATADD,
    OP_FLOATSUB,
    OP_FLOATMUL,
    OP_FLOATDIV,
    OP_RND_TO_NEAREST,
    OP_RND_TO_FLOOR,
    OP_RND_TO_CEIL,
    OP_RND_TO_ZERO,
    OP_FLOATCMP,
    OP_FLOAT_GT,
    OP_FLOAT_GE,
    OP_FLOAT_LT,
    OP_FLOAT_LE,
    OP_FLOAT_NE,
    OP_FLOAT_EQ,
    OP_FLOAT_NOT,
    OPCODES_LAST
};

#define OPCODES_TOTAL (ucell_t) OPCODES_LAST

} // namespace sp

#endif // _INCLUDE_SPFILE_HEADERS_v1_opcodes_H
