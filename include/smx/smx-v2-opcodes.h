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
    FOR_EACH(LOAD_GLB, 1, "load.glb", 3) \
    FOR_EACH(LOAD_S, 2, "load.s", 3) \
    FOR_EACH(LREF_S, 3, "lref.s", 3) \
    FOR_EACH(LOAD_I_I32, 4, "load.i.i32", 1) \
    FOR_EACH(LOAD_I_U8, 5, "load.i.u8", 1) \
    FOR_EACH(POP, 6, "pop", 1) \
    FOR_EACH(STOR_GLB, 7, "stor.glb", 3) \
    FOR_EACH(STOR_S, 8, "stor.s", 3) \
    FOR_EACH(DUP, 9, "dup", 1) \
    FOR_EACH(STOR_S_C, 10, "stor.s.c", 7) \
    FOR_EACH(STOR_I_I32, 11, "stor.i.i32", 1) \
    FOR_EACH(STOR_I_U8, 12, "stor.i.u8", 1) \
    FOR_EACH(IDXADDR, 13, "idxaddr", 1) \
    FOR_EACH(PUSH_C, 14, "push.c", 5) \
    FOR_EACH(RETN, 15, "retn", 1) \
    FOR_EACH(CALL, 16, "call", 5) \
    FOR_EACH(JUMP, 17, "jump", 5) \
    FOR_EACH(JZER, 18, "jzer", 5) \
    FOR_EACH(JNZ, 19, "jnz", 5) \
    FOR_EACH(JEQ, 20, "jeq", 5) \
    FOR_EACH(JNEQ, 21, "jneq", 5) \
    FOR_EACH(JSLESS, 22, "jsless", 5) \
    FOR_EACH(JSLEQ, 23, "jsleq", 5) \
    FOR_EACH(JSGRTR, 24, "jsgrtr", 5) \
    FOR_EACH(JSGEQ, 25, "jsgeq", 5) \
    FOR_EACH(SHL, 26, "shl", 1) \
    FOR_EACH(SHR, 27, "shr", 1) \
    FOR_EACH(SSHR, 28, "sshr", 1) \
    FOR_EACH(SMUL, 29, "smul", 1) \
    FOR_EACH(SDIV_I32, 30, "sdiv.i32", 1) \
    FOR_EACH(SMOD_I32, 31, "smod.i32", 1) \
    FOR_EACH(ADD, 32, "add", 1) \
    FOR_EACH(SUB, 33, "sub", 1) \
    FOR_EACH(AND, 34, "and", 1) \
    FOR_EACH(OR, 35, "or", 1) \
    FOR_EACH(XOR, 36, "xor", 1) \
    FOR_EACH(NOT, 37, "not", 1) \
    FOR_EACH(NEG, 38, "neg", 1) \
    FOR_EACH(INVERT, 39, "invert", 1) \
    FOR_EACH(EQ, 40, "eq", 1) \
    FOR_EACH(NEQ, 41, "neq", 1) \
    FOR_EACH(SLESS, 42, "sless", 1) \
    FOR_EACH(SLEQ, 43, "sleq", 1) \
    FOR_EACH(SGRTR, 44, "sgrtr", 1) \
    FOR_EACH(SGEQ, 45, "sgeq", 1) \
    FOR_EACH(INC, 46, "inc", 1) \
    FOR_EACH(DEC, 47, "dec", 1) \
    /* Pops two arrays off the stack:
     *   left = pop()
     *   right = pop()
     *
     * |left| and |right| must both be fixed-array types.
     * |left.length| must be >= |right.length|.
     * |left| and |right| must have types with the same element size, and
     * neither element type can be an array.
     */ \
    FOR_EACH(COPYARRAY, 48, "copyarray", 1) \
    FOR_EACH(SWITCH, 49, "switch", -1) \
    FOR_EACH(ADDR_S, 50, "addr.s", 3) \
    FOR_EACH(HEAP_SAVE, 51, "heap.save", 1) \
    FOR_EACH(HEAP_RESTORE, 52, "heap.restore", 1) \
    FOR_EACH(TEST_F32, 53, "test.f32", 1) \
    FOR_EACH(NEG_F32, 54, "neg.f32", 1) \
    FOR_EACH(MUL_F32, 55, "mul.f32", 1) \
    FOR_EACH(DIV_F32, 56, "div.f32", 1) \
    FOR_EACH(ADD_F32, 57, "add.f32", 1) \
    FOR_EACH(SUB_F32, 58, "sub.f32", 1) \
    FOR_EACH(EQ_F32, 59, "eq.f32", 1) \
    FOR_EACH(NEQ_F32, 60, "neq.f32", 1) \
    FOR_EACH(LESS_F32, 61, "less.f32", 1) \
    FOR_EACH(LEQ_F32, 62, "leq.f32", 1) \
    FOR_EACH(GRTR_F32, 63, "grtr.f32", 1) \
    FOR_EACH(GEQ_F32, 64, "geq.f32", 1) \
    FOR_EACH(CVT_F32, 65, "cvt.f32", 1) \
    FOR_EACH(MOD_F32, 66, "mod.f32", 1) \
    FOR_EACH(CVT_I64, 67, "cvt.i64", 1) \
    FOR_EACH(TRUNCATE_I64, 68, "truncate.i64", 1) \
    FOR_EACH(TEST_I64, 69, "test.i64", 1) \
    FOR_EACH(INVERT_I64, 70, "invert.i64", 1) \
    FOR_EACH(NEG_I64, 71, "neg.i64", 1) \
    FOR_EACH(SMUL_I64, 72, "smul.i64", 1) \
    FOR_EACH(SDIV_I64, 73, "sdiv.i64", 1) \
    FOR_EACH(ADD_I64, 74, "add.i64", 1) \
    FOR_EACH(SUB_I64, 75, "sub.i64", 1) \
    FOR_EACH(SHL_I64, 76, "shl.i64", 1) \
    FOR_EACH(SSHR_I64, 77, "sshr.i64", 1) \
    FOR_EACH(SHR_I64, 78, "shr.i64", 1) \
    FOR_EACH(EQ_I64, 79, "eq.i64", 1) \
    FOR_EACH(NEQ_I64, 80, "neq.i64", 1) \
    FOR_EACH(OR_I64, 81, "or.i64", 1) \
    FOR_EACH(AND_I64, 82, "and.i64", 1) \
    FOR_EACH(XOR_I64, 83, "xor.i64", 1) \
    FOR_EACH(SLESS_I64, 84, "sless.i64", 1) \
    FOR_EACH(SLEQ_I64, 85, "sleq.i64", 1) \
    FOR_EACH(SGRTR_I64, 86, "sgrtr.i64", 1) \
    FOR_EACH(SGEQ_I64, 87, "sgeq.i64", 1) \
    FOR_EACH(SMOD_I64, 88, "smod.i64", 1) \
    FOR_EACH(SWAP, 89, "swap", 1) \
    FOR_EACH(LOAD_FN, 90, "load.fn", 5) \
    FOR_EACH(LOAD_I_I64, 91, "load.i.i64", 1) \
    FOR_EACH(STOR_I_I64, 92, "stor.i.i64", 1) \
    FOR_EACH(RETV, 93, "retv", 1) \
    FOR_EACH(PUSH_C_I8, 94, "push.c.i8", 2) \
    FOR_EACH(CALLN, 95, "calln", 6) \
    FOR_EACH(PUSH_C_I64, 96, "push.c.i64", 9) \
    FOR_EACH(ADDR_GLB, 97, "addr.glb", 3) \
    FOR_EACH(LOAD_STR, 98, "load.str", 3) \
    /* Allocate a new array on the heap, given a uint32_t type_id for the
     * array. The size of the outermost dimension must be pushed onto the
     * stack as a cell_t. The resulting address of the array is pushed onto
     * the stack. If the outermost dimension is fixed, no value is popped
     * from the stack.
     */ \
    FOR_EACH(NEWARRAY, 99, "newarray", 5) \
    /* Same as newarray, except that there must be N values on the stack,
     * where N is <= number of kArrays in the type before any non-kArray
     * types appear. Eg, int[][][25][] must have two integers pushed onto
     * the stack. N must be >= 1, and is encoded as a uint8_t, which is
     * followed by a uint32_t type_id.
     *
     * This is effectively the same as GENARRAY from the v1 VM - it
     * initializes a tree of array pointers for the user.
     *
     * NEWBULKARRAY with N=1 is the same as NEWARRAY.
     */ \
    FOR_EACH(NEWBULKARRAY, 100, "newbulkarray", 6) \
    /* Pops an array address off the stack, then copies a preset set of
     * values from the constant pool (data section) to that array. The offset
     * to the values is encoded as a uint32_t argument.
     *
     * At that offset, there must be a compact-encoded uint32_t specifying the
     * number of bytes to read, followed by that many bytes. Each value is
     * sized according to the array type:
     *   kChar8: int8_t
     *   kInt64: int64_t
     *   everything else: int32_t
     */ \
    FOR_EACH(FILLARRAY, 101, "fillarray", 5) \
    FOR_EACH(ARRAY_TO_NATIVE, 102, "array2native", 1) \
    FOR_EACH(SLICE, 103, "slice", 1) \
    /* Pops a value from the stack, which must be an address to an object-
     * like structure. Loads a value from the specified field, which is
     * encoded as an index into the smx_rtti_field_refs table. The value
     * is then pushed onto the stack (except in the ADDR case, when the
     * address is pushed instead).
     *
     * The type in the field ref must match the type of the object.
     */ \
    FOR_EACH(LOAD_FLD, 104, "load.fld", 5) \
    FOR_EACH(ADDR_FLD, 105, "addr.fld", 5) \
    FOR_EACH(LOAD_ELEM_I32, 106, "load.elem.i32", 1) \
    FOR_EACH(LOAD_ELEM_F32, 107, "load.elem.f32", 1) \
    FOR_EACH(LOAD_ELEM_I64, 108, "load.elem.i64", 1) \
    FOR_EACH(LOAD_ELEM_U8, 109, "load.elem.i8", 1) \
    FOR_EACH(STOR_ELEM_I32, 110, "stor.elem.i32", 1) \
    FOR_EACH(STOR_ELEM_F32, 111, "stor.elem.f32", 1) \
    FOR_EACH(STOR_ELEM_I64, 112, "stor.elem.i64", 1) \
    FOR_EACH(STOR_ELEM_U8, 113, "stor.elem.i8", 1) \
    FOR_EACH(LOAD_I_F32, 114, "load.i.f32", 1) \
    FOR_EACH(STOR_I_F32, 115, "stor.i.f32", 1) \
    FOR_EACH(LOAD_ELEM_A, 116, "load.elem.a", 1) \


enum OPCODE {
#define FOR_EACH_OPCODE(op, val, text, cells) OP_##op = val,
    OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    OPCODES_LAST
};

} // namespace v2
} // namespace sp

