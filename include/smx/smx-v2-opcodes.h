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
    FOR_EACH(LOAD_I_I32, 3, "load.i.i32", 1) \
    FOR_EACH(LOAD_I_U8, 4, "load.i.u8", 1) \
    FOR_EACH(POP, 5, "pop", 1) \
    FOR_EACH(STOR_GLB, 6, "stor.glb", 3) \
    FOR_EACH(STOR_S, 7, "stor.s", 3) \
    FOR_EACH(DUP, 8, "dup", 1) \
    FOR_EACH(STOR_S_C, 9, "stor.s.c", 7) \
    FOR_EACH(STOR_I_I32, 10, "stor.i.i32", 1) \
    FOR_EACH(STOR_I_U8, 11, "stor.i.u8", 1) \
    FOR_EACH(IDXADDR, 12, "idxaddr", 1) \
    FOR_EACH(PUSH_C, 13, "push.c", 5) \
    FOR_EACH(RETN, 14, "retn", 1) \
    FOR_EACH(CALL, 15, "call", 5) \
    FOR_EACH(JUMP, 16, "jump", 5) \
    FOR_EACH(JZER, 17, "jzer", 5) \
    FOR_EACH(JNZ, 18, "jnz", 5) \
    FOR_EACH(JEQ, 19, "jeq", 5) \
    FOR_EACH(JNEQ, 20, "jneq", 5) \
    FOR_EACH(JSLESS, 21, "jsless", 5) \
    FOR_EACH(JSLEQ, 22, "jsleq", 5) \
    FOR_EACH(JSGRTR, 23, "jsgrtr", 5) \
    FOR_EACH(JSGEQ, 24, "jsgeq", 5) \
    FOR_EACH(SHL, 25, "shl", 1) \
    FOR_EACH(SHR, 26, "shr", 1) \
    FOR_EACH(SSHR, 27, "sshr", 1) \
    FOR_EACH(SMUL, 28, "smul", 1) \
    FOR_EACH(SDIV, 29, "sdiv", 1) \
    FOR_EACH(SMOD, 30, "smod", 1) \
    FOR_EACH(ADD, 31, "add", 1) \
    FOR_EACH(SUB, 32, "sub", 1) \
    FOR_EACH(AND, 33, "and", 1) \
    FOR_EACH(OR, 34, "or", 1) \
    FOR_EACH(XOR, 35, "xor", 1) \
    FOR_EACH(NOT, 36, "not", 1) \
    FOR_EACH(NEG, 37, "neg", 1) \
    FOR_EACH(INVERT, 38, "invert", 1) \
    FOR_EACH(EQ, 39, "eq", 1) \
    FOR_EACH(NEQ, 40, "neq", 1) \
    FOR_EACH(SLESS, 41, "sless", 1) \
    FOR_EACH(SLEQ, 42, "sleq", 1) \
    FOR_EACH(SGRTR, 43, "sgrtr", 1) \
    FOR_EACH(SGEQ, 44, "sgeq", 1) \
    FOR_EACH(INC, 45, "inc", 1) \
    FOR_EACH(DEC, 46, "dec", 1) \
    /* Pops two arrays off the stack:
     *   left = pop()
     *   right = pop()
     *
     * |left| and |right| must both be fixed-array types.
     * |left.length| must be >= |right.length|.
     * |left| and |right| must have types with the same element size, and
     * neither element type can be an array.
     */ \
    FOR_EACH(COPYARRAY, 47, "copyarray", 1) \
    FOR_EACH(SWITCH, 48, "switch", -1) \
    FOR_EACH(ADDR_S, 49, "addr.s", 3) \
    FOR_EACH(HEAP_SAVE, 50, "heap.save", 1) \
    FOR_EACH(HEAP_RESTORE, 51, "heap.restore", 1) \
    FOR_EACH(TEST, 52, "test", 1) \
    FOR_EACH(CVT_F32, 53, "cvt.f32", 1) \
    FOR_EACH(CVT_I64, 54, "cvt.i64", 1) \
    FOR_EACH(TRUNCATE_I64, 55, "truncate.i64", 1) \
    FOR_EACH(SWAP, 56, "swap", 1) \
    FOR_EACH(LOAD_FN, 57, "load.fn", 5) \
    FOR_EACH(LOAD_I_I64, 58, "load.i.i64", 1) \
    FOR_EACH(STOR_I_I64, 59, "stor.i.i64", 1) \
    FOR_EACH(RETV, 60, "retv", 1) \
    FOR_EACH(PUSH_C_I8, 61, "push.c.i8", 2) \
    FOR_EACH(CALLN, 62, "calln", 6) \
    FOR_EACH(PUSH_C_I64, 63, "push.c.i64", 9) \
    FOR_EACH(ADDR_GLB, 64, "addr.glb", 3) \
    FOR_EACH(LOAD_STR, 65, "load.str", 3) \
    /* Allocate a new array on the heap, given a uint32_t type_id for the
     * array. The size of the outermost dimension must be pushed onto the
     * stack as a cell_t. The resulting address of the array is pushed onto
     * the stack. If the outermost dimension is fixed, no value is popped
     * from the stack.
     */ \
    FOR_EACH(NEWARRAY, 66, "newarray", 5) \
    /* Same as newarray, except that there must be N values on the stack,
     * where N is the number of kArrays in the type before any non-kArray
     * types appear. Eg, int[][][25][] must have two integers pushed onto
     * the stack. N must be >= 1, and is encoded as a uint8_t, which is
     * followed by a uint32_t type_id.
     *
     * This is effectively the same as GENARRAY from the v1 VM - it
     * initializes a tree of array pointers for the user.
     *
     * NEWBULKARRAY with N=1 is the same as NEWARRAY.
     */ \
    FOR_EACH(NEWBULKARRAY, 67, "newbulkarray", 6) \
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
    FOR_EACH(FILLARRAY, 68, "fillarray", 5) \
    FOR_EACH(ARRAY_TO_NATIVE, 69, "array2native", 1) \
    FOR_EACH(SLICE, 70, "slice", 1) \
    /* Pops a value from the stack, which must be an address to an object-
     * like structure. Loads a value from the specified field, which is
     * encoded as an index into the smx_rtti_field_refs table. The value
     * is then pushed onto the stack (except in the ADDR case, when the
     * address is pushed instead).
     *
     * The object type in the field ref must match the type of the object.
     */ \
    FOR_EACH(LOAD_FLD, 71, "load.fld", 5) \
    FOR_EACH(ADDR_FLD, 72, "addr.fld", 5) \
    FOR_EACH(LOAD_ELEM_I32, 73, "load.elem.i32", 1) \
    FOR_EACH(LOAD_ELEM_F32, 74, "load.elem.f32", 1) \
    FOR_EACH(LOAD_ELEM_I64, 75, "load.elem.i64", 1) \
    FOR_EACH(LOAD_ELEM_U8, 76, "load.elem.i8", 1) \
    FOR_EACH(STOR_ELEM_I32, 77, "stor.elem.i32", 1) \
    FOR_EACH(STOR_ELEM_F32, 78, "stor.elem.f32", 1) \
    FOR_EACH(STOR_ELEM_I64, 79, "stor.elem.i64", 1) \
    FOR_EACH(STOR_ELEM_U8, 80, "stor.elem.i8", 1) \
    FOR_EACH(LOAD_I_F32, 81, "load.i.f32", 1) \
    FOR_EACH(STOR_I_F32, 82, "stor.i.f32", 1) \
    FOR_EACH(LOAD_ELEM_A, 83, "load.elem.a", 1) \
    FOR_EACH(PUSH_C_F32, 84, "push.c.f32", 5) \
    FOR_EACH(STOR_FLD, 85, "stor.fld", 5) \
    FOR_EACH(LOAD_FLD_OFFSET, 86, "load.fld.offset", 5) \
    FOR_EACH(LOAD_ES_SIZE, 87, "load.es.size", 5) \
    FOR_EACH(COPYOBJ, 88, "copyobj", 5) \
    FOR_EACH(SLICE_ES, 89, "slice.es", 5)


enum OPCODE {
#define FOR_EACH_OPCODE(op, val, text, cells) OP_##op = val,
    OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    OPCODES_LAST
};

} // namespace v2
} // namespace sp

