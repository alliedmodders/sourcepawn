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
    FOR_EACH(TEST, 50, "test", 1) \
    FOR_EACH(CVT_F32, 51, "cvt.f32", 1) \
    FOR_EACH(CVT_I64, 52, "cvt.i64", 1) \
    FOR_EACH(TRUNCATE_I64, 53, "truncate.i64", 1) \
    FOR_EACH(SWAP, 54, "swap", 1) \
    FOR_EACH(LOAD_FN, 55, "load.fn", 5) \
    FOR_EACH(LOAD_I_I64, 56, "load.i.i64", 1) \
    FOR_EACH(STOR_I_I64, 57, "stor.i.i64", 1) \
    FOR_EACH(RETV, 58, "retv", 1) \
    FOR_EACH(PUSH_C_I8, 59, "push.c.i8", 2) \
    /* CALLN is only used for variadic natives. */ \
    FOR_EACH(CALLN, 60, "calln", 6) \
    FOR_EACH(PUSH_C_I64, 61, "push.c.i64", 9) \
    FOR_EACH(ADDR_GLB, 62, "addr.glb", 3) \
    FOR_EACH(LOAD_STR, 63, "load.str", 3) \
    /* Allocate a new array on the heap, given a uint32_t type_id for the
     * array. The size of the outermost dimension must be pushed onto the
     * stack as a cell_t. The resulting address of the array is pushed onto
     * the stack. If the outermost dimension is fixed, no value is popped
     * from the stack.
     *
     * If the array's element type is a fixed array, then each element will
     * be initialized with an array of that type. This happens recursively.
     */ \
    FOR_EACH(NEWARRAY, 64, "newarray", 5) \
    /* Same as newarray, except that there must be N values on the stack,
     * where N is the number of kArrays in the type before any non-kArray
     * types appear. Eg, int[][][25][] must have two integers pushed onto
     * the stack. N must be >= 1, and is encoded as a uint8_t, which is
     * followed by a uint32_t type_id.
     *
     * This "bulkier" opcode exists for rarer array initialization cases
     * where more than one dimension is unspecified.
     *
     * NEWBULKARRAY with N=1 is the same as NEWARRAY.
     */ \
    FOR_EACH(NEWBULKARRAY, 65, "newbulkarray", 6) \
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
    FOR_EACH(FILLARRAY, 66, "fillarray", 5) \
    /* Given a flat or non-flat array, and an index, create a view into this
     * array at the given index. The returned array is always of a non-fixed
     * size.
     */ \
    FOR_EACH(SLICE, 67, "slice", 1) \
    /* Pops a value from the stack, which must be an address to an object-
     * like structure. Loads a value from the specified field, which is
     * encoded as a table ID (see smx-typeinfo.h MakeTableId). The selector
     * must be kTableId_RttiField, and the index is into the fields table.
     * The value is then pushed onto the stack (except in the ADDR case, when
     * the address is pushed instead).
     *
     * The owning classdef of the field must match the type of the object.
     */ \
    FOR_EACH(LOAD_FLD, 68, "load.fld", 5) \
    FOR_EACH(ADDR_FLD, 69, "addr.fld", 5) \
    FOR_EACH(LOAD_ELEM_I32, 70, "load.elem.i32", 1) \
    FOR_EACH(LOAD_ELEM_F32, 71, "load.elem.f32", 1) \
    FOR_EACH(LOAD_ELEM_I64, 72, "load.elem.i64", 1) \
    FOR_EACH(LOAD_ELEM_U8, 73, "load.elem.i8", 1) \
    FOR_EACH(STOR_ELEM_I32, 74, "stor.elem.i32", 1) \
    FOR_EACH(STOR_ELEM_F32, 75, "stor.elem.f32", 1) \
    FOR_EACH(STOR_ELEM_I64, 76, "stor.elem.i64", 1) \
    FOR_EACH(STOR_ELEM_U8, 77, "stor.elem.i8", 1) \
    FOR_EACH(LOAD_I_F32, 78, "load.i.f32", 1) \
    FOR_EACH(STOR_I_F32, 79, "stor.i.f32", 1) \
    FOR_EACH(LOAD_ELEM_A, 80, "load.elem.a", 1) \
    FOR_EACH(PUSH_C_F32, 81, "push.c.f32", 5) \
    FOR_EACH(STOR_FLD, 82, "stor.fld", 5) \
    FOR_EACH(LOAD_FLD_OFFSET, 83, "load.fld.offset", 5) \
    FOR_EACH(LOAD_ES_SIZE, 84, "load.es.size", 5) \
    FOR_EACH(COPYOBJ, 85, "copyobj", 5) \
    /* Slice an enumstruct of the given type, into an any[] array. The
     * size of the array is the size of the enum struct in cells.
     */ \
    FOR_EACH(SLICE_ES, 86, "slice.es", 5) \
    /* Create a view of a flat or non-flat array as a flat or non-flat array.
     * Takes a type ID representing the output type.
     */ \
    FOR_EACH(SLICE_AS, 87, "slice.as", 5) \
    FOR_EACH(STOR_I_A, 88, "stor.i.a", 1) \
    FOR_EACH(STOR_ELEM_A, 89, "stor.elem.a", 1) \
    FOR_EACH(LOAD_NULL, 90, "load.null", 1) \
    /* Call a variadic native function, unrolling the variadic argument vector
     * from the parent function into the arguments of the callee. The argument
     * is a method_id and uint8_t argc.
     */ \
    FOR_EACH(CALLVA, 91, "callva", 6) \
    /* Same as CALL, except the function is popped off the stack rather than
     * encoded as a method id.
     */ \
    FOR_EACH(CALLI, 92, "calli", 1) \
    /* Pops an encoded method ID off the stack and converts it into a closure
     * object, which is pushed back onto the stack.
     *
     * The function signature must match the encoded type id.
     */ \
    FOR_EACH(GETFNOBJ, 93, "getfnobj", 5) \
    /* Pops an SpFunction off the stack and pushes its funcid_t.
     * Runtime error if the SpFunction is a closure.
     */ \
    FOR_EACH(GETFUNCID, 94, "getfuncid", 1) \
    /* Allocate a new object. The operand is a uint32_t encoded identifier.
     * If the low bit of the identifier is 0, the remaining 31 bits are an
     * index into the class table.
     *
     * If the low bit of the identifier is 1, the instruction is invalid.
     *
     * The instance is zero-initialized and has a HeapItem header. The
     * resulting heap address is pushed onto the stack.
     */ \
    FOR_EACH(NEWOBJ, 95, "newobj", 5) \
    /* Creates a closure object. Operand is uint32 method_id. num_upvars is
     * derived from the method's kClosureSlots header. Pops num_upvars values
     * from the stack. Pushes SpFunction* onto the stack.
     */ \
    FOR_EACH(NEWCLOSURE, 96, "newclosure", 5) \
    /* Loads upvar[index] from the current closure object and pushes it.
     * Operand is uint16 index.
     */ \
    FOR_EACH(LOAD_UPVAR, 97, "load.upvar", 3) \
    /* Pops a value and stores it into upvar[index] of the current closure.
     * Operand is uint16 index.
     */ \
    FOR_EACH(STOR_UPVAR, 98, "stor.upvar", 3) \
    /* Loads the address of upvar[index] from the current closure object.
     * Operand is uint16 index. Pushes a pointer to the upvar cell.
     */ \
    FOR_EACH(ADDR_UPVAR, 99, "addr.upvar", 3) \


 enum OPCODE {
#define FOR_EACH_OPCODE(op, val, text, cells) OP_##op = val,
    OPCODE_LIST_V2(FOR_EACH_OPCODE)
#undef FOR_EACH_OPCODE
    OPCODES_LAST
};

} // namespace v2
} // namespace sp

