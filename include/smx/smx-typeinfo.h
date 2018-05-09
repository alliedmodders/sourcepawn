// vim: set sts=2 ts=8 sw=2 tw=99 et:
// =============================================================================
// SourcePawn
// Copyright (C) 2004-2018 AlliedModders LLC.  All rights RESERVED.
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

#ifndef _include_sourcepawn_smx_typeinfo_h
#define _include_sourcepawn_smx_typeinfo_h

#include <assert.h>

namespace sp {

#pragma pack(push)
#pragma pack(1)

// SourcePawn stores run-time type information in a set of additional sections
// defined in the SMX header.
//
// The most important of these is "rtti.data", which is an arbitrary-length
// blob encoding information about complex types, such as arrays and function
// signatures.
//
// Other types, such as enums, have more complex encodings in fixed-length
// tables.

// All row-based RTTI tables have the following layout:
struct smx_rtti_table_header {
  // Size of the header; row data is immediately after.
  uint32_t header_size;

  // Size of each row in the table.
  uint32_t row_size;

  // Number of elements in the table.
  uint32_t row_count;
};

// In some cases, a row will index another table, but that index may be
// optional. Rather than define all tables as 1-indexed, we declare a sentinel
// value kNoTableIndex for optional entries.
static const uint32_t kNoTableIndex = 0x7fffffff;

// The rtti.enums table has the following row structure:
struct smx_rtti_enum {
  // Index into the names table.
  uint32_t name;

  // Reserved - must be 0.
  uint32_t reserved0;
  uint32_t reserved1;
  uint32_t reserved2;
};

// The rtti.methods table has the following row structure:
struct smx_rtti_method {
  // Index into the name table.
  uint32_t name;

  // Function location, range is [pcode_start, pcode_end).
  uint32_t pcode_start;
  uint32_t pcode_end;

  // Method signature; offset into rtti.data. The encoding at this offset is:
  //    FormalArgs    uint8
  //    Variadic?     uint8
  //    ReturnType    <return-type>
  //    Params*       <param>
  //
  // <return-type> must be kVoid or a <type>.
  // <param> must be: kByRef? <type>
  uint32_t signature;
};

// The rtti.natives table has the following row structure. The rows must be
// identical to the native table mapping.
struct smx_rtti_native {
  // Index into the name table.
  uint32_t name;

  // Method signature; see smx_rtti_method::signature.
  uint32_t signature;
};

// The rtti.typedefs table has the following row structure:
struct smx_rtti_typedef {
  // Index into the name table.
  uint32_t name;

  // Type identifier.
  uint32_t type_id;
};

// The rtti.typesets table has the following row structure:
struct smx_rtti_typeset {
  // Index into the name table.
  uint32_t name;

  // Typeset signature; offset into rtti.data. The encoding is:
  //    NumTypes      uint32
  //    Types*        type
  uint32_t signature;
};

// A type identifier is a 32-bit value encoding a type. It is encoded as
// follows:
//   bits 0-3:  type kind
//   bits 4-31: type payload
//
// The kind is a type signature that can be completely inlined in the
// remaining 28 bits.
static const uint8_t kTypeId_Inline  = 0x0;
// The payload is an index into the rtti.data section.
static const uint8_t kTypeId_Complex = 0x1;

static inline uint32_t MakeTypeId(uint32_t payload, uint8_t kind) {
  uint32_t adjusted_payload = payload << 4;
  assert((adjusted_payload >> 4) == payload);
  assert(kind < (1 << 4));
  return adjusted_payload | kind;
}

// These are control bytes for type signatures.
//
// uint32 values are encoded with a variable length encoding:
//   0x00  - 0x7f:    1 byte
//   0x80  - 0x7fff:  2 bytes
//   0x8000 - 0x7fffff: 3 bytes
//   0x800000 - 0x7fffffff: 4 bytes
//   0x80000000 - 0xffffffff: 5 bytes
namespace cb {

// This section encodes raw types.
static const uint8_t kBool        = 0x01;
static const uint8_t kInt32       = 0x06;
static const uint8_t kFloat32     = 0x0c;
static const uint8_t kChar8       = 0x0e;
static const uint8_t kAny         = 0x10;
static const uint8_t kTopFunction = 0x11;

// This section encodes multi-byte raw types.

// kFixedArray is followed by:
//    Size          uint32
//    Type          <type>
//
// kArray is followed by:
//    Type          <type>
static const uint8_t kFixedArray = 0x30;
static const uint8_t kArray      = 0x31;

// kFunction is always followed by the same encoding as in
// smx_rtti_method::signature.
static const uint8_t kFunction   = 0x32;

// Each of these is followed by an index into an appropriate table.
static const uint8_t kEnum       = 0x42; // rtti.enums
static const uint8_t kTypedef    = 0x43; // rtti.typedefs
static const uint8_t kTypeset    = 0x44; // rtti.typesets

// This section encodes special indicator bytes that can appear within multi-
// byte types.

// For function signatures, indicating no return value.
static const uint8_t kVoid   = 0x70;
// For functions, indicating the last argument of a function is variadic.
static const uint8_t kVariadic = 0x71;
// For parameters, indicating pass-by-ref.
static const uint8_t kByRef  = 0x72;
// For reference and compound types, indicating const.
static const uint8_t kConst = 0x73;

} // namespace cb

#pragma pack(pop)

} // namespace sp

#endif // _include_sourcepawn_smx_typeinfo_h
