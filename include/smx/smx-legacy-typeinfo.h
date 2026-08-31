// vim: set sts=4 ts=8 sw=4 tw=99 et:
// =============================================================================
// SourcePawn
// Copyright (C) 2026 AlliedModders LLC.  All rights RESERVED.
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

#include <stdint.h>

namespace sp {

#pragma pack(push)
#pragma pack(1)

// DEPRECATED. No longer generated for v2 binaries.
//
// The rtti.natives table has the following row structure. The rows must be
// identical to the native table mapping.
struct smx_rtti_native {
    // Index into the name table.
    uint32_t name;

    // Method signature; see smx_rtti_method::signature.
    uint32_t signature;
};

// Only generated for v1 binaries.
//
// The rtti.enumstructs table has the following row structure:
struct smx_rtti_enumstruct {
    // Index into the name table.
    uint32_t name;

    // First row in the rtti.es_fields table. Rows up to the next
    // enumstruct's first row, or the end of the enumstruct table, are
    // owned by this entry.
    uint32_t first_field;

    // Size of the enum struct in cells.
    uint32_t size;
};

// Only generated for v1 binaries.
//
// The rtti.es_fields table has the following row structure:
struct smx_rtti_es_field {
    // Index into the name table.
    uint32_t name;

    // Type id.
    uint32_t type_id;

    // Offset from the base address, in bytes.
    uint32_t offset;
};


#pragma pack(pop)

} // namespace sp
