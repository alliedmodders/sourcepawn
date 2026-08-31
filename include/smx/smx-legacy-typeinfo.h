// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
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
