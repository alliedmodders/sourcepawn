// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC

#pragma once

namespace sp {

#if defined __GNUC__
#    pragma pack(1)
#else
#    pragma pack(push)
#    pragma pack(1)
#endif

// Backward compatibility hack - expose public p-structs to the embedder via
// a simple table.
//
// Table name: "pstruct_glb"
//
// This table is optional.
struct smx_pstruct_global {
    // Index into the name table.
    uint32_t name;

    // First owned row in the pstruct_values table. This pstruct owns rows up
    // until the last pstruct row, or the next pstruct global's first row,
    // whichever comes first.
    uint32_t first_value;
};

// Table name: "pstruct_glb.values"
struct smx_pstruct_value {
    uint32_t field_name;  // Index into the name table.
    uint32_t type_id;     // Type descriptor for the contents.
    uint32_t fill_data;   // For 32-bit scalars, contains the data value.
                          // For anything else, a pointer to the data section.
};

#if defined __GNUC__
#    pragma pack()
#else
#    pragma pack(pop)
#endif

} // namespace sp
