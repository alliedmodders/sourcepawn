// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2004-2026 AlliedModders LLC

#ifndef _INCLUDE_SPFILE_HEADERS_v1_H
#define _INCLUDE_SPFILE_HEADERS_v1_H

#include <smx/smx-headers.h>
#include <stddef.h>
#include <stdint.h>

namespace sp {

// These structures are byte-packed.
#if defined __GNUC__
#    pragma pack(1)
#else
#    pragma pack(push)
#    pragma pack(1)
#endif

// (Obsolete) debug-information is present.
static const uint16_t CODEFLAG_DEBUG = 0x1;

// The ".data" section.
typedef struct sp_file_data_s {
    uint32_t datasize; /**< Size of data section in memory */
    uint32_t memsize;  /**< Total mem required (includes data) */
    uint32_t data;     /**< File offset to data (helper) */
} sp_file_data_t;

// The ".publics" section.
typedef struct sp_file_publics_s {
    uint32_t address; /**< Address relative to code section */
    uint32_t name;    /**< Index into nametable */
} sp_file_publics_t;

// The ".natives" section.
typedef struct sp_file_natives_s {
    uint32_t name; /**< Index into nametable */
} sp_file_natives_t;

// The ".pubvars" section.
typedef struct sp_file_pubvars_s {
    uint32_t address; /**< Address relative to the DAT section */
    uint32_t name;    /**< Index into nametable */
} sp_file_pubvars_t;

// The ".tags" section.
typedef struct sp_file_tag_s {
    uint32_t tag_id; /**< Tag ID from compiler */
    uint32_t name;   /**< Index into nametable */
} sp_file_tag_t;

// The ".dbg.info" section.
typedef struct sp_fdbg_info_s {
    uint32_t num_files;  /**< number of files */
    uint32_t num_lines;  /**< number of lines */
    uint32_t num_syms;   /**< number of symbols in .dbg.symbols if present */
    uint32_t num_arrays; /**< number of num_syms which are arrays */
} sp_fdbg_info_t;

// The ".dbg.files" section. If .dbg.strings is present, the name offset
// references that section rather than .names.
typedef struct sp_fdbg_file_s {
    uint32_t addr; /**< Address into code */
    uint32_t name; /**< Offset into debug nametable */
} sp_fdbg_file_t;

// The ".dbg.lines" section.
typedef struct sp_fdbg_line_s {
    uint32_t addr; /**< Address into code */
    uint32_t line; /**< Line number */
} sp_fdbg_line_t;

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// DO NOT DEFINE NEW STRUCTURES BELOW.
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#if defined __GNUC__
#    pragma pack() /* reset default packing */
#else
#    pragma pack(pop) /* reset previous packing */
#endif

} // namespace sp

#endif //_INCLUDE_SPFILE_HEADERS_v1_H
