// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
// Copyright R. Cain, 1980
// Copyright J.E. Hendrix, 1982, 1983
// Copyright ITB CompuPhase, 1997-2006
//
#pragma once

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <memory>
#include <utility>

#include <amtl/am-maybe.h>
#include <amtl/am-vector.h>
#include <sp_vm_types.h>

#include "pool-objects.h"
#include "utils/string-pool.h"
#include "source-file.h"
#include "types.h"

typedef int32_t cell;
typedef uint32_t ucell;

namespace sp {
namespace cc {

/* Note: the "cell" and "ucell" types are defined in AMX.H */

#define sDEF_PREFIX "sourcemod.inc" /* default prefix filename */

class VarDecl;

// Values for symbol::usage.
#define uREAD       0x1     // Used/accessed.
#define uWRITTEN    0x2     // Altered/written (variables only).

#define uMAINFUNC "main"

#define DECLFLAG_ARGUMENT 0x02       // The declaration is for an argument.
#define DECLFLAG_VARIABLE 0x04       // The declaration is for a variable.
#define DECLFLAG_MAYBE_FUNCTION 0x10 // Might be a named function.
#define DECLFLAG_OLD 0x40            // Known old-style declaration.
#define DECLFLAG_FIELD 0x80          // Struct field.
#define DECLFLAG_NEW 0x100           // Known new-style declaration.
#define DECLMASK_NAMED_DECL \
    (DECLFLAG_ARGUMENT | DECLFLAG_VARIABLE | DECLFLAG_MAYBE_FUNCTION | DECLFLAG_FIELD)

/* For parsing declarations. */
struct declinfo_t {
    sp::Atom* name;
    typeinfo_t type;
};

/* codes for ffabort() */
#define xEXIT 1           /* exit code in PRI */
#define xASSERTION 2      /* abort caused by failing assertion */

/* Miscellaneous  */
#if !defined TRUE
#    define FALSE 0
#    define TRUE 1
#endif

const char* type_to_name(int tag);

void setcaption();
int RunCompiler(int argc, char** argv, CompileContext& cc);

static constexpr cell kMaxCells = (INT_MAX / 4) / sizeof(cell_t);

// Disable this to enable easy watchpoints on bitfield members.
#if 1
# define SP_BITFIELD(n) : n
#else
# define SP_BITFIELD(n)
#endif

} // namespace cc
} // namespace sp
