// vim: set sts=4 ts=8 sw=4 tw=99 et:
/* Pawn compiler
 *
 *  Drafted after the Small-C compiler Version 2.01, originally created
 *  by Ron Cain, july 1980, and enhanced by James E. Hendrix.
 *
 *  This version comes close to a complete rewrite.
 *
 *  Copyright R. Cain, 1980
 *  Copyright J.E. Hendrix, 1982, 1983
 *  Copyright ITB CompuPhase, 1997-2006
 *
 *  This software is provided "as-is", without any express or implied warranty.
 *  In no event will the authors be held liable for any damages arising from
 *  the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1.  The origin of this software must not be misrepresented; you must not
 *      claim that you wrote the original software. If you use this software in
 *      a product, an acknowledgment in the product documentation would be
 *      appreciated but is not required.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#ifndef SC_H_INCLUDED
#define SC_H_INCLUDED
#include <limits.h>
#include <setjmp.h>
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
#include "shared/string-pool.h"
#include "source-file.h"
#include "types.h"

typedef int32_t cell;
typedef uint32_t ucell;

/* Note: the "cell" and "ucell" types are defined in AMX.H */

#define PUBLIC_CHAR '@' /* character that defines a function "public" */
#define sCHARBITS 8     /* size of a packed character */

#define sLINEMAX 4095
#define PREPROC_TERM \
    '\x7f' /* termination character for preprocessor expressions (the "DEL" code) */
#define sDEF_PREFIX "sourcemod.inc" /* default prefix filename */

#ifdef _WIN32
static constexpr char DIRSEP_CHAR = '\\';
# define PATH_MAX _MAX_PATH
#else
static constexpr char DIRSEP_CHAR = '/';
#endif

struct DefaultArrayData;

struct DefaultArg : public PoolObject {
    int tag = 0;
    ke::Maybe<cell> val;
    DefaultArrayData* array = nullptr;
    symbol* sym = nullptr;

    ~DefaultArg();
};
struct methodmap_t;
struct stringlist;
 
class EnumData;
class EnumStructData;
class EnumStructVarData;
class FunctionData;
class SymbolData : public PoolObject
{
  public:
    virtual FunctionData* asFunction() { return nullptr; }
    virtual EnumStructVarData* asEnumStructVar() { return nullptr; }
    virtual methodmap_t* asMethodmap() { return nullptr; }
    virtual EnumStructData* asEnumStruct() { return nullptr; }
    virtual EnumData* asEnum() { return nullptr; }
};

struct symbol;

// Values for symbol::usage.
#define uREAD       0x1     // Used/accessed.
#define uWRITTEN    0x2     // Altered/written (variables only).
#define uLIVE       0x4     // Marked during liveness analysis.

#define uMAINFUNC "main"

struct methodmap_method_t;

#define DECLFLAG_ARGUMENT 0x02       // The declaration is for an argument.
#define DECLFLAG_VARIABLE 0x04       // The declaration is for a variable.
#define DECLFLAG_ENUMROOT 0x08       // Multi-dimensional arrays should have an enumroot.
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
    int opertok; // Operator token, if applicable.
};

/* codes for ffabort() */
#define xEXIT 1           /* exit code in PRI */
#define xASSERTION 2      /* abort caused by failing assertion */

/* Miscellaneous  */
#if !defined TRUE
#    define FALSE 0
#    define TRUE 1
#endif
#define sIN_CSEG 1     /* if parsing CODE */
#define sIN_DSEG 2     /* if parsing DATA */
#define sSYMBOLIC 2    /* bit position in "debug" variable: symbolic info */

#define CELL_MAX (((ucell)1 << (sizeof(cell) * 8 - 1)) - 1)

struct token_t;

const char* type_to_name(int tag);

void setcaption();
int RunCompiler(int argc, char** argv, CompileContext& cc);

constexpr cell char_array_cells(cell size) {
    return (size + sizeof(cell) - 1) / sizeof(cell);
}

static constexpr cell kMaxCells = INT_MAX / sizeof(cell);

// Disable this to enable easy watchpoints on bitfield members.
#if 1
# define SP_BITFIELD(n) : n
#else
# define SP_BITFIELD(n)
#endif

#endif /* SC_H_INCLUDED */
