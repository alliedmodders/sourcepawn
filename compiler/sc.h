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
#include <amtl/am-uniqueptr.h>
#include <amtl/am-vector.h>
#include <sp_vm_types.h>
#include "amx.h"
#include "osdefs.h"
#include "shared/string-pool.h"
#include "types.h"

/* Note: the "cell" and "ucell" types are defined in AMX.H */

#define PUBLIC_CHAR '@' /* character that defines a function "public" */
#define CTRL_CHAR '\\'  /* default control character */
#define sCHARBITS 8     /* size of a packed character */

#define MAXTAGS 16
#define sLINEMAX 4095      /* input line length (in characters) */
#define sCOMP_STACK 32     /* maximum nesting of #if .. #endif sections */
#define sDEF_LITMAX 500    /* initial size of the literal pool, in "cells" */
#define sDEF_AMXSTACK 4096 /* default stack size for AMX files */
#define PREPROC_TERM \
    '\x7f' /* termination character for preprocessor expressions (the "DEL" code) */
#define sDEF_PREFIX "sourcemod.inc" /* default prefix filename */

struct arginfo { /* function argument info */
    char name[sNAMEMAX + 1];
    char ident; /* iVARIABLE, iREFERENCE, iREFARRAY or iVARARGS */
    bool is_const;
    int tag;    /* argument tag id */
    int dim[sDIMEN_MAX];
    int idxtag[sDIMEN_MAX];
    int numdim;               /* number of dimensions */
    unsigned char hasdefault; /* bit0: is there a default value? bit6: "tagof"; bit7: "sizeof" */
    union {
        cell val; /* default value */
        struct {
            cell* data;    /* values of default array */
            int size;      /* complete length of default array */
            int arraysize; /* size to reserve on the heap */
            cell addr;     /* address of the default array in the data segment */
        } array;
    } defvalue;       /* default value, or pointer to default array */
    int defvalue_tag; /* tag of the default value */
};

/*  Equate table, tagname table, library table */
struct constvalue {
    constvalue* next;
    char name[sNAMEMAX + 1];
    cell value;
    int index; /* index level, for constants referring to array sizes/tags
                         * tag for enumeration lists */
};

struct methodmap_t;
struct stringlist;

/*  Possible entries for "ident". These are used in the "symbol", "value"
 *  and arginfo structures. Not every constant is valid for every use.
 *  In an argument list, the list is terminated with a "zero" ident; labels
 *  cannot be passed as function arguments, so the value 0 is overloaded.
 */
#define iVARIABLE 1  /* cell that has an address and that can be fetched directly (lvalue) */
#define iREFERENCE 2 /* iVARIABLE, but must be dereferenced */
#define iARRAY 3
#define iREFARRAY 4   /* an array passed by reference (i.e. a pointer) */
#define iARRAYCELL 5  /* array element, cell that must be fetched indirectly */
#define iARRAYCHAR 6  /* array element, character from cell from array */
#define iEXPRESSION 7 /* expression result, has no address (rvalue) */
#define iCONSTEXPR 8  /* constant expression (or constant symbol) */
#define iFUNCTN 9
#define iVARARGS 11    /* function specified ... as argument(s) */
#define iACCESSOR 13   /* property accessor via a methodmap_method_t */
#define iMETHODMAP 14  /* symbol defining a methodmap */
#define iENUMSTRUCT 15 /* symbol defining an enumstruct */

class EnumStructVarData;
class FunctionData;
class SymbolData
{
  public:
    virtual ~SymbolData() {}
    virtual FunctionData* asFunction() {
        return nullptr;
    }
    virtual EnumStructVarData* asEnumStructVar() {
        return nullptr;
    }
};

class FunctionData final : public SymbolData
{
  public:
    FunctionData();
    ~FunctionData() override;
    FunctionData* asFunction() override {
        return this;
    }

    void resizeArgs(size_t nargs);

    int funcid;          /* set for functions during codegen */
    stringlist* dbgstrs; /* debug strings - functions only */
    ke::Vector<arginfo> args;
};

class EnumStructVarData final : public SymbolData
{
  public:
    EnumStructVarData* asEnumStructVar() override {
        return this;
    }

    ke::Vector<ke::UniquePtr<symbol>> children;
};

struct symbol;

/*  Symbol table format
 *
 *  The symbol name read from the input file is stored in "name", the
 *  value of "addr" is written to the output file. The address in "addr"
 *  depends on the class of the symbol:
 *      global          offset into the data segment
 *      local           offset relative to the stack frame
 *      label           generated hexadecimal number
 *      function        offset into code segment
 */
struct symbol {
    symbol();
    symbol(const symbol& other);
    symbol(const char* name, cell addr, int ident, int vclass, int tag);
    ~symbol();

    symbol* next;
    cell codeaddr; /* address (in the code segment) where the symbol declaration starts */
    char vclass;   /* sLOCAL if "addr" refers to a local symbol */
    char ident;    /* see below for possible values */
    int compound;  /* compound level (braces nesting level) */
    int tag;       /* tagname id */

    // See uREAD/uWRITTEN above.
    uint8_t usage : 2;

    // Variable: the variable is defined in the source file.
    // Function: the function is defined ("implemented") in the source file
    // Constant: the symbol is defined in the source file.
    bool defined : 1;       // remove when moving to a single-pass system
    bool is_const : 1;

    // Variables and functions.
    bool stock : 1;         // discardable without warning
    bool is_public : 1;     // publicly exposed
    bool is_static : 1;     // declared as static

    // TODO: make this an ident.
    bool is_struct : 1;

    // Functions only.
    bool prototyped : 1;    // prototyped, implicitly via a definition or explicitly
    bool missing : 1;       // the function is not implemented in this source file
    bool callback : 1;      // used as a callback
    bool skipped : 1;       // skipped in codegen
    bool retvalue : 1;      // function returns (or should return) a value
    bool forward : 1;       // the function is explicitly forwardly declared
    bool native : 1;        // the function is native

    // Constants only.
    bool enumroot : 1;      // the constant is the "root" of an enumeration
    bool enumfield : 1;     // the constant is a field in a named enumeration
    bool predefined : 1;    // the constant is pre-defined and should be kept between passes

    // General symbol flags.
    bool deprecated : 1;    // symbol is deprecated (avoid use)
    bool queued : 1;        // symbol is queued for a local work algorithm

    union {
        struct {
            int index; /* array & enum: tag of array indices or the enum item */
            int field; /* enumeration fields, where a size is attached to the field */
        } tags;        /* extra tags */
    } x;               /* 'x' for 'extra' */
    union {
        constvalue* enumlist; /* list of names for the "root" of an enumeration */
        struct {
            cell length;  /* arrays: length (size) */
            cell slength; /* if a string index, this will be set to the original size */
            short level;  /* number of dimensions below this level */
        } array;
    } dim;       /* for 'dimension', both functions and arrays */
    int fnumber; /* file number in which the symbol is declared */
    int lnumber; /* line number for the declaration */
    ke::AString documentation; /* optional documentation string */
    methodmap_t* methodmap;    /* if ident == iMETHODMAP */

    int addr() const {
        return addr_;
    }
    void setAddr(int addr) {
        addr_ = addr;
    }
    sp::Atom* nameAtom() const {
        return name_;
    }
    const char* name() const {
        return name_ ? name_->chars() : "";
    }
    void setName(sp::Atom* name) {
        name_ = name;
    }
    FunctionData* function() const {
        assert(ident == iFUNCTN);
        return data_->asFunction();
    }
    symbol* parent() const {
        return parent_;
    }
    void set_parent(symbol* parent) {
        parent_ = parent;
    }

    symbol* array_return() const {
        assert(ident == iFUNCTN);
        return child_;
    }
    void set_array_return(symbol* child) {
        assert(ident == iFUNCTN);
        assert(!child_);
        child_ = child;
    }
    symbol* array_child() const {
        assert(ident == iARRAY || ident == iREFARRAY);
        return child_;
    }
    void set_array_child(symbol* child) {
        assert(ident == iARRAY || ident == iREFARRAY);
        assert(!child_);
        child_ = child;
    }
    SymbolData* data() const {
        return data_.get();
    }
    void set_data(ke::UniquePtr<SymbolData>&& data) {
        data_ = ke::Move(data);
    }

    void add_reference_to(symbol* other);
    void drop_reference_from(symbol* from);

    ke::Vector<symbol*>& refers_to() {
        return refers_to_;
    }
    bool is_unreferenced() const {
        return referred_from_count_ == 0;
    }
    void clear_refers() {
        refers_to_.clear();
        referred_from_.clear();
    }

  private:
    cell addr_; /* address or offset (or value for constant, index for native function) */
    sp::Atom* name_;
    ke::UniquePtr<SymbolData> data_;

    // Other symbols that this symbol refers to.
    ke::Vector<symbol*> refers_to_;

    // All the symbols that refer to this symbol.
    ke::Vector<symbol*> referred_from_;
    size_t referred_from_count_;

    symbol* parent_;
    symbol* child_;
};

// Values for symbol::usage.
#define uREAD       0x1     // Used/accessed.
#define uWRITTEN    0x2     // Altered/written (variables only).

#define uMAINFUNC "main"

#define sGLOBAL 0 /* global variable/constant class (no states) */
#define sLOCAL 1  /* local variable/constant */
#define sSTATIC 2 /* global life, local scope */

struct methodmap_method_t;

struct value {
    char ident;      /* iCONSTEXPR, iVARIABLE, iARRAY, iARRAYCELL,
                         * iEXPRESSION or iREFERENCE */
    /* symbol in symbol table, NULL for (constant) expression */
    symbol* sym;
    cell constval;   /* value of the constant expression (if ident==iCONSTEXPR)
                         * also used for the size of a literal array */
    int tag;         /* tag (of the expression) */

    // Returns whether the value can be rematerialized based on static
    // information, or whether it is the result of an expression.
    bool canRematerialize() const {
        switch (ident) {
            case iVARIABLE:
            case iCONSTEXPR:
                return true;
            case iREFERENCE:
                return sym->vclass == sLOCAL;
            default:
                return false;
        }
    }

    /* when ident == iACCESSOR */
    methodmap_method_t* accessor;

    static value ErrorValue() {
        value v = {};
        v.ident = iCONSTEXPR;
        return v;
    }
};

/* Wrapper around value + l/rvalue bit. */
struct svalue {
    value val;
    int lvalue;

    bool canRematerialize() const {
        return val.canRematerialize();
    }
};

#define DECLFLAG_ARGUMENT 0x02       // The declaration is for an argument.
#define DECLFLAG_VARIABLE 0x04       // The declaration is for a variable.
#define DECLFLAG_ENUMROOT 0x08       // Multi-dimensional arrays should have an enumroot.
#define DECLFLAG_MAYBE_FUNCTION 0x10 // Might be a named function.
#define DECLFLAG_DYNAMIC_ARRAYS 0x20 // Dynamic arrays are allowed.
#define DECLFLAG_OLD 0x40            // Known old-style declaration.
#define DECLFLAG_FIELD 0x80          // Struct field.
#define DECLFLAG_NEW 0x100           // Known new-style declaration.
#define DECLMASK_NAMED_DECL \
    (DECLFLAG_ARGUMENT | DECLFLAG_VARIABLE | DECLFLAG_MAYBE_FUNCTION | DECLFLAG_FIELD)

/* For parsing declarations. */
struct declinfo_t {
    char name[sNAMEMAX + 1];
    typeinfo_t type;
    int opertok; // Operator token, if applicable.
};

/*  "while" statement queue (also used for "for" and "do - while" loops) */
enum {
    wqBRK,  /* used to restore stack for "break" */
    wqCONT, /* used to restore stack for "continue" */
    wqLOOP, /* loop start label number */
    wqEXIT, /* loop exit label number (jump if false) */
    /* --- */
    wqSIZE /* "while queue" size */
};
#define wqTABSZ (24 * wqSIZE) /* 24 nested loop statements */

enum {
    statIDLE,  /* not compiling yet */
    statFIRST, /* first pass */
    statWRITE, /* writing output */
    statSKIP,  /* skipping output */
};

/* (reversed) evaluation of staging buffer */
#define sSTARTREORDER 0x01
#define sENDREORDER 0x02
#define sEXPRSTART 0x80 /* top bit set, rest is free */

#define sDOCSEP 0x01 /* to separate documentation comments between functions */

/* codes for ffabort() */
#define xEXIT 1           /* exit code in PRI */
#define xASSERTION 2      /* abort caused by failing assertion */
#define xSTACKERROR 3     /* stack/heap overflow */
#define xBOUNDSERROR 4    /* array index out of bounds */
#define xMEMACCESS 5      /* data access error */
#define xINVINSTR 6       /* invalid instruction */
#define xSTACKUNDERFLOW 7 /* stack underflow */
#define xHEAPUNDERFLOW 8  /* heap underflow */
#define xCALLBACKERR 9    /* no, or invalid, callback */
#define xSLEEP 12         /* sleep, exit code in PRI, tag in ALT */

/* Miscellaneous  */
#if !defined TRUE
#    define FALSE 0
#    define TRUE 1
#endif
#define sIN_CSEG 1     /* if parsing CODE */
#define sIN_DSEG 2     /* if parsing DATA */
#define sCHKBOUNDS 1   /* bit position in "debug" variable: check bounds */
#define sSYMBOLIC 2    /* bit position in "debug" variable: symbolic info */
#define sRESET 0       /* reset error flag */
#define sFORCESET 1    /* force error flag on */
#define sEXPRMARK 2    /* mark start of expression */
#define sEXPRRELEASE 3 /* mark end of expression */

#define CELL_MAX (((ucell)1 << (sizeof(cell) * 8 - 1)) - 1)

struct token_t;

/*
 * Functions you call from the "driver" program
 */
int pc_compile(int argc, char** argv);
int pc_addconstant(const char* name, cell value, int tag);
int pc_addtag(const char* name);
int pc_findtag(const char* name);
const char* pc_tagname(int tag);
int parse_decl(declinfo_t* decl, int flags);
const char* type_to_name(int tag);
bool parse_new_typename(const token_t* tok, int* tagp);

/* function prototypes in SC1.C */
void set_extension(char* filename, const char* extension, int force);
symbol* fetchfunc(const char* name);
char* operator_symname(char* symname, const char* opername, int tag1, int tag2, int numtags,
                       int resulttag);
char* funcdisplayname(char* dest, const char* funcname);
int exprconst(cell* val, int* tag, symbol** symptr);
constvalue* append_constval(constvalue* table, const char* name, cell val, int index);
constvalue* find_constval(constvalue* table, char* name, int index);
void delete_consttable(constvalue* table);
symbol* add_constant(const char* name, cell val, int vclass, int tag);
int get_actual_compound(symbol* sym);

#if defined WIN32
#    if !defined snprintf
#        define snprintf _snprintf
#        define vsnprintf _vsnprintf
#    endif
#endif

typedef struct array_info_s {
    const int* dim_list;          /* Dimension sizes */
    int dim_count;                /* Number of dimensions */
    const int* lastdim_list;      /* Sizes of last dimensions, if variable */
    const cell* dim_offs_precalc; /* Cached calculations into the lastdim_list array */
    cell* data_offs;              /* Current offset AFTER the indirection vectors (data) */
    int* cur_dims;                /* Current dimensions the recursion is at */
    cell* base;                   /* &litq[startlit] */
} array_info_t;

constexpr cell char_array_cells(cell size) {
    return (size + sizeof(cell) - 1) / sizeof(cell);
}

extern sp::StringPool gAtoms;

#endif /* SC_H_INCLUDED */
