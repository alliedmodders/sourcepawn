// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
//  Copyright (c) ITB CompuPhase, 1997-2006
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
#pragma once

#include <amtl/am-string.h>

#include "sc.h"

// The method name buffer is larger since we can include our parent class's
// name, a "." to separate it, a "~" for constructors, or a ".get/.set" for
// accessors.
#define METHOD_NAMEMAX sNAMEMAX * 2 + 6

class Type;

struct token_pos_t {
    int file;
    int line;
    int col;
};

// Helper for token info.
struct token_t {
    int id;
    cell val;
    char* str;
};

struct token_ident_t {
    token_t tok;
    char name[METHOD_NAMEMAX + 1];
};

struct full_token_t {
    int id;
    int value;
    char str[sLINEMAX + 1];
    size_t len;
    token_pos_t start;
    token_pos_t end;
};

#define MAX_TOKEN_DEPTH 4

struct token_buffer_t {
    // Total number of tokens parsed.
    int num_tokens;

    // Number of tokens that we've rewound back to.
    int depth;

    // Most recently fetched token.
    int cursor;

    // Circular token buffer.
    full_token_t tokens[MAX_TOKEN_DEPTH];
};

/*  Tokens recognized by lex()
 *  Some of these constants are assigned as well to the variable "lastst" (see SC1.C)
 */
enum TokenKind {
    /* value of first multi-character operator */
    tFIRST = 256,
    /* multi-character operators */
    taMULT = tFIRST, /* *= */
    taDIV,           /* /= */
    taMOD,           /* %= */
    taADD,           /* += */
    taSUB,           /* -= */
    taSHL,           /* <<= */
    taSHRU,          /* >>>= */
    taSHR,           /* >>= */
    taAND,           /* &= */
    taXOR,           /* ^= */
    taOR,            /* |= */
    tlOR,            /* || */
    tlAND,           /* && */
    tlEQ,            /* == */
    tlNE,            /* != */
    tlLE,            /* <= */
    tlGE,            /* >= */
    tSHL,            /* << */
    tSHRU,           /* >>> */
    tSHR,            /* >> */
    tINC,            /* ++ */
    tDEC,            /* -- */
    tELLIPS,         /* ... */
    tDBLDOT,         /* .. */
    tDBLCOLON,       /* :: */
    /* value of last multi-character operator */
    tMIDDLE = tDBLCOLON,
    /* reserved words (statements) */
    tACQUIRE,
    tAS,
    tASSERT,
    tBREAK,
    tBUILTIN,
    tCATCH,
    tCASE,
    tCAST_TO,
    tCHAR,
    tCONST,
    tCONTINUE,
    tDECL,
    tDEFAULT,
    tDEFINED,
    tDELETE,
    tDO,
    tDOUBLE,
    tELSE,
    tENUM,
    tEXIT,
    tEXPLICIT,
    tFINALLY,
    tFOR,
    tFOREACH,
    tFORWARD,
    tFUNCENUM,
    tFUNCTAG,
    tFUNCTION,
    tGOTO,
    tIF,
    tIMPLICIT,
    tIMPORT,
    tIN,
    tINT,
    tINT8,
    tINT16,
    tINT32,
    tINT64,
    tINTERFACE,
    tINTN,
    tLET,
    tMETHODMAP,
    tNAMESPACE,
    tNATIVE,
    tNEW,
    tNULL,
    tNULLABLE,
    tOBJECT,
    tOPERATOR,
    tPACKAGE,
    tPRIVATE,
    tPROTECTED,
    tPUBLIC,
    tREADONLY,
    tRETURN,
    tSEALED,
    tSIZEOF,
    tSTATIC,
    tSTOCK,
    tSTRUCT,
    tSWITCH,
    tTHIS,
    tTHROW,
    tTRY,
    tTYPEDEF,
    tTYPEOF,
    tTYPESET,
    tUINT8,
    tUINT16,
    tUINT32,
    tUINT64,
    tUINTN,
    tUNION,
    tUSING,
    tVAR,
    tVARIANT,
    tVIEW_AS,
    tVIRTUAL,
    tVOID,
    tVOLATILE,
    tWHILE,
    tWITH,
    /* compiler directives */
    tpASSERT, /* #assert */
    tpDEFINE,
    tpELSE,   /* #else */
    tpELSEIF, /* #elseif */
    tpENDIF,
    tpENDINPUT,
    tpENDSCRPT,
    tpERROR,
    tpWARNING,
    tpFILE,
    tpIF, /* #if */
    tINCLUDE,
    tpLINE,
    tpPRAGMA,
    tpTRYINCLUDE,
    tpUNDEF,
    tLAST = tpUNDEF, /* value of last multi-character match-able token */
    /* semicolon is a special case, because it can be optional */
    tTERM,    /* semicolon or newline */
    tENDEXPR, /* forced end of expression */
    /* other recognized tokens */
    tNUMBER,   /* integer number */
    tRATIONAL, /* rational number */
    tSYMBOL,
    tLABEL,
    tSTRING,
    tEXPR,           /* for assigment to "lastst" only (see SC1.C) */
    tENDLESS,        /* endless loop, for assigment to "lastst" only */
    tEMPTYBLOCK,     /* empty blocks for AM bug 4825 */
    tEOL,            /* newline, only returned by peek_new_line() */
    tNEWDECL,        /* for declloc() */
    tLAST_TOKEN_ID
};

static inline bool
IsChainedOp(int token)
{
    switch (token) {
        case tlGE:
        case tlLE:
        case '>':
        case '<':
            return true;
        default:
            return false;
    }
}

static inline bool
IsAssignOp(int token)
{
    switch (token) {
        case taMULT:
        case taDIV:
        case taMOD:
        case taADD:
        case taSUB:
        case taSHL:
        case taSHR:
        case taSHRU:
        case taAND:
        case taXOR:
        case taOR:
        case '=':
            return true;
        default:
            return false;
    }
}

int plungequalifiedfile(char* name); /* explicit path included */
int plungefile(char* name, int try_currentpath,
               int try_includepaths); /* search through "include" paths */
void preprocess(void);
void lexinit(void);
int lex(cell* lexvalue, char** lexsym);
int lextok(token_t* tok);
int lexpeek(int id);
void lexpush(void);
void lexclr(int clreol);
const token_pos_t& current_pos();
int matchtoken(int token);
int tokeninfo(cell* val, char** str);
full_token_t* current_token();
int needtoken(int token);
int matchtoken2(int id, token_t* tok);
int expecttoken(int id, token_t* tok);
int matchsymbol(token_ident_t* ident);
int needsymbol(token_ident_t* ident);
int peek_same_line();
void litadd(const char* str, size_t len);
int alphanum(char c);
int ishex(char c);
int isoctal(char c);
void delete_symbol(symbol* root, symbol* sym);
void delete_symbols(symbol* root, int level, int delete_functions);
void markusage(symbol* sym, int usage);
symbol* findglb(const char* name);
symbol* findloc(const char* name);
symbol* findconst(const char* name);
symbol* find_enumstruct_field(Type* type, const char* name);
symbol* addsym(const char* name, cell addr, int ident, int vclass, int tag);
symbol* addvariable(const char* name, cell addr, int ident, int vclass, int tag, int dim[],
                    int numdim, int idxtag[]);
symbol* addvariable2(const char* name, cell addr, int ident, int vclass, int tag, int dim[],
                     int numdim, int idxtag[], int slength);
symbol* addvariable3(declinfo_t* decl, cell addr, int vclass, int slength);
void declare_methodmap_symbol(methodmap_t* map, bool can_redef);
void declare_handle_intrinsics();
int getlabel(void);
char* itoh(ucell val);
std::string get_token_string(int tok_id);

enum class TerminatorPolicy { Newline, NewlineOrSemicolon, Semicolon };

int require_newline(TerminatorPolicy policy);
