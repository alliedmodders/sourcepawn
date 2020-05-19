/*  Pawn compiler
 *
 *  Global (cross-module) variables.
 *
 *  Copyright (c) ITB CompuPhase, 1997-2006
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
#include "scvars.h"
#include <stdio.h>
#include <stdlib.h> /* for _MAX_PATH */
#include "emitter.h"
#include "sc.h"
#include "sp_symhash.h"

/*  global variables
 *
 *  All global variables that are shared amongst the compiler files are
 *  declared here.
 */
symbol loctab;                             /* local symbol table */
symbol glbtab;                             /* global symbol table */
cell* litq;                                /* the literal queue */
unsigned char pline[sLINEMAX + 1];         /* the line read from the input file */
const unsigned char* lptr;                 /* points to the current position in "pline" */
constvalue tagname_tab = {NULL, "", 0, 0}; /* tagname table */
constvalue libname_tab = {NULL, "", 0, 0}; /* library table (#pragma library "..." syntax) */
constvalue* curlibrary = NULL;             /* current library */
int pc_addlibtable = TRUE;                 /* is the library table added to the AMX file? */
symbol* curfunc;                           /* pointer to current function */
char* inpfname;                            /* pointer to name of the file currently read from */
char outfname[_MAX_PATH];                  /* intermediate (assembler) file name */
char binfname[_MAX_PATH];                  /* binary file name */
char errfname[_MAX_PATH];                  /* error file name */
char sc_ctrlchar = CTRL_CHAR;              /* the control character (or escape character)*/
char sc_ctrlchar_org = CTRL_CHAR;          /* the default control character */
int litidx = 0;                            /* index to literal table */
int litmax = sDEF_LITMAX;                  /* current size of the literal table */
int stgidx = 0;                            /* index to the staging buffer */
int sc_labnum = 0;                         /* number of (internal) labels */
int staging = 0;                           /* true if staging output */
cell declared = 0;                         /* number of local cells declared */
cell glb_declared = 0;                     /* number of global cells declared */
cell code_idx = 0;                         /* number of bytes with generated code */
int errnum = 0;                            /* number of errors */
int warnnum = 0;                           /* number of warnings */
int sc_debug = sCHKBOUNDS;                 /* by default: bounds checking+assertions */
int sc_asmfile = FALSE;                    /* create .ASM file? */
int sc_listing = FALSE;                    /* create .LST file? */
int sc_needsemicolon = TRUE;               /* semicolon required to terminate expressions? */
int sc_dataalign = sizeof(cell);           /* data alignment value */
int curseg = 0;                            /* 1 if currently parsing CODE, 2 if parsing DATA */
cell pc_stksize = sDEF_AMXSTACK;           /* default stack size */
int freading = FALSE;                      /* Is there an input file ready for reading? */
int fline = 0;                             /* the line number in the current file */
short fnumber = 0;                         /* the file number in the file table (debugging) */
int sideeffect = 0;                        /* true if an expression causes a side-effect */
int stmtindent = 0;                        /* current indent of the statement */
int indent_nowarn = FALSE;                 /* skip warning "217 loose indentation" */
int sc_tabsize = 8;                        /* number of spaces that a TAB represents */
int sc_status;                             /* read/write status */
int sc_err_status;
int sc_rationaltag = 0;              /* tag for rational numbers */
int sc_allowproccall = 0;            /* allow/detect tagnames in lex() */
short sc_is_utf8 = FALSE;            /* is this source file in UTF-8 encoding */
std::string pc_deprecate;            /* if non-empty, mark next declaration as deprecated */
int pc_optimize = sOPTIMIZE_NOMACRO; /* (peephole) optimization level */
int pc_memflags = 0;                 /* special flags for the stack/heap usage */
int sc_showincludes = 0;             /* show include files */
int sc_require_newdecls = 0;         /* Require new-style declarations */
bool sc_warnings_are_errors = false;
int sc_compression_level = 9;
bool sc_use_new_parser = false;

void* inpf = NULL;      /* file read from (source or include) */
void* inpf_org = NULL;  /* main source file */
memfile_t* outf = NULL; /* (intermediate) text file written to */

bool sc_intest;
bool sc_allowtags;
short fcurrent; /* current file being processed */

ke::Vector<short> gCurrentFileStack;
ke::Vector<int> gCurrentLineStack;
ke::Vector<void*> gInputFileStack;
ke::Vector<char*> gInputFilenameStack;

jmp_buf errbuf;

HashTable* sp_Globals = NULL;
