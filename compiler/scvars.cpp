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
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "scvars.h"
#include "emitter.h"
#include "sc.h"

/*  global variables
 *
 *  All global variables that are shared amongst the compiler files are
 *  declared here.
 */
unsigned char pline[sLINEMAX + 1];         /* the line read from the input file */
const unsigned char* lptr;                 /* points to the current position in "pline" */
symbol* curfunc;                           /* pointer to current function */
char outfname[PATH_MAX];                   /* intermediate (assembler) file name */
char binfname[PATH_MAX];                   /* binary file name */
char errfname[PATH_MAX];                   /* error file name */
char sc_ctrlchar = CTRL_CHAR;              /* the control character (or escape character)*/
char sc_ctrlchar_org = CTRL_CHAR;          /* the default control character */
int sc_labnum = 0;                         /* number of (internal) labels */
cell glb_declared = 0;                     /* number of global cells declared */
cell code_idx = 0;                         /* number of bytes with generated code */
int sc_debug = sSYMBOLIC;                 /* by default: bounds checking+assertions */
int sc_asmfile = FALSE;                    /* create .ASM file? */
int sc_listing = FALSE;                    /* create .LST file? */
int sc_needsemicolon = TRUE;               /* semicolon required to terminate expressions? */
int curseg = 0;                            /* 1 if currently parsing CODE, 2 if parsing DATA */
cell pc_stksize = sDEF_AMXSTACK;           /* default stack size */
cell pc_stksize_override = 0;
int freading = FALSE;                      /* Is there an input file ready for reading? */
int fline = 0;                             /* the line number in the current file */
short fnumber = 0;                         /* the file number in the file table (debugging) */
int stmtindent = 0;                        /* current indent of the statement */
int indent_nowarn = FALSE;                 /* skip warning "217 loose indentation" */
int sc_tabsize = 8;                        /* number of spaces that a TAB represents */
int sc_err_status;
int sc_rationaltag = 0;              /* tag for rational numbers */
int sc_allowproccall = 0;            /* allow/detect tagnames in lex() */
short sc_is_utf8 = FALSE;            /* is this source file in UTF-8 encoding */
std::string pc_deprecate;            /* if non-empty, mark next declaration as deprecated */
int pc_optimize = sOPTIMIZE_NOMACRO; /* (peephole) optimization level */
int sc_showincludes = 0;             /* show include files */
int sc_require_newdecls = 0;         /* Require new-style declarations */
bool sc_warnings_are_errors = false;
int sc_compression_level = 9;
bool sc_use_new_parser = false;
int pc_max_func_memory = 0;          /* high stack watermark */
int pc_current_memory = 0;           /* current stack watermark */
int pc_max_memory = 0;               /* maximum stack watermark across all stacks */
int sc_use_stderr = FALSE;
int pc_current_stack = 0;
int sc_reparse = 0;

symbol* sScopeChain = nullptr;

std::shared_ptr<SourceFile> inpf;      /* file read from (source or include) */
std::shared_ptr<SourceFile> inpf_org;  /* main source file */

bool sc_intest;
bool sc_allowtags;
short fcurrent; /* current file being processed */

std::vector<short> gCurrentFileStack;
std::vector<int> gCurrentLineStack;
std::vector<std::shared_ptr<SourceFile>> gInputFileStack;
std::vector<bool> gNeedSemicolonStack;

jmp_buf errbuf;
