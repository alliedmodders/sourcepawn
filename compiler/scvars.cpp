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
int curseg = 0;                            /* 1 if currently parsing CODE, 2 if parsing DATA */
cell pc_stksize = sDEF_AMXSTACK;           /* default stack size */
cell pc_stksize_override = 0;
int freading = FALSE;                      /* Is there an input file ready for reading? */
int fline = 0;                             /* the line number in the current file */
short fnumber = 0;                         /* the file number in the file table (debugging) */
int sc_rationaltag = 0;              /* tag for rational numbers */
int sc_allowproccall = 0;            /* allow/detect tagnames in lex() */
short sc_is_utf8 = FALSE;            /* is this source file in UTF-8 encoding */
bool sc_use_new_parser = false;
int pc_max_func_memory = 0;          /* high stack watermark */
int pc_current_memory = 0;           /* current stack watermark */
int pc_max_memory = 0;               /* maximum stack watermark across all stacks */
int pc_current_stack = 0;

symbol* sScopeChain = nullptr;

std::shared_ptr<SourceFile> inpf;      /* file read from (source or include) */
std::shared_ptr<SourceFile> inpf_org;  /* main source file */

short fcurrent; /* current file being processed */

jmp_buf errbuf;
