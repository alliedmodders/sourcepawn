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

#include <setjmp.h>

#include <vector>

#include <amtl/am-string.h>
#include <amtl/am-vector.h>

#include "sc.h"
#include "source-file.h"

class SemaContext;
extern SemaContext* gCurrentSemaContext;

extern unsigned char pline[];     /* the line read from the input file */
extern const unsigned char* lptr; /* points to the current position in "pline" */
extern symbol* curfunc;           /* pointer to current function */
extern char outfname[];           /* intermediate (assembler) file name */
extern char binfname[];           /* binary file name */
extern char errfname[];           /* error file name */
extern char sc_ctrlchar;          /* the control character (or escape character) */
extern char sc_ctrlchar_org;      /* the default control character */
extern int sc_labnum;             /* number of (internal) labels */
extern cell glb_declared;         /* number of global cells declared */
extern cell code_idx;             /* number of bytes with generated code */
extern int errnum;                /* number of errors */
extern int warnnum;               /* number of warnings */
extern int sc_debug;              /* debug/optimization options (bit field) */
extern int sc_asmfile;            /* create .ASM file? */
extern int sc_listing;            /* create .LST file? */
extern int sc_needsemicolon;      /* semicolon required to terminate expressions? */
extern int sc_showincludes;       /* show include files? */
extern int curseg;                /* 1 if currently parsing CODE, 2 if parsing DATA */
extern cell pc_stksize;           /* stack size */
extern cell pc_stksize_override;  /* stack size override */
extern int freading;              /* is there an input file ready for reading? */
extern int fline;                 /* the line number in the current file */
extern short fnumber;             /* number of files in the input file table */
extern int sideeffect;            /* true if an expression causes a side-effect */
extern int stmtindent;            /* current indent of the statement */
extern int indent_nowarn;         /* skip warning "217 loose indentation" */
extern int sc_tabsize;            /* number of spaces that a TAB represents */
extern int sc_status;             /* read/write status */
extern int sc_err_status;         /* TRUE if errors should be generated even if sc_status = SKIP */
extern int sc_rationaltag;        /* tag for rational numbers */
extern int pc_functag;            /* global function tag */
extern int pc_tag_string;         /* global String tag */
extern int pc_tag_void;           /* global void tag */
extern int pc_tag_object;         /* root object tag */
extern int pc_tag_bool;           /* global bool tag */
extern int pc_tag_null_t;         /* the null type */
extern int pc_tag_nullfunc_t;     /* the null function type */
extern int pc_anytag;             /* global any tag */
extern int sc_require_newdecls;   /* only newdecls are allowed */
extern bool sc_warnings_are_errors;
extern int sc_compression_level;
extern int pc_max_func_memory;    /* high stack watermark */
extern int pc_current_memory;     /* current stack watermark */
extern int pc_max_memory;         /* maximum stack watermark across all stacks */
extern int pc_current_stack;
extern int sc_use_stderr;
extern int sc_reparse;            /* needs 3th parse because of changed prototypes? */

extern std::shared_ptr<SourceFile> inpf;      /* file read from (source or include) */
extern std::shared_ptr<SourceFile> inpf_org;  /* main source file */

extern jmp_buf errbuf; /* target of longjmp() on a fatal error */

extern std::string pc_deprecate;

extern bool sc_intest;
extern bool sc_allowtags;
extern short fcurrent; /* current file being processed */

extern std::vector<short> gCurrentFileStack;
extern std::vector<int> gCurrentLineStack;
extern std::vector<std::shared_ptr<SourceFile>> gInputFileStack;
