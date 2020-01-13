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

#include "sc.h"

enum {
    sOPTIMIZE_NONE,    /* no optimization */
    sOPTIMIZE_NOMACRO, /* no macro instructions */
    sOPTIMIZE_DEFAULT, /* full optimization */
    /* ----- */
    sOPTIMIZE_NUMBER
};

typedef enum s_regid {
    sPRI, /* indicates the primary register */
    sALT, /* indicates the secundary register */
} regid;

typedef enum s_optmark {
    sEXPR,  /* end of expression (for expressions that form a statement) */
    sPARM,  /* end of parameter (in a function parameter list) */
    sLDECL, /* start of local declaration (variable) */
} optmark;

void writeleader(symbol* root);
void writetrailer(void);
void begcseg(void);
void begdseg(void);
void setline(int chkbounds);
void setfiledirect(char* name);
void setlinedirect(int line);
void setlabel(int index);
void markexpr(optmark type, const char* name, cell offset);
void startfunc(const char* fname);
void endfunc(void);
void rvalue(value* lval);
void rvalue(svalue* sval);
void rvalue(const value& val);
void address(symbol* ptr, regid reg);
void store(const value* lval);
void loadreg(cell address, regid reg);
void storereg(cell address, regid reg);
void memcopy(cell size);
void copyarray(symbol* sym, cell size);
void fillarray(symbol* sym, cell size, cell value);
void ldconst(cell val, regid reg);
void moveto1(void);
void move_alt(void);
void pushreg(regid reg);
void pushval(cell val);
void popreg(regid reg);
void genarray(int dims, int _autozero);
void swap1(void);
void ffswitch(int label);
void ffcase(cell value, char* labelname, int newtable);
void ffcall(symbol* sym, int numargs);
void ffret();
void ffabort(int reason);
void ffbounds(cell size);
void ffbounds();
void jumplabel(int number);
void defstorage(void);
void modstk(int delta);
void modheap(int delta);
void modheap_i();
void setheap_pri(void);
void setheap(cell value);
void cell2addr(void);
void cell2addr_alt(void);
void char2addr(void);
void addconst(cell value);
void setheap_save(cell value);
void stradjust(regid reg);
void invoke_getter(methodmap_method_t* method);
void invoke_setter(methodmap_method_t* method, int save);
void inc_pri();
void dec_pri();
void load_hidden_arg();
void load_glbfn(symbol* sym);

/*  Code generation functions for arithmetic operators.
 *
 *  Syntax: o[u|s|b]_name
 *          |   |   | +--- name of operator
 *          |   |   +----- underscore
 *          |   +--------- "u"nsigned operator, "s"igned operator or "b"oth
 *          +------------- "o"perator
 */
void os_mult(void); /* multiplication (signed) */
void os_div(void);  /* division (signed) */
void os_mod(void);  /* modulus (signed) */
void ob_add(void);  /* addition */
void ob_sub(void);  /* subtraction */
void ob_sal(void);  /* shift left (arithmetic) */
void os_sar(void);  /* shift right (arithmetic, signed) */
void ou_sar(void);  /* shift right (logical, unsigned) */
void ob_or(void);   /* bitwise or */
void ob_xor(void);  /* bitwise xor */
void ob_and(void);  /* bitwise and */
void ob_eq(void);   /* equality */
void ob_ne(void);   /* inequality */
void relop_prefix(void);
void relop_suffix(void);
void os_le(void); /* less or equal (signed) */
void os_ge(void); /* greater or equal (signed) */
void os_lt(void); /* less (signed) */
void os_gt(void); /* greater (signed) */

void lneg(void);
void neg(void);
void invert(void);
void nooperation(void);
void inc(const value* lval);
void dec(const value* lval);
void jmp_ne0(int number);
void jmp_eq0(int number);
void outval(cell val, int newline);
void litadd(cell value);
void litinsert(cell value, int pos);
cell dumplits();
void dumpzero(int count);

/* macros for code generation */
#define opcodes(n) ((n) * sizeof(cell)) /* opcode size */
#define opargs(n) ((n) * sizeof(cell))  /* size of typical argument */
