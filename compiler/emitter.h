// vim: set ts=8 sts=4 sw=4 tw=99 et:
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
#include "symbols.h"

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

class DataQueue final
{
  public:
    DataQueue();

    void Add(cell value);
    void Add(PoolList<cell>&& cells);
    void Add(const char* text, size_t length);
    void AddZeroes(cell count);
    void Reset();
    void Emit();
    bool Compact();

    cell size() const {
      return size_;
    }
    cell dat_address() const;

  private:
    // A "run" of data is a vector of non-zero cells followed by a count of
    // zero cells.
    struct DataRun {
        DataRun()
         : zeroes(0)
        {}
        DataRun(DataRun&& run)
         : cells(std::move(run.cells)),
           zeroes(run.zeroes)
        {}

        PoolList<cell> cells;
        cell zeroes;
    };
    DataRun& current() {
      return data_.back();
    }

  private:
    std::vector<DataRun> data_;
    cell size_;
};
extern DataQueue gDataQueue;

void writeleader();
void writetrailer(void);
void begcseg(void);
void begdseg(void);
void setline(int chkbounds);
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
void emit_fill(cell val, cell size);
void ldconst(cell val, regid reg);
void moveto1(void);
void move_alt(void);
void pushreg(regid reg);
void pushval(cell val);
void popreg(regid reg);
void genarray(int dims, bool autozero);
void swap1(void);
void ffswitch(int label);
void ffcase(cell value, const char* labelname, int newtable);
void ffcall(symbol* sym, int numargs);
void ffret();
void ffabort(int reason);
void ffbounds(cell size);
void ffbounds();
void jumplabel(int number);
void jumplabel_cond(int token, int number);
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
void load_hidden_arg(symbol* fun, symbol* sym, bool save_pri);
void load_glbfn(symbol* sym);
void addr_reg(int val, regid reg);
void emit_initarray(regid reg, cell base_addr, cell iv_size, cell data_copy_size,
                    cell data_fill_size, cell fill_value);
void load_i();
void idxaddr();
void emit_default_array(arginfo* arg);

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
