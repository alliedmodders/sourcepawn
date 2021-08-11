// vim: set ts=8 sts=4 sw=4 tw=99 et:
/*  Pawn compiler - code generation (unoptimized "assembler" code)
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
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h> /* for _MAX_PATH */
#include <string.h>

#include <amtl/am-raii.h>

#include "array-helpers.h"
#include "emitter.h"
#include "errors.h"
#include "lexer.h"
#include "lexer-inl.h"
#include "libpawnc.h"
#include "output-buffer.h"
#include "sc.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "symbols.h"

/* macros for code generation */
#define opcodes(n) ((n) * sizeof(cell)) /* opcode size */
#define opargs(n) ((n) * sizeof(cell))  /* size of typical argument */

DataQueue gDataQueue;
static int fcurseg; /* the file number (fcurrent) for the active segment */

static inline void
stgwrite(const char* st)
{
    if (sc_status == statWRITE)
        gAsmBuffer << st;
}

DataQueue::DataQueue()
{
    Reset();
}

void
DataQueue::Reset()
{
    data_.clear();
    data_.emplace_back(DataRun());
    size_ = 0;
}

cell
DataQueue::dat_address() const
{
    return (glb_declared + size_) * sizeof(cell);
}

void
DataQueue::Add(cell value)
{
    size_++;

    if (!value) {
        current().zeroes++;
        return;
    }

    if (!Compact())
        data_.emplace_back(DataRun());

    current().cells.emplace_back(value);
}

bool
DataQueue::Compact()
{
    if (!current().zeroes)
        return true;

    // Either collapse a small number of zeroes into actual values, or
    // start a new data run. We collapse if there are only a few zeroes,
    // since the point is to avoid dumping huge DATA lines in the assembly.
    if (current().zeroes < 16) {
        current().cells.resize(current().cells.size() + current().zeroes);
        current().zeroes = 0;
        return true;
    }
    return false;
}

void
DataQueue::Add(const char* text, size_t length)
{
    StringToCells(text, length, [this](cell value) -> void {
        Add(value);
    });
}

void
DataQueue::Add(std::vector<cell>&& cells)
{
    if (cells.empty())
        return;

    // Always start a new run.
    if (!current().cells.empty() || current().zeroes)
        data_.emplace_back(DataRun());

    size_ += cells.size();
    current().cells = std::move(cells);
}

void
DataQueue::AddZeroes(cell count)
{
    size_ += count;
    current().zeroes += count;
}

class DataEmitter final
{
  public:
    DataEmitter();

    void Dump(const std::vector<cell>& vals);
    void DumpZeroes(cell nzeroes);
    void Finish();

  private:
    size_t col_ = 0;
};

DataEmitter::DataEmitter()
{
    assert(curseg == 2);
}

void
DataEmitter::Dump(const std::vector<cell>& vals)
{
    for (const auto& val : vals) {
        if (col_ == 0)
            defstorage();
        else
            stgwrite(" ");
        outval(val, FALSE);

        col_++;
        if (col_ == 16) {
            stgwrite("\n");
            col_ = 0;
        }
    }
}

void
DataEmitter::Finish()
{
    if (col_) {
        stgwrite("\n");
        col_ = 0;
    }
}

void
DataEmitter::DumpZeroes(cell nzeroes)
{
    if (sc_status == statSKIP || nzeroes <= 0)
        return;

    Finish();

    stgwrite("dumpfill ");
    outval(0, FALSE);
    stgwrite(" ");
    outval(nzeroes, TRUE);
}

void
DataQueue::Emit()
{
    Compact();

    if (!size_)
        return;

    begdseg();

    DataEmitter de;
    for (const auto& run : data_) {
        de.Dump(run.cells);
        if (run.zeroes)
            de.DumpZeroes(run.zeroes);
    }

    de.Finish();

    glb_declared += size_;
    Reset();
}

/* When a subroutine returns to address 0, the AMX must halt. In earlier
 * releases, the RET and RETN opcodes checked for the special case 0 address.
 * Today, the compiler simply generates a HALT instruction at address 0. So
 * a subroutine can savely return to 0, and then encounter a HALT.
 */
void
writeleader(symbol* root)
{
    assert(code_idx == 0);

    begcseg();
    stgwrite(";program exit point\n");
    stgwrite("\thalt 0\n\n");
    code_idx += opcodes(1) + opargs(1); /* calculate code length */
}

static constexpr cell sc_dataalign = sizeof(cell);

/*  writetrailer
 *  Not much left of this once important function.
 *
 *  Global references: pc_stksize       (referred to only)
 *                     sc_dataalign     (referred to only)
 *                     code_idx         (altered)
 *                     glb_declared     (altered)
 */
void
writetrailer(void)
{
    assert(sc_dataalign % opcodes(1) == 0); /* alignment must be a multiple of
                                             * the opcode size */
    assert(sc_dataalign != 0);

    /* pad code to align data segment */
    if ((code_idx % sc_dataalign) != 0) {
        begcseg();
        while ((code_idx % sc_dataalign) != 0)
            nooperation();
    }

    /* pad data segment to align the stack and the heap */
    assert(sc_dataalign % sizeof(cell) == 0);
    if (((glb_declared * sizeof(cell)) % sc_dataalign) != 0) {
        begdseg();
        defstorage();
        while (((glb_declared * sizeof(cell)) % sc_dataalign) != 0) {
            stgwrite("0 ");
            glb_declared++;
        }
    }

    stgwrite("\nSTKSIZE "); /* write stack size (align stack top) */
    outval(pc_stksize - (pc_stksize % sc_dataalign), TRUE);
}

/*
 *  Start (or restart) the CODE segment.
 *
 *  In fact, the code and data segment specifiers are purely informational;
 *  the "DUMP" instruction itself already specifies that the following values
 *  should go to the data segment. All other instructions go to the code
 *  segment.
 *
 *  Global references: curseg
 *                     fcurrent
 */
void
begcseg(void)
{
    if (sc_status != statSKIP && (curseg != sIN_CSEG || fcurrent != fcurseg)) {
        stgwrite("\n");
        stgwrite("CODE ");
        outval(fcurrent, FALSE);
        stgwrite("\t; ");
        outval(code_idx, TRUE);
        curseg = sIN_CSEG;
        fcurseg = fcurrent;
    } /* endif */
}

/*
 *  Start (or restart) the DATA segment.
 *
 *  Global references: curseg
 */
void
begdseg(void)
{
    if (sc_status != statSKIP && (curseg != sIN_DSEG || fcurrent != fcurseg)) {
        stgwrite("\n");
        stgwrite("DATA ");
        outval(fcurrent, FALSE);
        stgwrite("\t; ");
        outval(gDataQueue.dat_address(), TRUE);
        curseg = sIN_DSEG;
        fcurseg = fcurrent;
    }
}

void
setline(int chkbounds)
{
    static cell last_code_idx = -1;

    if (last_code_idx == code_idx)
        return;

    if (sc_asmfile) {
        stgwrite("\t; line ");
        outval(fline, TRUE);
    }
    if ((sc_debug & sSYMBOLIC) != 0 || (chkbounds && (sc_debug & sCHKBOUNDS) != 0)) {
        /* generate a "break" (start statement) opcode rather than a "line" opcode
         * because earlier versions of Small/Pawn have an incompatible version of the
         * line opcode
         */
        stgwrite("\tbreak\t; ");
        outval(code_idx, TRUE);
        code_idx += opcodes(1);
    }
    last_code_idx = code_idx;
}

void
setfiledirect(char* name)
{
    if (sc_status == statFIRST && sc_listing) {
        assert(name != NULL);
        gAsmBuffer << "#file " << name << "\n";
    }
}

void
setlinedirect(int line)
{
    if (sc_status == statFIRST && sc_listing)
        gAsmBuffer << "#line " << line << "\n";
}

/*  setlabel
 *
 *  Post a code label (specified as a number), on a new line.
 */
void
setlabel(int number)
{
    assert(number >= 0);
    stgwrite("l.");
    stgwrite((char*)itoh(number));
    /* To assist verification of the assembled code, put the address of the
     * label as a comment.
     */
    stgwrite("\t\t; ");
    outval(code_idx, FALSE);
    stgwrite("\n");
}

/* Write a token that signifies the start or end of an expression or special
 * statement. This allows several simple optimizations by the peephole
 * optimizer.
 */
void
markexpr(optmark type, const char* name, cell offset)
{
    switch (type) {
        case sEXPR:
            stgwrite("\t;$exp\n");
            break;
        case sPARM:
            stgwrite("\t;$par\n");
            break;
        case sLDECL:
            assert(name != NULL);
            stgwrite("\t;$lcl ");
            stgwrite(name);
            stgwrite(" ");
            outval(offset, TRUE);
            break;
        default:
            assert(0);
    }
}

/*  startfunc   - declare a CODE entry point (function start)
 *
 *  Global references: funcstatus  (referred to only)
 */
void
startfunc(const char* fname)
{
    stgwrite("\tproc");
    if (sc_asmfile) {
        auto symname = funcdisplayname(fname);
        stgwrite("\t; ");
        stgwrite(symname.c_str());
    }
    stgwrite("\n");
    code_idx += opcodes(1);
}

/*  endfunc
 *
 *  Declare a CODE ending point (function end)
 */
void
endfunc(void)
{
    stgwrite("\tendproc\n");
    stgwrite("\n"); /* skip a line */
    code_idx += opcodes(1);
}

/*  rvalue
 *
 *  Generate code to get the value of a symbol into "primary".
 */
void
rvalue(value* lval)
{
    symbol* sym;

    sym = lval->sym;
    if (lval->ident == iARRAYCELL) {
        /* indirect fetch, address already in PRI */
        load_i();
    } else if (lval->ident == iARRAYCHAR) {
        /* indirect fetch of a character from a pack, address already in PRI */
        stgwrite("\tlodb.i ");
        outval(sCHARBITS / 8, TRUE); /* read one or two bytes */
        code_idx += opcodes(1) + opargs(1);
    } else if (lval->ident == iREFERENCE) {
        /* indirect fetch, but address not yet in PRI */
        assert(sym != NULL);
        assert(sym->vclass == sLOCAL || sym->vclass == sARGUMENT); /* global references don't exist in Pawn */
        stgwrite("\tlref.s.pri ");
        outval(sym->addr(), TRUE);
        markusage(sym, uREAD);
        code_idx += opcodes(1) + opargs(1);
    } else if (lval->ident == iACCESSOR) {
        invoke_getter(lval->accessor);
        lval->ident = iEXPRESSION;
        lval->accessor = NULL;
    } else {
        /* direct or stack relative fetch */
        assert(sym != NULL);
        if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
            stgwrite("\tload.s.pri ");
        else
            stgwrite("\tload.pri ");
        outval(sym->addr(), TRUE);
        markusage(sym, uREAD);
        code_idx += opcodes(1) + opargs(1);
    }
}

// Wrapper that automatically markes lvalues as decayed if they are accessors,
// since it is illegal to evaluate them twice.
void
rvalue(svalue* sval)
{
    int ident = sval->val.ident;
    rvalue(&sval->val);
    if (ident == iACCESSOR)
        sval->lvalue = FALSE;
}

// Wrapper for new codegen, where we don't usually have a mutable value.
void
rvalue(const value& val)
{
    value tmp = val;
    rvalue(&tmp);
}

/* Get the address of a symbol into the primary or alternate register (used
 * for arrays, and for passing arguments by reference).
 */
void
address(symbol* sym, regid reg)
{
    assert(sym != NULL);
    assert(reg == sPRI || reg == sALT);
    /* the symbol can be a local array, a global array, or an array
     * that is passed by reference.
     */
    if (sym->ident == iREFARRAY || sym->ident == iREFERENCE) {
        /* reference to a variable or to an array; currently this is
         * always a local variable */
        switch (reg) {
            case sPRI:
                stgwrite("\tload.s.pri ");
                break;
            case sALT:
                stgwrite("\tload.s.alt ");
                break;
        }
    } else {
        /* a local array or local variable */
        switch (reg) {
            case sPRI:
                if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
                    stgwrite("\taddr.pri ");
                else
                    stgwrite("\tconst.pri ");
                break;
            case sALT:
                if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
                    stgwrite("\taddr.alt ");
                else
                    stgwrite("\tconst.alt ");
                break;
        }
    }
    outval(sym->addr(), TRUE);
    markusage(sym, uREAD);
    code_idx += opcodes(1) + opargs(1);
}

void
addr_reg(int val, regid reg)
{
    if (reg == sPRI)
        stgwrite("\taddr.pri ");
    else
        stgwrite("\taddr.alt ");
    outval(val, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

// Load the number of arguments into PRI. Frame layout:
//   base + 0*sizeof(cell) == previous "base"
//   base + 1*sizeof(cell) == function return address
//   base + 2*sizeof(cell) == number of arguments
//   base + 3*sizeof(cell) == first argument of the function
static void
load_argcount(regid reg)
{
    if (reg == sPRI)
        stgwrite("\tload.s.pri ");
    else
        stgwrite("\tload.s.alt ");
    outval(2 * sizeof(cell), TRUE);
    code_idx += opcodes(1) + opargs(1);
}

// PRI = ALT + (PRI * cellsize)
void
idxaddr()
{
    stgwrite("\tidxaddr\n");
    code_idx += opcodes(1);
}

void
load_i()
{
    stgwrite("\tload.i\n");
    code_idx += opcodes(1);
}

// Load the hidden array argument into ALT, preserving PRI.
void
load_hidden_arg(symbol* fun, symbol* sym, bool save_pri)
{
    if (!is_variadic(fun)) {
        address(sym, sALT);
        return;
    }

    // Compute an address to the first argument, then add the argument count
    // to find the address after the final argument:
    //    push.pri
    //    addr.alt   0xc   ; Compute &first_arg
    //    load.s.alt 0x8   ; Load arg count
    //    idxaddr          ; Compute (&first_arg) + argcount
    //    load.i           ; Load *(&first_arg + argcount)
    //    move.alt
    //    pop.pri
    if (save_pri)
        pushreg(sPRI);
    addr_reg(0xc, sALT);
    load_argcount(sPRI);
    idxaddr();
    load_i();
    move_alt();
    if (save_pri)
        popreg(sPRI);
}

/*  store
 *
 *  Saves the contents of "primary" into a memory cell, either directly
 *  or indirectly (at the address given in the alternate register).
 */
void
store(const value* lval)
{
    symbol* sym;

    sym = lval->sym;
    if (lval->ident == iARRAYCELL) {
        /* store at address in ALT */
        stgwrite("\tstor.i\n");
        code_idx += opcodes(1);
    } else if (lval->ident == iARRAYCHAR) {
        /* store at address in ALT */
        stgwrite("\tstrb.i ");
        outval(sCHARBITS / 8, TRUE); /* write one or two bytes */
        code_idx += opcodes(1) + opargs(1);
    } else if (lval->ident == iREFERENCE) {
        assert(sym != NULL);
        assert(sym->vclass == sLOCAL || sym->vclass == sARGUMENT);
        stgwrite("\tsref.s.pri ");
        outval(sym->addr(), TRUE);
        code_idx += opcodes(1) + opargs(1);
    } else if (lval->ident == iACCESSOR) {
        invoke_setter(lval->accessor, TRUE);
    } else {
        assert(sym != NULL);
        markusage(sym, uWRITTEN);
        if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
            stgwrite("\tstor.s.pri ");
        else
            stgwrite("\tstor.pri ");
        outval(sym->addr(), TRUE);
        code_idx += opcodes(1) + opargs(1);
    }
}

/* Get a cell from a fixed address in memory */
void
loadreg(cell address, regid reg)
{
    assert(reg == sPRI || reg == sALT);
    if (reg == sPRI)
        stgwrite("\tload.pri ");
    else
        stgwrite("\tload.alt ");
    outval(address, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/* Store a cell into a fixed address in memory */
void
storereg(cell address, regid reg)
{
    assert(reg == sPRI || reg == sALT);
    if (reg == sPRI)
        stgwrite("\tstor.pri ");
    else
        stgwrite("\tstor.alt ");
    outval(address, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/* source must in PRI, destination address in ALT. The "size"
 * parameter is in bytes, not cells.
 */
void
memcopy(cell size)
{
    stgwrite("\tmovs ");
    outval(size, TRUE);

    code_idx += opcodes(1) + opargs(1);
}

/* Address of the source must already have been loaded in PRI
 * "size" is the size in bytes (not cells).
 */
void
copyarray(symbol* sym, cell size)
{
    assert(sym != NULL);
    /* the symbol can be a local array, a global array, or an array
     * that is passed by reference.
     */
    if (sym->ident == iREFARRAY) {
        /* reference to an array; currently this is always a local variable */
        assert(sym->vclass == sLOCAL || sym->vclass == sARGUMENT); /* symbol must be stack relative */
        stgwrite("\tload.s.alt ");
    } else {
        /* a local or global array */
        if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
            stgwrite("\taddr.alt ");
        else
            stgwrite("\tconst.alt ");
    }
    outval(sym->addr(), TRUE);
    markusage(sym, uWRITTEN);

    code_idx += opcodes(1) + opargs(1);
    memcopy(size);
}

void
fillarray(symbol* sym, cell size, cell value)
{
    ldconst(value, sPRI); /* load value in PRI */

    assert(sym != NULL);
    /* the symbol can be a local array, a global array, or an array
     * that is passed by reference.
     */
    if (sym->ident == iREFARRAY) {
        /* reference to an array; currently this is always a local variable */
        assert(sym->vclass == sLOCAL || sym->vclass == sARGUMENT); /* symbol must be stack relative */
        stgwrite("\tload.s.alt ");
    } else {
        /* a local or global array */
        if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
            stgwrite("\taddr.alt ");
        else
            stgwrite("\tconst.alt ");
    }
    outval(sym->addr(), TRUE);
    markusage(sym, uWRITTEN);

    assert(size > 0);
    stgwrite("\tfill ");
    outval(size, TRUE);

    code_idx += opcodes(2) + opargs(2);
}

void
emit_fill(cell val, cell size)
{
    assert(size > 0);

    ldconst(val, sPRI);

    stgwrite("\tfill ");
    outval(size, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

void
stradjust(regid reg)
{
    assert(reg == sPRI);
    stgwrite("\tstradjust.pri\n");
    code_idx += opcodes(1);
}

/* Instruction to get an immediate value into the primary or the alternate
 * register
 */
void
ldconst(cell val, regid reg)
{
    assert(reg == sPRI || reg == sALT);
    switch (reg) {
        case sPRI:
            if (val == 0) {
                stgwrite("\tzero.pri\n");
                code_idx += opcodes(1);
            } else {
                stgwrite("\tconst.pri ");
                outval(val, TRUE);
                code_idx += opcodes(1) + opargs(1);
            }
            break;
        case sALT:
            if (val == 0) {
                stgwrite("\tzero.alt\n");
                code_idx += opcodes(1);
            } else {
                stgwrite("\tconst.alt ");
                outval(val, TRUE);
                code_idx += opcodes(1) + opargs(1);
            }
            break;
    }
}

/* Copy value in alternate register to the primary register */
void
moveto1(void)
{
    stgwrite("\tmove.pri\n");
    code_idx += opcodes(1) + opargs(0);
}

void
move_alt(void)
{
    stgwrite("\tmove.alt\n");
    code_idx += opcodes(1) + opargs(0);
}

/* Push primary or the alternate register onto the stack
 */
void
pushreg(regid reg)
{
    assert(reg == sPRI || reg == sALT);
    switch (reg) {
        case sPRI:
            stgwrite("\tpush.pri\n");
            break;
        case sALT:
            stgwrite("\tpush.alt\n");
            break;
    }
    code_idx += opcodes(1);
}

/*
 *  Push a constant value onto the stack
 */
void
pushval(cell val)
{
    stgwrite("\tpush.c ");
    outval(val, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/* Pop stack into the primary or the alternate register
 */
void
popreg(regid reg)
{
    assert(reg == sPRI || reg == sALT);
    switch (reg) {
        case sPRI:
            stgwrite("\tpop.pri\n");
            break;
        case sALT:
            stgwrite("\tpop.alt\n");
            break;
    }
    code_idx += opcodes(1);
}

/*
 * Generate an array
 *   stk -= dims
 *   [stk] = hea
 *   stk += 1
 *   hea += 1 + (# cells in array)
 */
void
genarray(int dims, bool autozero)
{
    if (autozero)
        stgwrite("\tgenarray.z ");
    else
        stgwrite("\tgenarray ");
    outval(dims, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/*
 *  swap the top-of-stack with the value in primary register
 */
void
swap1(void)
{
    stgwrite("\tswap.pri\n");
    code_idx += opcodes(1);
}

/* Switch statements
 * The "switch" statement generates a "case" table using the "CASE" opcode.
 * The case table contains a list of records, each record holds a comparison
 * value and a label to branch to on a match. The very first record is an
 * exception: it holds the size of the table (excluding the first record) and
 * the label to branch to when none of the values in the case table match.
 * The case table is sorted on the comparison value. This allows more advanced
 * abstract machines to sift the case table with a binary search.
 */
void
ffswitch(int label)
{
    stgwrite("\tswitch ");
    outval(label, TRUE); /* the label is the address of the case table */
    code_idx += opcodes(1) + opargs(1);
}

void
ffcase(cell value, const char* labelname, int newtable)
{
    if (newtable) {
        stgwrite("\tcasetbl\n");
        code_idx += opcodes(1);
    }
    stgwrite("\tcase ");
    outval(value, FALSE);
    stgwrite(" ");
    stgwrite(labelname);
    stgwrite("\n");
    code_idx += opcodes(0) + opargs(2);
}

/*
 *  Call specified function
 */
void
ffcall(symbol* sym, int numargs)
{
    std::string symname;

    assert(sym != NULL);
    assert(sym->ident == iFUNCTN);
    if (sc_asmfile)
        symname = funcdisplayname(sym->name());
    if (sym->native) {
        /* reserve a SYSREQ id if called for the first time */
        stgwrite("\tsysreq.n ");

        /* Look for an alias */
        symbol* target = sym;
        if (sp::Atom* aliasname = lookup_alias(sym->name())) {
            symbol* asym = findglb(aliasname);
            if (asym && asym->ident == iFUNCTN && sym->native)
                target = asym;
        }
        stgwrite(target->name());
        stgwrite(" ");
        outval(numargs, FALSE);
        stgwrite("\n");
        code_idx += opcodes(1) + opargs(2);
    } else {
        symname = sym->name();
        pushval(numargs);
        /* normal function */
        stgwrite("\tcall ");
        stgwrite(symname.c_str());
        if (sc_asmfile &&
            ((!isalpha(symname[0]) && symname[0] != '_' && symname[0] != sc_ctrlchar)))
        {
            stgwrite("\t; ");
            stgwrite(symname.c_str());
        }
        stgwrite("\n");
        code_idx += opcodes(1) + opargs(1);
    }
}

/*  Return from function
 *
 *  Global references: funcstatus  (referred to only)
 */
void
ffret()
{
    stgwrite("\tretn\n");
    code_idx += opcodes(1);
}

void
ffabort(int reason)
{
    stgwrite("\thalt ");
    outval(reason, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

void
ffbounds(cell size)
{
    stgwrite("\tbounds ");
    outval(size, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

void
ffbounds()
{
    // Since the VM uses an unsigned compare here, this effectively protects us
    // from negative array indices.
    stgwrite("\tbounds ");
    outval(INT_MAX, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/*
 *  Jump to local label number (the number is converted to a name)
 */
void
jumplabel(int number)
{
    stgwrite("\tjump ");
    outval(number, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

void
jumplabel_cond(int token, int number)
{
    switch (token) {
        case tlGE:
            stgwrite("\tjsgeq ");
            break;
        case tlLE:
            stgwrite("\tjsleq ");
            break;
        case '<':
            stgwrite("\tjsless ");
            break;
        case '>':
            stgwrite("\tjsgrtr ");
            break;
        default:
            assert(false);
    }
    outval(number, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/*
 *   Define storage (global and static variables)
 */
void
defstorage(void)
{
    stgwrite("dump ");
}

/*
 *  Increment/decrement stack pointer. Note that this routine does
 *  nothing if the delta is zero.
 */
void
modstk(int delta)
{
    if (delta) {
        stgwrite("\tstack ");
        outval(delta, TRUE);
        code_idx += opcodes(1) + opargs(1);
    }
}

void
modheap(int delta)
{
    if (delta) {
        stgwrite("\theap ");
        outval(delta, TRUE);
        code_idx += opcodes(1) + opargs(1);
    }
}

void
modheap_i()
{
    stgwrite("\ttracker.pop.setheap\n");
    code_idx += opcodes(1);
}

void
setheap_save(cell value)
{
    stgwrite("\ttracker.push.c ");
    outval(value, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

void
setheap_pri(void)
{
    stgwrite("\theap "); /* ALT = HEA++ */
    outval(sizeof(cell), TRUE);
    stgwrite("\tstor.i\n");   /* store PRI (default value) at address ALT */
    stgwrite("\tmove.pri\n"); /* move ALT to PRI: PRI contains the address */
    code_idx += opcodes(3) + opargs(1);
    markheap(MEMUSE_STATIC, 1, AllocScopeKind::Temp);
}

void
setheap(cell value)
{
    stgwrite("\tconst.pri "); /* load default value in PRI */
    outval(value, TRUE);
    code_idx += opcodes(1) + opargs(1);
    setheap_pri();
}

/*
 *  Convert a cell number to a "byte" address; i.e. double or quadruple
 *  the primary register.
 */
void
cell2addr(void)
{
    stgwrite("\tshl.c.pri 2\n");
    code_idx += opcodes(1) + opargs(1);
}

/*
 *  Double or quadruple the alternate register.
 */
void
cell2addr_alt(void)
{
    stgwrite("\tshl.c.alt 2\n");
    code_idx += opcodes(1) + opargs(1);
}

/* Convert from character index to byte address. This routine does
 * nothing if a character has the size of a byte.
 */
void
char2addr(void)
{
#if sCHARBITS == 16
    stgwrite("\tshl.c.pri 1\n");
    code_idx += opcodes(1) + opargs(1);
#endif
}

/*
 *  Add a constant to the primary register.
 */
void
addconst(cell value)
{
    if (value != 0) {
        stgwrite("\tadd.c ");
        outval(value, TRUE);
        code_idx += opcodes(1) + opargs(1);
    }
}

/*
 *  signed multiply of primary and secundairy registers (result in primary)
 */
void
os_mult(void)
{
    stgwrite("\tsmul\n");
    code_idx += opcodes(1);
}

/*
 *  signed divide of alternate register by primary register (quotient in
 *  primary; remainder in alternate)
 */
void
os_div(void)
{
    stgwrite("\tsdiv.alt\n");
    code_idx += opcodes(1);
}

/*
 *  modulus of (alternate % primary), result in primary (signed)
 */
void
os_mod(void)
{
    stgwrite("\tsdiv.alt\n");
    stgwrite("\tmove.pri\n"); /* move ALT to PRI */
    code_idx += opcodes(2);
}

/*
 *  Add primary and alternate registers (result in primary).
 */
void
ob_add(void)
{
    stgwrite("\tadd\n");
    code_idx += opcodes(1);
}

/*
 *  subtract primary register from alternate register (result in primary)
 */
void
ob_sub(void)
{
    stgwrite("\tsub.alt\n");
    code_idx += opcodes(1);
}

/*
 *  arithmic shift left alternate register the number of bits
 *  given in the primary register (result in primary).
 *  There is no need for a "logical shift left" routine, since
 *  logical shift left is identical to arithmic shift left.
 */
void
ob_sal(void)
{
    stgwrite("\txchg\n");
    stgwrite("\tshl\n");
    code_idx += opcodes(2);
}

/*
 *  arithmic shift right alternate register the number of bits
 *  given in the primary register (result in primary).
 */
void
os_sar(void)
{
    stgwrite("\txchg\n");
    stgwrite("\tsshr\n");
    code_idx += opcodes(2);
}

/*
 *  logical (unsigned) shift right of the alternate register by the
 *  number of bits given in the primary register (result in primary).
 */
void
ou_sar(void)
{
    stgwrite("\txchg\n");
    stgwrite("\tshr\n");
    code_idx += opcodes(2);
}

/*
 *  inclusive "or" of primary and alternate registers (result in primary)
 */
void
ob_or(void)
{
    stgwrite("\tor\n");
    code_idx += opcodes(1);
}

/*
 *  "exclusive or" of primary and alternate registers (result in primary)
 */
void
ob_xor(void)
{
    stgwrite("\txor\n");
    code_idx += opcodes(1);
}

/*
 *  "and" of primary and secundairy registers (result in primary)
 */
void
ob_and(void)
{
    stgwrite("\tand\n");
    code_idx += opcodes(1);
}

/*
 *  test ALT==PRI; result in primary register (1 or 0).
 */
void
ob_eq(void)
{
    stgwrite("\teq\n");
    code_idx += opcodes(1);
}

/*
 *  test ALT!=PRI
 */
void
ob_ne(void)
{
    stgwrite("\tneq\n");
    code_idx += opcodes(1);
}

/* The abstract machine defines the relational instructions so that PRI is
 * on the left side and ALT on the right side of the operator. For example,
 * SLESS sets PRI to either 1 or 0 depending on whether the expression
 * "PRI < ALT" is true.
 *
 * The compiler generates comparisons with ALT on the left side of the
 * relational operator and PRI on the right side. The XCHG instruction
 * prefixing the relational operators resets this. We leave it to the
 * peephole optimizer to choose more compact instructions where possible.
 */

/* Relational operator prefix for chained relational expressions. The
 * "suffix" code restores the stack.
 * For chained relational operators, the goal is to keep the comparison
 * result "so far" in PRI and the value of the most recent operand in
 * ALT, ready for a next comparison.
 * The "prefix" instruction pushed the comparison result (PRI) onto the
 * stack and moves the value of ALT into PRI. If there is a next comparison,
 * PRI can now serve as the "left" operand of the relational operator.
 */
void
relop_prefix(void)
{
    stgwrite("\tpush.pri\n");
    stgwrite("\tmove.pri\n");
    code_idx += opcodes(2);
}

void
relop_suffix(void)
{
    stgwrite("\tswap.alt\n");
    stgwrite("\tand\n");
    stgwrite("\tpop.alt\n");
    code_idx += opcodes(3);
}

/*
 *  test ALT<PRI (signed)
 */
void
os_lt(void)
{
    stgwrite("\txchg\n");
    stgwrite("\tsless\n");
    code_idx += opcodes(2);
}

/*
 *  test ALT<=PRI (signed)
 */
void
os_le(void)
{
    stgwrite("\txchg\n");
    stgwrite("\tsleq\n");
    code_idx += opcodes(2);
}

/*
 *  test ALT>PRI (signed)
 */
void
os_gt(void)
{
    stgwrite("\txchg\n");
    stgwrite("\tsgrtr\n");
    code_idx += opcodes(2);
}

/*
 *  test ALT>=PRI (signed)
 */
void
os_ge(void)
{
    stgwrite("\txchg\n");
    stgwrite("\tsgeq\n");
    code_idx += opcodes(2);
}

/*
 *  logical negation of primary register
 */
void
lneg(void)
{
    stgwrite("\tnot\n");
    code_idx += opcodes(1);
}

/*
 *  two's complement primary register
 */
void
neg(void)
{
    stgwrite("\tneg\n");
    code_idx += opcodes(1);
}

/*
 *  one's complement of primary register
 */
void
invert(void)
{
    stgwrite("\tinvert\n");
    code_idx += opcodes(1);
}

/*
 *  nop
 */
void
nooperation(void)
{
    stgwrite("\tnop\n");
    code_idx += opcodes(1);
}

void
inc_pri()
{
    stgwrite("\tinc.pri\n");
    code_idx += opcodes(1);
}

void
dec_pri()
{
    stgwrite("\tdec.pri\n");
    code_idx += opcodes(1);
}

/*  increment symbol
 */
void
inc(const value* lval)
{
    symbol* sym;

    sym = lval->sym;
    if (lval->ident == iARRAYCELL) {
        /* indirect increment, address already in PRI */
        stgwrite("\tinc.i\n");
        code_idx += opcodes(1);
    } else if (lval->ident == iARRAYCHAR) {
        /* indirect increment of single character, address already in PRI */
        stgwrite("\tpush.pri\n");
        stgwrite("\tpush.alt\n");
        stgwrite("\tmove.alt\n");    /* copy address */
        stgwrite("\tlodb.i ");       /* read from PRI into PRI */
        outval(sCHARBITS / 8, TRUE); /* read one or two bytes */
        stgwrite("\tinc.pri\n");
        stgwrite("\tstrb.i ");       /* write PRI to ALT */
        outval(sCHARBITS / 8, TRUE); /* write one or two bytes */
        stgwrite("\tpop.alt\n");
        stgwrite("\tpop.pri\n");
        code_idx += opcodes(8) + opargs(2);
    } else if (lval->ident == iREFERENCE) {
        assert(sym != NULL);
        stgwrite("\tpush.pri\n");
        /* load dereferenced value */
        assert(sym->vclass == sLOCAL || sym->vclass == sARGUMENT); /* global references don't exist in Pawn */
        stgwrite("\tlref.s.pri ");
        outval(sym->addr(), TRUE);
        /* increment */
        stgwrite("\tinc.pri\n");
        /* store dereferenced value */
        stgwrite("\tsref.s.pri ");
        outval(sym->addr(), TRUE);
        stgwrite("\tpop.pri\n");
        code_idx += opcodes(5) + opargs(2);
    } else if (lval->ident == iACCESSOR) {
        inc_pri();
        invoke_setter(lval->accessor, FALSE);
    } else {
        /* local or global variable */
        assert(sym != NULL);
        if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
            stgwrite("\tinc.s ");
        else
            stgwrite("\tinc ");
        outval(sym->addr(), TRUE);
        code_idx += opcodes(1) + opargs(1);
    }
}

/*  decrement symbol
 *
 *  in case of an integer pointer, the symbol must be incremented by 2.
 */
void
dec(const value* lval)
{
    symbol* sym;

    sym = lval->sym;
    if (lval->ident == iARRAYCELL) {
        /* indirect decrement, address already in PRI */
        stgwrite("\tdec.i\n");
        code_idx += opcodes(1);
    } else if (lval->ident == iARRAYCHAR) {
        /* indirect decrement of single character, address already in PRI */
        stgwrite("\tpush.pri\n");
        stgwrite("\tpush.alt\n");
        stgwrite("\tmove.alt\n");    /* copy address */
        stgwrite("\tlodb.i ");       /* read from PRI into PRI */
        outval(sCHARBITS / 8, TRUE); /* read one or two bytes */
        stgwrite("\tdec.pri\n");
        stgwrite("\tstrb.i ");       /* write PRI to ALT */
        outval(sCHARBITS / 8, TRUE); /* write one or two bytes */
        stgwrite("\tpop.alt\n");
        stgwrite("\tpop.pri\n");
        code_idx += opcodes(8) + opargs(2);
    } else if (lval->ident == iREFERENCE) {
        assert(sym != NULL);
        stgwrite("\tpush.pri\n");
        /* load dereferenced value */
        assert(sym->vclass == sLOCAL || sym->vclass == sARGUMENT); /* global references don't exist in Pawn */
        stgwrite("\tlref.s.pri ");
        outval(sym->addr(), TRUE);
        /* decrement */
        stgwrite("\tdec.pri\n");
        /* store dereferenced value */
        stgwrite("\tsref.s.pri ");
        outval(sym->addr(), TRUE);
        stgwrite("\tpop.pri\n");
        code_idx += opcodes(5) + opargs(2);
    } else if (lval->ident == iACCESSOR) {
        dec_pri();
        invoke_setter(lval->accessor, FALSE);
    } else {
        /* local or global variable */
        assert(sym != NULL);
        if (sym->vclass == sLOCAL || sym->vclass == sARGUMENT)
            stgwrite("\tdec.s ");
        else
            stgwrite("\tdec ");
        outval(sym->addr(), TRUE);
        code_idx += opcodes(1) + opargs(1);
    }
}

/*
 *  Jumps to "label" if PRI != 0
 */
void
jmp_ne0(int number)
{
    stgwrite("\tjnz ");
    outval(number, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/*
 *  Jumps to "label" if PRI == 0
 */
void
jmp_eq0(int number)
{
    stgwrite("\tjzer ");
    outval(number, TRUE);
    code_idx += opcodes(1) + opargs(1);
}

/* write a value in hexadecimal; optionally adds a newline */
void
outval(cell val, int newline)
{
    stgwrite(itoh(val));
    if (newline)
        stgwrite("\n");
}

void
invoke_getter(methodmap_method_t* method)
{
    if (!method->getter) {
        error(149, method->name);
        return;
    }

    // sysreq.n N 1
    // stack 8
    pushreg(sPRI);
    ffcall(method->getter, 1);

    if (sc_status != statSKIP)
        markusage(method->getter, uREAD);
}

void
invoke_setter(methodmap_method_t* method, int save)
{
    if (!method->setter) {
        error(152, method->name);
        return;
    }

    if (save)
        pushreg(sPRI);
    pushreg(sPRI);
    pushreg(sALT);
    ffcall(method->setter, 2);
    if (save)
        popreg(sPRI);

    if (sc_status != statSKIP)
        markusage(method->setter, uREAD);
}

// function value -> pri
void
load_glbfn(symbol* sym)
{
    assert(sym->ident == iFUNCTN);
    assert(!sym->native);
    stgwrite("\tldgfn.pri ");
    stgwrite(sym->name());
    stgwrite("\n");
    code_idx += opcodes(1) + opargs(1);

    if (sc_status != statSKIP)
        markusage(sym, uREAD);
}

// Array initialization. Uses and clobbers PRI.
void
emit_initarray(regid reg, cell base_addr, cell iv_size, cell data_copy_size, cell data_fill_size,
               cell fill_value)
{
    if (reg == sPRI)
        stgwrite("\tinitarray.pri ");
    else
        stgwrite("\tinitarray.alt ");
    outval(base_addr, FALSE);
    stgwrite(" ");
    outval(iv_size, FALSE);
    stgwrite(" ");
    outval(data_copy_size, FALSE);
    stgwrite(" ");
    outval(data_fill_size, FALSE);
    stgwrite(" ");
    outval(fill_value, TRUE);
    code_idx += opcodes(1) + opargs(5);
}

void
emit_default_array(arginfo* arg)
{
    DefaultArg* def = arg->def.get();
    if (!def->val) {
        def->val = ke::Some(gDataQueue.dat_address());

        // Make copies since we cache DefaultArgs across compilations. This
        // will go away when the two-pass parser dies.
        std::vector<cell> iv = def->array->iv;
        std::vector<cell> data = def->array->data;

        gDataQueue.Add(std::move(iv));
        gDataQueue.Add(std::move(data));
        gDataQueue.AddZeroes(def->array->zeroes);
    }

    if (arg->is_const || !def->array) {
        // No modification is possible, so use the array we emitted. (This is
        // why we emitted the zeroes above.)
        ldconst(def->val.get(), sPRI);
    } else {
        cell iv_size = (cell)def->array->iv.size();
        cell data_size = (cell)def->array->data.size();
        cell total_size = iv_size + data_size + def->array->zeroes;

        //  heap <size>
        //  move.alt        ; pri = new address
        //  init.array
        //  move.alt        ; pri = new address
        modheap(total_size * sizeof(cell));
        markheap(MEMUSE_STATIC, total_size, AllocScopeKind::Temp);
        emit_initarray(sALT, def->val.get(), iv_size, data_size, def->array->zeroes, 0);
        moveto1();
    }
}
