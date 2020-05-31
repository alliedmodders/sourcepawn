/* vim: set ts=8 sts=4 sw=4 tw=99 et: */
/*  Pawn compiler - Recursive descend expresion parser
 *
 *  Copyright (c) ITB CompuPhase, 1997-2005
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
#include <stdio.h>
#include <stdlib.h> /* for _MAX_PATH */
#include <string.h>
#include <amtl/am-algorithm.h>
#include "emitter.h"
#include "errors.h"
#include "expressions.h"
#include "new-parser.h"
#include "lexer.h"
#include "optimizer.h"
#include "sc.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "types.h"

/* Function addresses of binary operators for signed operations */
static void (*op1[17])(void) = {
    os_mult, os_div, os_mod,        /* hier3, index 0 */
    ob_add,  ob_sub,                /* hier4, index 3 */
    ob_sal,  os_sar, ou_sar,        /* hier5, index 5 */
    ob_and,                         /* hier6, index 8 */
    ob_xor,                         /* hier7, index 9 */
    ob_or,                          /* hier8, index 10 */
    os_le,   os_ge,  os_lt,  os_gt, /* hier9, index 11 */
    ob_eq,   ob_ne,                 /* hier10, index 15 */
};

// The "op1" array in sc3.cpp must have the same ordering as if these lists
// were flattened.
int ExpressionParser::list3[] = {'*', '/', '%', 0};
int ExpressionParser::list4[] = {'+', '-', 0};
int ExpressionParser::list5[] = {tSHL, tSHR, tSHRU, 0};
int ExpressionParser::list6[] = {'&', 0};
int ExpressionParser::list7[] = {'^', 0};
int ExpressionParser::list8[] = {'|', 0};
int ExpressionParser::list9[] = {tlLE, tlGE, '<', '>', 0};
int ExpressionParser::list10[] = {tlEQ, tlNE, 0};
int ExpressionParser::list11[] = {tlAND, 0};
int ExpressionParser::list12[] = {tlOR, 0};

/* These two functions are defined because the functions inc() and dec() in
 * SC4.C have a different prototype than the other code generation functions.
 * The arrays for user-defined functions use the function pointers for
 * identifying what kind of operation is requested; these functions must all
 * have the same prototype. As inc() and dec() are special cases already, it
 * is simplest to add two "do-nothing" functions.
 */
void
user_inc(void)
{
}
void
user_dec(void)
{
}

bool
find_userop(void (*oper)(), int tag1, int tag2, int numparam, const value* lval, UserOperation* op)
{
    static const char* binoperstr[] = {"*", "/", "%",  "+",  "-", "",  "",   "",  "",
                                       "",  "",  "<=", ">=", "<", ">", "==", "!="};
    static bool binoper_savepri[] = {false, false, false, false, false, false, false, false, false,
                                     false, false, true,  true,  true,  true,  false, false};
    static const char* unoperstr[] = {"!", "-", "++", "--"};
    static void (*unopers[])(void) = {lneg, neg, user_inc, user_dec};

    char opername[4] = "", symbolname[sNAMEMAX + 1];
    size_t i;
    bool savepri, savealt;
    symbol* sym;

    /* since user-defined operators on untagged operands are forbidden, we have
     * a quick exit.
     */
    assert(numparam == 1 || numparam == 2);
    if (tag1 == 0 && (numparam == 1 || tag2 == 0))
        return false;

    savepri = savealt = false;
    /* find the name with the operator */
    if (numparam == 2) {
        if (oper == NULL) {
            /* assignment operator: a special case */
            strcpy(opername, "=");
            if (lval != NULL && (lval->ident == iARRAYCELL || lval->ident == iARRAYCHAR))
                savealt = true;
        } else {
            assert((sizeof binoperstr / sizeof binoperstr[0]) == (sizeof op1 / sizeof op1[0]));
            for (i = 0; i < sizeof op1 / sizeof op1[0]; i++) {
                if (oper == op1[i]) {
                    strcpy(opername, binoperstr[i]);
                    savepri = binoper_savepri[i];
                    break;
                }
            }
        }
    } else {
        assert(oper != NULL);
        assert(numparam == 1);
        /* try a select group of unary operators */
        assert((sizeof unoperstr / sizeof unoperstr[0]) == (sizeof unopers / sizeof unopers[0]));
        if (opername[0] == '\0') {
            for (i = 0; i < sizeof unopers / sizeof unopers[0]; i++) {
                if (oper == unopers[i]) {
                    strcpy(opername, unoperstr[i]);
                    break;
                }
            }
        }
    }
    /* if not found, quit */
    if (opername[0] == '\0')
        return false;

    /* create a symbol name from the tags and the operator name */
    assert(numparam == 1 || numparam == 2);
    operator_symname(symbolname, opername, tag1, tag2, numparam, tag2);
    bool swapparams = false;
    sym = findglb(symbolname);
    if (!sym) {
        /* check for commutative operators */
        if (tag1 == tag2 || oper == NULL || !commutative(oper))
            return false; /* not commutative, cannot swap operands */
        /* if arrived here, the operator is commutative and the tags are different,
         * swap tags and try again
         */
        assert(numparam == 2); /* commutative operator must be a binary operator */
        operator_symname(symbolname, opername, tag2, tag1, numparam, tag1);
        swapparams = true;
        sym = findglb(symbolname);
        if (!sym)
            return false;
    }

    /* check existance and the proper declaration of this function */
    if (sym->missing || !sym->prototyped) {
        char symname[2 * sNAMEMAX + 16]; /* allow space for user defined operators */
        funcdisplayname(symname, sym->name());
        if (sym->missing)
            error(4, symname); /* function not defined */
        if (!sym->prototyped)
            error(71, symname); /* operator must be declared before use */
    }

    /* we don't want to use the redefined operator in the function that
     * redefines the operator itself, otherwise the snippet below gives
     * an unexpected recursion:
     *    fixed:operator+(fixed:a, fixed:b)
     *        return a + b
     */
    if (sym == curfunc)
        return false;

    if (sc_status != statSKIP)
        markusage(sym, uREAD); /* do not mark as "used" when this call itself is skipped */

    op->sym = sym;
    op->oper = oper;
    op->paramspassed = (oper == NULL) ? 1 : numparam;
    op->savepri = savepri;
    op->savealt = savealt;
    op->swapparams = swapparams;
    return true;
}

void
emit_userop(const UserOperation& user_op, value* lval)
{
    /* for increment and decrement operators, the symbol must first be loaded
     * (and stored back afterwards)
     */
    if (user_op.oper == user_inc || user_op.oper == user_dec) {
        assert(!user_op.savepri);
        assert(lval != NULL);
        if (lval->ident == iARRAYCELL || lval->ident == iARRAYCHAR)
            pushreg(sPRI); /* save current address in PRI */
        if (lval->ident != iACCESSOR)
            rvalue(lval); /* get the symbol's value in PRI */
    }

    assert(!user_op.savepri || !user_op.savealt); /* either one MAY be set, but not both */
    if (user_op.savepri) {
        /* the chained comparison operators require that the ALT register is
         * unmodified, so we save it here; actually, we save PRI because the normal
         * instruction sequence (without user operator) swaps PRI and ALT
         */
        pushreg(sPRI); /* right-hand operand is in PRI */
    } else if (user_op.savealt) {
        /* for the assignment operator, ALT may contain an address at which the
         * result must be stored; this address must be preserved accross the
         * call
         */
        assert(lval != NULL); /* this was checked earlier */
        assert(lval->ident == iARRAYCELL || lval->ident == iARRAYCHAR); /* checked earlier */
        pushreg(sALT);
    }

    /* push parameters, call the function */
    switch (user_op.paramspassed) {
        case 1:
            pushreg(sPRI);
            break;
        case 2:
            /* note that 1) a function expects that the parameters are pushed
             * in reversed order, and 2) the left operand is in the secondary register
             * and the right operand is in the primary register */
            if (user_op.swapparams) {
                pushreg(sALT);
                pushreg(sPRI);
            } else {
                pushreg(sPRI);
                pushreg(sALT);
            }
            break;
        default:
            assert(0);
    }
    markexpr(sPARM, NULL, 0); /* mark the end of a sub-expression */
    assert(user_op.sym->ident == iFUNCTN);
    ffcall(user_op.sym, user_op.paramspassed);

    if (user_op.savepri || user_op.savealt)
        popreg(sALT); /* restore the saved PRI/ALT that into ALT */
    if (user_op.oper == user_inc || user_op.oper == user_dec) {
        assert(lval != NULL);
        if (lval->ident == iARRAYCELL || lval->ident == iARRAYCHAR)
            popreg(sALT); /* restore address (in ALT) */
        if (lval->ident != iACCESSOR) {
            store(lval); /* store PRI in the symbol */
            moveto1();   /* make sure PRI is restored on exit */
        }
    }
}

int
check_userop(void (*oper)(void), int tag1, int tag2, int numparam, value* lval, int* resulttag)
{
    UserOperation user_op;
    if (!find_userop(oper, tag1, tag2, numparam, lval, &user_op))
        return FALSE;

    sideeffect = TRUE;         /* assume functions carry out a side-effect */

    assert(resulttag != NULL);
    *resulttag = user_op.sym->tag; /* save tag of the called function */

    emit_userop(user_op, lval);
    return TRUE;
}

int
checktag_string(int tag, const value* sym1)
{
    if (sym1->ident == iARRAY || sym1->ident == iREFARRAY)
        return FALSE;
    if ((sym1->tag == pc_tag_string && tag == 0) || (sym1->tag == 0 && tag == pc_tag_string)) {
        return TRUE;
    }
    return FALSE;
}

int
checkval_string(const value* sym1, const value* sym2)
{
    if (sym1->ident == iARRAY || sym2->ident == iARRAY || sym1->ident == iREFARRAY ||
        sym2->ident == iREFARRAY)
    {
        return FALSE;
    }
    if ((sym1->tag == pc_tag_string && sym2->tag == 0) ||
        (sym1->tag == 0 && sym2->tag == pc_tag_string))
    {
        return TRUE;
    }

    return FALSE;
}

const char*
type_to_name(int tag)
{
    if (tag == 0)
        return "int";
    if (tag == sc_rationaltag)
        return "float";
    if (tag == pc_tag_string)
        return "char";
    if (tag == pc_anytag)
        return "any";

    Type* type = gTypes.find(tag);
    if (!type)
        return "-unknown-";
    return type->prettyName();
}

int
matchtag_string(int ident, int tag)
{
    if (ident == iARRAY || ident == iREFARRAY)
        return FALSE;
    return (tag == pc_tag_string) ? TRUE : FALSE;
}

static int
obj_typeerror(int id, int tag1, int tag2)
{
    const char* left = pc_tagname(tag1);
    const char* right = pc_tagname(tag2);
    if (!left || strcmp(left, "_") == 0)
        left = "int";
    if (!right || strcmp(right, "_") == 0)
        right = "int";
    error(id, right, left);
    return FALSE;
}

static int
matchobjecttags(Type* formal, Type* actual, int flags)
{
    int formaltag = formal->tagid();
    int actualtag = actual->tagid();

    // objects never coerce to non-objects, YET.
    if (formal->isObject() && !(actual->isObject() || actual->isFunction())) {
        if (!(flags & MATCHTAG_SILENT))
            obj_typeerror(132, formaltag, actualtag);
        return FALSE;
    }

    if (actualtag == pc_tag_nullfunc_t) {
        // All functions are nullable. We use a separate constant for backward
        // compatibility; plugins and extensions check -1, not 0.
        if (formal->isFunction())
            return TRUE;

        if (!(flags & MATCHTAG_SILENT))
            error(154, pc_tagname(formaltag));
        return FALSE;
    }

    if (actualtag == pc_tag_null_t) {
        // All objects are nullable.
        if (formal->isObject())
            return TRUE;

        // Some methodmaps are nullable. The nullable property is inherited
        // automatically.
        methodmap_t* map = formal->asMethodmap();
        if (map && map->nullable)
            return TRUE;

        if (!(flags & MATCHTAG_SILENT))
            error(148, pc_tagname(formaltag));
        return FALSE;
    }

    if (!formal->isObject() && actual->isObject())
        return obj_typeerror(131, formaltag, actualtag);

    // Every object coerces to "object".
    if (formaltag == pc_tag_object)
        return TRUE;

    if (flags & MATCHTAG_COERCE)
        return obj_typeerror(134, formaltag, actualtag);

    methodmap_t* map = actual->asMethodmap();
    for (; map; map = map->parent) {
        if (map->tag == formaltag)
            return TRUE;
    }

    if (!(flags & MATCHTAG_SILENT))
        obj_typeerror(133, formaltag, actualtag);
    return FALSE;
}

static int
matchreturntag(const functag_t* formal, const functag_t* actual)
{
    if (formal->ret_tag == actual->ret_tag)
        return TRUE;
    if (formal->ret_tag == pc_tag_void) {
        if (actual->ret_tag == 0)
            return TRUE;
    }
    return FALSE;
}

static int
funcarg_compare(const funcarg_t* formal, const funcarg_t* actual)
{
    // Check type.
    if (actual->ident != formal->ident)
        return FALSE;

    // Check rank.
    if (actual->dimcount != formal->dimcount)
        return FALSE;

    // Check arity.
    for (int i = 0; i < formal->dimcount; i++) {
        if (actual->dims[i] != formal->dims[i])
            return FALSE;
    }

    // Note we invert the order we pass things to matchtag() here. If the
    // typedef specifies base type X, and the function specifies derived
    // type Y, we want this to type since such an assignment is valid.
    //
    // Most programming languages do not subtype arguments like this. We do
    // it in SourcePawn to preserve compatibility during the Transitional
    // Syntax effort.
    if (!matchtag(actual->tag, formal->tag, MATCHTAG_SILENT | MATCHTAG_COERCE))
        return FALSE;
    return TRUE;
}

static int
functag_compare(const functag_t* formal, const functag_t* actual)
{
    // Check return types.
    if (!matchreturntag(formal, actual))
        return FALSE;

    // Make sure there are no trailing arguments.
    if (actual->args.size() > formal->args.size())
        return FALSE;

    // Check arguments.
    for (size_t i = 0; i < formal->args.size(); i++) {
        const funcarg_t* formal_arg = &formal->args[i];

        if (i >= actual->args.size())
            return FALSE;

        const funcarg_t* actual_arg = &actual->args[i];
        if (!funcarg_compare(formal_arg, actual_arg))
            return FALSE;
    }

    return TRUE;
}

static int
matchfunctags(Type* formal, Type* actual)
{
    int formaltag = formal->tagid();
    int actualtag = actual->tagid();

    if (formaltag == pc_functag && actual->isFunction())
        return TRUE;

    if (actualtag == pc_tag_nullfunc_t)
        return TRUE;

    if (!actual->isFunction())
        return FALSE;

    functag_t* actualfn = functag_find_intrinsic(actualtag);
    if (!actualfn)
        return FALSE;

    funcenum_t* e = formal->toFunction();
    if (!e)
        return FALSE;

    for (const auto& formalfn : e->entries) {
        if (functag_compare(formalfn, actualfn))
            return TRUE;
    }

    return FALSE;
}

int
matchtag(int formaltag, int actualtag, int flags)
{
    if (formaltag == actualtag)
        return TRUE;

    Type* actual = gTypes.find(actualtag);
    Type* formal = gTypes.find(formaltag);
    assert(actual && formal);

    if (formaltag == pc_tag_string && actualtag == 0)
        return TRUE;

    if (formal->isObject() || actual->isObject())
        return matchobjecttags(formal, actual, flags);

    if (actual->isFunction() && !formal->isFunction()) {
        // We're being given a function, but the destination is not a function.
        error(130);
        return FALSE;
    }

    /* if the formal tag is zero and the actual tag is not "fixed", the actual
     * tag is "coerced" to zero
     */
    if ((flags & MATCHTAG_COERCE) && !formaltag && actual && !actual->isFixed()) {
        return TRUE;
    }

    if (formaltag == pc_anytag || actualtag == pc_anytag)
        return TRUE;

    if (formal->isFunction()) {
        if (!matchfunctags(formal, actual)) {
            error(100);
            return FALSE;
        }
        return TRUE;
    }

    if (flags & (MATCHTAG_COERCE | MATCHTAG_DEDUCE)) {
        // See if the tag has a methodmap associated with it. If so, see if the given
        // tag is anywhere on the inheritance chain.
        if (methodmap_t* map = actual->asMethodmap()) {
            for (; map; map = map->parent) {
                if (map->tag == formaltag)
                    return TRUE;
            }
        }
    }

    if (!(flags & MATCHTAG_SILENT))
        error(213, type_to_name(formaltag), type_to_name(actualtag));
    return FALSE;
}

int matchtag_commutative(int formaltag, int actualtag, int flags)
{
    if (matchtag(formaltag, actualtag, flags | MATCHTAG_SILENT))
        return TRUE;
    if (matchtag(actualtag, formaltag, flags | MATCHTAG_SILENT))
        return TRUE;
    // Report the error.
    return matchtag(formaltag, actualtag, flags);
}

/*
 *  Searches for a binary operator a list of operators. The list is stored in
 *  the array "list". The last entry in the list should be set to 0.
 *
 *  The index of an operator in "list" (if found) is returned in "opidx". If
 *  no operator is found, nextop() returns 0.
 *
 *  If an operator is found in the expression, it cannot be used in a function
 *  call with omitted parantheses. Mark this...
 */
int
ExpressionParser::nextop(int* opidx, int* list)
{
    *opidx = 0;
    while (*list) {
        if (matchtoken(*list)) {
            return TRUE; /* found! */
        } else {
            list += 1;
            *opidx += 1;
        }
    }
    return FALSE; /* entire list scanned, nothing found */
}

int
findnamedarg(arginfo* arg, const char* name)
{
    int i;

    for (i = 0; arg[i].ident != 0 && arg[i].ident != iVARARGS; i++)
        if (strcmp(arg[i].name, name) == 0)
            return i;
    return -1;
}

cell
array_totalsize(symbol* sym)
{
    cell length;

    assert(sym != NULL);
    assert(sym->ident == iARRAY || sym->ident == iREFARRAY);
    length = sym->dim.array.length;
    if (sym->dim.array.level > 0) {
        cell sublength = array_totalsize(sym->array_child());
        if (sublength > 0)
            length = length + length * sublength;
        else
            length = 0;
    }
    return length;
}

cell
array_levelsize(symbol* sym, int level)
{
    assert(sym != NULL);
    assert(sym->ident == iARRAY || sym->ident == iREFARRAY);
    assert(level <= sym->dim.array.level);
    while (level-- > 0) {
        sym = sym->array_child();
        assert(sym != NULL);
    }
    return (sym->dim.array.slength ? sym->dim.array.slength : sym->dim.array.length);
}

static void
checkfunction(const value* lval)
{
    symbol* sym = lval->sym;

    if (sym == NULL || (sym->ident != iFUNCTN))
        return; /* no known symbol, or not a function result */

    if (sym->defined) {
        /* function is defined, can now check the return value (but make an
         * exception for directly recursive functions)
         */
        if (sym != curfunc && !sym->retvalue) {
            char symname[2 * sNAMEMAX + 16]; /* allow space for user defined operators */
            funcdisplayname(symname, sym->name());
            error(209, symname); /* function should return a value */
        }
    } else {
        /* function not yet defined, set */
        sym->retvalue = true;    /* make sure that a future implementation of
                                  * the function uses "return <value>" */
    }
}

cell
calc(cell left, void (*oper)(), cell right, char* boolresult)
{
    if (oper == ob_or)
        return (left | right);
    else if (oper == ob_xor)
        return (left ^ right);
    else if (oper == ob_and)
        return (left & right);
    else if (oper == ob_eq)
        return (left == right);
    else if (oper == ob_ne)
        return (left != right);
    else if (oper == os_sar)
        return (left >> (int)right);
    else if (oper == ou_sar)
        return ((ucell)left >> (ucell)right);
    else if (oper == ob_sal)
        return ((ucell)left << (int)right);
    else if (oper == ob_add)
        return (left + right);
    else if (oper == ob_sub)
        return (left - right);
    else if (oper == os_mult)
        return (left * right);
    else if (oper == os_div) {
        if (right == 0) {
            error(29);
            return 0;
        }
        return left / right;
    } else if (oper == os_mod) {
        if (right == 0) {
            error(29);
            return 0;
        }
        return left % right;
    }
    assert(false);
    error(29); /* invalid expression, assumed 0 (this should never occur) */
    return 0;
}

int
lvalexpr(svalue* sval)
{
    memset(sval, 0, sizeof(*sval));

    errorset(sEXPRMARK, 0);

    Parser parser;
    sval->lvalue = parser.expression(&sval->val);

    errorset(sEXPRRELEASE, 0);

    return sval->val.ident;
}

int
expression(cell* val, int* tag, symbol** symptr, int chkfuncresult, value* _lval)
{
    value lval = {0};
    pushheaplist();

    Parser parser;
    int lvalue = parser.expression(&lval);
    if (lvalue)
        rvalue(&lval);
    /* scrap any arrays left on the heap */
    popheaplist(true);

    if (lval.ident == iCONSTEXPR && val != NULL) /* constant expression */
        *val = lval.constval;
    if (tag != NULL)
        *tag = lval.tag;
    if (symptr != NULL)
        *symptr = lval.sym;
    if (chkfuncresult)
        checkfunction(&lval);
    if (_lval)
        *_lval = lval;
    return lval.ident;
}

bool
is_valid_index_tag(int tag)
{
    if (tag == 0 || tag == pc_anytag)
        return true;

    Type* idx_type = gTypes.find(tag);
    return idx_type->isEnum();
}

void
setdefarray(cell* string, cell size, cell array_sz, cell* dataaddr, int fconst)
{
    /* The routine must copy the default array data onto the heap, as to avoid
     * that a function can change the default value. An optimization is that
     * the default array data is "dumped" into the data segment only once (on the
     * first use).
     */
    /* check whether to dump the default array */
    assert(dataaddr != NULL);
    if (sc_status == statWRITE && *dataaddr < 0) {
        int i;
        *dataaddr = (litidx + glb_declared) * sizeof(cell);
        for (i = 0; i < size; i++)
            litadd(*string++);
    }

    /* if the function is known not to modify the array (meaning that it also
     * does not modify the default value), directly pass the address of the
     * array in the data segment.
     */
    if (fconst || !string) {
        ldconst(*dataaddr, sPRI);
    } else {
        /* Generate the code:
         *  CONST.pri dataaddr                ;address of the default array data
         *  HEAP      array_sz*sizeof(cell)   ;heap address in ALT
         *  MOVS      size*sizeof(cell)       ;copy data from PRI to ALT
         *  MOVE.PRI                          ;PRI = address on the heap
         */
        ldconst(*dataaddr, sPRI);
        /* "array_sz" is the size of the argument (the value between the brackets
         * in the declaration), "size" is the size of the default array data.
         */
        assert(array_sz >= size);
        modheap((int)array_sz * sizeof(cell));
        markheap(MEMUSE_STATIC, array_sz);
        /* ??? should perhaps fill with zeros first */
        memcopy(size * sizeof(cell));
        moveto1();
    }
}

int
checktag(int tag, int exprtag)
{
    int errcount = errnum;

    if (matchtag(tag, exprtag, MATCHTAG_COERCE))
        return TRUE; /* matching tag */

    // If matchtag() didn't error, report an error.
    if (errnum == errcount)
        error(213, type_to_name(tag), type_to_name(exprtag));

    return FALSE; /* no tag matched */
}

/*  commutative
 *
 *  Test whether an operator is commutative, i.e. x oper y == y oper x.
 *  Commutative operators are: +  (addition)
 *                             *  (multiplication)
 *                             == (equality)
 *                             != (inequality)
 *                             &  (bitwise and)
 *                             ^  (bitwise xor)
 *                             |  (bitwise or)
 *
 *  If in an expression, code for the left operand has been generated and
 *  the right operand is a constant and the operator is commutative, the
 *  precautionary "push" of the primary register is scrapped and the constant
 *  is read into the secondary register immediately.
 */
int
commutative(void (*oper)())
{
    return oper == ob_add || oper == os_mult || oper == ob_eq || oper == ob_ne || oper == ob_and ||
           oper == ob_xor || oper == ob_or;
}
