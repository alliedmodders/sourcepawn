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
#include "compile-context.h"
#include "errors.h"
#include "expressions.h"
#include "lexer.h"
#include "parser.h"
#include "sc.h"
#include "sctracker.h"
#include "semantics.h"
#include "symbols.h"
#include "types.h"

/* Function addresses of binary operators for signed operations */
static const int op1[17] = {
    // hier3
    '*', '/', '%',
    // hier4
    '+', '-',
    // hier5
    tSHL, tSHR, tSHRU,
    // hier6
    '&',
    // hier7
    '^',
    // hier8
    '|',
    // hier9
    tlLE, tlGE, '<', '>',
    // hier10
    tlEQ, tlNE
};

static inline bool
MatchOperator(int oper, symbol* sym, int tag1, int tag2, int numparam)
{
    if (!oper)
        numparam = 1;

    auto fun = sym->function()->node;
    const auto& args = fun->args();
    if (args.size() != size_t(numparam))
        return false;

    assert(numparam == 1 || numparam == 2);
    int tags[2] = { tag1, tag2 };

    for (int i = 0; i < numparam; i++) {
        if (args[i]->type().ident != iVARIABLE)
            return false;
        if (args[i]->type().tag() != tags[i])
            return false;
    }

    if (!oper && sym->tag != tag2)
        return false;
    return true;
}

bool
find_userop(SemaContext& sc, int oper, int tag1, int tag2, int numparam, const value* lval,
            UserOperation* op)
{
    static const char* binoperstr[] = {"*", "/", "%",  "+",  "-", "",  "",   "",  "",
                                       "",  "",  "<=", ">=", "<", ">", "==", "!="};
    static const bool binoper_savepri[] = {false, false, false, false, false, false, false, false,
                                           false, false, false, true,  true,  true,  true,  false,
                                           false};
    static const char* unoperstr[] = {"!", "-", "++", "--"};
    static const int unopers[] = {'!', '-', tINC, tDEC};

    char opername[4] = "";
    size_t i;
    bool savepri, savealt;

    /* since user-defined operators on untagged operands are forbidden, we have
     * a quick exit.
     */
    assert(numparam == 1 || numparam == 2);
    if (sc.cc().in_preprocessor())
        return false;
    if (tag1 == 0 && (numparam == 1 || tag2 == 0))
        return false;

    savepri = savealt = false;
    /* find the name with the operator */
    if (numparam == 2) {
        if (oper == 0) {
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
        assert(oper);
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

    // :TODO: restrict this to globals.
    auto opername_atom = sc.cc().atom(opername);
    symbol* chain = FindSymbol(sc, opername_atom);
    if (!chain)
        return false;

    symbol* sym = nullptr;
    bool swapparams;
    bool is_commutative = commutative(oper);
    for (auto iter = chain; iter; iter = iter->next) {
        bool matched = MatchOperator(oper, iter, tag1, tag2, numparam);
        bool swapped = false;
        if (!matched && is_commutative && tag1 != tag2 && oper) {
            matched = MatchOperator(oper, iter, tag2, tag1, numparam);
            swapped = true;
        }
        if (matched) {
            sym = iter;
            swapparams = swapped;
            break;
        }
    }

    if (!sym)
        return false;

    /* check existance and the proper declaration of this function */
    if (!sym->defined) {
        auto types = CompileContext::get().types();
        if (numparam == 1)
            report(406) << opername << types->find(tag1);
        else
            report(407) << opername << types->find(tag1) << types->find(tag2);
        return false;
    }

    /* we don't want to use the redefined operator in the function that
     * redefines the operator itself, otherwise the snippet below gives
     * an unexpected recursion:
     *    fixed:operator+(fixed:a, fixed:b)
     *        return a + b
     */
    if (sym == sc.func()) {
        report(408);
    }

    markusage(sym, uREAD);

    op->sym = sym;
    op->oper = oper;
    op->paramspassed = (oper == 0) ? 1 : numparam;
    op->savepri = savepri;
    op->savealt = savealt;
    op->swapparams = swapparams;
    return true;
}

int
checktag_string(int tag, const value* sym1)
{
    if (sym1->ident == iARRAY || sym1->ident == iREFARRAY)
        return FALSE;

    auto types = CompileContext::get().types();
    if ((sym1->tag == types->tag_string() && tag == 0) ||
        (sym1->tag == 0 && tag == types->tag_string())) {
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

    auto types = CompileContext::get().types();
    if ((sym1->tag == types->tag_string() && sym2->tag == 0) ||
        (sym1->tag == 0 && sym2->tag == types->tag_string()))
    {
        return TRUE;
    }

    return FALSE;
}

const char*
type_to_name(int tag)
{
    auto types = CompileContext::get().types();
    if (tag == 0)
        return "int";
    if (tag == types->tag_float())
        return "float";
    if (tag == types->tag_string())
        return "char";
    if (tag == types->tag_any())
        return "any";

    Type* type = types->find(tag);
    if (!type)
        return "-unknown-";
    return type->prettyName();
}

int
matchtag_string(int ident, int tag)
{
    auto types = CompileContext::get().types();
    if (ident == iARRAY || ident == iREFARRAY)
        return FALSE;
    return (tag == types->tag_string()) ? TRUE : FALSE;
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
    auto types = CompileContext::get().types();
    int formaltag = formal->tagid();
    int actualtag = actual->tagid();

    // objects never coerce to non-objects, YET.
    if (formal->isObject() && !(actual->isObject() || actual->isFunction())) {
        if (!(flags & MATCHTAG_SILENT))
            obj_typeerror(132, formaltag, actualtag);
        return FALSE;
    }

    if (actualtag == types->tag_nullfunc()) {
        // All functions are nullable. We use a separate constant for backward
        // compatibility; plugins and extensions check -1, not 0.
        if (formal->isFunction())
            return TRUE;

        if (!(flags & MATCHTAG_SILENT))
            error(154, pc_tagname(formaltag));
        return FALSE;
    }

    if (actualtag == types->tag_null()) {
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
    if (formaltag == types->tag_object())
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

    auto types = CompileContext::get().types();
    if (formal->ret_tag == types->tag_void()) {
        if (actual->ret_tag == 0)
            return TRUE;
    }
    return FALSE;
}

static bool
IsValidImplicitArrayCast(int formal_tag, int actual_tag)
{
    // Dumb check for now. This should really do a deep type validation though.
    // Fix this when we overhaul types in 1.12.
    auto types = CompileContext::get().types();
    if ((formal_tag == types->tag_any() && actual_tag != types->tag_string()) ||
        (actual_tag == types->tag_any() && formal_tag != types->tag_string()))
    {
        return true;
    }
    return formal_tag == actual_tag;
}

static int
funcarg_compare(const funcarg_t* formal, const funcarg_t* actual)
{
    // Check type.
    if (actual->type.ident != formal->type.ident)
        return FALSE;

    if (actual->type.ident == iREFARRAY || actual->type.ident == iARRAY) {
        if (actual->type.dim != formal->type.dim)
            return FALSE;
    }

    // Do not allow casting between different array types, eg:
    //   any[] <-> float[] is illegal.
    if (!formal->type.dim.empty() &&
        !IsValidImplicitArrayCast(formal->type.tag(), actual->type.tag()))
    {
        return FALSE;
    }

    if (!matchtag(formal->type.tag(), actual->type.tag(), MATCHTAG_SILENT | MATCHTAG_FUNCARG))
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

    auto types = CompileContext::get().types();
    if (formaltag == types->tag_function() && actual->isFunction())
        return TRUE;

    if (actualtag == types->tag_nullfunc())
        return TRUE;

    if (!actual->isFunction())
        return FALSE;

    functag_t* actualfn = functag_from_tag(actualtag);
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

static bool
HasTagOnInheritanceChain(Type* type, int tag)
{
    methodmap_t* map = type->asMethodmap();
    if (!map)
        return false;
    for (; map; map = map->parent) {
        if (map->tag == tag)
            return true;
    }
    return false;
}

int
matchtag(int formaltag, int actualtag, int flags)
{
    if (formaltag == actualtag)
        return TRUE;

    auto types = CompileContext::get().types();
    Type* actual = types->find(actualtag);
    Type* formal = types->find(formaltag);
    assert(actual && formal);

    if (formaltag == types->tag_string() && actualtag == 0)
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

    if (actualtag == types->tag_any())
        return TRUE;

    // We allow this even on function signature checks as a convenient shorthand,
    // even though it violates standard contravariance rules.
    if (formaltag == types->tag_any())
        return TRUE;

    if (formal->isFunction()) {
        if (!matchfunctags(formal, actual)) {
            error(100);
            return FALSE;
        }
        return TRUE;
    }

    if (flags & (MATCHTAG_COERCE | MATCHTAG_DEDUCE | MATCHTAG_FUNCARG)) {
        // See if the tag has a methodmap associated with it. If so, see if the given
        // tag is anywhere on the inheritance chain.
        if (HasTagOnInheritanceChain(actual, formaltag))
            return TRUE;

        // As a special exception to the "any" rule above, we allow the inverse
        // to succeed for signature matching. This is a convenience and allows
        // something like:
        //
        //   void f(DataPack x);
        //   void g(void f(Handle h), Handle h) {
        //     f(h);
        //   }
        //
        // In the future, we can insert a runtime check here. For now, we can't,
        // but we allow it anyway.
        if ((flags & MATCHTAG_FUNCARG) && HasTagOnInheritanceChain(formal, actualtag))
            return TRUE;
    }

    if (flags & MATCHTAG_ENUM_ASSN) {
        if (formal->isEnum() && actualtag == 0)
            return TRUE;
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

cell
calc(cell left, int oper_tok, cell right, char* boolresult)
{
    switch (oper_tok) {
        case '|':
            return (left | right);
        case '^':
            return (left ^ right);
        case '&':
            return (left & right);
        case tlEQ:
            return (left == right);
        case tlNE:
            return (left != right);
        case tSHR:
            return (left >> (int)right);
        case tSHRU:
            return ((ucell)left >> (ucell)right);
        case tSHL:
            return ((ucell)left << (int)right);
        case '+':
            return (left + right);
        case '-':
            return (left - right);
        case '*':
            return (left * right);
        case '/':
            if (right == 0) {
                error(29);
                return 0;
            }
            return left / right;
        case '%':
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

bool
is_valid_index_tag(int tag)
{
    auto types = CompileContext::get().types();
    if (tag == 0 || tag == types->tag_any() || tag == types->tag_string())
        return true;

    Type* idx_type = types->find(tag);
    return idx_type->isEnum();
}

int
checktag(int tag, int exprtag)
{
    AutoCountErrors errors;

    if (matchtag(tag, exprtag, MATCHTAG_COERCE))
        return TRUE; /* matching tag */

    // If matchtag() didn't error, report an error.
    if (errors.ok())
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
commutative(int oper)
{
    switch (oper) {
        case '+':
        case '*':
        case tlEQ:
        case tlNE:
        case '&':
        case '^':
        case '|':
            return true;
        default:
            return false;
    }
}
