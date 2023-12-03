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
#include "value-inl.h"

namespace sp {

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

static inline bool MatchOperator(int oper, FunctionDecl* fun, Type* type1, Type* type2,
                                 int numparam)
{
    if (!oper)
        numparam = 1;

    const auto& args = fun->args();
    if (args.size() != size_t(numparam))
        return false;

    assert(numparam == 1 || numparam == 2);
    Type* types[2] = { type1, type2 };

    for (int i = 0; i < numparam; i++) {
        if (args[i]->type_info().ident != iVARIABLE)
            return false;
        if (args[i]->type_info().type != types[i])
            return false;
    }

    if (!oper && fun->type() != type2)
        return false;
    return true;
}

bool find_userop(SemaContext& sc, int oper, int tag1, int tag2, int numparam, const value* lval,
                 UserOperation* op)
{
    auto& cc = CompileContext::get();

    Type* type1 = cc.types()->find(tag1);
    Type* type2 = nullptr;
    if (numparam == 2)
        type2 = cc.types()->find(tag2);

    return find_userop(sc, oper, type1, type2, numparam, lval, op);
}

bool find_userop(SemaContext& sc, int oper, Type* type1, Type* type2, int numparam,
                 const value* lval, UserOperation* op)
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
    if (type1->isInt() && (numparam == 1 || type2->isInt()))
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
    Decl* chain = FindSymbol(sc, opername_atom);
    if (!chain)
        return false;

    FunctionDecl* decl = nullptr;
    bool swapparams;
    bool is_commutative = commutative(oper);
    for (auto iter = chain; iter; iter = iter->next) {
        auto fun = iter->as<FunctionDecl>();
        if (!fun)
            continue;
        fun = fun->canonical();

        bool matched = MatchOperator(oper, fun, type1, type2, numparam);
        bool swapped = false;
        if (!matched && is_commutative && type1 != type2 && oper) {
            matched = MatchOperator(oper, fun, type2, type1, numparam);
            swapped = true;
        }
        if (matched) {
            decl = fun;
            swapparams = swapped;
            break;
        }
    }

    if (!decl)
        return false;

    /* we don't want to use the redefined operator in the function that
     * redefines the operator itself, otherwise the snippet below gives
     * an unexpected recursion:
     *    fixed:operator+(fixed:a, fixed:b)
     *        return a + b
     */
    if (decl == sc.func()) {
        report(408);
    }

    markusage(decl, uREAD);

    op->sym = decl;
    op->oper = oper;
    op->paramspassed = (oper == 0) ? 1 : numparam;
    op->savepri = savepri;
    op->savealt = savealt;
    op->swapparams = swapparams;
    return true;
}

bool checktag_string(Type* type, const value* sym1) {
    if (sym1->ident == iARRAY || sym1->ident == iREFARRAY)
        return false;

    if ((sym1->type()->isChar() && type->isInt()) ||
        (sym1->type()->isInt() && type->isChar()))
    {
        return true;
    }
    return false;
}

bool checktag_string(int tag, const value* sym1) {
    auto types = CompileContext::get().types();
    return checktag_string(types->find(tag), sym1);
}

bool checkval_string(const value* sym1, const value* sym2) {
    if (sym1->ident == iARRAY || sym2->ident == iARRAY || sym1->ident == iREFARRAY ||
        sym2->ident == iREFARRAY)
    {
        return false;
    }
    if ((sym1->type()->isChar() && sym2->type()->isInt()) ||
        (sym1->type()->isInt() && sym2->type()->isChar()))
    {
        return true;
    }
    return false;
}

const char* type_to_name(int tag) {
    auto types = CompileContext::get().types();
    Type* type = types->find(tag);
    return type->prettyName();
}

bool matchtag_string(int ident, int tag) {
    auto types = CompileContext::get().types();
    return matchtag_string(ident, types->find(tag));
}

bool matchtag_string(int ident, Type* type) {
    if (ident == iARRAY || ident == iREFARRAY)
        return false;
    return type->isChar();
}

static bool matchobjecttags(Type* formal, Type* actual, int flags) {
    // objects never coerce to non-objects, YET.
    if (formal->isObject() && !(actual->isObject() || actual->isFunction())) {
        if (!(flags & MATCHTAG_SILENT))
            report(132) << formal << actual;
        return false;
    }

    if (actual->isNull()) {
        // All objects are nullable.
        if (formal->isFunction() || formal->isObject())
            return true;

        // Some methodmaps are nullable. The nullable property is inherited
        // automatically.
        auto map = formal->asMethodmap();
        if (map && map->nullable())
            return true;

        if (!(flags & MATCHTAG_SILENT))
            report(148) << formal;
        return false;
    }

    if (!formal->isObject() && actual->isObject()) {
        report(131) << formal << actual;
        return false;
    }

    // Every object coerces to "object".
    if (formal->isObject())
        return true;

    if (flags & MATCHTAG_COERCE) {
        report(134) << formal << actual;
        return false;
    }

    auto map = actual->asMethodmap();
    for (; map; map = map->parent()) {
        if (map->type() == formal)
            return true;
    }

    if (!(flags & MATCHTAG_SILENT))
        report(133) << formal << actual;
    return false;
}

static bool matchreturntag(const functag_t* formal, const functag_t* actual) {
    if (formal->ret_type == actual->ret_type)
        return true;

    if (formal->ret_type->isVoid()) {
        if (actual->ret_type->isInt())
            return true;
    }
    return false;
}

static bool IsValidImplicitArrayCast(Type* formal, Type* actual) {
    // Dumb check for now. This should really do a deep type validation though.
    // Fix this when we overhaul types in 1.12.
    if ((formal->isAny() && !actual->isChar()) || (actual->isAny() && !formal->isChar())) {
        return true;
    }
    return formal == actual;
}

static bool funcarg_compare(const typeinfo_t& formal, const typeinfo_t& actual) {
    // Check type.
    if (actual.ident != formal.ident)
        return false;

    if (actual.ident == iREFARRAY || actual.ident == iARRAY) {
        if (actual.dim_vec() != formal.dim_vec())
            return false;
    }

    // Do not allow casting between different array types, eg:
    //   any[] <. float[] is illegal.
    if (formal.numdim() &&
        !IsValidImplicitArrayCast(formal.type, actual.type))
    {
        return false;
    }

    if (!matchtag(formal.type, actual.type, MATCHTAG_SILENT | MATCHTAG_FUNCARG))
        return false;
    return true;
}

static bool functag_compare(const functag_t* formal, const functag_t* actual) {
    // Check return types.
    if (!matchreturntag(formal, actual))
        return false;

    // Make sure there are no trailing arguments.
    if (actual->args.size() > formal->args.size())
        return false;

    // Check arguments.
    for (size_t i = 0; i < formal->args.size(); i++) {
        const typeinfo_t& formal_arg = formal->args[i];

        if (i >= actual->args.size())
            return false;

        const typeinfo_t& actual_arg = actual->args[i];
        if (!funcarg_compare(formal_arg, actual_arg))
            return false;
    }
    return true;
}

static bool matchfunctags(Type* formal, Type* actual) {
    if (formal->isCanonicalFunction() && actual->isFunction())
        return true;

    if (actual->isNull())
        return true;

    if (!actual->isFunction())
        return false;

    auto actual_fe = actual->asFunction();
    if (!actual_fe || actual_fe->entries.empty())
        return false;

    functag_t* actualfn = actual_fe->entries.back();
    if (!actualfn)
        return false; 

    funcenum_t* e = formal->toFunction();
    if (!e)
        return false;

    for (const auto& formalfn : e->entries) {
        if (functag_compare(formalfn, actualfn))
            return true;
    }
    return false;
}

static bool HasTagOnInheritanceChain(Type* type, Type* other) {
    auto map = type->asMethodmap();
    if (!map)
        return false;
    for (; map; map = map->parent()) {
        if (map->type() == other)
            return true;
    }
    return false;
}

bool matchtag(int formaltag, int actualtag, int flags) {
    auto& cc = CompileContext::get();
    auto formal = cc.types()->find(formaltag);
    auto actual = cc.types()->find(actualtag);
    return matchtag(formal, actual, flags);
}

bool matchtag(Type* formal, Type* actual, int flags) {
    if (formal == actual)
        return true;

    if (formal->isChar() && actual->isInt())
        return true;

    if (formal->isObject() || actual->isObject())
        return matchobjecttags(formal, actual, flags);

    if (actual->isFunction() && !formal->isFunction()) {
        // We're being given a function, but the destination is not a function.
        report(130);
        return false;
    }

    // int coerces to bool/any.
    if ((flags & MATCHTAG_COERCE) && formal->isInt() && actual->coercesFromInt())
        return true;

    if (actual->isAny())
        return true;

    // We allow this even on function signature checks as a convenient shorthand,
    // even though it violates standard contravariance rules.
    if (formal->isAny())
        return true;

    if (formal->isFunction()) {
        if (!matchfunctags(formal, actual)) {
            report(100);
            return false;
        }
        return true;
    }

    if (flags & (MATCHTAG_COERCE | MATCHTAG_DEDUCE | MATCHTAG_FUNCARG)) {
        // See if the tag has a methodmap associated with it. If so, see if the given
        // tag is anywhere on the inheritance chain.
        if (HasTagOnInheritanceChain(actual, formal))
            return true;

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
        if ((flags & MATCHTAG_FUNCARG) && HasTagOnInheritanceChain(formal, actual))
            return true;
    }

    if (flags & MATCHTAG_ENUM_ASSN) {
        if (formal->isEnum() && actual->isInt())
            return true;
    }

    if (!(flags & MATCHTAG_SILENT))
        report(213) << formal << actual;
    return false;
}

bool matchtag_commutative(Type* formal, Type* actual, int flags) {
    if (matchtag(formal, actual, flags | MATCHTAG_SILENT))
        return true;
    if (matchtag(actual, formal, flags | MATCHTAG_SILENT))
        return false;
    // Report the error.
    return matchtag(formal, actual, flags);
}

bool matchtag_commutative(int formaltag, int actualtag, int flags) {
    auto& cc = CompileContext::get();
    auto formal = cc.types()->find(formaltag);
    auto actual = cc.types()->find(actualtag);
    return matchtag_commutative(formal, actual, flags);
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
                report(29);
                return 0;
            }
            return left / right;
        case '%':
            if (right == 0) {
                report(29);
                return 0;
            }
            return left % right;
    }
    assert(false);
    report(29); /* invalid expression, assumed 0 (this should never occur) */
    return 0;
}

bool IsValidIndexType(Type* type) {
    return type->isInt() || type->isAny() || type->isChar() || type->isEnum();
}

bool checktag(Type* type, Type* expr_type) {
    AutoCountErrors errors;

    if (matchtag(type, expr_type, MATCHTAG_COERCE))
        return true; /* matching tag */

    // If matchtag() didn't error, report an error.
    if (errors.ok())
        report(213) << type << expr_type;
    return false;
}

bool checktag(int tag, int exprtag) {
    auto types = CompileContext::get().types();
    Type* type = types->find(tag);
    Type* expr_type = types->find(exprtag);
    return checktag(type, expr_type);
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

} // namespace sp
