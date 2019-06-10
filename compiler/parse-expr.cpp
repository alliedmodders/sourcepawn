/* vim: set ts=8 sts=2 sw=2 tw=99 et: */
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
 *      appreciated but is not reeq;quired.
 *  2.  Altered source versions must be plainly marked as such, and must not be
 *      misrepresented as being the original software.
 *  3.  This notice may not be removed or altered from any source distribution.
 *
 *  Version: $Id$
 */
#include "parse-expr.h"
#include <assert.h>
#include <string.h>
#include "errors.h"
#include "lexer.h"
#include "sc.h"
#include "sclist.h"
#include "scvars.h"
#include "types.h"

// The "op1" array in sc3.cpp must have the same ordering as if these lists
// were flattened.
int BaseExpressionParser::list3[] = {'*', '/', '%', 0};
int BaseExpressionParser::list4[] = {'+', '-', 0};
int BaseExpressionParser::list5[] = {tSHL, tSHR, tSHRU, 0};
int BaseExpressionParser::list6[] = {'&', 0};
int BaseExpressionParser::list7[] = {'^', 0};
int BaseExpressionParser::list8[] = {'|', 0};
int BaseExpressionParser::list9[] = {tlLE, tlGE, '<', '>', 0};
int BaseExpressionParser::list10[] = {tlEQ, tlNE, 0};
int BaseExpressionParser::list11[] = {tlAND, 0};
int BaseExpressionParser::list12[] = {tlOR, 0};

BaseExpressionParser::BaseExpressionParser()
 : bitwise_opercount_(0)
{}

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
BaseExpressionParser::nextop(int* opidx, int* list)
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
BaseExpressionParser::findnamedarg(arginfo* arg, char* name)
{
    int i;

    for (i = 0; arg[i].ident != 0 && arg[i].ident != iVARARGS; i++)
        if (strcmp(arg[i].name, name) == 0)
            return i;
    return -1;
}

cell
BaseExpressionParser::array_totalsize(symbol* sym)
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
BaseExpressionParser::array_levelsize(symbol* sym, int level)
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

cell
BaseExpressionParser::parse_defined()
{
    cell val;
    char* st;
    int paranthese = 0;
    while (matchtoken('('))
        paranthese++;
    int tok = lex(&val, &st);
    if (tok != tSYMBOL) {
        error(20, st); /* illegal symbol name */
        return 0;
    }
    symbol* sym = findloc(st);
    if (!sym)
        sym = findglb(st);
    if (sym && sym->ident != iFUNCTN && (sym->usage & uDEFINE) == 0)
        sym = nullptr; /* symbol is not a function, it is in the table, but not "defined" */
    val = !!sym;
    if (!val && find_subst(st, strlen(st), nullptr))
        val = 1;
    while (paranthese--)
        needtoken(')');
    return val;
}

cell
BaseExpressionParser::parse_sizeof()
{
    int paranthese = 0;
    while (matchtoken('('))
        paranthese++;

    cell result = sizeof_impl();

    while (paranthese--)
        needtoken(')');
    return result;
}

cell
BaseExpressionParser::sizeof_impl()
{
    cell val;
    char* st;
    int tok = lex(&val, &st);
    if (tok != tSYMBOL) {
        error(20, st);
        return 0;
    }

    symbol* sym = findloc(st);
    if (!sym)
        sym = findglb(st);
    if (!sym) {
        Type* type = gTypes.find(st);
        if (type) {
            if (symbol* sym = type->asEnumStruct())
                return sym->addr();
        }
        error(17, st);
        return 0;
    }
    if (sym->ident == iCONSTEXPR) {
        error(39); /* constant symbol has no size */
    } else if (sym->ident == iFUNCTN) {
        error(72); /* "function" symbol has no size */
    } else if ((sym->usage & uDEFINE) == 0) {
        error(17, st);
        return 0;
    }

    cell result = 1;
    markusage(sym, uREAD);
    if (sym->ident == iARRAY || sym->ident == iREFARRAY || sym->ident == iENUMSTRUCT) {
        int level;
        symbol* idxsym = NULL;
        symbol* subsym = sym;
        for (level = 0; matchtoken('['); level++) {
            // Forbid index operations on enum structs.
            if (sym->ident == iENUMSTRUCT || gTypes.find(sym->x.tags.index)->isEnumStruct())
                error(111, sym->name());

            idxsym = NULL;
            if (subsym != NULL && level == subsym->dim.array.level && matchtoken(tSYMBOL)) {
                char* idxname;
                int cmptag = subsym->x.tags.index;
                tokeninfo(&val, &idxname);
                if ((idxsym = findconst(idxname)) == NULL)
                    error(80, idxname); /* unknown symbol, or non-constant */
                else if (cmptag != idxsym->tag)
                    error(91, idxname); /* ambiguous constant */
            }
            needtoken(']');
            if (subsym != NULL)
                subsym = subsym->array_child();
        }

        Type* enum_type = nullptr;
        if (matchtoken(tDBLCOLON)) {
            if (subsym->ident != iENUMSTRUCT) {
                error(112, subsym->name());
                return 0;
            }
            enum_type = gTypes.find(subsym->tag);
        } else if (matchtoken('.')) {
            enum_type = gTypes.find(subsym->x.tags.index);
            if (!enum_type->asEnumStruct()) {
                error(116, sym->name());
                return 0;
            }
        }

        if (enum_type) {
            assert(enum_type->asEnumStruct());

            token_ident_t tok;
            if (!needsymbol(&tok))
                return 0;
            symbol* field = find_enumstruct_field(enum_type, tok.name);
            if (!field) {
                error(105, enum_type->name(), tok.name);
                return 0;
            }
            if (int string_size = field->dim.array.slength)
                return string_size;
            if (int array_size = field->dim.array.length)
                return array_size;
            return 1;
        }

        if (sym->ident == iENUMSTRUCT)
            return sym->addr();

        if (level > sym->dim.array.level + 1) {
            error(28, sym->name()); /* invalid subscript */
        } else if (level == sym->dim.array.level + 1) {
            result =
                (idxsym != NULL && idxsym->dim.array.length > 0) ? idxsym->dim.array.length : 1;
        } else {
            result = array_levelsize(sym, level);
        }
        if (result == 0 && strchr((char*)lptr, PREPROC_TERM) == NULL)
            error(163, sym->name()); /* indeterminate array size in "sizeof" expression */
    }
    return result;
}

cell
BaseExpressionParser::parse_cellsof()
{
    int paranthese = 0;
    while (matchtoken('('))
        paranthese++;

    cell val;
    char* st;
    int tok = lex(&val, &st);
    if (tok != tSYMBOL) {
        error(20, st);
        return 0;
    }

    symbol* sym = findloc(st);
    if (!sym)
        sym = findglb(st);
    if (!sym) {
        error(17, st);
        return 0;
    }
    if (sym->ident == iCONSTEXPR) {
        error(39); /* constant symbol has no size */
    } else if (sym->ident == iFUNCTN) {
        error(72); /* "function" symbol has no size */
    } else if ((sym->usage & uDEFINE) == 0) {
        error(17, st); /* undefined symbol (symbol is in the table, but it is "used" only) */
        return 0;
    }

    cell result = 1;
    if (sym->ident == iARRAY || sym->ident == iREFARRAY) {
        int level;
        symbol* idxsym = NULL;
        symbol* subsym = sym;
        for (level = 0; matchtoken('['); level++) {
            // Forbid index operations on enum structs.
            if (gTypes.find(sym->x.tags.index)->isEnumStruct())
                error(111, sym->name());

            idxsym = NULL;
            if (subsym != NULL && level == subsym->dim.array.level && matchtoken(tSYMBOL)) {
                char* idxname;
                int cmptag = subsym->x.tags.index;
                tokeninfo(&val, &idxname);
                if ((idxsym = findconst(idxname)) == NULL)
                    error(80, idxname); /* unknown symbol, or non-constant */
                else if (cmptag != idxsym->tag)
                    error(91, idxname); /* ambiguous constant */
            }
            needtoken(']');
            if (subsym != NULL)
                subsym = subsym->array_child();
        }
        if (level > sym->dim.array.level + 1) {
            error(28, sym->name()); /* invalid subscript */
        } else if (level == sym->dim.array.level + 1) {
            result =
                (idxsym != NULL && idxsym->dim.array.length > 0) ? idxsym->dim.array.length : 1;
        } else {
            result = array_levelsize(sym, level);
        }
        if (result == 0 && strchr((char*)lptr, PREPROC_TERM) == NULL)
            error(163, sym->name()); /* indeterminate array size in "sizeof" expression */
    }

    while (paranthese--)
        needtoken(')');
    return result;
}

cell
BaseExpressionParser::parse_tagof()
{
    int paranthese = 0;
    while (matchtoken('('))
        paranthese++;

    cell val;
    char* st;
    int tok = lex(&val, &st);
    if (tok != tSYMBOL && tok != tLABEL) {
        error(20, st); /* illegal symbol name */
        return 0;
    }

    int tag;
    symbol* sym = nullptr;
    if (tok == tLABEL) {
        Type* type = gTypes.find(st);
        tag = type ? type->tagid() : 0;
    } else {
        sym = findloc(st);
        if (sym == NULL)
            sym = findglb(st);
        if (sym == NULL) {
            error(17, st);
            return 0;
        }
        if ((sym->usage & uDEFINE) == 0) {
            error(17, st);
            return 0;
        }
        tag = sym->tag;
    }
    if (sym && (sym->ident == iARRAY || sym->ident == iREFARRAY)) {
        int level;
        symbol* idxsym = NULL;
        symbol* subsym = sym;
        for (level = 0; matchtoken('['); level++) {
            // Forbid index operations on enum structs.
            if (gTypes.find(sym->x.tags.index)->isEnumStruct())
                error(111, sym->name());

            idxsym = NULL;
            if (subsym != NULL && level == subsym->dim.array.level && matchtoken(tSYMBOL)) {
                char* idxname;
                int cmptag = subsym->x.tags.index;
                tokeninfo(&val, &idxname);
                if ((idxsym = findconst(idxname)) == NULL)
                    error(80, idxname); /* unknown symbol, or non-constant */
                else if (cmptag != idxsym->tag)
                    error(91, idxname); /* ambiguous constant */
            }
            needtoken(']');
            if (subsym != NULL)
                subsym = subsym->array_child();
        }
        if (level > sym->dim.array.level + 1)
            error(28, sym->name()); /* invalid subscript */
        else if (level == sym->dim.array.level + 1 && idxsym != NULL)
            tag = idxsym->x.tags.index;
    }
    while (paranthese--)
        needtoken(')');
    return tag;
}
