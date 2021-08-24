// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
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
//
//  Version: $Id$

#include <assert.h>
#include <string.h>

#include <amtl/am-raii.h>
#include "emitter.h"
#include "errors.h"
#include "lexer.h"
#include "new-parser.h"
#include "optimizer.h"
#include "parse-node.h"
#include "sc.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "types.h"

using namespace sp;

bool Parser::sInPreprocessor = false;
bool Parser::sDetectedIllegalPreprocessorSymbols = false;
bool Parser::sAllowEnumNameBinding = false;

void
Parser::parse()
{
    while (freading) {
        Stmt* decl = nullptr;

        token_t tok;
        switch (lextok(&tok)) {
            case 0:
                /* ignore zero's */
                break;
            case tSYMBOL:
                // Fallthrough.
            case tINT:
            case tOBJECT:
            case tCHAR:
            case tVOID:
            case tLABEL:
                lexpush();
                // Fallthrough.
            case tNEW:
            case tSTATIC:
            case tPUBLIC:
            case tSTOCK:
            case tOPERATOR:
            case tNATIVE:
            case tFORWARD:
                decl = parse_unknown_decl(&tok);
                break;
            case tSTATIC_ASSERT:
                decl = parse_static_assert();
                break;
            case tFUNCENUM:
            case tFUNCTAG:
                error(FATAL_ERROR_FUNCENUM);
                break;
            case tTYPEDEF:
                decl = parse_typedef();
                break;
            case tTYPESET:
                decl = parse_typeset();
                break;
            case tSTRUCT:
                decl = parse_pstruct();
                break;
            case tCONST:
                decl = parse_const(sGLOBAL);
                break;
            case tENUM:
                if (matchtoken(tSTRUCT))
                    decl_enumstruct();
                else
                    decl = parse_enum(sGLOBAL);
                break;
            case tMETHODMAP:
                domethodmap(Layout_MethodMap);
                break;
            case tUSING:
                decl = parse_using();
                break;
            case '}':
                error(54); /* unmatched closing brace */
                break;
            case '{':
                error(55); /* start of function body without function header */
                break;
            default:
                if (freading) {
                    error(10);    /* illegal function or declaration */
                    lexclr(TRUE); /* drop the rest of the line */
                    litidx = 0;   /* drop any literal arrays (strings) */
                }
        }

        // Until we can eliminate the two-pass parser, top-level decls must be
        // resolved immediately.
        if (decl)
            decl->Process();
    }
}

Stmt*
Parser::parse_unknown_decl(const token_t* tok)
{
    declinfo_t decl;

    if (tok->id == tNATIVE || tok->id == tFORWARD) {
        parse_decl(&decl, DECLFLAG_MAYBE_FUNCTION);
        funcstub(tok->id, &decl, NULL);
        return nullptr;
    }

    auto pos = current_pos();

    int fpublic = FALSE, fstock = FALSE, fstatic = FALSE;
    switch (tok->id) {
        case tPUBLIC:
            fpublic = TRUE;
            break;
        case tSTOCK:
            fstock = TRUE;
            if (matchtoken(tSTATIC))
                fstatic = TRUE;
            break;
        case tSTATIC:
            fstatic = TRUE;

            // For compatibility, we must include this case. Though "stock" should
            // come first.
            if (matchtoken(tSTOCK))
                fstock = TRUE;
            break;
    }

    int flags = DECLFLAG_MAYBE_FUNCTION | DECLFLAG_VARIABLE | DECLFLAG_ENUMROOT;
    if (tok->id == tNEW)
        flags |= DECLFLAG_OLD;

    if (!parse_decl(&decl, flags)) {
        // Error will have been reported earlier. Reset |decl| so we don't crash
        // thinking tag -1 has every flag.
        decl.type.tag = 0;
    }

    // Hacky bag o' hints as to whether this is a variable decl.
    bool probablyVariable = tok->id == tNEW || decl.type.has_postdims || !lexpeek('(') ||
                            decl.type.is_const;

    if (!decl.opertok && probablyVariable) {
        if (tok->id == tNEW && decl.type.is_new)
            error(143);
        Type* type = gTypes.find(decl.type.tag);
        if (type && type->kind() == TypeKind::Struct) {
            Expr* init = nullptr;
            if (matchtoken('=')) {
                needtoken('{');
                init = struct_init();
            }
            matchtoken(';');
            // Without an initializer, the stock keyword is implied.
            return new VarDecl(pos, gAtoms.add(decl.name), decl.type, sGLOBAL, fpublic && init,
                               false, !init, init);
        }
        declglb(&decl, fpublic, fstatic, fstock);
    } else {
        if (!newfunc(&decl, NULL, fpublic, fstatic, fstock, NULL)) {
            // Illegal function or declaration. Drop the line, reset literal queue.
            error(10);
            lexclr(TRUE);
            litidx = 0;
        }
    }
    return nullptr;
}

Decl*
Parser::parse_enum(int vclass)
{
    auto pos = current_pos();

    cell val;
    char* str;
    Atom* label = nullptr;
    if (lex(&val, &str) == tLABEL)
        label = gAtoms.add(str);
    else
        lexpush();

    Atom* name = nullptr;
    if (lex(&val, &str) == tSYMBOL)
        name = gAtoms.add(str);
    else
        lexpush();

    cell increment = 1;
    cell multiplier = 1;
    if (matchtoken('(')) {
        error(228);
        if (matchtoken(taADD)) {
            exprconst(&increment, NULL, NULL);
        } else if (matchtoken(taMULT)) {
            exprconst(&multiplier, NULL, NULL);
        } else if (matchtoken(taSHL)) {
            exprconst(&val, NULL, NULL);
            while (val-- > 0)
                multiplier *= 2;
        }
        needtoken(')');
    }

    EnumDecl* decl = new EnumDecl(pos, vclass, label, name, increment, multiplier);

    needtoken('{');

    cell size;
    do {
        if (matchtoken('}')) {
            lexpush();
            break;
        }
        if (matchtoken(tLABEL))
            error(153);

        sp::Atom* field_name = nullptr;
        if (needtoken(tSYMBOL)) {
            tokeninfo(&val, &str);
            field_name = gAtoms.add(str);
        }

        auto pos = current_pos();

        if (matchtoken('[')) {
            error(153);
            exprconst(&size, nullptr, nullptr);
            needtoken(']');
        }

        Expr* value = nullptr;
        if (matchtoken('='))
            value = hier14();

        if (field_name)
            decl->fields().push_back(EnumField(pos, field_name, value));
    } while (matchtoken(','));

    needtoken('}');
    matchtoken(';');
    return decl;
}

Decl*
Parser::parse_pstruct()
{
    PstructDecl* struct_decl = nullptr;

    auto pos = current_pos();

    token_ident_t ident = {};
    if (needsymbol(&ident))
        struct_decl = new PstructDecl(pos, gAtoms.add(ident.name));

    needtoken('{');
    do {
        if (matchtoken('}')) {
            /* Quick exit */
            lexpush();
            break;
        }

        declinfo_t decl = {};
        decl.type.ident = iVARIABLE;
        decl.type.size = 1;

        needtoken(tPUBLIC);
        auto pos = current_pos();
        if (!parse_new_decl(&decl, nullptr, DECLFLAG_FIELD)) {
            lexclr(TRUE);
            continue;
        }

        if (struct_decl) {
            auto name = gAtoms.add(decl.name);
            struct_decl->fields().push_back(StructField(pos, name, decl.type));
        }

        require_newline(TerminatorPolicy::NewlineOrSemicolon);
    } while (!lexpeek('}'));

    needtoken('}');
    matchtoken(';'); // eat up optional semicolon
    return struct_decl;
}

Decl*
Parser::parse_typedef()
{
    auto pos = current_pos();

    token_ident_t ident;
    if (!needsymbol(&ident))
        return new ErrorDecl();

    needtoken('=');

    auto type = parse_function_type();
    return new TypedefDecl(pos, gAtoms.add(ident.name), type);
}

Decl*
Parser::parse_typeset()
{
    auto pos = current_pos();

    token_ident_t ident;
    if (!needsymbol(&ident))
        return new ErrorDecl();

    TypesetDecl* decl = new TypesetDecl(pos, gAtoms.add(ident.name));

    needtoken('{');
    while (!matchtoken('}')) {
        auto type = parse_function_type();
        decl->types().push_back(type);
    }

    require_newline(TerminatorPolicy::NewlineOrSemicolon);
    return decl;
}

Decl*
Parser::parse_using()
{
    auto pos = current_pos();

    auto validate = []() -> bool {
        token_ident_t ident;
        if (!needsymbol(&ident))
            return false;
        if (strcmp(ident.name, "__intrinsics__") != 0) {
            error(156);
            return false;
        }
        if (!needtoken('.'))
            return false;
        if (!needsymbol(&ident))
            return false;
        if (strcmp(ident.name, "Handle") != 0) {
            error(156);
            return false;
        }
        return true;
    };
    if (!validate()) {
        lexclr(TRUE);
        return new ErrorDecl();
    }

    require_newline(TerminatorPolicy::Semicolon);
    return new UsingDecl(pos);
}

Stmt*
Parser::parse_const(int vclass)
{
    StmtList* list = nullptr;
    Stmt* decl = nullptr;

    do {
        auto pos = current_pos();

        // Since spcomp is terrible, it's hard to use parse_decl() here - there
        // are all sorts of restrictions on const. We just implement some quick
        // detection instead.
        int tag = 0;
        token_t tok;
        switch (lextok(&tok)) {
            case tINT:
            case tOBJECT:
            case tCHAR:
                tag = parse_new_typename(&tok);
                break;
            case tLABEL:
                tag = pc_addtag(tok.str);
                break;
            case tSYMBOL:
                // See if we can peek ahead another symbol.
                if (lexpeek(tSYMBOL)) {
                    // This is a new-style declaration.
                    tag = parse_new_typename(&tok);
                } else {
                    // Otherwise, we got "const X ..." so the tag is int. Give the
                    // symbol back to the lexer so we get it as the name.
                    lexpush();
                }
                break;
            default:
                error(122);
                break;
        }

        sp::Atom* name = nullptr;
        if (expecttoken(tSYMBOL, &tok))
            name = gAtoms.add(tok.str);

        needtoken('=');

        int expr_val, expr_tag;
        exprconst(&expr_val, &expr_tag, nullptr);

        typeinfo_t type = {};
        type.size = 1;
        type.tag = tag;
        type.is_const = true;

        if (!name)
            continue;

        VarDecl* var = new ConstDecl(pos, name, type, vclass, expr_tag, expr_val);
        if (decl) {
            if (!list) {
                list = new StmtList(var->pos());
                list->stmts().push_back(decl);
            }
            list->stmts().push_back(var);
        } else {
            decl = var;
        }
    } while (matchtoken(','));

    needtoken(tTERM);
    return list ? list : decl;
}

int
Parser::expression(value* lval)
{
    Expr* expr = hier14();
    if (!expr->Bind() || !expr->Analyze()) {
        sideeffect = TRUE;
        *lval = value::ErrorValue();
        return FALSE;
    }
    expr->ProcessUses();

    *lval = expr->val();
    if (cc_ok())
        expr->Emit();

    sideeffect = expr->HasSideEffects();
    return expr->lvalue();
}

Expr*
Parser::hier14()
{
    Expr* node = hier13();

    cell val;
    char* st;
    int tok = lex(&val, &st);
    auto pos = current_pos();
    switch (tok) {
        case taOR:
        case taXOR:
        case taAND:
        case taADD:
        case taSUB:
        case taMULT:
        case taDIV:
        case taMOD:
        case taSHRU:
        case taSHR:
        case taSHL:
            break;
        case '=': /* simple assignment */
            if (sc_intest)
                error(211); /* possibly unintended assignment */
            break;
        default:
            lexpush();
            return node;
    }

    Expr* right = hier14();
    return new BinaryExpr(pos, tok, node, right);
}

Expr*
Parser::plnge(int* opstr, NewHierFn hier)
{
    int opidx;

    Expr* node = (this->*hier)();
    if (nextop(&opidx, opstr) == 0)
        return node;

    do {
        auto pos = current_pos();
        Expr* right = (this->*hier)();

        int token = opstr[opidx];
        switch (token) {
            case tlOR:
            case tlAND:
                node = new LogicalExpr(pos, token, node, right);
                break;
            default:
                node = new BinaryExpr(pos, token, node, right);
                break;
        }
    } while (nextop(&opidx, opstr));

    return node;
}

Expr*
Parser::plnge_rel(int* opstr, NewHierFn hier)
{
    int opidx;

    Expr* first = (this->*hier)();
    if (nextop(&opidx, opstr) == 0)
        return first;

    ChainedCompareExpr* chain = new ChainedCompareExpr(current_pos(), first);

    do {
        auto pos = current_pos();
        Expr* right = (this->*hier)();

        chain->ops().push_back(CompareOp(pos, opstr[opidx], right));
    } while (nextop(&opidx, opstr));

    return chain;
}

Expr*
Parser::hier13()
{
    Expr* node = hier12();
    if (matchtoken('?')) {
        auto pos = current_pos();
        Expr* left;
        {
            /* do not allow tagnames here (colon is a special token) */
            ke::SaveAndSet<bool> allowtags(&sc_allowtags, false);
            left = hier13();
        }
        needtoken(':');
        Expr* right = hier13();
        return new TernaryExpr(pos, node, left, right);
    }
    return node;
}

Expr*
Parser::hier12()
{
    return plnge(list12, &Parser::hier11);
}

Expr*
Parser::hier11()
{
    return plnge(list11, &Parser::hier10);
}

Expr*
Parser::hier10()
{
    return plnge(list10, &Parser::hier9);
}

Expr*
Parser::hier9()
{
    return plnge_rel(list9, &Parser::hier8);
}

Expr*
Parser::hier8()
{
    return plnge(list8, &Parser::hier7);
}

Expr*
Parser::hier7()
{
    return plnge(list7, &Parser::hier6);
}

Expr*
Parser::hier6()
{
    return plnge(list6, &Parser::hier5);
}

Expr*
Parser::hier5()
{
    return plnge(list5, &Parser::hier4);
}

Expr*
Parser::hier4()
{
    return plnge(list4, &Parser::hier3);
}

Expr*
Parser::hier3()
{
    return plnge(list3, &Parser::hier2);
}

Expr*
Parser::hier2()
{
    int val;
    char* st;
    int tok = lex(&val, &st);
    auto pos = current_pos();
    switch (tok) {
        case tINC: /* ++lval */
        case tDEC: /* --lval */
        {
            Expr* node = hier2();
            return new PreIncExpr(pos, tok, node);
        }
        case '~':
        case '-':
        case '!':
        {
            Expr* node = hier2();
            return new UnaryExpr(pos, tok, node);
        }
        case tNEW:
        {
            token_ident_t ident;
            if (!needsymbol(&ident))
                return new ErrorExpr();

            Expr* target = new SymbolExpr(current_pos(), gAtoms.add(ident.name));

            needtoken('(');
            return parse_call(pos, tok, target);
        }
        case tLABEL: /* tagname override */
        {
            int tag = pc_addtag(st);
            if (sc_require_newdecls) {
                // Warn: old style cast used when newdecls pragma is enabled
                error(240, st, type_to_name(tag));
            }
            Expr* expr = hier2();
            return new CastExpr(pos, tok, tag, expr);
        }
        case tDEFINED:
        {
            int parens = 0;
            while (matchtoken('('))
                parens++;

            token_ident_t ident;
            if (!needsymbol(&ident))
                return new ErrorExpr();
            while (parens--)
                needtoken(')');
            return new IsDefinedExpr(pos, gAtoms.add(ident.name));
        }
        case tSIZEOF:
        {
            int parens = 0;
            while (matchtoken('('))
                parens++;

            token_ident_t ident;
            if (matchtoken(tTHIS)) {
                strcpy(ident.name, "this");
            } else {
                if (!needsymbol(&ident))
                    return new ErrorExpr();
            }

            int array_levels = 0;
            while (matchtoken('[')) {
                array_levels++;
                needtoken(']');
            }

            Atom* field = nullptr;
            int token = lex(&val, &st);
            if (token == tDBLCOLON || token == '.') {
                token_ident_t field_name;
                if (!needsymbol(&field_name))
                    return new ErrorExpr();
                field = gAtoms.add(field_name.name);
            } else {
                lexpush();
                token = 0;
            }

            while (parens--)
                needtoken(')');

            Atom* name = gAtoms.add(ident.name);
            return new SizeofExpr(pos, name, field, token, array_levels);
        }
        default:
            lexpush();
            break;
    }

    Expr* node = hier1();

    /* check for postfix operators */
    if (matchtoken(';')) {
        /* Found a ';', do not look further for postfix operators */
        lexpush(); /* push ';' back after successful match */
        return node;
    }
    if (matchtoken(tTERM)) {
        /* Found a newline that ends a statement (this is the case when
         * semicolons are optional). Note that an explicit semicolon was
         * handled above. This case is similar, except that the token must
         * not be pushed back.
         */
        return node;
    }

    tok = lex(&val, &st);
    switch (tok) {
        case tINC: /* lval++ */
        case tDEC: /* lval-- */
            return new PostIncExpr(current_pos(), tok, node);
        default:
            lexpush();
            break;
    }
    return node;
}

Expr*
Parser::hier1()
{
    Expr* base = nullptr;
    if (matchtoken(tVIEW_AS)) {
        base = parse_view_as();
    } else {
        base = primary();
    }

    for (;;) {
        char* st;
        cell val;
        int tok = lex(&val, &st);
        if (tok == '.' || tok == tDBLCOLON) {
            auto pos = current_pos();
            token_ident_t ident;
            if (!needsymbol(&ident))
                break;
            base = new FieldAccessExpr(pos, tok, base, gAtoms.add(ident.name));
        } else if (tok == '[') {
            auto pos = current_pos();
            Expr* inner = hier14();
            base = new IndexExpr(pos, base, inner);
            needtoken(']');
        } else if (tok == '(') {
            auto pos = current_pos();
            base = parse_call(pos, tok, base);
        } else {
            lexpush();
            break;
        }
    }
    return base;
}

Expr*
Parser::primary()
{
    if (matchtoken('(')) { /* sub-expression - (expression,...) */
        /* no longer in "test" expression */
        ke::SaveAndSet<bool> in_test(&sc_intest, false);
        /* allow tagnames to be used in parenthesized expressions */
        ke::SaveAndSet<bool> allowtags(&sc_allowtags, true);

        CommaExpr* expr = new CommaExpr(current_pos());
        do {
            Expr* child = hier14();
            expr->exprs().push_back(child);
        } while (matchtoken(','));
        needtoken(')');
        lexclr(FALSE); /* clear lex() push-back, it should have been
                        * cleared already by needtoken() */
        return expr;
    }

    cell val;
    char* st;
    int tok = lex(&val, &st);

    if (tok == tTHIS)
        return new ThisExpr(current_pos());
    if (tok == tSYMBOL)
        return new SymbolExpr(current_pos(), gAtoms.add(st));

    lexpush();

    return constant();
}

Expr*
Parser::constant()
{
    cell val;
    char* st;
    int tok = lex(&val, &st);
    auto pos = current_pos();
    switch (tok) {
        case tNULL:
            return new NullExpr(pos);
        case tNUMBER:
            return new NumberExpr(pos, val);
        case tRATIONAL:
            return new FloatExpr(pos, val);
        case tSTRING:
            return new StringExpr(pos, current_token()->str, current_token()->len);
        case '{':
        {
            ArrayExpr* expr = new ArrayExpr(pos);
            do {
                Expr* child = hier14();
                expr->exprs().push_back(child);
            } while (matchtoken(','));
            if (!needtoken('}'))
                lexclr(FALSE);
            return expr;
        }
        default:
          error(29);
          return new ErrorExpr();
    }
}

CallExpr*
Parser::parse_call(const token_pos_t& pos, int tok, Expr* target)
{
    CallExpr* call = new CallExpr(pos, tok, target);

    if (matchtoken(')'))
        return call;

    bool named_params = false;
    do {
        sp::Atom* name = nullptr;
        if (matchtoken('.')) {
            named_params = true;

            token_ident_t ident;
            if (!needsymbol(&ident))
                break;
            needtoken('=');

            name = gAtoms.add(ident.name);
        } else {
            if (named_params)
                error(44);
        }

        Expr* expr = nullptr;
        if (!matchtoken('_'))
            expr = hier14();

        call->args().emplace_back(name, expr);

        if (matchtoken(')'))
            break;
        if (!needtoken(','))
            break;
    } while (freading && !matchtoken(tENDEXPR));

    return call;
}

Expr*
Parser::parse_view_as()
{
    auto pos = current_pos();

    needtoken('<');
    int tag = 0;
    {
        token_t tok;
        lextok(&tok);
        if (!parse_new_typename(&tok, &tag))
            tag = 0;
    }
    needtoken('>');

    int paren = needtoken('(');

    Expr* expr = hier14();
    if (paren)
        needtoken(')');
    else
        matchtoken(')');
    return new CastExpr(pos, tVIEW_AS, tag, expr);
}

Expr*
Parser::struct_init()
{
    StructExpr* init = new StructExpr(current_pos());

    // '}' has already been lexed.
    do {
        sp::Atom* name = nullptr;

        token_ident_t ident;
        if (needsymbol(&ident))
            name = gAtoms.add(ident.name);

        needtoken('=');

        auto pos = current_pos();

        cell value;
        char* str;
        Expr* expr = nullptr;
        switch (lex(&value, &str)) {
            case tSTRING:
                expr = new StringExpr(pos, current_token()->str, current_token()->len);
                break;
            case tNUMBER:
                expr = new NumberExpr(pos, value);
                break;
            case tRATIONAL:
                expr = new FloatExpr(pos, value);
                break;
            case tSYMBOL:
                expr = new SymbolExpr(pos, gAtoms.add(str));
                break;
            default:
                error(1, "-constant-", str);
                break;
        }

        if (name && expr)
            init->fields().push_back(StructInitField(name, expr));
    } while (matchtoken(',') && !lexpeek('}'));

    needtoken('}');
    return init;
}

Stmt*
Parser::parse_static_assert()
{
    auto pos = current_pos();

    needtoken('(');

    int expr_val, expr_tag;
    bool is_const = exprconst(&expr_val, &expr_tag, nullptr);

    PoolString * text = nullptr;
    if (matchtoken(',') && needtoken(tSTRING)) {
        auto tok = current_token();
        text = new PoolString(tok->str, tok->len);
    }

    needtoken(')');
    require_newline(TerminatorPolicy::NewlineOrSemicolon);

    if (!is_const)
        return nullptr;

    return new StaticAssertStmt(pos, expr_val, text);
}
