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

#include <deque>

#include <amtl/am-raii.h>
#include "emitter.h"
#include "errors.h"
#include "lexer.h"
#include "lexer-inl.h"
#include "parser.h"
#include "parse-node.h"
#include "sc.h"
#include "sclist.h"
#include "sctracker.h"
#include "scvars.h"
#include "semantics.h"
#include "types.h"

using namespace sp;

bool Parser::sInPreprocessor = false;
bool Parser::sDetectedIllegalPreprocessorSymbols = false;
int Parser::sActive = 0;

Parser::Parser()
{
    sActive++;
}

Parser::~Parser()
{
    sActive--;
}

StmtList*
Parser::parse()
{
    ke::SaveAndSet<bool> limit_errors(&sc_one_error_per_statement, true);

    std::deque<Stmt*> add_to_end;

    auto list = new ParseTree(token_pos_t{});
    while (freading) {
        Stmt* decl = nullptr;

        switch (lex()) {
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
            case tFORWARD: {
                auto tok = *current_token();
                decl = parse_unknown_decl(&tok);
                break;
            }
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
                    decl = parse_enumstruct();
                else
                    decl = parse_enum(sGLOBAL);
                break;
            case tMETHODMAP:
                decl = parse_methodmap();
                break;
            case tUSING:
                decl = parse_using();
                break;
            case tSYN_PRAGMA_UNUSED:
                // These get added to the end so they can bind before use.
                if (auto decl = parse_pragma_unused())
                    add_to_end.emplace_back(decl);
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
                }
        }

        // Until we can eliminate the two-pass parser, top-level decls must be
        // resolved immediately.
        if (decl) {
            errorset(sRESET, 0);

            list->stmts().emplace_back(decl);
        }

        pc_deprecate = {};
    }

    while (!add_to_end.empty()) {
        list->stmts().emplace_back(add_to_end.front());
        add_to_end.pop_front();
    }
    return list;
}

Stmt*
Parser::parse_unknown_decl(const full_token_t* tok)
{
    declinfo_t decl = {};

    if (tok->id == tNATIVE || tok->id == tFORWARD) {
        parse_decl(&decl, DECLFLAG_MAYBE_FUNCTION);
        return parse_inline_function(tok->id, decl, nullptr);
    }

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
        decl.type.set_tag(0);
    }

    // Hacky bag o' hints as to whether this is a variable decl.
    bool probablyVariable = tok->id == tNEW || decl.type.has_postdims || !lexpeek('(') ||
                            decl.type.is_const;

    if (!decl.opertok && probablyVariable) {
        if (tok->id == tNEW && decl.type.is_new)
            error(143);

        VarParams params;
        params.vclass = sGLOBAL;
        params.is_public = !!fpublic;
        params.is_static = !!fstatic;
        params.is_stock = !!fstock;

        auto stmt = parse_var(&decl, params);

        // The old parser had a different line ending policy for struct
        // initializers, so we approximate that here.
        if (params.struct_init)
            matchtoken(';');
        else
            needtoken(tTERM);
        return stmt;
    } else {
        auto pos = current_pos();
        FunctionInfo* info = new FunctionInfo(pos, decl);
        info->set_name(decl.name);
        if (fpublic)
            info->set_is_public();
        if (fstatic)
            info->set_is_static();
        if (fstock)
            info->set_is_stock();
        if (!parse_function(info, 0))
            return nullptr;
        auto stmt = new FunctionDecl(pos, info);
        if (!pc_deprecate.empty()) {
            stmt->set_deprecate(pc_deprecate);
            pc_deprecate = {};
        }
        return stmt;
    }
    return nullptr;
}

bool
Parser::PreprocExpr(cell* val, int* tag)
{
    Parser parser;
    auto expr = parser.hier14();

    SemaContext sc;
    if (!expr->Bind(sc) || !expr->Analyze(sc))
        return false;
    return expr->EvalConst(val, tag);
}

Stmt*
Parser::parse_var(declinfo_t* decl, VarParams& params)
{
    StmtList* list = nullptr;
    Stmt* stmt = nullptr;

    for (;;) {
        auto pos = current_pos();

        Expr* init = nullptr;
        if (matchtoken('='))
            init = var_init(params.vclass);

        // Keep updating this field, as we only care about the last initializer.
        params.struct_init = init && init->AsStructExpr();

        VarDecl* var = new VarDecl(pos, decl->name, decl->type, params.vclass, params.is_public,
                                   params.is_static, params.is_stock, init);
        if (!params.autozero)
            var->set_no_autozero();

        if (stmt) {
            if (!list) {
                list = new StmtList(var->pos());
                list->stmts().emplace_back(stmt);
            }
            list->stmts().emplace_back(var);
        } else {
            stmt = var;
        }

        if (!matchtoken(','))
            break;

        if (decl->type.is_new)
            reparse_new_decl(decl, DECLFLAG_VARIABLE | DECLFLAG_ENUMROOT);
        else
            reparse_old_decl(decl, DECLFLAG_VARIABLE | DECLFLAG_ENUMROOT);
    }
    return list ? list : stmt;
}

Decl*
Parser::parse_enum(int vclass)
{
    auto pos = current_pos();

    Atom* label = nullptr;
    if (lex() == tLABEL)
        label = current_token()->atom;
    else
        lexpush();

    Atom* name = nullptr;
    if (lex() == tSYMBOL)
        name = current_token()->atom;
    else
        lexpush();

    cell increment = 1;
    cell multiplier = 1;
    if (matchtoken('(')) {
        error(228);
        if (matchtoken(taADD)) {
            if (needtoken(tNUMBER)) {
                if (current_token()->value != 1)
                    report(404);
            }
        } else if (matchtoken(taMULT)) {
            if (needtoken(tNUMBER))
                report(404);
        } else if (matchtoken(taSHL)) {
            if (needtoken(tNUMBER)) {
                if (current_token()->value != 1)
                    report(404);
                multiplier = 2;
            }
        }
        needtoken(')');
    }

    EnumDecl* decl = new EnumDecl(pos, vclass, label, name, increment, multiplier);

    needtoken('{');

    do {
        if (matchtoken('}')) {
            lexpush();
            break;
        }
        if (matchtoken(tLABEL))
            error(153);

        sp::Atom* field_name = nullptr;
        if (needtoken(tSYMBOL))
            field_name = current_token()->atom;

        auto pos = current_pos();

        if (matchtoken('[')) {
            error(153);
            if (!matchtoken(']')) {
                hier14();
                needtoken(']');
            }
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
Parser::parse_enumstruct()
{
    auto pos = current_pos();

    sp::Atom* struct_name;
    if (!needsymbol(&struct_name))
        return nullptr;

    if (!matchtoken('{')) {
        needtoken('{');
        return nullptr;
    }

    auto stmt = new EnumStructDecl(pos, struct_name);

    int opening_line = fline;
    while (!matchtoken('}')) {
        if (!freading) {
            error(151, opening_line);
            break;
        }

        declinfo_t decl = {};
        if (!parse_new_decl(&decl, nullptr, DECLFLAG_FIELD))
            continue;

        auto decl_pos = current_pos();
        if (!decl.type.has_postdims && lexpeek('(')) {
            auto info = new FunctionInfo(decl_pos, decl);
            info->set_name(decl.name);
            info->set_is_stock();
            if (!parse_function(info, 0))
                continue;

            auto method = new FunctionDecl(decl_pos, info);
            stmt->methods().emplace_back(method);
            continue;
        }

        stmt->fields().emplace_back(EnumStructField{decl_pos, decl});

        require_newline(TerminatorPolicy::Semicolon);
    }

    require_newline(TerminatorPolicy::Newline);
    return stmt;
}

Decl*
Parser::parse_pstruct()
{
    PstructDecl* struct_decl = nullptr;

    auto pos = current_pos();

    sp::Atom* ident;
    if (needsymbol(&ident))
        struct_decl = new PstructDecl(pos, ident);

    needtoken('{');
    do {
        if (matchtoken('}')) {
            /* Quick exit */
            lexpush();
            break;
        }

        declinfo_t decl = {};
        decl.type.ident = iVARIABLE;

        needtoken(tPUBLIC);
        auto pos = current_pos();
        if (!parse_new_decl(&decl, nullptr, DECLFLAG_FIELD)) {
            lexclr(TRUE);
            continue;
        }

        if (struct_decl)
            struct_decl->fields().push_back(StructField(pos, decl.name, decl.type));

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

    sp::Atom* ident;
    if (!needsymbol(&ident))
        return new ErrorDecl();

    needtoken('=');

    auto type = parse_function_type();
    return new TypedefDecl(pos, ident, type);
}

Decl*
Parser::parse_typeset()
{
    auto pos = current_pos();

    sp::Atom* ident;
    if (!needsymbol(&ident))
        return new ErrorDecl();

    TypesetDecl* decl = new TypesetDecl(pos, ident);

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
        sp::Atom* ident;
        if (!needsymbol(&ident))
            return false;
        if (strcmp(ident->chars(), "__intrinsics__") != 0) {
            error(156);
            return false;
        }
        if (!needtoken('.'))
            return false;
        if (!needsymbol(&ident))
            return false;
        if (strcmp(ident->chars(), "Handle") != 0) {
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
Parser::parse_pragma_unused()
{
    PragmaUnusedStmt* stmt = new PragmaUnusedStmt(current_pos());

    int tok = lex_same_line();
    for (;;) {
        if (tok != tSYMBOL) {
            report(1) << get_token_string(tSYMBOL) << get_token_string(tok);
            lexclr(TRUE);
            break;
        }

        sp::Atom* name = current_token()->atom;
        stmt->names().emplace_back(name);

        tok = lex_same_line();
        if (tok == ',') {
            tok = lex_same_line();
            continue;
        }
        if (tok == tEOL)
            break;
    }
    return stmt;
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
        TypenameInfo rt;
        switch (lex()) {
            case tINT:
            case tOBJECT:
            case tCHAR: {
                auto tok = *current_token();
                parse_new_typename(&tok, &rt);
                break;
            }
            case tLABEL:
                rt = TypenameInfo{current_token()->atom};
                rt.set_is_label();
                break;
            case tSYMBOL: {
                auto tok = *current_token();
                // See if we can peek ahead another symbol.
                if (lexpeek(tSYMBOL)) {
                    // This is a new-style declaration.
                    parse_new_typename(&tok, &rt);
                } else {
                    // Otherwise, we got "const X ..." so the tag is int. Give the
                    // symbol back to the lexer so we get it as the name.
                    lexpush();
                    rt = TypenameInfo{0};
                }
                break;
            }
            default:
                error(122);
                break;
        }

        sp::Atom* name = nullptr;
        needsymbol(&name);

        needtoken('=');

        Expr* expr = hier14();
        if (!expr)
            continue;

        typeinfo_t type = {};
        type.set_type(rt);
        type.is_const = true;

        if (!name)
            continue;

        VarDecl* var = new ConstDecl(pos, name, type, vclass, expr);
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

Expr*
Parser::hier14()
{
    Expr* node = hier13();

    int tok = lex();
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
    int tok = lex();
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
            // :TODO: unify this to only care about types. This will depend on
            // removing immediate name resolution from parse_new_typename.
            sp::Atom* ident;
            if (matchsymbol(&ident)) {
                if (matchtoken('(')) {
                    Expr* target = new SymbolExpr(current_pos(), ident);
                    return parse_call(pos, tok, target);
                }
                lexpush();
            }

            TypenameInfo rt;
            if (!parse_new_typename(nullptr, &rt))
                rt = TypenameInfo{0};

            if (!needtoken('['))
                return new ErrorExpr();

            return parse_new_array(pos, rt);
        }
        case tLABEL: /* tagname override */
        {
            TypenameInfo ti(current_token()->atom, true);
            if (sc_require_newdecls) {
                // Warn: old style cast used when newdecls pragma is enabled
                report(240) << current_token()->atom;
            }
            Expr* expr = hier2();
            return new CastExpr(pos, tok, ti, expr);
        }
        case tDEFINED:
        {
            int parens = 0;
            while (matchtoken('('))
                parens++;

            sp::Atom* ident;
            if (!needsymbol(&ident))
                return new ErrorExpr();
            while (parens--)
                needtoken(')');
            return new IsDefinedExpr(pos, ident);
        }
        case tSIZEOF:
        {
            int parens = 0;
            while (matchtoken('('))
                parens++;

            sp::Atom* ident;
            if (matchtoken(tTHIS)) {
                ident = gAtoms.add("this");
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
            int token = lex();
            if (token == tDBLCOLON || token == '.') {
                if (!needsymbol(&field))
                    return new ErrorExpr();
            } else {
                lexpush();
                token = 0;
            }

            while (parens--)
                needtoken(')');

            return new SizeofExpr(pos, ident, field, token, array_levels);
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

    tok = lex();
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
        int tok = lex();
        if (tok == '.' || tok == tDBLCOLON) {
            auto pos = current_pos();
            sp::Atom* ident;
            if (!needsymbol(&ident))
                break;
            base = new FieldAccessExpr(pos, tok, base, ident);
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

    int tok = lex();
    if (tok == tTHIS)
        return new ThisExpr(current_pos());
    if (tok == tSYMBOL)
        return new SymbolExpr(current_pos(), current_token()->atom);

    lexpush();

    return constant();
}

Expr*
Parser::constant()
{
    int tok = lex();
    auto pos = current_pos();
    switch (tok) {
        case tNULL:
            return new NullExpr(pos);
        case tNUMBER:
            return new NumberExpr(pos, current_token()->value);
        case tRATIONAL:
            return new FloatExpr(pos, current_token()->value);
        case tSTRING: {
            const auto& str = current_token()->data;
            return new StringExpr(pos, str.c_str(), str.size());
        }
        case '{':
        {
            ArrayExpr* expr = new ArrayExpr(pos);
            do {
                if (matchtoken(tELLIPS)) {
                    expr->set_ellipses();
                    break;
                }
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

            if (!needsymbol(&name))
                break;
            needtoken('=');
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
    TypenameInfo ti;
    {
        if (!parse_new_typename(nullptr, &ti))
            ti = TypenameInfo{0};
    }
    needtoken('>');

    int paren = needtoken('(');

    Expr* expr = hier14();
    if (paren)
        needtoken(')');
    else
        matchtoken(')');
    return new CastExpr(pos, tVIEW_AS, ti, expr);
}

Expr*
Parser::struct_init()
{
    StructExpr* init = new StructExpr(current_pos());

    // '}' has already been lexed.
    do {
        sp::Atom* name = nullptr;
        needsymbol(&name);

        needtoken('=');

        auto pos = current_pos();

        Expr* expr = nullptr;
        switch (lex()) {
            case tSTRING: {
                const auto& str = current_token()->data;
                expr = new StringExpr(pos, str.c_str(), str.size());
                break;
            }
            case tNUMBER:
                expr = new NumberExpr(pos, current_token()->value);
                break;
            case tRATIONAL:
                expr = new FloatExpr(pos, current_token()->value);
                break;
            case tSYMBOL:
                expr = new SymbolExpr(pos, current_token()->atom);
                break;
            default:
                report(1) << "-constant-" << get_token_string(current_token()->id);
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

    Expr* expr = hier14();
    if (!expr)
        return nullptr;

    PoolString * text = nullptr;
    if (matchtoken(',') && needtoken(tSTRING)) {
        auto tok = current_token();
        text = new PoolString(tok->data.c_str(), tok->data.size());
    }

    needtoken(')');
    require_newline(TerminatorPolicy::NewlineOrSemicolon);

    return new StaticAssertStmt(pos, expr, text);
}

Expr*
Parser::var_init(int vclass)
{
    if (matchtoken('{')) {
        // Peek for " <symbol> = " to see if this is a struct initializer.
        if (matchtoken(tSYMBOL)) {
            if (matchtoken('=')) {
                lexpush();
                lexpush();
                return struct_init();
            }
            lexpush();
        }

        ArrayExpr* expr = new ArrayExpr(current_pos());
        do {
            if (lexpeek('}'))
                break;
            if (matchtoken(tELLIPS)) {
                expr->set_ellipses();
                break;
            }
            Expr* child = var_init(vclass);
            expr->exprs().emplace_back(child);
        } while (matchtoken(','));
        needtoken('}');
        return expr;
    }

    if (matchtoken(tSTRING)) {
        auto tok = current_token();
        return new StringExpr(tok->start, tok->data.c_str(), tok->data.size());
    }

    // We'll check const or symbol-ness for non-sLOCALs in the semantic pass.
    return hier14();
}

Expr*
Parser::parse_new_array(const token_pos_t& pos, const TypenameInfo& rt)
{
    auto expr = new NewArrayExpr(pos, rt);

    do {
        Expr* child = hier14();
        expr->exprs().emplace_back(child);

        needtoken(']');
    } while (matchtoken('['));
    return expr;
}

void
Parser::parse_post_dims(typeinfo_t* type)
{
    std::vector<Expr*> dim_exprs;
    bool has_dim_exprs = false;
    do {
        type->dim.emplace_back(0);

        if (matchtoken(']')) {
            dim_exprs.emplace_back(nullptr);
        } else {
            has_dim_exprs = true;
            dim_exprs.emplace_back(hier14());
            needtoken(']');
        }
    } while (matchtoken('['));

    if (has_dim_exprs)
        type->dim_exprs = PoolList<Expr*>(dim_exprs.begin(), dim_exprs.end());
}

Stmt*
Parser::parse_stmt(int* lastindent, bool allow_decl)
{
    // :TODO: remove this when compound goes private
    ke::SaveAndSet<bool> limit_errors(&sc_one_error_per_statement, true);

    if (!freading) {
        error(36); /* empty statement */
        return nullptr;
    }
    errorset(sRESET, 0);

    int tok = lex();

    /* lex() has set stmtindent */
    if (lastindent && tok != tLABEL) {
        if (*lastindent >= 0 && *lastindent != stmtindent && !indent_nowarn && sc_tabsize > 0)
            error(217); /* loose indentation */
        *lastindent = stmtindent;
        indent_nowarn = FALSE; /* if warning was blocked, re-enable it */
    }

    if (tok == tSYMBOL) {
        // We reaaaally don't have enough lookahead for this, so we cheat and try
        // to determine whether this is probably a declaration.
        int is_decl = FALSE;
        if (matchtoken('[')) {
            if (lexpeek(']'))
                is_decl = TRUE;
            lexpush();
        } else if (lexpeek(tSYMBOL)) {
            is_decl = TRUE;
        }

        if (is_decl) {
            if (!allow_decl) {
                error(3);
                return nullptr;
            }
            lexpush();
            auto stmt = parse_local_decl(tNEWDECL, true);
            needtoken(tTERM);
            return stmt;
        }
    }

    switch (tok) {
        case 0:
            /* nothing */
            return nullptr;
        case tINT:
        case tVOID:
        case tCHAR:
        case tOBJECT:
            lexpush();
            // Fall-through.
        case tDECL:
        case tSTATIC:
        case tNEW: {
            if (tok == tNEW && matchtoken(tSYMBOL)) {
                if (lexpeek('(')) {
                    lexpush();
                    break;
                }
                lexpush(); // we matchtoken'ed, give it back to lex for declloc
            }
            if (!allow_decl) {
                error(3);
                return nullptr;
            }
            auto stmt = parse_local_decl(tok, tok != tDECL);
            needtoken(tTERM);
            return stmt;
        }
        case tIF:
            return parse_if();
        case tCONST:
            return parse_const(sLOCAL);
        case tENUM:
            return parse_enum(sLOCAL);
        case tCASE:
        case tDEFAULT:
            error(14); /* not in switch */
            return nullptr;
        case '{': {
            int save = fline;
            if (matchtoken('}'))
                return new BlockStmt(current_pos());
            return parse_compound(save == fline);
        }
        case ';':
            error(36); /* empty statement */
            return nullptr;
        case tBREAK:
        case tCONTINUE: {
            auto pos = current_pos();
            needtoken(tTERM);
            if (!in_loop_) {
                error(24);
                return nullptr;
            }
            return new LoopControlStmt(pos, tok);
        }
        case tRETURN: {
            auto pos = current_pos();
            Expr* expr = nullptr;
            if (!matchtoken(tTERM)) {
                expr = parse_expr(false);
                needtoken(tTERM);
            }
            return new ReturnStmt(pos, expr);
        }
        case tASSERT: {
            auto pos = current_pos();
            Expr* expr = parse_expr(true);
            needtoken(tTERM);
            if (!expr)
                return nullptr;
            return new AssertStmt(pos, expr);
        }
        case tDELETE: {
            auto pos = current_pos();
            Expr* expr = parse_expr(false);
            needtoken(tTERM);
            if (!expr)
                return nullptr;
            return new DeleteStmt(pos, expr);
        }
        case tEXIT: {
            auto pos = current_pos();
            Expr* expr = nullptr;
            if (matchtoken(tTERM)) {
                expr = parse_expr(false);
                needtoken(tTERM);
            }
            return new ExitStmt(pos, expr);
        }
        case tDO: {
            auto pos = current_pos();
            Stmt* stmt = nullptr;
            {
                ke::SaveAndSet<bool> in_loop(&in_loop_, true);
                stmt = parse_stmt(nullptr, false);
            }
            needtoken(tWHILE);
            bool parens = matchtoken('(');
            Expr* cond = parse_expr(false);
            if (parens)
                needtoken(')');
            else
                error(243);
            needtoken(tTERM);
            if (!stmt || !cond)
                return nullptr;
            return new DoWhileStmt(pos, tok, cond, stmt);
        }
        case tWHILE: {
            auto pos = current_pos();
            Expr* cond = parse_expr(true);
            Stmt* stmt = nullptr;
            {
                ke::SaveAndSet<bool> in_loop(&in_loop_, true);
                stmt = parse_stmt(nullptr, false);
            }
            if (!stmt || !cond)
                return nullptr;
            return new DoWhileStmt(pos, tok, cond, stmt);
        }
        case tFOR:
            return parse_for();
        case tSWITCH:
            return parse_switch();
        case tSYN_PRAGMA_UNUSED:
            return parse_pragma_unused();
        default: /* non-empty expression */
            break;
    }

    lexpush(); /* analyze token later */
    Expr* expr = parse_expr(false);
    needtoken(tTERM);
    if (!expr)
        return nullptr;
    return new ExprStmt(expr->pos(), expr);
}

Stmt*
Parser::parse_compound(bool sameline)
{
    auto block_start = fline;

    BlockStmt* block = new BlockStmt(current_pos());

    /* if there is more text on this line, we should adjust the statement indent */
    if (sameline) {
        int i;
        const unsigned char* p = lptr;
        /* go back to the opening brace */
        while (*p != '{') {
            assert(p > pline);
            p--;
        }
        assert(*p == '{'); /* it should be found */
        /* go forward, skipping white-space */
        p++;
        while (*p <= ' ' && *p != '\0')
            p++;
        assert(*p != '\0'); /* a token should be found */
        stmtindent = 0;
        for (i = 0; i < (int)(p - pline); i++)
            if (pline[i] == '\t' && sc_tabsize > 0)
                stmtindent += (int)(sc_tabsize - (stmtindent + sc_tabsize) % sc_tabsize);
            else
                stmtindent++;
    }

    int indent = -1;
    while (matchtoken('}') == 0) { /* repeat until compound statement is closed */
        if (!freading) {
            error(30, block_start); /* compound block not closed at end of file */
            break;
        }
        if (Stmt* stmt = parse_stmt(&indent, true))
            block->stmts().push_back(stmt);
    }

    return block;
}

Stmt*
Parser::parse_local_decl(int tokid, bool autozero)
{
    declinfo_t decl = {};

    int declflags = DECLFLAG_VARIABLE | DECLFLAG_ENUMROOT;
    if (tokid == tNEW || tokid == tDECL)
        declflags |= DECLFLAG_OLD;
    else if (tokid == tNEWDECL)
        declflags |= DECLFLAG_NEW;

    parse_decl(&decl, declflags);

    Parser::VarParams params;
    params.vclass = (tokid == tSTATIC) ? sSTATIC : sLOCAL;
    params.autozero = autozero;
    return parse_var(&decl, params);
}

Stmt*
Parser::parse_if()
{
    auto ifindent = stmtindent;
    auto pos = current_pos();
    auto expr = parse_expr(true);
    if (!expr)
        return nullptr;
    auto stmt = parse_stmt(nullptr, false);
    Stmt* else_stmt = nullptr;
    if (matchtoken(tELSE)) {
        /* to avoid the "dangling else" error, we want a warning if the "else"
         * has a lower indent than the matching "if" */
        if (stmtindent < ifindent && sc_tabsize > 0)
            error(217); /* loose indentation */
        else_stmt = parse_stmt(nullptr, false);
        if (!else_stmt)
            return nullptr;
    }
    if (!stmt)
        return nullptr;
    return new IfStmt(pos, expr, stmt, else_stmt);
}

Expr*
Parser::parse_expr(bool parens)
{
    ke::SaveAndSet<bool> in_test(&sc_intest, parens);

    if (parens)
        needtoken('(');

    Expr* expr = nullptr;
    CommaExpr* comma = nullptr;
    while (true) {
        expr = hier14();
        if (!expr)
            break;

        if (comma)
            comma->exprs().push_back(expr);

        if (!matchtoken(','))
            break;

        if (!comma) {
            comma = new CommaExpr(expr->pos());
            comma->exprs().push_back(expr);
        }
    }
    if (parens)
        needtoken(')');

    return comma ? comma : expr;
}

Stmt*
Parser::parse_for()
{
    auto pos = current_pos();

    int endtok = matchtoken('(') ? ')' : tDO;
    if (endtok != ')')
        error(243);

    Stmt* init = nullptr;
    if (!matchtoken(';')) {
        /* new variable declarations are allowed here */
        int tok_id = lex();
        switch (tok_id) {
            case tINT:
            case tCHAR:
            case tOBJECT:
            case tVOID:
                lexpush();
                // Fallthrough.
            case tNEW:
                /* The variable in expr1 of the for loop is at a
                 * 'compound statement' level of it own.
                 */
                // :TODO: test needtoken(tTERM) accepting newlines here
                init = parse_local_decl(tok_id, true);
                needtoken(';');
                break;
            case tSYMBOL: {
                // See comment in statement() near tSYMBOL.
                bool is_decl = false;
                if (matchtoken('[')) {
                    if (lexpeek(']'))
                        is_decl = true;
                    lexpush();
                } else if (lexpeek(tSYMBOL)) {
                    is_decl = true;
                }

                if (is_decl) {
                    lexpush();
                    init = parse_local_decl(tSYMBOL, true);
                    needtoken(';');
                    break;
                }
                // Fall-through to default!
            }
            default:
                lexpush();
                if (Expr* expr = parse_expr(false))
                    init = new ExprStmt(expr->pos(), expr);
                needtoken(';');
                break;
        }
    }

    Expr* cond = nullptr;
    if (!matchtoken(';')) {
        cond = parse_expr(false);
        needtoken(';');
    }

    Expr* advance = nullptr;
    if (!matchtoken(endtok)) {
        advance = parse_expr(false);
        needtoken(endtok);
    }

    Stmt* body = nullptr;
    {
        ke::SaveAndSet<bool> in_loop(&in_loop_, true);
        body = parse_stmt(nullptr, false);
    }
    if (!body)
        return nullptr;
    return new ForStmt(pos, init, cond, advance, body);
}

Stmt*
Parser::parse_switch()
{
    auto pos = current_pos();

    int endtok = matchtoken('(') ? ')' : tDO;
    if (endtok != ')')
        error(243);

    Expr* cond = parse_expr(false);
    needtoken(endtok);

    SwitchStmt* sw = new SwitchStmt(pos, cond);

    endtok = '}';
    needtoken('{');
    while (true) {
        int tok = lex();

        switch (tok) {
            case tCASE:
                if (sw->default_case())
                    error(15); /* "default" case must be last in switch statement */
                parse_case(sw);
                break;
            case tDEFAULT:
                needtoken(':');
                if (Stmt* stmt = parse_stmt(nullptr, false)) {
                    if (!sw->default_case())
                        sw->set_default_case(stmt);
                    else
                        error(16);
                }
                break;
            default:
                if (tok != '}') {
                    error(2);
                    indent_nowarn = TRUE;
                    tok = endtok;
                }
                break;
        }
        if (tok == endtok)
            break;
    }

    if (!cond)
        return nullptr;

    return sw;
}

void
Parser::parse_case(SwitchStmt* sw)
{
    PoolList<Expr*> exprs;
    do {
        /* do not allow tagnames here */
        ke::SaveAndSet<bool> allowtags(&sc_allowtags, false);

        // hier14 because parse_expr() allows comma exprs
        if (Expr* expr = hier14())
            exprs.push_back(expr);
        if (matchtoken(tDBLDOT))
            error(1, ":", "..");
    } while (matchtoken(','));

    needtoken(':');

    Stmt* stmt = parse_stmt(nullptr, false);
    if (!stmt || exprs.empty())
        return;

    sw->AddCase(std::move(exprs), stmt);
}

Decl*
Parser::parse_inline_function(int tokid, const declinfo_t& decl, const int* this_tag)
{
    auto pos = current_pos();
    auto info = new FunctionInfo(pos, decl);
    info->set_name(decl.name);
    if (this_tag)
        info->set_this_tag(*this_tag);

    if (tokid == tNATIVE || tokid == tMETHODMAP)
        info->set_is_native();
    else if (tokid == tPUBLIC)
        info->set_is_public();
    else if (tokid == tFORWARD)
        info->set_is_forward();
    else
        info->set_is_stock();

    if (!parse_function(info, tokid))
        return nullptr;

    auto stmt = new FunctionDecl(pos, info);
    if (!pc_deprecate.empty()) {
        stmt->set_deprecate(pc_deprecate);
        pc_deprecate = {};
    }
    return stmt;
}

bool
Parser::parse_function(FunctionInfo* info, int tokid)
{
    if (!matchtoken('(')) {
        error(10);
        lexclr(TRUE);
        return false;
    }
    parse_args(info); // eats the close paren

    if (info->is_native()) {
        if (info->decl().opertok != 0) {
            needtoken('=');
            lexpush();
        }
        if (matchtoken('=')) {
            sp::Atom* ident;
            if (needsymbol(&ident))
                info->set_alias(ident);
        }
    }

    switch (tokid) {
        case tNATIVE:
        case tFORWARD:
            needtoken(tTERM);
            return true;
        case tMETHODMAP:
            // Don't look for line endings if we're inline.
            return true;
        default:
            if (matchtoken(';')) {
                if (!NeedSemicolon())
                    error(10); /* old style prototypes used with optional semicolumns */
                info->set_is_forward();
                return true;
            }
            break;
    }

    if (matchtoken('{'))
        lexpush();
    else if (info->decl().type.is_new)
        needtoken('{');

    Stmt* body = parse_stmt(nullptr, false);
    if (!body)
        return false;

    info->set_body(BlockStmt::WrapStmt(body));
    info->set_end_pos(current_pos());
    return true;
}

void
Parser::parse_args(FunctionInfo* info)
{
    if (matchtoken(')'))
        return;

    do {
        auto pos = current_pos();

        declinfo_t decl = {};
        parse_decl(&decl, DECLFLAG_ARGUMENT | DECLFLAG_ENUMROOT);

        if (decl.type.ident == iVARARGS) {
            if (info->IsVariadic())
                error(401);

            auto p = new VarDecl(pos, gAtoms.add("..."), decl.type, sARGUMENT, false, false,
                                 false, nullptr);
            info->AddArg(p);
            continue;
        }

        if (info->IsVariadic())
            error(402);

        Expr* init = nullptr;
        if (matchtoken('='))
            init = var_init(sARGUMENT);

        if (info->args().size() >= SP_MAX_CALL_ARGUMENTS)
            error(45);
        if (decl.name->chars()[0] == PUBLIC_CHAR)
            report(56) << decl.name; // function arguments cannot be public

        auto p = new VarDecl(pos, decl.name, decl.type, sARGUMENT, false, false,
                             false, init);
        info->AddArg(p);
    } while (matchtoken(','));

    needtoken(')');
    errorset(sRESET, 0);
}

Decl*
Parser::parse_methodmap()
{
    auto pos = current_pos();

    sp::Atom* ident;
    needsymbol(&ident);

    auto name_atom = ident;
    if (!isupper(name_atom->chars()[0]))
        report(109) << "methodmap";

    auto old_spec = deduce_layout_spec_by_name(name_atom);
    if (!can_redef_layout_spec(Layout_MethodMap, old_spec))
        report(110) << name_atom << layout_spec_name(old_spec);

    bool nullable = matchtoken(tNULLABLE);

    sp::Atom* extends = nullptr;
    if (matchtoken('<') && needsymbol(&ident))
        extends = ident;

    auto decl = new MethodmapDecl(pos, name_atom, nullable, extends);

    needtoken('{');
    while (!matchtoken('}')) {
        bool ok = false;
        int tok_id = lex();
        if (tok_id == tPUBLIC) {
            ok = parse_methodmap_method(decl);
        } else if (tok_id == tSYMBOL && current_token()->atom->str() == "property") {
            ok = parse_methodmap_property(decl);
        } else {
            error(124);
        }
        if (!ok) {
            if (!consume_line())
                return decl;
            continue;
        }
    }

    require_newline(TerminatorPolicy::NewlineOrSemicolon);
    return decl;
}

bool
Parser::parse_methodmap_method(MethodmapDecl* map)
{
    auto pos = current_pos();

    bool is_static = matchtoken(tSTATIC);
    bool is_native = matchtoken(tNATIVE);

    sp::Atom* symbol = nullptr;
    full_token_t symbol_tok;
    if (matchsymbol(&symbol))
        symbol_tok = *current_token();

    if (matchtoken('~'))
        error(118);

    declinfo_t ret_type = {};

    bool is_ctor = false;
    if (symbol && matchtoken('(')) {
        // ::= ident '('

        // Push the '(' token back for parse_args().
        is_ctor = true;
        lexpush();

        // Force parser to require { for the method body.
        ret_type.type.is_new = true;
    } else {
        // The first token of the type expression is either the symbol we
        // predictively parsed earlier, or it's been pushed back into the
        // lex buffer.
        const full_token_t* first = symbol ? &symbol_tok : nullptr;

        // Parse for type expression, priming it with the token we predicted
        // would be an identifier.
        if (!parse_new_typeexpr(&ret_type.type, first, 0))
            return false;

        // Now, we should get an identifier.
        if (!needsymbol(&symbol))
            return false;

        ret_type.type.ident = iVARIABLE;
    }

    // Build a new symbol. Construct a temporary name including the class.
    auto fullname = ke::StringPrintf("%s.%s", map->name()->chars(), symbol->chars());
    auto fqn = gAtoms.add(fullname);

    auto fun = new FunctionInfo(pos, ret_type);
    fun->set_name(fqn);

    if (is_native)
        fun->set_is_native();
    else
        fun->set_is_stock();

    ke::SaveAndSet<int> require_newdecls(&sc_require_newdecls, TRUE);
    if (!parse_function(fun, is_native ? tMETHODMAP : 0))
        return false;

    // Use the short name for the function decl
    auto method = new MethodmapMethod;
    method->is_static = is_static;
    method->decl = new FunctionDecl(pos, symbol, fun);
    map->methods().emplace_back(method);

    if (is_native)
        require_newline(TerminatorPolicy::Semicolon);
    else
        require_newline(TerminatorPolicy::Newline);
    return true;
}

bool
Parser::parse_methodmap_property(MethodmapDecl* map)
{
    auto prop = new MethodmapProperty;
    prop->pos = current_pos();

    if (!parse_new_typeexpr(&prop->type, nullptr, 0))
        return false;

    sp::Atom* ident;
    if (!needsymbol(&ident))
        return false;
    if (!needtoken('{'))
        return false;

    prop->name = ident;

    while (!matchtoken('}')) {
        if (!parse_methodmap_property_accessor(map, prop))
            lexclr(TRUE);
    }

    map->properties().emplace_back(prop);

    require_newline(TerminatorPolicy::Newline);
    return true;
}

bool
Parser::parse_methodmap_property_accessor(MethodmapDecl* map, MethodmapProperty* prop)
{
    bool is_native = false;
    auto pos = current_pos();

    needtoken(tPUBLIC);

    sp::Atom* ident;
    if (!matchsymbol(&ident)) {
        if (!matchtoken(tNATIVE)) {
            report(125);
            return false;
        }
        is_native = true;
        if (!needsymbol(&ident))
            return false;
    }

    bool getter = (ident->str() == "get");
    bool setter = (ident->str() == "set");

    if (!getter && !setter) {
        report(125);
        return false;
    }

    declinfo_t ret_type = {};
    if (getter) {
        ret_type.type = prop->type;
    } else {
        ret_type.type.set_tag(pc_tag_void);
        ret_type.type.ident = iVARIABLE;
    }

    auto fun = new FunctionInfo(pos, ret_type);
    std::string tmpname = map->name()->str() + "." + prop->name->str();
    if (getter)
        tmpname += ".get";
    else
        tmpname += ".set";
    fun->set_name(gAtoms.add(tmpname));

    if (is_native)
        fun->set_is_native();
    else
        fun->set_is_stock();

    if (!parse_function(fun, is_native ? tMETHODMAP : 0))
        return false;

    if (getter && prop->getter) {
        report(126) << "getter" << prop->name;
        return false;
    }
    if (setter && prop->setter) {
        report(126) << "setter" << prop->name;
        return false;
    }

    if (getter)
        prop->getter = fun;
    else
        prop->setter = fun;

    if (is_native)
        require_newline(TerminatorPolicy::Semicolon);
    else
        require_newline(TerminatorPolicy::Newline);
    return true;
}

// Consumes a line, returns FALSE if EOF hit.
bool
Parser::consume_line()
{
    // First check for EOF.
    if (lex() == 0)
        return false;
    lexpush();

    while (!matchtoken(tTERM)) {
        // Check for EOF.
        if (lex() == 0)
            return false;
    }
    return true;
}

/**
 * function-type ::= "(" function-type-inner ")"
 *                 | function-type-inner
 * function-type-inner ::= "function" type-expr "(" new-style-args ")"
 */
TypedefInfo*
Parser::parse_function_type()
{
    int lparen = matchtoken('(');
    if (!needtoken(tFUNCTION))
        return nullptr;

    auto info = new TypedefInfo;
    info->pos = current_pos();

    parse_new_typename(nullptr, &info->ret_type);

    needtoken('(');

    while (!matchtoken(')')) {
        auto decl = gPoolAllocator.alloc<declinfo_t>();
        decl->type.ident = iVARIABLE;

        parse_new_decl(decl, nullptr, DECLFLAG_ARGUMENT);

        // Eat optional symbol name.
        matchtoken(tSYMBOL);

        info->args.emplace_back(decl);

        if (!matchtoken(',')) {
            needtoken(')');
            break;
        }
    }

    // Error once when we're past max args.
    if (info->args.size() >= SP_MAX_EXEC_PARAMS)
        report(45);

    if (lparen)
        needtoken(')');

    require_newline(TerminatorPolicy::Semicolon);
    errorset(sRESET, 0);
    return info;
}

// Parse a declaration.
//
// Grammar for named declarations is:
//    "const"? symbol ('[' ']')* '&'? symbol
//  | "const"? label? '&'? symbol '[' ']'
//
bool
Parser::parse_decl(declinfo_t* decl, int flags)
{
    sp::Atom* ident = nullptr;

    decl->type.ident = iVARIABLE;

    // Match early varargs as old decl.
    if (lexpeek(tELLIPS))
        return parse_old_decl(decl, flags);

    // Must attempt to match const first, since it's a common prefix.
    if (matchtoken(tCONST))
        decl->type.is_const = true;

    // Sometimes we know ahead of time whether the declaration will be old, for
    // example, if preceded by tNEW or tDECL.
    if (flags & DECLFLAG_OLD)
        return parse_old_decl(decl, flags);
    if (flags & DECLFLAG_NEW)
        return parse_new_decl(decl, NULL, flags);

    // If parsing an argument, there are two simple checks for whether this is a
    // new or old-style declaration.
    if ((flags & DECLFLAG_ARGUMENT) && (lexpeek('&') || lexpeek('{')))
        return parse_old_decl(decl, flags);

    // Another dead giveaway is there being a label or typeless operator.
    if (lexpeek(tLABEL) || lexpeek(tOPERATOR))
        return parse_old_decl(decl, flags);

    // Otherwise, we have to eat a symbol to tell.
    if (matchsymbol(&ident)) {
        auto ident_tok = *current_token();

        if (lexpeek(tSYMBOL) || lexpeek(tOPERATOR) || lexpeek('&') || lexpeek(tELLIPS)) {
            // A new-style declaration only allows array dims or a symbol name, so
            // this is a new-style declaration.
            return parse_new_decl(decl, &ident_tok, flags);
        }

        if ((flags & DECLMASK_NAMED_DECL) && matchtoken('[')) {
            // Oh no - we have to parse array dims before we can tell what kind of
            // declarator this is. It could be either:
            //    "x[] y" (new-style), or
            //    "y[],"  (old-style)
            parse_post_array_dims(decl, flags);

            if (matchtoken(tSYMBOL) || matchtoken('&')) {
                // This must be a newdecl, "x[] y" or "x[] &y", the latter of which
                // is illegal, but we flow it through the right path anyway.
                lexpush();
                fix_mispredicted_postdims(decl);
                return parse_new_decl(decl, &ident_tok, flags);
            }

            if (sc_require_newdecls)
                error(147);

            // The most basic - "x[]" and that's it. Well, we know it has no tag and
            // we know its name. We might as well just complete the entire decl.
            decl->name = ident;
            decl->type.set_tag(0);
            return true;
        }

        // Give the symbol back to the lexer. This is an old decl.
        lexpush();
        return parse_old_decl(decl, flags);
    }

    // All else has failed. Probably got a type keyword. New-style.
    return parse_new_decl(decl, NULL, flags);
}

void
Parser::fix_mispredicted_postdims(declinfo_t* decl)
{
    assert(decl->type.has_postdims);
    assert(decl->type.ident == iARRAY);

    decl->type.has_postdims = false;

    // We got a declaration like:
    //      int[3] x;
    //
    // This is illegal, so report it now, and strip dim_exprs.
    if (!decl->type.dim_exprs.empty()) {
        for (int i = 0; i < decl->type.numdim(); i++) {
            if (decl->type.dim_exprs[i]) {
                report(decl->type.dim_exprs[i]->pos(), 101);
                break;
            }
        }
        decl->type.dim_exprs.clear();
    }

    // If has_postdims is false, we never want to report an iARRAY.
    decl->type.ident = iREFARRAY;
}

bool
Parser::parse_old_decl(declinfo_t* decl, int flags)
{
    typeinfo_t* type = &decl->type;

    if (matchtoken(tCONST)) {
        if (type->is_const)
            error(138);
        type->is_const = true;
    }

    TypenameInfo ti = TypenameInfo(0);

    int numtags = 0;
    if (flags & DECLFLAG_ARGUMENT) {
        if (matchtoken('&'))
            type->ident = iREFERENCE;

        // grammar for multitags is:
        //   multi-tag ::= '{' (symbol (',' symbol)*)? '}' ':'
        if (matchtoken('{')) {
            while (true) {
                if (!matchtoken('_')) {
                    // If we don't get the magic tag '_', then we should have a symbol.
                    sp::Atom* name;
                    if (needsymbol(&name))
                        ti = TypenameInfo(name, true);
                }
                numtags++;

                if (matchtoken('}'))
                    break;
                needtoken(',');
            }
            needtoken(':');
        }
        if (numtags > 1)
            error(158);
    }

    if (numtags == 0) {
        if (matchtoken(tLABEL))
            ti = TypenameInfo(current_token()->atom, true);
    }

    // All finished with tag stuff.
    type->set_type(ti);

    if (sc_require_newdecls)
        error(147);

    // Look for varargs and end early.
    if (matchtoken(tELLIPS)) {
        type->ident = iVARARGS;
        return TRUE;
    }

    if (flags & DECLMASK_NAMED_DECL) {
        if ((flags & DECLFLAG_MAYBE_FUNCTION) && matchtoken(tOPERATOR)) {
            decl->opertok = operatorname(&decl->name);
            if (decl->opertok == 0)
                decl->name = gAtoms.add("__unknown__");
        } else {
            if (!lexpeek(tSYMBOL)) {
                extern const char* sc_tokens[];
                int tok_id = lex();
                switch (tok_id) {
                    case tOBJECT:
                    case tCHAR:
                    case tVOID:
                    case tINT:
                        if (lexpeek(tSYMBOL)) {
                            error(143);
                        } else {
                            error(157, sc_tokens[tok_id - tFIRST]);
                            decl->name = gAtoms.add(sc_tokens[tok_id - tFIRST]);
                        }
                        break;
                    default:
                        lexpush();
                        break;
                }
            }
            needsymbol(&decl->name);
        }
    }

    if ((flags & DECLMASK_NAMED_DECL) && !decl->opertok) {
        if (matchtoken('['))
            parse_post_array_dims(decl, flags);
    }

    return true;
}

bool
Parser::parse_new_decl(declinfo_t* decl, const full_token_t* first, int flags)
{
    if (!parse_new_typeexpr(&decl->type, first, flags))
        return false;

    decl->type.is_new = TRUE;

    if (flags & DECLMASK_NAMED_DECL) {
        if ((flags & DECLFLAG_ARGUMENT) && matchtoken(tELLIPS)) {
            decl->type.ident = iVARARGS;
            return true;
        }

        if ((flags & DECLFLAG_MAYBE_FUNCTION) && matchtoken(tOPERATOR)) {
            decl->opertok = operatorname(&decl->name);
            if (decl->opertok == 0)
                decl->name = gAtoms.add("__unknown__");
        } else {
            needsymbol(&decl->name);
        }
    }

    if (flags & DECLMASK_NAMED_DECL) {
        if (matchtoken('[')) {
            if (decl->type.numdim() == 0)
                parse_post_array_dims(decl, flags);
            else
                error(121);
        }
    }

    rewrite_type_for_enum_struct(&decl->type);
    return true;
}

int
Parser::operatorname(sp::Atom** name)
{
    /* check the operator */
    int opertok = lex();
    switch (opertok) {
        case '+':
        case '-':
        case '*':
        case '/':
        case '%':
        case '>':
        case '<':
        case '!':
        case '~':
        case '=':
        {
            char str[] = {(char)opertok, '\0'};
            *name = gAtoms.add(str);
            break;
        }
        case tINC:
            *name = gAtoms.add("++");
            break;
        case tDEC:
            *name = gAtoms.add("--");
            break;
        case tlEQ:
            *name = gAtoms.add("==");
            break;
        case tlNE:
            *name = gAtoms.add("!=");
            break;
        case tlLE:
            *name = gAtoms.add("<=");
            break;
        case tlGE:
            *name = gAtoms.add(">=");
            break;
        default:
            *name = gAtoms.add("");
            error(7); /* operator cannot be redefined (or bad operator name) */
            return 0;
    }

    return opertok;
}

void
Parser::rewrite_type_for_enum_struct(typeinfo_t* info)
{
    Type* type = gTypes.find(info->declared_tag);
    symbol* enum_type = type->asEnumStruct();
    if (!enum_type)
        return;

    // Note that the size here is incorrect. It's fixed up in initials() by
    // parse_var_decl. Unfortunately type->size is difficult to remove because
    // it can't be recomputed from array sizes (yet), in the case of
    // initializers with inconsistent final arrays. We could set it to
    // anything here, but we follow what parse_post_array_dims() does.
    info->set_tag(0);
    info->dim.emplace_back(enum_type->addr());
    if (!info->dim_exprs.empty())
        info->dim_exprs.emplace_back(nullptr);
    assert(info->declared_tag = enum_type->tag);

    if (info->ident != iARRAY && info->ident != iREFARRAY) {
        info->ident = iARRAY;
        info->has_postdims = true;
    }
}

bool
Parser::reparse_new_decl(declinfo_t* decl, int flags)
{
    if (matchtoken(tSYMBOL))
        decl->name = current_token()->atom;

    if (decl->type.declared_tag && !decl->type.tag()) {
        assert(decl->type.numdim() > 0);
        decl->type.dim.pop_back();
    }

    decl->type.dim_exprs.clear();

    if (decl->type.has_postdims) {
        // We have something like:
        //    int x[], y...
        //
        // Reset the fact that we saw an array.
        decl->type.dim.clear();
        decl->type.ident = iVARIABLE;
        decl->type.has_postdims = false;
        if (matchtoken('[')) {
            // int x[], y[]
            //           ^-- parse this
            parse_post_array_dims(decl, flags);
        }
    } else {
        if (matchtoken('[')) {
            if (decl->type.numdim() > 0) {
                // int[] x, y[]
                //           ^-- not allowed
                error(121);
            }

            // int x, y[]
            //         ^-- parse this
            parse_post_array_dims(decl, flags);
        } else if (decl->type.numdim()) {
            // int[] x, y
            //          ^-- still an array, because the type is int[]
            //
            // Dim count should be 0 but we zap it anyway.
            for (auto& dim : decl->type.dim)
                dim = 0;
        }
    }

    rewrite_type_for_enum_struct(&decl->type);
    return true;
}

bool
Parser::reparse_old_decl(declinfo_t* decl, int flags)
{
    bool is_const = decl->type.is_const;

    *decl = {};
    decl->type.ident = iVARIABLE;
    decl->type.is_const = is_const;

    return parse_old_decl(decl, flags);
}

void
Parser::parse_post_array_dims(declinfo_t* decl, int flags)
{
    typeinfo_t* type = &decl->type;

    // Illegal declaration (we'll have a name since ref requires decl).
    if (type->ident == iREFERENCE)
        report(67) << decl->name;

    parse_post_dims(type);

    // We can't deduce iARRAY vs iREFARRAY until the analysis phase. Start with
    // iARRAY for now.
    decl->type.ident = iARRAY;
    decl->type.has_postdims = TRUE;
}

bool
Parser::parse_new_typeexpr(typeinfo_t* type, const full_token_t* first, int flags)
{
    full_token_t tok;

    if (first)
        tok = *first;
    else
        tok = lex_tok();

    if (tok.id == tCONST) {
        if (type->is_const)
            error(138);
        type->is_const = true;
        tok = lex_tok();
    }

    TypenameInfo ti;
    if (!parse_new_typename(&tok, &ti))
        return false;
    type->set_type(ti);

    // Note: we could have already filled in the prefix array bits, so we check
    // that ident != iARRAY before looking for an open bracket.
    if (type->ident != iARRAY && matchtoken('[')) {
        do {
            type->dim.emplace_back(0);
            if (!matchtoken(']')) {
                error(101);

                // Try to eat a close bracket anyway.
                hier14();
                matchtoken(']');
            }
        } while (matchtoken('['));
        type->ident = iREFARRAY;
    }

    if (flags & DECLFLAG_ARGUMENT) {
        if (matchtoken('&')) {
            if (type->ident == iARRAY || type->ident == iREFARRAY)
                error(137);
            else
                type->ident = iREFERENCE;
        }
    }

    // We're not getting another chance to do enum struct desugaring, since our
    // caller is not looking for a declaration. Do it now.
    if (!flags)
        rewrite_type_for_enum_struct(type);

    return true;
}

bool
Parser::parse_new_typename(const full_token_t* tok, TypenameInfo* out)
{
    full_token_t tmp;

    if (!tok) {
        tmp = lex_tok();
        tok = &tmp;
    }

    switch (tok->id) {
        case tINT:
            *out = TypenameInfo{0};
            return true;
        case tCHAR:
            *out = TypenameInfo{pc_tag_string};
            return true;
        case tVOID:
            *out = TypenameInfo{pc_tag_void};
            return true;
        case tOBJECT:
            *out = TypenameInfo{pc_tag_object};
            return true;
        case tLABEL:
        case tSYMBOL:
            if (tok->id == tLABEL)
                error(120);
            if (tok->atom->str() == "float") {
                *out = TypenameInfo{sc_rationaltag};
                return true;
            }
            if (tok->atom->str() == "bool") {
                *out = TypenameInfo{pc_tag_bool};
                return true;
            }
            if (tok->atom->str() == "Float") {
                error(98, "Float", "float");
                *out = TypenameInfo{sc_rationaltag};
                return true;
            }
            if (tok->atom->str() == "String") {
                error(98, "String", "char");
                *out = TypenameInfo{pc_tag_string};
                return true;
            }
            if (tok->atom->str() == "_") {
                error(98, "_", "int");
                *out = TypenameInfo{0};
                return true;
            }
            if (tok->atom->str() == "any") {
                *out = TypenameInfo(pc_anytag);
                return true;
            }
            *out = TypenameInfo(tok->atom, tok->id == tLABEL);
            return true;
    }

    error(122);
    return false;
}
