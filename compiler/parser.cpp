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
#include "compile-options.h"
#include "emitter.h"
#include "errors.h"
#include "lexer.h"
#include "lexer-inl.h"
#include "parser.h"
#include "parse-node.h"
#include "sc.h"
#include "sctracker.h"
#include "scvars.h"
#include "semantics.h"
#include "types.h"

using namespace sp;

bool Parser::sInPreprocessor = false;
bool Parser::sDetectedIllegalPreprocessorSymbols = false;

Parser::Parser(CompileContext& cc)
  : cc_(cc),
    lexer_(cc.lexer())
{
}

Parser::~Parser()
{
}

ParseTree*
Parser::Parse()
{
    cc_.set_one_error_per_stmt(true);
    auto restore_errors = ke::MakeScopeGuard([this]() -> void {
        cc_.set_one_error_per_stmt(false);
    });

    std::deque<Stmt*> add_to_end;

    auto list = new ParseTree(token_pos_t{});

    // Create a static scope for the main file.
    assert(fcurrent == 0);
    static_scopes_.emplace_back(new SymbolScope(cc_.globals(), sFILE_STATIC, fcurrent));
    list->stmts().emplace_back(new ChangeScopeNode({}, static_scopes_.back(),
                                                   cc_.input_files().at(fcurrent)));

    if (!cc_.default_include().empty()) {
        const char* incfname = cc_.default_include().c_str();
        if (lexer_->PlungeFile(incfname, FALSE, TRUE)) {
            static_scopes_.emplace_back(new SymbolScope(cc_.globals(), sFILE_STATIC, fcurrent));
            list->stmts().emplace_back(new ChangeScopeNode({}, static_scopes_.back(),
                                                           cc_.input_files().at(fcurrent)));
        }
    }

    // Prime the lexer.
    lexer_->Start();

    while (freading) {
        Stmt* decl = nullptr;

        int tok = lexer_->lex();

        // We don't have end-of-file tokens (yet), so we pop static scopes
        // before every declaration. This should be after lexer_->lex() so we've
        // processed any end-of-file events. 
        bool changed = false;
        while (!static_scopes_.empty() && static_scopes_.back()->fnumber() != fcurrent) {
            changed = true;
            static_scopes_.pop_back();
        }
        assert(!static_scopes_.empty());

        if (changed)
            list->stmts().emplace_back(new ChangeScopeNode(lexer_->pos(), static_scopes_.back(),
                                                           cc_.input_files().at(fcurrent)));

        switch (tok) {
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
                lexer_->lexpush();
                // Fallthrough.
            case tNEW:
            case tSTATIC:
            case tPUBLIC:
            case tSTOCK:
            case tOPERATOR:
            case tNATIVE:
            case tFORWARD: {
                auto tok = *lexer_->current_token();
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
                if (lexer_->match(tSTRUCT))
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
            case tINCLUDE:
            case tpTRYINCLUDE: {
                if (!lexer_->need(tSYN_INCLUDE_PATH))
                    break;
                auto name = lexer_->current_token()->data;
                auto result = lexer_->PlungeFile(name.c_str() + 1, (name[0] != '<'), TRUE);
                if (!result && tok != tpTRYINCLUDE)
                    report(FATAL_ERROR_READ) << name.substr(1);

                static_scopes_.emplace_back(new SymbolScope(cc_.globals(), sFILE_STATIC, fcurrent));
                decl = new ChangeScopeNode(lexer_->pos(), static_scopes_.back(), name.substr(1));
                break;
            }
            case '}':
                error(54); /* unmatched closing brace */
                break;
            case '{':
                error(55); /* start of function body without function header */
                break;
            default:
                if (freading) {
                    error(10);    /* illegal function or declaration */
                    lexer_->lexclr(TRUE); /* drop the rest of the line */
                }
        }

        // Until we can eliminate the two-pass parser, top-level decls must be
        // resolved immediately.
        if (decl) {
            cc_.reports()->ResetErrorFlag();

            list->stmts().emplace_back(decl);
        }

        lexer_->deprecate() = {};
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
            if (lexer_->match(tSTATIC))
                fstatic = TRUE;
            break;
        case tSTATIC:
            fstatic = TRUE;

            // For compatibility, we must include this case. Though "stock" should
            // come first.
            if (lexer_->match(tSTOCK))
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
    bool probablyVariable = tok->id == tNEW || decl.type.has_postdims || !lexer_->peek('(') ||
                            decl.type.is_const;

    if (!decl.opertok && probablyVariable) {
        if (tok->id == tNEW && decl.type.is_new)
            error(143);

        VarParams params;
        params.vclass = fstatic ? sSTATIC : sGLOBAL;
        params.is_public = !!fpublic;
        params.is_static = !!fstatic;
        params.is_stock = !!fstock;

        auto stmt = parse_var(&decl, params);

        // The old parser had a different line ending policy for struct
        // initializers, so we approximate that here.
        if (params.struct_init)
            lexer_->match(';');
        else
            lexer_->need(tTERM);
        return stmt;
    } else {
        auto pos = lexer_->pos();
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
        if (!lexer_->deprecate().empty()) {
            stmt->set_deprecate(lexer_->deprecate());
            lexer_->deprecate() = {};
        }
        return stmt;
    }
    return nullptr;
}

bool
Parser::PreprocExpr(cell* val, int* tag)
{
    Parser parser(CompileContext::get());
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
        auto pos = lexer_->pos();

        Expr* init = nullptr;
        if (lexer_->match('='))
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

        if (!lexer_->match(','))
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
    auto pos = lexer_->pos();

    Atom* label = nullptr;
    if (lexer_->lex() == tLABEL)
        label = lexer_->current_token()->atom;
    else
        lexer_->lexpush();

    Atom* name = nullptr;
    if (lexer_->lex() == tSYMBOL)
        name = lexer_->current_token()->atom;
    else
        lexer_->lexpush();

    cell increment = 1;
    cell multiplier = 1;
    if (lexer_->match('(')) {
        error(228);
        if (lexer_->match(taADD)) {
            if (lexer_->need(tNUMBER)) {
                if (lexer_->current_token()->value != 1)
                    report(404);
            }
        } else if (lexer_->match(taMULT)) {
            if (lexer_->need(tNUMBER))
                report(404);
        } else if (lexer_->match(taSHL)) {
            if (lexer_->need(tNUMBER)) {
                if (lexer_->current_token()->value != 1)
                    report(404);
                multiplier = 2;
            }
        }
        lexer_->need(')');
    }

    EnumDecl* decl = new EnumDecl(pos, vclass, label, name, increment, multiplier);

    lexer_->need('{');

    do {
        if (lexer_->match('}')) {
            lexer_->lexpush();
            break;
        }
        if (lexer_->match(tLABEL))
            error(153);

        sp::Atom* field_name = nullptr;
        if (lexer_->need(tSYMBOL))
            field_name = lexer_->current_token()->atom;

        auto pos = lexer_->pos();

        if (lexer_->match('[')) {
            error(153);
            if (!lexer_->match(']')) {
                hier14();
                lexer_->need(']');
            }
        }

        Expr* value = nullptr;
        if (lexer_->match('='))
            value = hier14();

        if (field_name)
            decl->fields().push_back(EnumField(pos, field_name, value));
    } while (lexer_->match(','));

    lexer_->need('}');
    lexer_->match(';');
    return decl;
}

Decl*
Parser::parse_enumstruct()
{
    auto pos = lexer_->pos();

    sp::Atom* struct_name;
    if (!lexer_->needsymbol(&struct_name))
        return nullptr;

    if (!lexer_->match('{')) {
        lexer_->need('{');
        return nullptr;
    }

    auto stmt = new EnumStructDecl(pos, struct_name);

    int opening_line = fline;
    while (!lexer_->match('}')) {
        if (!freading) {
            error(151, opening_line);
            break;
        }

        declinfo_t decl = {};
        if (!parse_new_decl(&decl, nullptr, DECLFLAG_FIELD))
            continue;

        auto decl_pos = lexer_->pos();
        if (!decl.type.has_postdims && lexer_->peek('(')) {
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

        lexer_->require_newline(TerminatorPolicy::Semicolon);
    }

    lexer_->require_newline(TerminatorPolicy::Newline);
    return stmt;
}

Decl*
Parser::parse_pstruct()
{
    PstructDecl* struct_decl = nullptr;

    auto pos = lexer_->pos();

    sp::Atom* ident;
    if (lexer_->needsymbol(&ident))
        struct_decl = new PstructDecl(pos, ident);

    lexer_->need('{');
    do {
        if (lexer_->match('}')) {
            /* Quick exit */
            lexer_->lexpush();
            break;
        }

        declinfo_t decl = {};
        decl.type.ident = iVARIABLE;

        lexer_->need(tPUBLIC);
        auto pos = lexer_->pos();
        if (!parse_new_decl(&decl, nullptr, DECLFLAG_FIELD)) {
            lexer_->lexclr(TRUE);
            continue;
        }

        if (struct_decl)
            struct_decl->fields().push_back(StructField(pos, decl.name, decl.type));

        lexer_->require_newline(TerminatorPolicy::NewlineOrSemicolon);
    } while (!lexer_->peek('}'));

    lexer_->need('}');
    lexer_->match(';'); // eat up optional semicolon
    return struct_decl;
}

Decl*
Parser::parse_typedef()
{
    auto pos = lexer_->pos();

    sp::Atom* ident;
    if (!lexer_->needsymbol(&ident))
        return new ErrorDecl();

    lexer_->need('=');

    auto type = parse_function_type();
    return new TypedefDecl(pos, ident, type);
}

Decl*
Parser::parse_typeset()
{
    auto pos = lexer_->pos();

    sp::Atom* ident;
    if (!lexer_->needsymbol(&ident))
        return new ErrorDecl();

    TypesetDecl* decl = new TypesetDecl(pos, ident);

    lexer_->need('{');
    while (!lexer_->match('}')) {
        auto type = parse_function_type();
        decl->types().push_back(type);
    }

    lexer_->require_newline(TerminatorPolicy::NewlineOrSemicolon);
    return decl;
}

Decl*
Parser::parse_using()
{
    auto pos = lexer_->pos();

    auto validate = [this]() -> bool {
        sp::Atom* ident;
        if (!lexer_->needsymbol(&ident))
            return false;
        if (strcmp(ident->chars(), "__intrinsics__") != 0) {
            error(156);
            return false;
        }
        if (!lexer_->need('.'))
            return false;
        if (!lexer_->needsymbol(&ident))
            return false;
        if (strcmp(ident->chars(), "Handle") != 0) {
            error(156);
            return false;
        }
        return true;
    };
    if (!validate()) {
        lexer_->lexclr(TRUE);
        return new ErrorDecl();
    }

    lexer_->require_newline(TerminatorPolicy::Semicolon);
    return new UsingDecl(pos);
}

Stmt*
Parser::parse_pragma_unused()
{
    PragmaUnusedStmt* stmt = new PragmaUnusedStmt(lexer_->pos());

    int tok = lexer_->lex_same_line();
    for (;;) {
        if (tok != tSYMBOL) {
            report(1) << get_token_string(tSYMBOL) << get_token_string(tok);
            lexer_->lexclr(TRUE);
            break;
        }

        sp::Atom* name = lexer_->current_token()->atom;
        stmt->names().emplace_back(name);

        tok = lexer_->lex_same_line();
        if (tok == ',') {
            tok = lexer_->lex_same_line();
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
        auto pos = lexer_->pos();

        // Since spcomp is terrible, it's hard to use parse_decl() here - there
        // are all sorts of restrictions on const. We just implement some quick
        // detection instead.
        TypenameInfo rt;
        switch (lexer_->lex()) {
            case tINT:
            case tOBJECT:
            case tCHAR: {
                auto tok = *lexer_->current_token();
                parse_new_typename(&tok, &rt);
                break;
            }
            case tLABEL:
                rt = TypenameInfo{lexer_->current_token()->atom};
                rt.set_is_label();
                break;
            case tSYMBOL: {
                auto tok = *lexer_->current_token();
                // See if we can peek ahead another symbol.
                if (lexer_->peek(tSYMBOL)) {
                    // This is a new-style declaration.
                    parse_new_typename(&tok, &rt);
                } else {
                    // Otherwise, we got "const X ..." so the tag is int. Give the
                    // symbol back to the lexer so we get it as the name.
                    lexer_->lexpush();
                    rt = TypenameInfo{0};
                }
                break;
            }
            default:
                error(122);
                break;
        }

        sp::Atom* name = nullptr;
        lexer_->needsymbol(&name);

        lexer_->need('=');

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
    } while (lexer_->match(','));

    lexer_->need(tTERM);
    return list ? list : decl;
}

Expr*
Parser::hier14()
{
    Expr* node = hier13();

    int tok = lexer_->lex();
    auto pos = lexer_->pos();
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
            if (in_test_)
                error(211); /* possibly unintended assignment */
            break;
        default:
            lexer_->lexpush();
            return node;
    }

    Expr* right = hier14();
    return new BinaryExpr(pos, tok, node, right);
}

// Each of these lists is an operator precedence level, and each list is a
// zero-terminated list of operators in that level (in precedence order).
//
// The "op1" array in sc3.cpp must have the same ordering as if these lists
// were flattened.
static int list3[] = {'*', '/', '%', 0};
static int list4[] = {'+', '-', 0};
static int list5[] = {tSHL, tSHR, tSHRU, 0};
static int list6[] = {'&', 0};
static int list7[] = {'^', 0};
static int list8[] = {'|', 0};
static int list9[] = {tlLE, tlGE, '<', '>', 0};
static int list10[] = {tlEQ, tlNE, 0};
static int list11[] = {tlAND, 0};
static int list12[] = {tlOR, 0};

Expr*
Parser::plnge(int* opstr, NewHierFn hier)
{
    int opidx;

    Expr* node = (this->*hier)();
    if (nextop(&opidx, opstr) == 0)
        return node;

    do {
        auto pos = lexer_->pos();
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

    ChainedCompareExpr* chain = new ChainedCompareExpr(lexer_->pos(), first);

    do {
        auto pos = lexer_->pos();
        Expr* right = (this->*hier)();

        chain->ops().push_back(CompareOp(pos, opstr[opidx], right));
    } while (nextop(&opidx, opstr));

    return chain;
}

Expr*
Parser::hier13()
{
    Expr* node = hier12();
    if (lexer_->match('?')) {
        auto pos = lexer_->pos();
        Expr* left;
        {
            /* do not allow tagnames here (colon is a special token) */
            ke::SaveAndSet<bool> allowtags(&lexer_->allow_tags(), false);
            left = hier13();
        }
        lexer_->need(':');
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
    int tok = lexer_->lex();
    auto pos = lexer_->pos();
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
            if (lexer_->matchsymbol(&ident)) {
                if (lexer_->match('(')) {
                    Expr* target = new SymbolExpr(lexer_->pos(), ident);
                    return parse_call(pos, tok, target);
                }
                lexer_->lexpush();
            }

            TypenameInfo rt;
            if (!parse_new_typename(nullptr, &rt))
                rt = TypenameInfo{0};

            if (!lexer_->need('['))
                return new ErrorExpr();

            return parse_new_array(pos, rt);
        }
        case tLABEL: /* tagname override */
        {
            TypenameInfo ti(lexer_->current_token()->atom, true);
            if (lexer_->require_newdecls()) {
                // Warn: old style cast used when newdecls pragma is enabled
                report(240) << lexer_->current_token()->atom;
            }
            Expr* expr = hier2();
            return new CastExpr(pos, tok, ti, expr);
        }
        case tDEFINED:
        {
            int parens = 0;
            while (lexer_->match('('))
                parens++;

            sp::Atom* ident;
            if (!lexer_->needsymbol(&ident))
                return new ErrorExpr();
            while (parens--)
                lexer_->need(')');
            return new IsDefinedExpr(pos, ident);
        }
        case tSIZEOF:
        {
            int parens = 0;
            while (lexer_->match('('))
                parens++;

            sp::Atom* ident;
            if (lexer_->match(tTHIS)) {
                ident = gAtoms.add("this");
            } else {
                if (!lexer_->needsymbol(&ident))
                    return new ErrorExpr();
            }

            int array_levels = 0;
            while (lexer_->match('[')) {
                array_levels++;
                lexer_->need(']');
            }

            Atom* field = nullptr;
            int token = lexer_->lex();
            if (token == tDBLCOLON || token == '.') {
                if (!lexer_->needsymbol(&field))
                    return new ErrorExpr();
            } else {
                lexer_->lexpush();
                token = 0;
            }

            while (parens--)
                lexer_->need(')');

            return new SizeofExpr(pos, ident, field, token, array_levels);
        }
        default:
            lexer_->lexpush();
            break;
    }

    Expr* node = hier1();

    /* check for postfix operators */
    if (lexer_->match(';')) {
        /* Found a ';', do not look further for postfix operators */
        lexer_->lexpush(); /* push ';' back after successful match */
        return node;
    }
    if (lexer_->match(tTERM)) {
        /* Found a newline that ends a statement (this is the case when
         * semicolons are optional). Note that an explicit semicolon was
         * handled above. This case is similar, except that the token must
         * not be pushed back.
         */
        return node;
    }

    tok = lexer_->lex();
    switch (tok) {
        case tINC: /* lval++ */
        case tDEC: /* lval-- */
            return new PostIncExpr(lexer_->pos(), tok, node);
        default:
            lexer_->lexpush();
            break;
    }
    return node;
}

Expr*
Parser::hier1()
{
    Expr* base = nullptr;
    if (lexer_->match(tVIEW_AS)) {
        base = parse_view_as();
    } else {
        base = primary();
    }

    for (;;) {
        int tok = lexer_->lex();
        if (tok == '.' || tok == tDBLCOLON) {
            auto pos = lexer_->pos();
            sp::Atom* ident;
            if (!lexer_->needsymbol(&ident))
                break;
            base = new FieldAccessExpr(pos, tok, base, ident);
        } else if (tok == '[') {
            auto pos = lexer_->pos();
            Expr* inner = hier14();
            base = new IndexExpr(pos, base, inner);
            lexer_->need(']');
        } else if (tok == '(') {
            auto pos = lexer_->pos();
            base = parse_call(pos, tok, base);
        } else {
            lexer_->lexpush();
            break;
        }
    }
    return base;
}

Expr*
Parser::primary()
{
    if (lexer_->match('(')) { /* sub-expression - (expression,...) */
        /* no longer in "test" expression */
        ke::SaveAndSet<bool> in_test(&in_test_, false);
        /* allow tagnames to be used in parenthesized expressions */
        ke::SaveAndSet<bool> allowtags(&lexer_->allow_tags(), true);

        CommaExpr* expr = new CommaExpr(lexer_->pos());
        do {
            Expr* child = hier14();
            expr->exprs().push_back(child);
        } while (lexer_->match(','));
        lexer_->need(')');
        lexer_->lexclr(FALSE); /* clear lexer_->lex() push-back, it should have been
                        * cleared already by lexer_->need() */
        return expr;
    }

    int tok = lexer_->lex();
    if (tok == tTHIS)
        return new ThisExpr(lexer_->pos());
    if (tok == tSYMBOL)
        return new SymbolExpr(lexer_->pos(), lexer_->current_token()->atom);

    lexer_->lexpush();

    return constant();
}

Expr*
Parser::constant()
{
    int tok = lexer_->lex();
    auto pos = lexer_->pos();
    switch (tok) {
        case tNULL:
            return new NullExpr(pos);
        case tNUMBER:
            return new NumberExpr(pos, lexer_->current_token()->value);
        case tRATIONAL:
            return new FloatExpr(pos, lexer_->current_token()->value);
        case tSTRING: {
            const auto& str = lexer_->current_token()->data;
            return new StringExpr(pos, str.c_str(), str.size());
        }
        case tTRUE:
            return new TaggedValueExpr(lexer_->pos(), pc_tag_bool, 1);
        case tFALSE:
            return new TaggedValueExpr(lexer_->pos(), pc_tag_bool, 0);
        case '{':
        {
            ArrayExpr* expr = new ArrayExpr(pos);
            do {
                if (lexer_->match(tELLIPS)) {
                    expr->set_ellipses();
                    break;
                }
                Expr* child = hier14();
                expr->exprs().push_back(child);
            } while (lexer_->match(','));
            if (!lexer_->need('}'))
                lexer_->lexclr(FALSE);
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

    if (lexer_->match(')'))
        return call;

    bool named_params = false;
    do {
        sp::Atom* name = nullptr;
        if (lexer_->match('.')) {
            named_params = true;

            if (!lexer_->needsymbol(&name))
                break;
            lexer_->need('=');
        } else {
            if (named_params)
                error(44);
        }

        Expr* expr = nullptr;
        if (!lexer_->match('_'))
            expr = hier14();

        call->args().emplace_back(name, expr);

        if (lexer_->match(')'))
            break;
        if (!lexer_->need(','))
            break;
    } while (freading && !lexer_->match(tENDEXPR));

    return call;
}

Expr*
Parser::parse_view_as()
{
    auto pos = lexer_->pos();

    lexer_->need('<');
    TypenameInfo ti;
    {
        if (!parse_new_typename(nullptr, &ti))
            ti = TypenameInfo{0};
    }
    lexer_->need('>');

    int paren = lexer_->need('(');

    Expr* expr = hier14();
    if (paren)
        lexer_->need(')');
    else
        lexer_->match(')');
    return new CastExpr(pos, tVIEW_AS, ti, expr);
}

Expr*
Parser::struct_init()
{
    StructExpr* init = new StructExpr(lexer_->pos());

    // '}' has already been lexed.
    do {
        sp::Atom* name = nullptr;
        lexer_->needsymbol(&name);

        lexer_->need('=');

        auto pos = lexer_->pos();

        Expr* expr = nullptr;
        switch (lexer_->lex()) {
            case tSTRING: {
                const auto& str = lexer_->current_token()->data;
                expr = new StringExpr(pos, str.c_str(), str.size());
                break;
            }
            case tNUMBER:
                expr = new NumberExpr(pos, lexer_->current_token()->value);
                break;
            case tRATIONAL:
                expr = new FloatExpr(pos, lexer_->current_token()->value);
                break;
            case tSYMBOL:
                expr = new SymbolExpr(pos, lexer_->current_token()->atom);
                break;
            default:
                report(1) << "-constant-" << get_token_string(lexer_->current_token()->id);
                break;
        }

        if (name && expr)
            init->fields().push_back(StructInitField(name, expr));
    } while (lexer_->match(',') && !lexer_->peek('}'));

    lexer_->need('}');
    return init;
}

Stmt*
Parser::parse_static_assert()
{
    auto pos = lexer_->pos();

    lexer_->need('(');

    Expr* expr = hier14();
    if (!expr)
        return nullptr;

    PoolString * text = nullptr;
    if (lexer_->match(',') && lexer_->need(tSTRING)) {
        auto tok = lexer_->current_token();
        text = new PoolString(tok->data.c_str(), tok->data.size());
    }

    lexer_->need(')');
    lexer_->require_newline(TerminatorPolicy::NewlineOrSemicolon);

    return new StaticAssertStmt(pos, expr, text);
}

Expr*
Parser::var_init(int vclass)
{
    if (lexer_->match('{')) {
        // Peek for " <symbol> = " to see if this is a struct initializer.
        if (lexer_->match(tSYMBOL)) {
            if (lexer_->match('=')) {
                lexer_->lexpush();
                lexer_->lexpush();
                return struct_init();
            }
            lexer_->lexpush();
        }

        ArrayExpr* expr = new ArrayExpr(lexer_->pos());
        do {
            if (lexer_->peek('}'))
                break;
            if (lexer_->match(tELLIPS)) {
                expr->set_ellipses();
                break;
            }
            Expr* child = var_init(vclass);
            expr->exprs().emplace_back(child);
        } while (lexer_->match(','));
        lexer_->need('}');
        return expr;
    }

    if (lexer_->match(tSTRING)) {
        auto tok = lexer_->current_token();
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

        lexer_->need(']');
    } while (lexer_->match('['));
    return expr;
}

void
Parser::parse_post_dims(typeinfo_t* type)
{
    std::vector<Expr*> dim_exprs;
    bool has_dim_exprs = false;
    do {
        type->dim.emplace_back(0);

        if (lexer_->match(']')) {
            dim_exprs.emplace_back(nullptr);
        } else {
            has_dim_exprs = true;
            dim_exprs.emplace_back(hier14());
            lexer_->need(']');
        }
    } while (lexer_->match('['));

    if (has_dim_exprs)
        type->dim_exprs = PoolList<Expr*>(dim_exprs.begin(), dim_exprs.end());
}

Stmt*
Parser::parse_stmt(int* lastindent, bool allow_decl)
{
    if (!freading) {
        error(36); /* empty statement */
        return nullptr;
    }
    cc_.reports()->ResetErrorFlag();

    int tok = lexer_->lex();

    /* lexer_->lex() has set stmtindent */
    if (lastindent && tok != tLABEL) {
        int tabsize = cc_.options()->tabsize;
        if (*lastindent >= 0 && *lastindent != lexer_->stmtindent() && !lexer_->indent_nowarn() &&
            tabsize > 0)
        {
            error(217); /* loose indentation */
        }
        *lastindent = lexer_->stmtindent();
        lexer_->indent_nowarn() = false; /* if warning was blocked, re-enable it */
    }

    if (tok == tSYMBOL) {
        // We reaaaally don't have enough lookahead for this, so we cheat and try
        // to determine whether this is probably a declaration.
        int is_decl = FALSE;
        if (lexer_->match('[')) {
            if (lexer_->peek(']'))
                is_decl = TRUE;
            lexer_->lexpush();
        } else if (lexer_->peek(tSYMBOL)) {
            is_decl = TRUE;
        }

        if (is_decl) {
            if (!allow_decl) {
                error(3);
                return nullptr;
            }
            lexer_->lexpush();
            auto stmt = parse_local_decl(tNEWDECL, true);
            lexer_->need(tTERM);
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
            lexer_->lexpush();
            // Fall-through.
        case tDECL:
        case tSTATIC:
        case tNEW: {
            if (tok == tNEW && lexer_->match(tSYMBOL)) {
                if (lexer_->peek('(')) {
                    lexer_->lexpush();
                    break;
                }
                lexer_->lexpush(); // we lexer_->match'ed, give it back to lex for declloc
            }
            if (!allow_decl) {
                error(3);
                return nullptr;
            }
            auto stmt = parse_local_decl(tok, tok != tDECL);
            lexer_->need(tTERM);
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
            if (lexer_->match('}'))
                return new BlockStmt(lexer_->pos());
            return parse_compound(save == fline);
        }
        case ';':
            error(36); /* empty statement */
            return nullptr;
        case tBREAK:
        case tCONTINUE: {
            auto pos = lexer_->pos();
            lexer_->need(tTERM);
            if (!in_loop_) {
                error(24);
                return nullptr;
            }
            return new LoopControlStmt(pos, tok);
        }
        case tRETURN: {
            auto pos = lexer_->pos();
            Expr* expr = nullptr;
            if (!lexer_->match(tTERM)) {
                expr = parse_expr(false);
                lexer_->need(tTERM);
            }
            return new ReturnStmt(pos, expr);
        }
        case tASSERT: {
            auto pos = lexer_->pos();
            Expr* expr = parse_expr(true);
            lexer_->need(tTERM);
            if (!expr)
                return nullptr;
            return new AssertStmt(pos, expr);
        }
        case tDELETE: {
            auto pos = lexer_->pos();
            Expr* expr = parse_expr(false);
            lexer_->need(tTERM);
            if (!expr)
                return nullptr;
            return new DeleteStmt(pos, expr);
        }
        case tEXIT: {
            auto pos = lexer_->pos();
            Expr* expr = nullptr;
            if (lexer_->match(tTERM)) {
                expr = parse_expr(false);
                lexer_->need(tTERM);
            }
            return new ExitStmt(pos, expr);
        }
        case tDO: {
            auto pos = lexer_->pos();
            Stmt* stmt = nullptr;
            {
                ke::SaveAndSet<bool> in_loop(&in_loop_, true);
                stmt = parse_stmt(nullptr, false);
            }
            lexer_->need(tWHILE);
            bool parens = lexer_->match('(');
            Expr* cond = parse_expr(false);
            if (parens)
                lexer_->need(')');
            else
                error(243);
            lexer_->need(tTERM);
            if (!stmt || !cond)
                return nullptr;
            return new DoWhileStmt(pos, tok, cond, stmt);
        }
        case tWHILE: {
            auto pos = lexer_->pos();
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
        case tINCLUDE:
        case tpTRYINCLUDE:
            report(414);
            break;
        default: /* non-empty expression */
            break;
    }

    lexer_->lexpush(); /* analyze token later */
    Expr* expr = parse_expr(false);
    lexer_->need(tTERM);
    if (!expr)
        return nullptr;
    return new ExprStmt(expr->pos(), expr);
}

Stmt*
Parser::parse_compound(bool sameline)
{
    auto block_start = fline;

    BlockStmt* block = new BlockStmt(lexer_->pos());

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
        lexer_->stmtindent() = 0;
        for (i = 0; i < (int)(p - pline); i++) {
            int tabsize = cc_.options()->tabsize;
            if (pline[i] == '\t' && tabsize > 0)
                lexer_->stmtindent() += (int)(tabsize - (lexer_->stmtindent() + tabsize) % tabsize);
            else
                lexer_->stmtindent()++;
        }
    }

    int indent = -1;
    while (lexer_->match('}') == 0) { /* repeat until compound statement is closed */
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
    params.is_static = (tokid == tSTATIC);
    return parse_var(&decl, params);
}

Stmt*
Parser::parse_if()
{
    auto ifindent = lexer_->stmtindent();
    auto pos = lexer_->pos();
    auto expr = parse_expr(true);
    if (!expr)
        return nullptr;
    auto stmt = parse_stmt(nullptr, false);
    Stmt* else_stmt = nullptr;
    if (lexer_->match(tELSE)) {
        /* to avoid the "dangling else" error, we want a warning if the "else"
         * has a lower indent than the matching "if" */
        if (lexer_->stmtindent() < ifindent && cc_.options()->tabsize > 0)
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
    ke::SaveAndSet<bool> in_test(&in_test_, parens);

    if (parens)
        lexer_->need('(');

    Expr* expr = nullptr;
    CommaExpr* comma = nullptr;
    while (true) {
        expr = hier14();
        if (!expr)
            break;

        if (comma)
            comma->exprs().push_back(expr);

        if (!lexer_->match(','))
            break;

        if (!comma) {
            comma = new CommaExpr(expr->pos());
            comma->exprs().push_back(expr);
        }
    }
    if (parens)
        lexer_->need(')');

    return comma ? comma : expr;
}

Stmt*
Parser::parse_for()
{
    auto pos = lexer_->pos();

    int endtok = lexer_->match('(') ? ')' : tDO;
    if (endtok != ')')
        error(243);

    Stmt* init = nullptr;
    if (!lexer_->match(';')) {
        /* new variable declarations are allowed here */
        int tok_id = lexer_->lex();
        switch (tok_id) {
            case tINT:
            case tCHAR:
            case tOBJECT:
            case tVOID:
                lexer_->lexpush();
                // Fallthrough.
            case tNEW:
                /* The variable in expr1 of the for loop is at a
                 * 'compound statement' level of it own.
                 */
                // :TODO: test lexer_->need(tTERM) accepting newlines here
                init = parse_local_decl(tok_id, true);
                lexer_->need(';');
                break;
            case tSYMBOL: {
                // See comment in statement() near tSYMBOL.
                bool is_decl = false;
                if (lexer_->match('[')) {
                    if (lexer_->peek(']'))
                        is_decl = true;
                    lexer_->lexpush();
                } else if (lexer_->peek(tSYMBOL)) {
                    is_decl = true;
                }

                if (is_decl) {
                    lexer_->lexpush();
                    init = parse_local_decl(tSYMBOL, true);
                    lexer_->need(';');
                    break;
                }
                // Fall-through to default!
            }
            default:
                lexer_->lexpush();
                if (Expr* expr = parse_expr(false))
                    init = new ExprStmt(expr->pos(), expr);
                lexer_->need(';');
                break;
        }
    }

    Expr* cond = nullptr;
    if (!lexer_->match(';')) {
        cond = parse_expr(false);
        lexer_->need(';');
    }

    Expr* advance = nullptr;
    if (!lexer_->match(endtok)) {
        advance = parse_expr(false);
        lexer_->need(endtok);
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
    auto pos = lexer_->pos();

    int endtok = lexer_->match('(') ? ')' : tDO;
    if (endtok != ')')
        error(243);

    Expr* cond = parse_expr(false);
    lexer_->need(endtok);

    SwitchStmt* sw = new SwitchStmt(pos, cond);

    endtok = '}';
    lexer_->need('{');
    while (true) {
        int tok = lexer_->lex();

        switch (tok) {
            case tCASE:
                if (sw->default_case())
                    error(15); /* "default" case must be last in switch statement */
                parse_case(sw);
                break;
            case tDEFAULT:
                lexer_->need(':');
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
                    lexer_->indent_nowarn() = true;
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
        ke::SaveAndSet<bool> allowtags(&lexer_->allow_tags(), false);

        // hier14 because parse_expr() allows comma exprs
        if (Expr* expr = hier14())
            exprs.push_back(expr);
        if (lexer_->match(tDBLDOT))
            error(1, ":", "..");
    } while (lexer_->match(','));

    lexer_->need(':');

    Stmt* stmt = parse_stmt(nullptr, false);
    if (!stmt || exprs.empty())
        return;

    sw->AddCase(std::move(exprs), stmt);
}

Decl*
Parser::parse_inline_function(int tokid, const declinfo_t& decl, const int* this_tag)
{
    auto pos = lexer_->pos();
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
    if (!lexer_->deprecate().empty()) {
        stmt->set_deprecate(lexer_->deprecate());
        lexer_->deprecate() = {};
    }
    return stmt;
}

bool
Parser::parse_function(FunctionInfo* info, int tokid)
{
    if (!lexer_->match('(')) {
        error(10);
        lexer_->lexclr(TRUE);
        return false;
    }
    parse_args(info); // eats the close paren

    if (info->is_native()) {
        if (info->decl().opertok != 0) {
            lexer_->need('=');
            lexer_->lexpush();
        }
        if (lexer_->match('=')) {
            sp::Atom* ident;
            if (lexer_->needsymbol(&ident))
                info->set_alias(ident);
        }
    }

    switch (tokid) {
        case tNATIVE:
        case tFORWARD:
            lexer_->need(tTERM);
            return true;
        case tMETHODMAP:
            // Don't look for line endings if we're inline.
            return true;
        default:
            if (lexer_->match(';')) {
                if (!lexer_->NeedSemicolon())
                    error(10); /* old style prototypes used with optional semicolumns */
                info->set_is_forward();
                return true;
            }
            break;
    }

    if (lexer_->match('{'))
        lexer_->lexpush();
    else if (info->decl().type.is_new)
        lexer_->need('{');

    Stmt* body = parse_stmt(nullptr, false);
    if (!body)
        return false;

    info->set_body(BlockStmt::WrapStmt(body));
    info->set_end_pos(lexer_->pos());
    return true;
}

void
Parser::parse_args(FunctionInfo* info)
{
    if (lexer_->match(')'))
        return;

    do {
        auto pos = lexer_->pos();

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
        if (lexer_->match('='))
            init = var_init(sARGUMENT);

        if (info->args().size() >= SP_MAX_CALL_ARGUMENTS)
            error(45);
        if (decl.name->chars()[0] == PUBLIC_CHAR)
            report(56) << decl.name; // function arguments cannot be public

        auto p = new VarDecl(pos, decl.name, decl.type, sARGUMENT, false, false,
                             false, init);
        info->AddArg(p);
    } while (lexer_->match(','));

    lexer_->need(')');
    cc_.reports()->ResetErrorFlag();
}

Decl*
Parser::parse_methodmap()
{
    auto pos = lexer_->pos();

    sp::Atom* ident;
    lexer_->needsymbol(&ident);

    auto name_atom = ident;
    if (!isupper(name_atom->chars()[0]))
        report(109) << "methodmap";

    bool nullable = lexer_->match(tNULLABLE);

    sp::Atom* extends = nullptr;
    if (lexer_->match('<') && lexer_->needsymbol(&ident))
        extends = ident;

    auto decl = new MethodmapDecl(pos, name_atom, nullable, extends);

    lexer_->need('{');
    while (!lexer_->match('}')) {
        bool ok = false;
        int tok_id = lexer_->lex();
        if (tok_id == tPUBLIC) {
            ok = parse_methodmap_method(decl);
        } else if (tok_id == tSYMBOL && lexer_->current_token()->atom->str() == "property") {
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

    lexer_->require_newline(TerminatorPolicy::NewlineOrSemicolon);
    return decl;
}

bool
Parser::parse_methodmap_method(MethodmapDecl* map)
{
    auto pos = lexer_->pos();

    bool is_static = lexer_->match(tSTATIC);
    bool is_native = lexer_->match(tNATIVE);

    sp::Atom* symbol = nullptr;
    full_token_t symbol_tok;
    if (lexer_->matchsymbol(&symbol))
        symbol_tok = *lexer_->current_token();

    if (lexer_->match('~'))
        error(118);

    declinfo_t ret_type = {};

    bool is_ctor = false;
    if (symbol && lexer_->match('(')) {
        // ::= ident '('

        // Push the '(' token back for parse_args().
        is_ctor = true;
        lexer_->lexpush();

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
        if (!lexer_->needsymbol(&symbol))
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

    ke::SaveAndSet<int> require_newdecls(&lexer_->require_newdecls(), TRUE);
    if (!parse_function(fun, is_native ? tMETHODMAP : 0))
        return false;

    // Use the short name for the function decl
    auto method = new MethodmapMethod;
    method->is_static = is_static;
    method->decl = new FunctionDecl(pos, symbol, fun);
    map->methods().emplace_back(method);

    if (is_native)
        lexer_->require_newline(TerminatorPolicy::Semicolon);
    else
        lexer_->require_newline(TerminatorPolicy::Newline);
    return true;
}

bool
Parser::parse_methodmap_property(MethodmapDecl* map)
{
    auto prop = new MethodmapProperty;
    prop->pos = lexer_->pos();

    if (!parse_new_typeexpr(&prop->type, nullptr, 0))
        return false;

    sp::Atom* ident;
    if (!lexer_->needsymbol(&ident))
        return false;
    if (!lexer_->need('{'))
        return false;

    prop->name = ident;

    while (!lexer_->match('}')) {
        if (!parse_methodmap_property_accessor(map, prop))
            lexer_->lexclr(TRUE);
    }

    map->properties().emplace_back(prop);

    lexer_->require_newline(TerminatorPolicy::Newline);
    return true;
}

bool
Parser::parse_methodmap_property_accessor(MethodmapDecl* map, MethodmapProperty* prop)
{
    bool is_native = false;
    auto pos = lexer_->pos();

    lexer_->need(tPUBLIC);

    sp::Atom* ident;
    if (!lexer_->matchsymbol(&ident)) {
        if (!lexer_->match(tNATIVE)) {
            report(125);
            return false;
        }
        is_native = true;
        if (!lexer_->needsymbol(&ident))
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
        lexer_->require_newline(TerminatorPolicy::Semicolon);
    else
        lexer_->require_newline(TerminatorPolicy::Newline);
    return true;
}

// Consumes a line, returns FALSE if EOF hit.
bool
Parser::consume_line()
{
    // First check for EOF.
    if (lexer_->lex() == 0)
        return false;
    lexer_->lexpush();

    while (!lexer_->match(tTERM)) {
        // Check for EOF.
        if (lexer_->lex() == 0)
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
    int lparen = lexer_->match('(');
    if (!lexer_->need(tFUNCTION))
        return nullptr;

    auto info = new TypedefInfo;
    info->pos = lexer_->pos();

    parse_new_typename(nullptr, &info->ret_type);

    lexer_->need('(');

    while (!lexer_->match(')')) {
        auto decl = gPoolAllocator.alloc<declinfo_t>();
        decl->type.ident = iVARIABLE;

        parse_new_decl(decl, nullptr, DECLFLAG_ARGUMENT);

        // Eat optional symbol name.
        lexer_->match(tSYMBOL);

        info->args.emplace_back(decl);

        if (!lexer_->match(',')) {
            lexer_->need(')');
            break;
        }
    }

    // Error once when we're past max args.
    if (info->args.size() >= SP_MAX_EXEC_PARAMS)
        report(45);

    if (lparen)
        lexer_->need(')');

    lexer_->require_newline(TerminatorPolicy::Semicolon);
    cc_.reports()->ResetErrorFlag();
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
    if (lexer_->peek(tELLIPS))
        return parse_old_decl(decl, flags);

    // Must attempt to match const first, since it's a common prefix.
    if (lexer_->match(tCONST))
        decl->type.is_const = true;

    // Sometimes we know ahead of time whether the declaration will be old, for
    // example, if preceded by tNEW or tDECL.
    if (flags & DECLFLAG_OLD)
        return parse_old_decl(decl, flags);
    if (flags & DECLFLAG_NEW)
        return parse_new_decl(decl, NULL, flags);

    // If parsing an argument, there are two simple checks for whether this is a
    // new or old-style declaration.
    if ((flags & DECLFLAG_ARGUMENT) && (lexer_->peek('&') || lexer_->peek('{')))
        return parse_old_decl(decl, flags);

    // Another dead giveaway is there being a label or typeless operator.
    if (lexer_->peek(tLABEL) || lexer_->peek(tOPERATOR))
        return parse_old_decl(decl, flags);

    // Otherwise, we have to eat a symbol to tell.
    if (lexer_->matchsymbol(&ident)) {
        auto ident_tok = *lexer_->current_token();

        if (lexer_->peek(tSYMBOL) || lexer_->peek(tOPERATOR) || lexer_->peek('&') || lexer_->peek(tELLIPS)) {
            // A new-style declaration only allows array dims or a symbol name, so
            // this is a new-style declaration.
            return parse_new_decl(decl, &ident_tok, flags);
        }

        if ((flags & DECLMASK_NAMED_DECL) && lexer_->match('[')) {
            // Oh no - we have to parse array dims before we can tell what kind of
            // declarator this is. It could be either:
            //    "x[] y" (new-style), or
            //    "y[],"  (old-style)
            parse_post_array_dims(decl, flags);

            if (lexer_->match(tSYMBOL) || lexer_->match('&')) {
                // This must be a newdecl, "x[] y" or "x[] &y", the latter of which
                // is illegal, but we flow it through the right path anyway.
                lexer_->lexpush();
                fix_mispredicted_postdims(decl);
                return parse_new_decl(decl, &ident_tok, flags);
            }

            if (lexer_->require_newdecls())
                error(147);

            // The most basic - "x[]" and that's it. Well, we know it has no tag and
            // we know its name. We might as well just complete the entire decl.
            decl->name = ident;
            decl->type.set_tag(0);
            return true;
        }

        // Give the symbol back to the lexer. This is an old decl.
        lexer_->lexpush();
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

    if (lexer_->match(tCONST)) {
        if (type->is_const)
            error(138);
        type->is_const = true;
    }

    TypenameInfo ti = TypenameInfo(0);

    int numtags = 0;
    if (flags & DECLFLAG_ARGUMENT) {
        if (lexer_->match('&'))
            type->ident = iREFERENCE;

        // grammar for multitags is:
        //   multi-tag ::= '{' (symbol (',' symbol)*)? '}' ':'
        if (lexer_->match('{')) {
            while (true) {
                if (!lexer_->match('_')) {
                    // If we don't get the magic tag '_', then we should have a symbol.
                    sp::Atom* name;
                    if (lexer_->needsymbol(&name))
                        ti = TypenameInfo(name, true);
                }
                numtags++;

                if (lexer_->match('}'))
                    break;
                lexer_->need(',');
            }
            lexer_->need(':');
        }
        if (numtags > 1)
            error(158);
    }

    if (numtags == 0) {
        if (lexer_->match(tLABEL))
            ti = TypenameInfo(lexer_->current_token()->atom, true);
    }

    // All finished with tag stuff.
    type->set_type(ti);

    if (lexer_->require_newdecls())
        error(147);

    // Look for varargs and end early.
    if (lexer_->match(tELLIPS)) {
        type->ident = iVARARGS;
        return TRUE;
    }

    if (flags & DECLMASK_NAMED_DECL) {
        if ((flags & DECLFLAG_MAYBE_FUNCTION) && lexer_->match(tOPERATOR)) {
            decl->opertok = operatorname(&decl->name);
            if (decl->opertok == 0)
                decl->name = gAtoms.add("__unknown__");
        } else {
            if (!lexer_->peek(tSYMBOL)) {
                extern const char* sc_tokens[];
                int tok_id = lexer_->lex();
                switch (tok_id) {
                    case tOBJECT:
                    case tCHAR:
                    case tVOID:
                    case tINT:
                        if (lexer_->peek(tSYMBOL)) {
                            error(143);
                        } else {
                            error(157, sc_tokens[tok_id - tFIRST]);
                            decl->name = gAtoms.add(sc_tokens[tok_id - tFIRST]);
                        }
                        break;
                    default:
                        lexer_->lexpush();
                        break;
                }
            }
            lexer_->needsymbol(&decl->name);
        }
    }

    if ((flags & DECLMASK_NAMED_DECL) && !decl->opertok) {
        if (lexer_->match('['))
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
        if ((flags & DECLFLAG_ARGUMENT) && lexer_->match(tELLIPS)) {
            decl->type.ident = iVARARGS;
            return true;
        }

        if ((flags & DECLFLAG_MAYBE_FUNCTION) && lexer_->match(tOPERATOR)) {
            decl->opertok = operatorname(&decl->name);
            if (decl->opertok == 0)
                decl->name = gAtoms.add("__unknown__");
        } else {
            lexer_->needsymbol(&decl->name);
        }
    }

    if (flags & DECLMASK_NAMED_DECL) {
        if (lexer_->match('[')) {
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
    int opertok = lexer_->lex();
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
    if (lexer_->match(tSYMBOL))
        decl->name = lexer_->current_token()->atom;

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
        if (lexer_->match('[')) {
            // int x[], y[]
            //           ^-- parse this
            parse_post_array_dims(decl, flags);
        }
    } else {
        if (lexer_->match('[')) {
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
        tok = lexer_->lex_tok();

    if (tok.id == tCONST) {
        if (type->is_const)
            error(138);
        type->is_const = true;
        tok = lexer_->lex_tok();
    }

    TypenameInfo ti;
    if (!parse_new_typename(&tok, &ti))
        return false;
    type->set_type(ti);

    // Note: we could have already filled in the prefix array bits, so we check
    // that ident != iARRAY before looking for an open bracket.
    if (type->ident != iARRAY && lexer_->match('[')) {
        do {
            type->dim.emplace_back(0);
            if (!lexer_->match(']')) {
                error(101);

                // Try to eat a close bracket anyway.
                hier14();
                lexer_->match(']');
            }
        } while (lexer_->match('['));
        type->ident = iREFARRAY;
    }

    if (flags & DECLFLAG_ARGUMENT) {
        if (lexer_->match('&')) {
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
        tmp = lexer_->lex_tok();
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
Parser::nextop(int* opidx, int* list)
{
    *opidx = 0;
    while (*list) {
        if (lexer_->match(*list)) {
            return TRUE; /* found! */
        } else {
            list += 1;
            *opidx += 1;
        }
    }
    return FALSE; /* entire list scanned, nothing found */
}
