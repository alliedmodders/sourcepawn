// vim: set ts=8 sts=4 sw=4 tw=99 et:
//  Pawn compiler - Recursive descend expresion parser
//
//  Copyright (c) ITB CompuPhase, 1997-2005
//  Copyright (c) AlliedModders 2021
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
#include "semantics.h"

#include <unordered_set>

#include <amtl/am-raii.h>
#include "array-helpers.h"
#include "code-generator.h"
#include "errors.h"
#include "expressions.h"
#include "lexer.h"
#include "parse-node.h"
#include "sctracker.h"
#include "symbols.h"

Semantics::Semantics(CompileContext& cc, ParseTree* tree)
  : cc_(cc),
    tree_(tree)
{
    types_ = cc.types();
}

bool Semantics::Analyze() {
    SemaContext sc(this);
    ke::SaveAndSet<SemaContext*> push_sc(&sc_, &sc);

    AutoCountErrors errors;
    if (!CheckStmtList(tree_->stmts()) || !errors.ok())
        return false;

    // This inserts missing return statements at the global scope, so it cannot
    // be omitted.
    bool has_public = false;
    for (const auto& entry : static_scopes_)
        has_public |= TestSymbols(entry, false);
    has_public |= TestSymbols(cc_.globals(), false);

    if (!has_public) {
        report(13); /* no entry point (no public functions) */
        return false;
    }

    // All heap allocations must be owned by a ParseNode.
    assert(!pending_heap_allocation_);
    return true;
}

bool Semantics::CheckStmtList(StmtList* list) {
    bool ok = true;
    for (const auto& stmt : list->stmts()) {
        cc_.reports()->ResetErrorFlag();

        ok &= CheckStmt(stmt);

        FlowType flow = stmt->flow_type();
        if (flow != Flow_None && list->flow_type() == Flow_None)
            list->set_flow_type(flow);
    }
    return ok;
}

bool Semantics::CheckStmt(Stmt* stmt, StmtFlags flags) {
    AutoErrorPos aep(stmt->pos());
    ke::Maybe<ke::SaveAndSet<bool>> restore_heap_ownership;
    if (flags & STMT_OWNS_HEAP)
        restore_heap_ownership.init(&pending_heap_allocation_, false);

    auto owns_heap = ke::MakeScopeGuard([&, this]() {
        if (flags & STMT_OWNS_HEAP)
            AssignHeapOwnership(stmt);
    });

    switch (stmt->kind()) {
        case StmtKind::ChangeScopeNode:
            return CheckChangeScopeNode(stmt->to<ChangeScopeNode>());
        case StmtKind::VarDecl:
            return CheckVarDecl(stmt->to<VarDecl>());
        case StmtKind::ArgDecl:
            return CheckVarDecl(stmt->to<ArgDecl>());
        case StmtKind::ExprStmt:
            return CheckExprStmt(stmt->to<ExprStmt>());
        case StmtKind::ExitStmt:
            return CheckExitStmt(stmt->to<ExitStmt>());
        case StmtKind::BlockStmt:
            return CheckBlockStmt(stmt->to<BlockStmt>());
        case StmtKind::AssertStmt:
            return CheckAssertStmt(stmt->to<AssertStmt>());
        case StmtKind::IfStmt:
            return CheckIfStmt(stmt->to<IfStmt>());
        case StmtKind::DeleteStmt:
            return CheckDeleteStmt(stmt->to<DeleteStmt>());
        case StmtKind::DoWhileStmt:
            return CheckDoWhileStmt(stmt->to<DoWhileStmt>());
        case StmtKind::ForStmt:
            return CheckForStmt(stmt->to<ForStmt>());
        case StmtKind::SwitchStmt:
            return CheckSwitchStmt(stmt->to<SwitchStmt>());
        case StmtKind::FunctionDecl:
            return CheckFunctionDecl(stmt->to<FunctionDecl>());
        case StmtKind::EnumStructDecl:
            return CheckEnumStructDecl(stmt->to<EnumStructDecl>());
        case StmtKind::MethodmapDecl:
            return CheckMethodmapDecl(stmt->to<MethodmapDecl>());
        case StmtKind::ReturnStmt:
            return CheckReturnStmt(stmt->to<ReturnStmt>());
        case StmtKind::PragmaUnusedStmt:
            return CheckPragmaUnusedStmt(stmt->to<PragmaUnusedStmt>());
        case StmtKind::StmtList:
            return CheckStmtList(stmt->to<StmtList>());
        case StmtKind::StaticAssertStmt:
            return CheckStaticAssertStmt(stmt->to<StaticAssertStmt>());
        case StmtKind::BreakStmt:
            return CheckBreakStmt(stmt->to<BreakStmt>());
        case StmtKind::ContinueStmt:
            return CheckContinueStmt(stmt->to<ContinueStmt>());
        case StmtKind::EnumDecl:
        case StmtKind::PstructDecl:
        case StmtKind::TypedefDecl:
        case StmtKind::TypesetDecl:
        case StmtKind::UsingDecl:
            return true;
        default:
            assert(false);

            report(stmt, 420) << (int)stmt->kind();
            return false;
    }
}

bool Semantics::CheckVarDecl(VarDeclBase* decl) {
    AutoErrorPos aep(decl->pos());

    auto sym = decl->sym();
    const auto& type = decl->type();

    // Constants are checked during binding.
    if (sym->ident == iCONSTEXPR)
        return true;

    if (types_->find(sym->tag)->kind() == TypeKind::Struct)
        return CheckPstructDecl(decl);

    if (!decl->as<ArgDecl>() && type.is_const && !decl->init() && !decl->is_public())
        report(decl->pos(), 251);

    if (type.ident == iARRAY || type.ident == iREFARRAY) {
        if (!CheckArrayDeclaration(decl))
            return false;
        if (decl->vclass() == sLOCAL && sym->ident == iREFARRAY)
            pending_heap_allocation_ = true;
        return true;
    }

    assert(type.ident == iVARIABLE || type.ident == iREFERENCE);

    auto init = decl->init();

    // Since we always create an assignment expression, all type checks will
    // be performed by the Analyze(sc) call here.
    //
    // :TODO: write flag when removing ProcessUses
    if (init && !CheckExpr(init))
        return false;

    auto vclass = decl->vclass();
    auto init_rhs = decl->init_rhs();
    if (init && vclass != sLOCAL) {
        if (!init_rhs->EvalConst(nullptr, nullptr)) {
            if (vclass == sARGUMENT && init_rhs->is(ExprKind::SymbolExpr))
                return true;
            report(init_rhs->pos(), 8);
        }
    }
    return true;
}

bool Semantics::CheckPstructDecl(VarDeclBase* decl) {
    if (!decl->init())
        return true;

    auto sym = decl->sym();
    auto init = decl->init()->right()->as<StructExpr>();
    assert(init); // If we parse struct initializers as a normal global, this check will need to be
                  // soft.
    auto type = types_->find(sym->tag);
    auto ps = type->as<pstruct_t>();

    std::vector<bool> visited;
    visited.resize(ps->args.size());

    // Do as much checking as we can before bailing out.
    bool ok = true;
    for (const auto& field : init->fields())
        ok &= CheckPstructArg(decl, ps, field, &visited);

    if (!ok)
        return false;

    // Fill in default values as needed.
    for (size_t i = 0; i < visited.size(); i++) {
        if (visited[i])
            continue;
        if (ps->args[i]->type.ident == iREFARRAY) {
            assert(ps->args[i]->type.tag() == types_->tag_string());

            auto expr = new StringExpr(decl->pos(), "", 0);
            init->fields().push_back(new StructInitFieldExpr(ps->args[i]->name, expr,
                                                             decl->pos()));
        }
    }

    return true;
}

bool Semantics::CheckPstructArg(VarDeclBase* decl, const pstruct_t* ps,
                                const StructInitFieldExpr* field, std::vector<bool>* visited)
{
    auto arg = ps->GetArg(field->name);
    if (!arg) {
        report(field->pos(), 96) << field->name << "struct" << decl->name();
        return false;
    }

    if (visited->at(arg->index))
        error(field->value->pos(), 244, field->name->chars());

    visited->at(arg->index) = true;

    if (auto expr = field->value->as<StringExpr>()) {
        if (arg->type.ident != iREFARRAY) {
            error(expr->pos(), 48);
            return false;
        }
        if (arg->type.tag() != types_->tag_string())
            error(expr->pos(), 213, type_to_name(types_->tag_string()), type_to_name(arg->type.tag()));
    } else if (auto expr = field->value->as<TaggedValueExpr>()) {
        if (arg->type.ident != iVARIABLE) {
            error(expr->pos(), 23);
            return false;
        }

        // Proper tag checks were missing in the old parser, and unfortunately
        // adding them breaks older code. As a special case, we allow implicit
        // coercion of constants 0 or 1 to bool.
        if (!(arg->type.tag() == types_->tag_bool() && expr->tag() == 0 &&
            (expr->value() == 0 || expr->value() == 1)))
        {
            matchtag(arg->type.tag(), expr->tag(), MATCHTAG_COERCE);
        }
    } else if (auto expr = field->value->as<SymbolExpr>()) {
        auto sym = expr->sym();
        if (arg->type.ident == iVARIABLE) {
            if (sym->ident != iVARIABLE) {
                error(expr->pos(), 405);
                return false;
            }
            matchtag(arg->type.tag(), sym->tag, MATCHTAG_COERCE);
        } else if (arg->type.ident == iREFARRAY) {
            if (sym->ident != iARRAY) {
                error(expr->pos(), 405);
                return false;
            }
            if (sym->dim_count() != 1) {
                error(expr->pos(), 405);
                return false;
            }
        } else {
            error(expr->pos(), 405);
            return false;
        }
    } else {
        assert(false);
        return false;
    }
    return true;
}

static inline int GetOperToken(int token) {
    switch (token) {
        case tlEQ:
        case tlNE:
        case tlLE:
        case tlGE:
        case '<':
        case '>':
        case '|':
        case '^':
        case '&':
        case '*':
        case '/':
        case '%':
        case '+':
        case '-':
        case tSHL:
        case tSHR:
        case tSHRU:
            return token;
        case taMULT:
            return '*';
        case taDIV:
            return '/';
        case taMOD:
            return '%';
        case taADD:
            return '+';
        case taSUB:
            return '-';
        case taSHL:
            return tSHL;
        case taSHR:
            return tSHR;
        case taSHRU:
            return tSHRU;
        case taAND:
            return '&';
        case taXOR:
            return '^';
        case taOR:
            return '|';
        case '=':
        case tlOR:
        case tlAND:
            return 0;
        default:
            assert(false);
            return 0;
    }
}

bool Semantics::CheckExpr(Expr* expr) {
    AutoErrorPos aep(expr->pos());
    switch (expr->kind()) {
        case ExprKind::UnaryExpr:
            return CheckUnaryExpr(expr->to<UnaryExpr>());
        case ExprKind::IncDecExpr:
            return CheckIncDecExpr(expr->to<IncDecExpr>());
        case ExprKind::BinaryExpr:
            return CheckBinaryExpr(expr->to<BinaryExpr>());
        case ExprKind::LogicalExpr:
            return CheckLogicalExpr(expr->to<LogicalExpr>());
        case ExprKind::ChainedCompareExpr:
            return CheckChainedCompareExpr(expr->to<ChainedCompareExpr>());
        case ExprKind::TernaryExpr:
            return CheckTernaryExpr(expr->to<TernaryExpr>());
        case ExprKind::CastExpr:
            return CheckCastExpr(expr->to<CastExpr>());
        case ExprKind::SymbolExpr:
            return CheckSymbolExpr(expr->to<SymbolExpr>(), false);
        case ExprKind::CommaExpr:
            return CheckCommaExpr(expr->to<CommaExpr>());
        case ExprKind::ThisExpr:
            return CheckThisExpr(expr->to<ThisExpr>());
        case ExprKind::NullExpr:
            return CheckNullExpr(expr->to<NullExpr>());
        case ExprKind::StringExpr:
            return CheckStringExpr(expr->to<StringExpr>());
        case ExprKind::ArrayExpr:
            return CheckArrayExpr(expr->to<ArrayExpr>());
        case ExprKind::IndexExpr:
            return CheckIndexExpr(expr->to<IndexExpr>());
        case ExprKind::FieldAccessExpr:
            return CheckFieldAccessExpr(expr->to<FieldAccessExpr>(), false);
        case ExprKind::CallExpr:
            return CheckCallExpr(expr->to<CallExpr>());
        case ExprKind::NewArrayExpr:
            return CheckNewArrayExpr(expr->to<NewArrayExpr>());
        case ExprKind::TaggedValueExpr:
            return CheckTaggedValueExpr(expr->to<TaggedValueExpr>());
        case ExprKind::SizeofExpr:
            return CheckSizeofExpr(expr->to<SizeofExpr>());
        case ExprKind::RvalueExpr:
            return CheckWrappedExpr(expr, expr->to<RvalueExpr>()->expr());
        case ExprKind::NamedArgExpr:
            return CheckWrappedExpr(expr, expr->to<NamedArgExpr>()->expr);
        default:
            assert(false);
            report(expr, 420) << (int)expr->kind();
            return false;
    }
}

bool Semantics::CheckWrappedExpr(Expr* outer, Expr* inner) {
    if (!CheckExpr(inner))
        return false;

    outer->val() = inner->val();
    outer->set_lvalue(inner->lvalue());
    return true;
}

CompareOp::CompareOp(const token_pos_t& pos, int token, Expr* expr)
  : pos(pos),
    token(token),
    expr(expr),
    oper_tok(GetOperToken(token))
{
}

bool Expr::EvalConst(cell* value, int* tag) {
    if (val_.ident != iCONSTEXPR) {
        if (!FoldToConstant())
            return false;
        assert(val_.ident == iCONSTEXPR);
    }

    if (value)
        *value = val_.constval();
    if (tag)
        *tag = val_.tag;
    return true;
}

static inline bool HasSideEffects(const PoolArray<Expr*>& exprs) {
    for (const auto& child : exprs) {
        if (child->HasSideEffects())
            return true;
    }
    return false;
}

bool Expr::HasSideEffects() {
    if (val().ident == iACCESSOR)
        return true;

    switch (kind()) {
        case ExprKind::UnaryExpr: {
            auto e = to<UnaryExpr>();
            return e->userop() || e->expr()->HasSideEffects();
        }
        case ExprKind::BinaryExpr: {
            auto e = to<BinaryExpr>();
            return e->userop().sym || IsAssignOp(e->token()) || e->left()->HasSideEffects() ||
                   e->right()->HasSideEffects();
        }
        case ExprKind::LogicalExpr: {
            auto e = to<LogicalExpr>();
            return e->left()->HasSideEffects() || e->right()->HasSideEffects();
        }
        case ExprKind::ChainedCompareExpr: {
            auto e = to<ChainedCompareExpr>();
            if (e->first()->HasSideEffects())
                return true;
            for (const auto& op : e->ops()) {
                if (op.userop.sym || op.expr->HasSideEffects())
                    return true;
            }
            return false;
        }
        case ExprKind::TernaryExpr: {
            auto e = to<TernaryExpr>();
            return e->first()->HasSideEffects() || e->second()->HasSideEffects() ||
                   e->third()->HasSideEffects();
        }
        case ExprKind::CastExpr:
            return to<CastExpr>()->expr()->HasSideEffects();
        case ExprKind::CommaExpr: {
            auto e = to<CommaExpr>();
            return ::HasSideEffects(e->exprs());
        }
        case ExprKind::ArrayExpr: {
            auto e = to<ArrayExpr>();
            return ::HasSideEffects(e->exprs());
        }
        case ExprKind::NewArrayExpr: {
            auto e = to<NewArrayExpr>();
            return ::HasSideEffects(e->exprs());
        }
        case ExprKind::IndexExpr: {
            auto e = to<IndexExpr>();
            return e->base()->HasSideEffects() || e->index()->HasSideEffects();
        }
        case ExprKind::FieldAccessExpr: {
            auto e = to<FieldAccessExpr>();
            return e->base()->HasSideEffects();
        }
        case ExprKind::RvalueExpr:
            return to<RvalueExpr>()->expr()->HasSideEffects();
        case ExprKind::CallExpr: // Not intelligent yet.
        case ExprKind::IncDecExpr:
        case ExprKind::CallUserOpExpr:
            return true;
        case ExprKind::NullExpr:
        case ExprKind::SizeofExpr:
        case ExprKind::StringExpr:
        case ExprKind::SymbolExpr:
        case ExprKind::TaggedValueExpr:
        case ExprKind::ThisExpr:
            return false;
        default:
            assert(false);
            return true;
    }
}

Expr* Semantics::AnalyzeForTest(Expr* expr) {
    if (!CheckExpr(expr))
        return nullptr;

    auto& val = expr->val();
    if (val.ident == iARRAY || val.ident == iREFARRAY) {
        if (val.sym)
            report(expr, 33) << val.sym->name();
        else
            report(expr, 29);
        return nullptr;
    }

    if (val.tag != 0 || val.tag != types_->tag_bool()) {
        UserOperation userop;
        if (find_userop(*sc_, '!', val.tag, 0, 1, &val, &userop)) {
            // Call user op for '!', then invert it. EmitTest will fold out the
            // extra invert.
            //
            // First convert to rvalue, since user operators should never
            // taken an lvalue.
            if (expr->lvalue())
                expr = new RvalueExpr(expr);

            expr = new CallUserOpExpr(userop, expr);
            expr = new UnaryExpr(expr->pos(), '!', expr);
            expr->val().ident = iEXPRESSION;
            expr->val().tag = types_->tag_bool();
            return expr;
        }
    }

    if (val.ident == iCONSTEXPR) {
        if (val.constval())
            report(expr, 206);
        else
            report(expr, 205);
    } else if (auto sym_expr = expr->as<SymbolExpr>()) {
        if (sym_expr->sym()->ident == iFUNCTN)
            report(expr, 249);
    }

    if (expr->lvalue())
        return new RvalueExpr(expr);

    return expr;
}

RvalueExpr::RvalueExpr(Expr* expr)
  : EmitOnlyExpr(ExprKind::RvalueExpr, expr->pos()),
    expr_(expr)
{
    assert(expr_->lvalue());

    val_ = expr_->val();
    if (val_.ident == iACCESSOR) {
        if (val_.accessor()->getter)
            markusage(val_.accessor()->getter, uREAD);
        val_.ident = iEXPRESSION;
    }
}

void
RvalueExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

bool Semantics::CheckUnaryExpr(UnaryExpr* unary) {
    AutoErrorPos aep(unary->pos());

    auto expr = unary->expr();
    if (!CheckExpr(expr))
        return false;

    if (expr->lvalue())
        expr = unary->set_expr(new RvalueExpr(expr));

    auto& out_val = unary->val();
    out_val = expr->val();

    // :TODO: check for invalid types

    UserOperation userop;
    switch (unary->token()) {
        case '~':
            if (out_val.ident == iCONSTEXPR)
                out_val.set_constval(~out_val.constval());
            break;
        case '!':
            if (find_userop(*sc_, '!', out_val.tag, 0, 1, &out_val, &userop)) {
                expr = unary->set_expr(new CallUserOpExpr(userop, expr));
                out_val = expr->val();
                unary->set_userop();
            } else if (out_val.ident == iCONSTEXPR) {
                out_val.set_constval(!out_val.constval());
            }
            out_val.tag = types_->tag_bool();
            break;
        case '-':
            if (out_val.ident == iCONSTEXPR && out_val.tag == types_->tag_float()) {
                float f = sp::FloatCellUnion(out_val.constval()).f32;
                out_val.set_constval(sp::FloatCellUnion(-f).cell);
            } else if (find_userop(*sc_, '-', out_val.tag, 0, 1, &out_val, &userop)) {
                expr = unary->set_expr(new CallUserOpExpr(userop, expr));
                out_val = expr->val();
                unary->set_userop();
            } else if (out_val.ident == iCONSTEXPR) {
                /* the negation of a fixed point number is just an integer negation */
                out_val.set_constval(-out_val.constval());
            }
            break;
        default:
            assert(false);
    }

    if (out_val.ident != iCONSTEXPR)
        out_val.ident = iEXPRESSION;
    return true;
}

void
UnaryExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

bool Semantics::CheckIncDecExpr(IncDecExpr* incdec) {
    AutoErrorPos aep(incdec->pos());

    auto expr = incdec->expr();
    if (!CheckExpr(expr))
        return false;
    if (!expr->lvalue()) {
        report(incdec, 22);
        return false;
    }

    const auto& expr_val = expr->val();
    if (expr_val.ident != iACCESSOR) {
        if (expr_val.sym->is_const) {
            report(incdec, 22); /* assignment to const argument */
            return false;
        }
    } else {
        if (!expr_val.accessor()->setter) {
            report(incdec, 152) << expr_val.accessor()->name;
            return false;
        }
        if (!expr_val.accessor()->getter) {
            report(incdec, 149) << expr_val.accessor()->name;
            return false;
        }
        markusage(expr_val.accessor()->getter, uREAD);
        markusage(expr_val.accessor()->setter, uREAD);
    }

    find_userop(*sc_, incdec->token(), expr_val.tag, 0, 1, &expr_val, &incdec->userop());

    // :TODO: more type checks
    auto& val = incdec->val();
    val.ident = iEXPRESSION;
    val.tag = expr_val.tag;
    return true;
}

void
IncDecExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

void
BinaryExprBase::ProcessUses(SemaContext& sc)
{
    // Assign ops, even read/write ones, do not count as variable uses for TestSymbols.
    if (IsAssignOp(token_))
        left_->ProcessUses(sc);
    else
        left_->MarkAndProcessUses(sc);
    right_->MarkAndProcessUses(sc);
}

BinaryExpr::BinaryExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
  : BinaryExprBase(ExprKind::BinaryExpr, pos, token, left, right)
{
    oper_tok_ = GetOperToken(token_);
}

bool Semantics::CheckBinaryExpr(BinaryExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto left = expr->left();
    auto right = expr->right();
    if (!CheckExpr(left) || !CheckExpr(right))
        return false;

    int token = expr->token();
    if (IsAssignOp(token)) {
        // Mark the left-hand side as written as soon as we can.
        if (symbol* sym = left->val().sym) {
            markusage(sym, uWRITTEN);

            // If it's an outparam, also mark it as read.
            if (sym->vclass == sARGUMENT && (sym->ident == iREFERENCE || sym->ident == iREFARRAY))
                markusage(sym, uREAD);

            // Update the line number as a hack so we can warn that it was never
            // used.
            sym->lnumber = expr->pos().line;
        } else if (auto* accessor = left->val().accessor()) {
            if (!accessor->setter) {
                report(expr, 152) << accessor->name;
                return false;
            }
            markusage(accessor->setter, uREAD);
            if (accessor->getter && token != '=')
                markusage(accessor->getter, uREAD);
        }

        if (!CheckAssignmentLHS(expr))
            return false;
    } else if (left->lvalue()) {
        left = expr->set_left(new RvalueExpr(left));
    }

    // RHS is always loaded. Note we do this after validating the left-hand side,
    // so ValidateAssignment has an original view of RHS.
    if (right->lvalue())
        right = expr->set_right(new RvalueExpr(right));

    const auto& left_val = left->val();
    const auto& right_val = right->val();

    auto oper_tok = expr->oper();
    if (oper_tok) {
        assert(token != '=');

        if (left_val.ident == iARRAY || left_val.ident == iREFARRAY) {
            const char* ptr = (left_val.sym != nullptr) ? left_val.sym->name() : "-unknown-";
            report(expr, 33) << ptr; /* array must be indexed */
            return false;
        }
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            const char* ptr = (right_val.sym != nullptr) ? right_val.sym->name() : "-unknown-";
            report(expr, 33) << ptr; /* array must be indexed */
            return false;
        }
        /* ??? ^^^ should do same kind of error checking with functions */
    }

    // The assignment operator is overloaded separately.
    if (IsAssignOp(token)) {
        if (!CheckAssignmentRHS(expr))
            return false;
    }

    auto& val = expr->val();
    val.ident = iEXPRESSION;
    val.tag = left_val.tag;

    auto& assignop = expr->assignop();
    if (assignop.sym)
        val.tag = assignop.sym->tag;

    if (oper_tok) {
        auto& userop = expr->userop();
        if (find_userop(*sc_, oper_tok, left_val.tag, right_val.tag, 2, nullptr, &userop)) {
            val.tag = userop.sym->tag;
        } else if (left_val.ident == iCONSTEXPR && right_val.ident == iCONSTEXPR) {
            char boolresult = FALSE;
            matchtag(left_val.tag, right_val.tag, FALSE);
            val.ident = iCONSTEXPR;
            val.set_constval(calc(left_val.constval(), oper_tok, right_val.constval(),
                                  &boolresult));
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_val.tag, right_val.tag, MATCHTAG_DEDUCE);
        }

        if (IsChainedOp(token) || token == tlEQ || token == tlNE)
            val.tag = types_->tag_bool();
    }

    return true;
}

bool Semantics::CheckAssignmentLHS(BinaryExpr* expr) {
    auto left = expr->left();
    int left_ident = left->val().ident;
    if (left_ident == iARRAYCHAR) {
        // This is a special case, assigned to a packed character in a cell
        // is permitted.
        return true;
    }

    int oper_tok = expr->oper();
    if (left_ident == iARRAY || left_ident == iREFARRAY) {
        // array assignment is permitted too (with restrictions)
        if (oper_tok) {
            report(expr, 23);
            return false;
        }
        symbol* left_sym = left->val().sym;
        if (!left_sym) {
            report(expr, 142);
            return false;
        }

        for (int i = 0; i < left_sym->dim_count(); i++) {
            if (!left_sym->dim(i)) {
                report(expr, 46) << left_sym->name();
                return false;
            }
        }
        return true;
    }
    if (!left->lvalue()) {
        report(expr, 22);
        return false;
    }

    const auto& left_val = left->val();
    assert(left_val.sym || left_val.accessor());

    // may not change "constant" parameters
    if (!expr->initializer() && left_val.sym && left_val.sym->is_const) {
        report(expr, 22);
        return false;
    }
    return true;
}

bool Semantics::CheckAssignmentRHS(BinaryExpr* expr) {
    auto left = expr->left();
    auto right = expr->right();
    const auto& left_val = left->val();
    const auto& right_val = right->val();

    if (left_val.ident == iVARIABLE) {
        const auto& right_val = right->val();
        if (right_val.ident == iVARIABLE && right_val.sym == left_val.sym && !expr->oper())
            report(expr, 226) << left_val.sym->name(); // self-assignment
    }

    if (left_val.ident == iARRAY || left_val.ident == iREFARRAY) {
        if (right_val.ident != iARRAY && right_val.ident != iREFARRAY) {
            report(expr, 47);
            return false;
        }

        bool exact_match = true;
        cell right_length = 0;
        int right_idxtag = 0;
        int left_length = left_val.array_size();
        if (right_val.sym) {
            // Change from the old logic - we immediately reject multi-dimensional
            // arrays in assignment and don't bother validating subarray assignment.
            if (right_val.sym->dim_count() - right_val.array_level() > 1) {
                report(expr, 23);
                return false;
            }

            right_length = right_val.array_size();
            right_idxtag = right_val.sym->semantic_tag;
            if (right_idxtag == 0 && left_val.sym->semantic_tag == 0)
                exact_match = false;
        } else {
            right_length = right_val.array_size();

            if (right_val.tag == types_->tag_string()) {
                if (left_val.sym->semantic_tag == 0)
                    exact_match = false;
            }
        }
        if (left_val.array_dim_count() != 1) {
            report(expr, 47); // array dimensions must match
            return false;
        }
        if (left_length < right_length || (exact_match && left_length > right_length) ||
            right_length == 0)
        {
            report(expr, 47); // array sizes must match
            return false;
        }
        if (left_val.ident != iARRAYCELL &&
            !matchtag(left_val.sym->semantic_tag, right_idxtag, MATCHTAG_COERCE | MATCHTAG_SILENT))
        {
            report(expr, 229) << (right_val.sym ? right_val.sym->name() : left_val.sym->name());
        }

        expr->set_array_copy_length(right_length);
        if (left_val.sym->tag == types_->tag_string())
            expr->set_array_copy_length(char_array_cells(expr->array_copy_length()));
    } else {
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            report(expr, 6); // must be assigned to an array
            return false;
        }

        // Userop tag will be propagated by the caller.
        find_userop(*sc_, 0, right_val.tag, left_val.tag, 2, &left_val, &expr->assignop());
    }

    if (!expr->oper() &&
        !checkval_string(&left_val, &right_val) &&
        !expr->assignop().sym)
    {
        if ((left_val.ident == iARRAY || left_val.ident == iREFARRAY) &&
            ((left_val.tag == types_->tag_string() && right_val.tag != types_->tag_string()) ||
             (left_val.tag != types_->tag_string() && right_val.tag == types_->tag_string())))
        {
            report(expr, 179) << type_to_name(left_val.tag) << type_to_name(right_val.tag);
            return false;
        }
        matchtag(left_val.tag, right_val.tag, TRUE);
    }
    return true;
}

static inline bool
IsTypeBinaryConstantFoldable(Type* type)
{
    if (type->isEnum() || type->tagid() == 0)
        return true;
    return false;
}

bool
BinaryExpr::FoldToConstant()
{
    cell left_val, right_val;
    int left_tag, right_tag;

    if (!left_->EvalConst(&left_val, &left_tag) || !right_->EvalConst(&right_val, &right_tag))
        return false;
    if (IsAssignOp(token_) || userop_.sym)
        return false;

    auto types = CompileContext::get().types();
    Type* left_type = types->find(left_tag);
    Type* right_type = types->find(right_tag);
    if (!IsTypeBinaryConstantFoldable(left_type) || !IsTypeBinaryConstantFoldable(right_type))
        return false;

    switch (token_) {
        case '*':
            val_.set_constval(left_val * right_val);
            break;
        case '/':
        case '%':
            if (!right_val) {
                error(pos_, 93);
                return false;
            }
            if (left_val == cell(0x80000000) && right_val == -1) {
                error(pos_, 97);
                return false;
            }
            if (token_ == '/')
                val_.set_constval(left_val / right_val);
            else
                val_.set_constval(left_val % right_val);
            break;
        case '+':
            val_.set_constval(left_val + right_val);
            break;
        case '-':
            val_.set_constval(left_val - right_val);
            break;
        case tSHL:
            val_.set_constval(left_val << right_val);
            break;
        case tSHR:
            val_.set_constval(left_val >> right_val);
            break;
        case tSHRU:
            val_.set_constval(uint32_t(left_val) >> uint32_t(right_val));
            break;
        case '&':
            val_.set_constval(left_val & right_val);
            break;
        case '^':
            val_.set_constval(left_val ^ right_val);
            break;
        case '|':
            val_.set_constval(left_val | right_val);
            break;
        default:
            return false;
    }
    return true;
}

bool Semantics::CheckLogicalExpr(LogicalExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto left = expr->left();
    auto right = expr->right();
    if (!CheckExpr(left) || !CheckExpr(right))
        return false;

    if (left->lvalue())
        left = expr->set_left(new RvalueExpr(left));
    if (right->lvalue())
        right = expr->set_right(new RvalueExpr(right));

    const auto& left_val = left->val();
    const auto& right_val = right->val();
    auto& val = expr->val();
    if (left_val.ident == iCONSTEXPR && right_val.ident == iCONSTEXPR) {
        val.ident = iCONSTEXPR;
        if (expr->token() == tlOR)
            val.set_constval((left_val.constval() || right_val.constval()));
        else if (expr->token() == tlAND)
            val.set_constval((left_val.constval() && right_val.constval()));
        else
            assert(false);
    } else {
        val.ident = iEXPRESSION;
    }
    val.sym = nullptr;
    val.tag = types_->tag_bool();
    return true;
}

bool Semantics::CheckChainedCompareExpr(ChainedCompareExpr* chain) {
    auto first = chain->first();
    if (!CheckExpr(first))
        return false;
    if (first->lvalue())
        first = chain->set_first(new RvalueExpr(first));

    for (auto& op : chain->ops()) {
        if (!CheckExpr(op.expr))
            return false;
        if (op.expr->lvalue())
            op.expr = new RvalueExpr(op.expr);
    }

    Expr* left = first;
    bool all_const = (left->val().ident == iCONSTEXPR);
    bool constval = true;

    auto& val = chain->val();
    val.ident = iEXPRESSION;
    val.tag = types_->tag_bool();

    for (auto& op : chain->ops()) {
        Expr* right = op.expr;
        const auto& left_val = left->val();
        const auto& right_val = right->val();

        if (left_val.ident == iARRAY || left_val.ident == iREFARRAY) {
            const char* ptr = (left_val.sym != nullptr) ? left_val.sym->name() : "-unknown-";
            report(left, 33) << ptr; /* array must be indexed */
            return false;
        }
        if (right_val.ident == iARRAY || right_val.ident == iREFARRAY) {
            const char* ptr = (right_val.sym != nullptr) ? right_val.sym->name() : "-unknown-";
            report(right, 33) << ptr; /* array must be indexed */
            return false;
        }

        if (find_userop(*sc_, op.oper_tok, left_val.tag, right_val.tag, 2, nullptr, &op.userop)) {
            if (op.userop.sym->tag != types_->tag_bool()) {
                report(op.pos, 51) << get_token_string(op.token);
                return false;
            }
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_val.tag, right_val.tag, MATCHTAG_DEDUCE);
        }

        if (right_val.ident != iCONSTEXPR || op.userop.sym)
            all_const = false;

        // Fold constants as we go.
        if (all_const) {
            switch (op.token) {
                case tlLE:
                    constval &= left_val.constval() <= right_val.constval();
                    break;
                case tlGE:
                    constval &= left_val.constval() >= right_val.constval();
                    break;
                case '>':
                    constval &= left_val.constval() > right_val.constval();
                    break;
                case '<':
                    constval &= left_val.constval() < right_val.constval();
                    break;
                default:
                    assert(false);
                    break;
            }
        }

        left = right;
    }

    if (all_const)
        val.set_constval(constval ? 1 : 0);
    return true;
}

void
ChainedCompareExpr::ProcessUses(SemaContext& sc)
{
    first_->MarkAndProcessUses(sc);
    for (const auto& op : ops_)
        op.expr->MarkAndProcessUses(sc);
}

bool Semantics::CheckTernaryExpr(TernaryExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto first = expr->first();
    auto second = expr->second();
    auto third = expr->third();

    if (!CheckExpr(first) || !CheckExpr(second) || !CheckExpr(third))
        return false;

    if (first->lvalue()) {
        first = expr->set_first(new RvalueExpr(first));
    } else if (first->val().ident == iCONSTEXPR) {
        report(first, first->val().constval() ? 206 : 205);
    }

    if (second->lvalue())
        second = expr->set_second(new RvalueExpr(second));
    if (third->lvalue())
        third = expr->set_third(new RvalueExpr(third));

    const auto& left = second->val();
    const auto& right = third->val();
    bool left_array = (left.ident == iARRAY || right.ident == iREFARRAY);
    bool right_array = (left.ident == iARRAY || right.ident == iREFARRAY);
    if (!left_array && right_array) {
        const char* ptr = "-unknown-";
        if (left.sym != nullptr)
            ptr = left.sym->name();
        report(expr, 33) << ptr; /* array must be indexed */
        return false;
    } else if (left_array && !right_array) {
        const char* ptr = "-unknown-";
        if (right.sym != nullptr)
            ptr = right.sym->name();
        report(expr, 33) << ptr; /* array must be indexed */
        return false;
    }

    matchtag_commutative(left.tag, right.tag, FALSE);

    /* If both sides are arrays, we should return the maximal as the lvalue.
     * Otherwise we could buffer overflow and the compiler is too stupid.
     * Literal strings have a constval == -(num_cells) so the cmp is flipped.
     */
    auto& val = expr->val();
    val = left;
    if (val.ident == iARRAY && right.ident == iARRAY && right.array_size() > val.array_size())
        val = right;

    if (val.ident == iARRAY)
        val.ident = iREFARRAY;
    else if (val.ident != iREFARRAY)
        val.ident = iEXPRESSION;
    return true;
}

bool
TernaryExpr::FoldToConstant()
{
    cell cond, left, right;
    if (!first_->EvalConst(&cond, nullptr) || second_->EvalConst(&left, nullptr) ||
        !third_->EvalConst(&right, nullptr))
    {
        return false;
    }

    val_.set_constval(cond ? left : right);
    return true;
}

void
TernaryExpr::ProcessUses(SemaContext& sc)
{
    first_->MarkAndProcessUses(sc);
    second_->MarkAndProcessUses(sc);
    third_->MarkAndProcessUses(sc);
}

void
TernaryExpr::ProcessDiscardUses(SemaContext& sc)
{
    first_->MarkAndProcessUses(sc);
    second_->ProcessUses(sc);
    third_->ProcessUses(sc);
}

bool Semantics::CheckCastExpr(CastExpr* expr) {
    AutoErrorPos aep(expr->pos());

    const auto& type = expr->type();
    if (type.tag() == types_->tag_void()) {
        report(expr, 144);
        return false;
    }

    if (!CheckExpr(expr->expr()))
        return false;

    auto& out_val = expr->val();

    out_val = expr->expr()->val();
    expr->set_lvalue(expr->expr()->lvalue());

    Type* ltype = types_->find(out_val.tag);
    Type* atype = types_->find(type.tag());
    if (ltype->isObject() || atype->isObject()) {
        matchtag(type.tag(), out_val.tag, MATCHTAG_COERCE);
    } else if (ltype->isFunction() != atype->isFunction()) {
        // Warn: unsupported cast.
        report(expr, 237);
    } else if (ltype->isFunction() && atype->isFunction()) {
        matchtag(type.tag(), out_val.tag, MATCHTAG_COERCE);
    } else if (out_val.sym && out_val.sym->tag == types_->tag_void()) {
        report(expr, 89);
    } else if (atype->isEnumStruct()) {
        report(expr, 95) << atype->name();
    }
    out_val.tag = type.tag();
    return true;
}

void
CastExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

void
SymbolExpr::MarkUsed(SemaContext& sc)
{
    markusage(sym_, uREAD);
}

// This is a hack. Most code is not prepared to handle iMETHODMAP in type
// checks, so for now, we forbid it by default. Since the '.' operator *is*
// prepared for this, we have a special analysis option to allow returning
// types as values.
bool Semantics::CheckSymbolExpr(SymbolExpr* expr, bool allow_types) {
    AutoErrorPos aep(expr->pos());

    auto sym = expr->sym();
    if (!sym) {
        // This can happen if CheckSymbolExpr is called during name resolution.
        assert(cc_.reports()->total_errors() > 0);
        return false;
    }

    auto& val = expr->val();
    val.ident = sym->ident;
    val.sym = sym;

    // Don't expose the tag of old enumroots.
    Type* type = types_->find(sym->tag);
    if (sym->enumroot && !type->asEnumStruct() && sym->ident == iCONSTEXPR) {
        val.tag = 0;
        report(expr, 174) << sym->name();
    } else {
        val.tag = sym->tag;
    }

    if (sym->ident == iCONSTEXPR)
        val.set_constval(sym->addr());

    if (sym->vclass == sGLOBAL && sym->ident != iFUNCTN) {
        if (!sym->defined) {
            report(expr, 17) << sym->name();
            return false;
        }
    }
    if (sym->ident == iFUNCTN) {
        if (sym->native) {
            report(expr, 76);
            return false;
        }
        if (sym->array_return()) {
            report(expr, 182);
            return false;
        }
        if (!sym->defined) {
            report(expr, 4) << sym->name();
            return false;
        }

        funcenum_t* fe = funcenum_for_symbol(cc_, sym);

        // New-style "closure".
        val.ident = iEXPRESSION;
        val.tag = fe->tag;

        // Mark as being indirectly invoked. Direct invocations go through
        // BindCallTarget.
        sym->callback = true;
    }

    switch (sym->ident) {
        case iVARIABLE:
        case iREFERENCE:
            expr->set_lvalue(true);
            break;
        case iARRAY:
        case iREFARRAY:
        case iFUNCTN:
        case iCONSTEXPR:
            // Not an l-value.
            break;
        case iMETHODMAP:
        case iENUMSTRUCT:
            if (!allow_types) {
                report(expr, 174) << sym->name();
                return false;
            }
            break;
        default:
            // Should not be a symbol.
            assert(false);
    }
    return true;
}

bool Semantics::CheckCommaExpr(CommaExpr* comma) {
    AutoErrorPos aep(comma->pos());

    for (const auto& expr : comma->exprs()) {
        if (!CheckExpr(expr))
            return false;
    }

    Expr* last = comma->exprs().back();
    if (comma->exprs().size() > 1 && last->lvalue()) {
        last = new RvalueExpr(last);
        comma->exprs().back() = last;
    }

    comma->val() = last->val();
    comma->set_lvalue(last->lvalue());

    // Don't propagate a constant if it would cause Emit() to shortcut and not
    // emit other expressions.
    if (comma->exprs().size() > 1 && comma->val().ident == iCONSTEXPR)
        comma->val().ident = iEXPRESSION;
    return true;
}

void
CommaExpr::ProcessUses(SemaContext& sc)
{
    for (const auto& expr : exprs_)
        expr->ProcessUses(sc);
    exprs_.back()->MarkUsed(sc);
}

void
CommaExpr::ProcessDiscardUses(SemaContext& sc)
{
    for (const auto& expr : exprs_)
        expr->ProcessUses(sc);
}

bool Semantics::CheckArrayExpr(ArrayExpr* array) {
    AutoErrorPos aep(array->pos());

    int lasttag = -1;
    for (const auto& expr : array->exprs()) {
        if (!CheckExpr(expr))
            return false;

        const auto& val = expr->val();
        if (val.ident != iCONSTEXPR) {
            report(expr, 8);
            return false;
        }
        if (lasttag < 0)
            lasttag = val.tag;
        else
            matchtag(lasttag, val.tag, FALSE);
    }

    auto& val = array->val();
    val.set_array(iARRAY, array->exprs().size());
    val.tag = lasttag;
    return true;
}

bool Semantics::CheckIndexExpr(IndexExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto base = expr->base();
    auto index = expr->index();
    if (!CheckExpr(base) || !CheckExpr(index))
        return false;
    if (base->lvalue() && base->val().ident == iACCESSOR)
        base = expr->set_base(new RvalueExpr(base));
    if (index->lvalue())
        index = expr->set_index(new RvalueExpr(index));

    const auto& base_val = base->val();
    if (!base_val.sym) {
        report(base, 29);
        return false;
    }
    if (base_val.sym->ident != iARRAY && base_val.sym->ident != iREFARRAY) {
        report(base, 28) << base_val.sym->name();
        return false;
    }

    if (base_val.sym->enumroot) {
        if (!matchtag(base_val.sym->semantic_tag, index->val().tag, TRUE))
            return false;
    }

    const auto& index_val = index->val();
    if (index_val.ident == iARRAY || index_val.ident == iREFARRAY) {
        report(index, 33) << (index_val.sym ? index_val.sym->name() : "-unknown-"); /* array must be indexed */
        return false;
    }

    if (base_val.array_dim_count() == 1 &&
        types_->find(base_val.sym->semantic_tag)->isEnumStruct())
    {
        report(base, 117);
        return false;
    }

    int idx_tag = index->val().tag;
    if (!is_valid_index_tag(idx_tag)) {
        report(index, 77) << types_->find(idx_tag)->prettyName();
        return false;
    }

    auto& out_val = expr->val();
    out_val = base_val;

    if (index_val.ident == iCONSTEXPR) {
        if (!(base_val.sym->tag == types_->tag_string() && base_val.array_level() == 0)) {
            /* normal array index */
            if (index_val.constval() < 0 ||
                (base_val.array_size() != 0 &&
                 base_val.array_size() <= index_val.constval()))
            {
                report(index, 32) << base_val.sym->name(); /* array index out of bounds */
                return false;
            }
        } else {
            /* character index */
            if (index_val.constval() < 0 ||
                (base_val.array_size() != 0 &&
                 base_val.array_size() <= index_val.constval()))
            {
                report(index, 32) << base_val.sym->name(); /* array index out of bounds */
                return false;
            }
        }
    }

    if (base_val.array_level() < base_val.sym->dim_count() - 1) {
        // Note: Intermediate arrays are not l-values.
        out_val.set_array(iREFARRAY, base_val.sym, base_val.array_level() + 1);
        return true;
    }

    /* set type to fetch... INDIRECTLY */
    if (base_val.sym->tag == types_->tag_string())
        out_val.set_array(iARRAYCHAR, base_val.sym, 0);
    else
        out_val.set_array(iARRAYCELL, base_val.sym, 0);
    out_val.tag = base_val.sym->tag;

    expr->set_lvalue(true);
    return true;
}

void
IndexExpr::ProcessUses(SemaContext& sc)
{
    base_->MarkAndProcessUses(sc);
    expr_->MarkAndProcessUses(sc);
}

bool Semantics::CheckThisExpr(ThisExpr* expr) {
    auto sym = expr->sym();
    assert(sym->ident == iREFARRAY || sym->ident == iVARIABLE);

    auto& val = expr->val();
    val.ident = sym->ident;
    val.sym = sym;
    val.tag = sym->tag;
    expr->set_lvalue(sym->ident != iREFARRAY);
    return true;
}

bool Semantics::CheckNullExpr(NullExpr* expr) {
    auto& val = expr->val();
    val.set_constval(0);
    val.tag = types_->tag_null();
    return true;
}

bool Semantics::CheckTaggedValueExpr(TaggedValueExpr* expr) {
    auto& val = expr->val();
    val.tag = expr->tag();
    val.set_constval(expr->value());
    return true;
}

bool Semantics::CheckStringExpr(StringExpr* expr) {
    auto& val = expr->val();
    val.set_array(iARRAY, (cell)expr->text()->length() + 1);
    val.tag = types_->tag_string();
    return true;
}

bool Semantics::CheckFieldAccessExpr(FieldAccessExpr* expr, bool from_call) {
    AutoErrorPos aep(expr->pos());

    auto base = expr->base();
    if (auto sym_expr = base->as<SymbolExpr>()) {
        if (!CheckSymbolExpr(sym_expr, true))
            return false;
    } else {
        if (!CheckExpr(base))
            return false;
    }

    int token = expr->token();
    if (token == tDBLCOLON)
        return CheckStaticFieldAccessExpr(expr);

    const auto& base_val = base->val();
    switch (base_val.ident) {
        case iARRAY:
        case iREFARRAY:
            if (base_val.sym && base_val.array_dim_count() == 1) {
                Type* type = types_->find(base_val.sym->semantic_tag);
                if (symbol* root = type->asEnumStruct())
                    return CheckEnumStructFieldAccessExpr(expr, type, root, from_call);
            }
            report(expr, 96) << expr->name() << "type" << "array";
            return false;
        case iFUNCTN:
            report(expr, 107);
            return false;
    }

    auto& val = expr->val();
    if (base_val.ident == iMETHODMAP && base_val.sym->data()) {
        methodmap_t* map = base_val.sym->data()->asMethodmap();
        if (map)
            expr->set_method(methodmap_find_method(map, expr->name()));

        auto method = expr->method();
        if (!method) {
            report(expr, 105) << base_val.sym->name() << expr->name();
            return false;
        }
        if (!method->is_static) {
            report(expr, 176) << method->name << map->name;
            return false;
        }
        val.ident = iFUNCTN;
        val.sym = method->target;
        markusage(method->target, uREAD);
        return true;
    }

    Type* base_type = types_->find(base_val.tag);
    methodmap_t* map = base_type->asMethodmap();
    if (!map) {
        report(expr, 104) << type_to_name(base_val.tag);
        return false;
    }

    expr->set_method(methodmap_find_method(map, expr->name()));
    auto method = expr->method();
    if (!method) {
        report(expr, 105) << map->name << expr->name();
        return false;
    }

    if (method->getter || method->setter) {
        // This is the only scenario in which we need to compute a load of the
        // base address. Otherwise, we're only accessing the type.
        if (base->lvalue())
            base = expr->set_base(new RvalueExpr(base));
        val.tag = method->property_tag();
        val.set_accessor(method);
        expr->set_lvalue(true);
        return true;
    }

    if (method->is_static) {
        report(expr, 177) << method->name << map->name << method->name;
        return false;
    }

    if (!from_call) {
        report(expr, 50);
        return false;
    }

    val.ident = iFUNCTN;
    val.sym = method->target;
    markusage(method->target, uREAD);
    return true;
}

void
FieldAccessExpr::ProcessUses(SemaContext& sc)
{
    base_->MarkAndProcessUses(sc);
}

symbol* Semantics::BindCallTarget(CallExpr* call, Expr* target) {
    AutoErrorPos aep(target->pos());

    switch (target->kind()) {
        case ExprKind::FieldAccessExpr: {
            auto expr = target->to<FieldAccessExpr>();
            if (!CheckFieldAccessExpr(expr, true))
                return nullptr;

            auto& val = expr->val();
            if (val.ident != iFUNCTN) {
                report(target, 12);
                return nullptr;
            }

            // The static accessor (::) is offsetof(), so it can't return functions.
            assert(expr->token() == '.');

            auto method = expr->method();
            if (method && method->parent->ctor == method) {
                report(call, 84) << method->parent->name;
                return nullptr;
            }

            auto base = expr->base();
            if (base->lvalue())
                base = expr->set_base(new RvalueExpr(base));
            if (expr->field() || !method->is_static)
                call->set_implicit_this(base);
            return val.sym;
        }
        case ExprKind::SymbolExpr: {
            call->set_implicit_this(nullptr);

            auto expr = target->to<SymbolExpr>();
            auto sym = expr->sym();
            if (call->token() != tNEW && sym->ident == iMETHODMAP && sym->data()) {
                auto map = sym->data()->asMethodmap();
                if (!map->ctor) {
                    // Immediately fatal - no function to call.
                    report(target, 172) << sym->name();
                    return nullptr;
                }
                if (map->must_construct_with_new()) {
                    // Keep going, this is basically a style thing.
                    report(target, 170) << map->name;
                    return nullptr;
                }
                return map->ctor->target;
            }
            if (sym->ident != iFUNCTN) {
                report(target, 12);
                return nullptr;
            }
            if (!sym->defined) {
                report(target, 4) << sym->name();
                return nullptr;
            }
            return sym;
        }
        default:
            report(target, 12);
            return nullptr;
    }
}

symbol* Semantics::BindNewTarget(Expr* target) {
    AutoErrorPos aep(target->pos());

    switch (target->kind()) {
        case ExprKind::SymbolExpr: {
            auto expr = target->to<SymbolExpr>();
            auto sym = expr->sym();

            if (sym->ident != iMETHODMAP) {
                report(expr, 116) << sym->name();
                return nullptr;
            }

            methodmap_t* methodmap = sym->data()->asMethodmap();
            if (!methodmap->must_construct_with_new()) {
                report(expr, 171) << methodmap->name;
                return nullptr;
            }
            if (!methodmap->ctor) {
                report(expr, 172) << methodmap->name;
                return nullptr;
            }
            return methodmap->ctor->target;
        }
    }
    return nullptr;
}

bool Semantics::CheckEnumStructFieldAccessExpr(FieldAccessExpr* expr, Type* type, symbol* root,
                                               bool from_call)
{
    auto base = expr->base();

    // Enum structs are always arrays, so they're never l-values.
    assert(!base->lvalue());

    expr->set_field(FindEnumStructField(type, expr->name()));

    auto field = expr->field();
    if (!field) {
        report(expr, 105) << type->name() << expr->name();
        return false;
    }

    auto& val = expr->val();
    if (field->ident == iFUNCTN) {
        if (!from_call) {
            report(expr, 76);
            return false;
        }

        val.ident = iFUNCTN;
        val.sym = field;
        markusage(val.sym, uREAD);
        return true;
    }

    int tag = field->tag;

    symbol* var = base->val().sym;
    if (!var->data())
        var->set_data(new EnumStructVarData());

    EnumStructVarData* es_var = var->data()->asEnumStructVar();
    es_var->children.emplace_back(new symbol(*field));

    symbol* child = es_var->children.back();
    child->setName(expr->name());
    child->vclass = var->vclass;

    if (types_->find(tag)->isEnumStruct()) {
        val.tag = 0;
        child->tag = 0;
        child->semantic_tag = tag;
    } else {
        val.tag = tag;
        child->tag = tag;
        child->semantic_tag = 0;
    }

    if (field->dim_count()) {
        child->set_dim_count(1);
        child->set_dim(0, field->dim(0));
        child->ident = iREFARRAY;
    } else {
        child->ident = (tag == types_->tag_string()) ? iARRAYCHAR : iARRAYCELL;
        expr->set_lvalue(true);
    }
    val.ident = child->ident;
    val.sym = child;
    return true;
}

bool Semantics::CheckStaticFieldAccessExpr(FieldAccessExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto base = expr->base();
    const auto& base_val = base->val();
    if (base_val.ident != iENUMSTRUCT) {
        report(expr, 108);
        return false;
    }

    Type* type = types_->find(base_val.tag);
    symbol* field = FindEnumStructField(type, expr->name());
    if (!field) {
        report(expr, 105) << type->name() << expr->name();
        return FALSE;
    }

    auto& val = expr->val();
    val.set_constval(field->addr());
    val.tag = 0;
    return true;
}

bool Semantics::CheckSizeofExpr(SizeofExpr* expr) {
    AutoErrorPos aep(expr->pos());

    symbol* sym = expr->sym();

    markusage(sym, uREAD);

    if (sym->ident == iCONSTEXPR) {
        report(expr, 39); // constant symbol has no size
        return false;
    } else if (sym->ident == iFUNCTN) {
        report(expr, 72); // "function" symbol has no size
        return false;
    } else if (!sym->defined) {
        report(expr, 17) << expr->ident();
        return false;
    }

    auto& val = expr->val();
    val.set_constval(1);

    if (sym->ident == iARRAY || sym->ident == iREFARRAY || sym->ident == iENUMSTRUCT) {
        bool is_enum_struct = types_->find(sym->semantic_tag)->isEnumStruct();
        for (int level = 0; level < expr->array_levels(); level++) {
            // Forbid index operations on enum structs.
            if (sym->ident == iENUMSTRUCT || (level == sym->dim_count() - 1 && is_enum_struct)) {
                report(expr, 111) << sym->name();
                return false;
            }
        }

        Type* enum_type = nullptr;
        if (expr->suffix_token() == tDBLCOLON) {
            if (sym->ident != iENUMSTRUCT) {
                report(expr, 112) << sym->name();
                return false;
            }
            enum_type = types_->find(sym->tag);
        } else if (expr->suffix_token() == '.') {
            enum_type = types_->find(sym->semantic_tag);
            if (!enum_type->asEnumStruct()) {
                report(expr, 116) << sym->name();
                return false;
            }
        }

        if (enum_type) {
            assert(enum_type->asEnumStruct());

            symbol* field = FindEnumStructField(enum_type, expr->field());
            if (!field) {
                report(expr, 105) << enum_type->name() << expr->field();
                return false;
            }
            if (field->dim_count()) {
                val.set_constval(field->dim(0));
                return true;
            }
            return true;
        }

        if (sym->ident == iENUMSTRUCT) {
            val.set_constval(sym->addr());
            return true;
        }

        if (expr->array_levels() > sym->dim_count()) {
            report(expr, 28) << sym->name(); // invalid subscript
            return false;
        }
        if (expr->array_levels() != sym->dim_count()) {
            int size = sym->dim(expr->array_levels());

            if (!size) {
                report(expr, 163) << sym->name(); // indeterminate array size in "sizeof"
                return false;
            }
            val.set_constval(size);
            return true;
        }
    }
    return true;
}

CallUserOpExpr::CallUserOpExpr(const UserOperation& userop, Expr* expr)
  : EmitOnlyExpr(ExprKind::CallUserOpExpr, expr->pos()),
    userop_(userop),
    expr_(expr)
{
    val_.ident = iEXPRESSION;
    val_.tag = userop_.sym->tag;
}

void
CallUserOpExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

DefaultArgExpr::DefaultArgExpr(const token_pos_t& pos, ArgDecl* arg)
  : Expr(ExprKind::DefaultArgExpr, pos),
    arg_(arg)
{
    // Leave val bogus, it doesn't participate in anything, and we can't
    // accurately construct it.
}

bool Semantics::CheckCallExpr(CallExpr* call) {
    AutoErrorPos aep(call->pos());

    // Note: we do not Analyze the call target. We leave this to the
    // implementation of BindCallTarget.
    symbol* sym;
    if (call->token() == tNEW)
        sym = BindNewTarget(call->target());
    else
        sym = BindCallTarget(call, call->target());
    if (!sym)
        return false;

    call->set_sym(sym);

    auto fun = sym->function()->node;
    if (fun &&
        (fun->decl().type.numdim() > 0 || fun->maybe_returns_array()) &&
        !sym->array_return())
    {
        // We need to know the size of the returned array. Recursively analyze
        // the function.
        if (fun->is_analyzing() || !CheckFunctionDecl(fun)) {
            report(call, 411);
            return false;
        }
    }

    markusage(sym, uREAD);

    auto& val = call->val();
    val.ident = iEXPRESSION;
    val.tag = sym->tag;
    if (sym->array_return()) {
        val.ident = iREFARRAY;
        val.sym = sym->array_return();
        NeedsHeapAlloc(call);
    }

    if (sym->deprecated) {
        const char* ptr = sym->documentation->chars();
        report(call, 234) << sym->name() << ptr; /* deprecated (probably a native function) */
    }

    ParamState ps;

    unsigned int nargs = 0;
    unsigned int argidx = 0;
    auto& arglist = sym->function()->node->args();
    if (call->implicit_this()) {
        if (arglist.empty()) {
            report(call->implicit_this(), 92);
            return false;
        }
        if (!CheckArgument(call, arglist[0], call->implicit_this(), &ps, 0))
            return false;
        nargs++;
        argidx++;
    }

    bool namedparams = false;
    for (const auto& param : call->args()) {
        unsigned int argpos;
        if (auto named = param->as<NamedArgExpr>()) {
            int pos = sym->function()->node->FindNamedArg(named->name);
            if (pos < 0) {
                report(call, 17) << named->name;
                break;
            }
            argpos = pos;
            argidx = pos;
        } else {
            if (namedparams) {
                report(call, 44); // positional parameters must precede named parameters
                return false;
            }
            argpos = nargs;
            if (argidx >= arglist.size()) {
                report(param->pos(), 92);
                return false;
            }
        }

        if (argpos >= SP_MAX_CALL_ARGUMENTS) {
            report(call, 45); // too many function arguments
            return false;
        }
        if (argpos < ps.argv.size() && ps.argv[argpos]) {
            report(call, 58); // argument already set
            return false;
        }

        // Add the argument to |argv| and perform type checks.
        if (!CheckArgument(call, arglist[argidx], param, &ps, argpos))
            return false;

        assert(ps.argv[argpos] != nullptr);
        nargs++;

        // Don't iterate past terminators (0 or varargs).
        switch (arglist[argidx]->type().ident) {
            case iVARARGS:
                break;
            default:
                argidx++;
                break;
        }
    }

    if (!sc_->func()) {
        report(call, 10);
        return false;
    }

    // Check for missing or invalid extra arguments, and fill in default
    // arguments.
    for (unsigned int argidx = 0; argidx < arglist.size(); argidx++) {
        auto arg = arglist[argidx];
        if (arg->type().ident == iVARARGS)
            break;
        if (argidx >= ps.argv.size() || !ps.argv[argidx]) {
            if (!CheckArgument(call, arg, nullptr, &ps, argidx))
                return false;
        }

        Expr* expr = ps.argv[argidx];
        if (expr->as<DefaultArgExpr>() && arg->type().ident == iVARIABLE) {
            UserOperation userop;
            if (find_userop(*sc_, 0, arg->default_value()->tag, arg->type().tag(), 2, nullptr,
                            &userop))
            {
                ps.argv[argidx] = new CallUserOpExpr(userop, expr);
            }
        }
    }

    // Copy newly deduced argument information.
    if (call->args().size() == ps.argv.size()) {
        for (size_t i = 0; i < ps.argv.size(); i++)
            call->args()[i] = ps.argv[i];
    } else {
        new (&call->args()) PoolArray<Expr*>(ps.argv);
    }
    return true;
}

bool Semantics::CheckArgument(CallExpr* call, ArgDecl* arg, Expr* param,
                              ParamState* ps, unsigned int pos)
{
    while (pos >= ps->argv.size())
        ps->argv.push_back(nullptr);

    unsigned int visual_pos = call->implicit_this() ? pos : pos + 1;

    if (!param || param->as<DefaultArgExpr>()) {
        assert(arg->type().ident != 0);
        if (arg->type().ident == iVARARGS) {
            report(call, 92); // argument count mismatch
            return false;
        }
        if (!arg->default_value()) {
            report(call, 34) << visual_pos; // argument has no default value
            return false;
        }

        if (!param)
            param = new DefaultArgExpr(call->pos(), arg);
        else
            param->as<DefaultArgExpr>()->set_arg(arg);

        // The rest of the code to handle default values is in DoEmit.
        ps->argv[pos] = param;

        if (arg->type().ident == iREFERENCE ||
            (arg->type().ident == iREFARRAY && !arg->type().is_const &&
             arg->default_value()->array))
        {
            NeedsHeapAlloc(ps->argv[pos]);
        }
        return true;
    }

    if (param != call->implicit_this()) {
        if (!CheckExpr(param))
            return false;
    }

    AutoErrorPos aep(param->pos());

    bool handling_this = call->implicit_this() && (pos == 0);

    if (param->val().ident == iACCESSOR) {
        // We must always compute r-values for accessors.
        if (!param->val().accessor()->getter) {
            report(param, 149) << param->val().accessor()->name;
            return false;
        }
        param = new RvalueExpr(param);
    }

    const auto* val = &param->val();
    bool lvalue = param->lvalue();
    switch (arg->type().ident) {
        case iVARARGS:
            assert(!handling_this);

            // Always pass by reference.
            if (val->ident == iVARIABLE || val->ident == iREFERENCE) {
                if (val->sym->is_const && !arg->type().is_const) {
                    // Treat a "const" variable passed to a function with a
                    // non-const "variable argument list" as a constant here.
                    if (!lvalue) {
                        report(param, 22); // need lvalue
                        return false;
                    }
                    NeedsHeapAlloc(param);
                } else if (!lvalue) {
                    NeedsHeapAlloc(param);
                }
            } else if (val->ident == iCONSTEXPR || val->ident == iEXPRESSION) {
                NeedsHeapAlloc(param);
            }
            if (!checktag_string(arg->type().tag(), val) && !checktag(arg->type().tag(), val->tag))
                report(param, 213) << type_to_name(arg->type().tag()) << type_to_name(val->tag);
            break;
        case iVARIABLE:
        {
            if (val->ident == iFUNCTN || val->ident == iARRAY || val->ident == iREFARRAY) {
                report(param, 35) << visual_pos; // argument type mismatch
                return false;
            }

            if (lvalue) {
                param = new RvalueExpr(param);
                val = &param->val();
            }

            // Do not allow user operators to transform |this|.
            UserOperation userop;
            if (!handling_this &&
                find_userop(*sc_, 0, val->tag, arg->type().tag(), 2, nullptr, &userop))
            {
                param = new CallUserOpExpr(userop, param);
                val = &param->val();
            }
            if (!checktag_string(arg->type().tag(), val))
                checktag(arg->type().tag(), val->tag);
            break;
        }
        case iREFERENCE:
            assert(!handling_this);

            if (!lvalue || val->ident == iARRAYCHAR) {
                report(param, 35) << visual_pos; // argument type mismatch
                return false;
            }
            if (val->sym && val->sym->is_const && !arg->type().is_const) {
                report(param, 35) << visual_pos; // argument type mismatch
                return false;
            }
            checktag(arg->type().tag(), val->tag);
            break;
        case iREFARRAY:
            if (val->ident != iARRAY && val->ident != iREFARRAY && val->ident != iARRAYCELL &&
                val->ident != iARRAYCHAR)
            {
                report(param, 35) << visual_pos; // argument type mismatch
                return false;
            }
            if (val->sym && val->sym->is_const && !arg->type().is_const) {
                report(param, 35) << visual_pos; // argument type mismatch
                return false;
            }
            // Verify that the dimensions match those in |arg|. A literal array
            // always has a single dimension. An iARRAYCELL parameter is also
            // assumed to have a single dimension.
            if (!val->sym) {
                assert(val->ident == iARRAY || val->ident == iREFARRAY);
                if (arg->type().numdim() != 1) {
                    report(param, 48); // array dimensions must match
                    return false;
                }
                if (arg->type().dim[0] != 0) {
                    assert(arg->type().dim[0] > 0);
                    if (val->array_size() == 0) {
                        report(param, 47);
                        return false;
                    }
                    if (arg->type().tag() == types_->tag_string()) {
                        if (arg->type().dim[0] < val->array_size()) {
                            report(param, 47); // array sizes must match
                            return false;
                        }
                    } else if (arg->type().dim[0] != val->array_size()) {
                        report(param, 47); // array sizes must match
                        return false;
                    }
                }
            } else {
                if (val->array_dim_count() != arg->type().numdim()) {
                    report(param, 48); // array dimensions must match
                    return false;
                }
                // The lengths for all dimensions must match, unless the dimension
                // length was defined at zero (which means "undefined").
                for (int i = 0; i < arg->type().numdim(); i++) {
                    if (arg->type().dim[i] != 0 &&
                        val->array_dim(i) != arg->type().dim[i])
                    {
                        report(param, 47); // array sizes must match
                        return false;
                    }
                }
                auto sym = val->sym;
                if (!matchtag(arg->type().enum_struct_tag(), sym->semantic_tag, MATCHTAG_SILENT)) {
                    // We allow enumstruct -> any[].
                    if (arg->type().tag() != types_->tag_any() ||
                        !types_->find(sym->semantic_tag)->asEnumStruct())
                    {
                        report(param, 229) << sym->name();
                    }
                }
            }

            checktag(arg->type().tag(), val->tag);
            if ((arg->type().tag() != types_->tag_string() && val->tag == types_->tag_string()) ||
                (arg->type().tag() == types_->tag_string() && val->tag != types_->tag_string()))
            {
                report(param, 178) << type_to_name(val->tag) << type_to_name(arg->type().tag());
                return false;
            }
            break;
        default:
            assert(false);
            break;
    }

    ps->argv[pos] = param;
    return true;
}

void
CallExpr::ProcessUses(SemaContext& sc)
{
    for (const auto& arg : args_)
        arg->MarkAndProcessUses(sc);
}

void
CallExpr::MarkUsed(SemaContext& sc)
{
    if (sym_)
        sym_->retvalue_used = true;
}

bool Semantics::CheckStaticAssertStmt(StaticAssertStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckExpr(expr))
        return false;

    // :TODO: insert coercion to bool.
    int tag;
    cell value;
    if (!expr->EvalConst(&value, &tag)) {
        report(expr, 8);
        return false;
    }

    if (value)
        return true;

    std::string message;
    if (stmt->text())
        message += ": " + std::string(stmt->text()->chars(), stmt->text()->length());

    report(expr, 70) << message;
    return false;
}

bool Semantics::CheckNewArrayExpr(NewArrayExpr* expr) {
    // We can't handle random refarrays floating around yet, so forbid this.
    report(expr, 142);
    return false;
}

bool Semantics::CheckExprForArrayInitializer(Expr* expr) {
    switch (expr->kind()) {
        case ExprKind::NewArrayExpr: {
            auto actual = expr->to<NewArrayExpr>();
            return CheckNewArrayExprForArrayInitializer(actual);
        }
        default:
            return CheckExpr(expr);
    }
}

bool Semantics::CheckNewArrayExprForArrayInitializer(NewArrayExpr* na) {
    if (na->analyzed())
        return na->analysis_result();

    na->set_analysis_result(false);

    auto& val = na->val();
    auto& type = na->type();
    val.ident = iREFARRAY;
    val.tag = type.tag();
    for (auto& expr : na->exprs()) {
        if (!CheckExpr(expr))
            return false;
        if (expr->lvalue())
            expr = new RvalueExpr(expr);

        const auto& v = expr->val();
        if (IsLegacyEnumTag(sc_->scope(), v.tag)) {
            report(expr, 153);
            return false;
        }
        if (!is_valid_index_tag(v.tag)) {
            report(expr, 77) << type_to_name(v.tag);
            return false;
        }
        if (v.ident == iCONSTEXPR && v.constval() <= 0) {
            report(expr, 9);
            return false;
        }
    }

    na->set_analysis_result(true);
    return true;
}

void
NewArrayExpr::ProcessUses(SemaContext& sc)
{
    for (const auto& expr : exprs_)
        expr->MarkAndProcessUses(sc);
}

bool Semantics::CheckIfStmt(IfStmt* stmt) {
    if (Expr* expr = AnalyzeForTest(stmt->cond()))
        stmt->set_cond(expr);

    // Note: unlike loop conditions, we don't factor in constexprs here, it's
    // too much work and way less common than constant loop conditions.

    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        if (!CheckStmt(stmt->on_true(), STMT_OWNS_HEAP))
            return false;
    }
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        if (stmt->on_false() && !CheckStmt(stmt->on_false(), STMT_OWNS_HEAP))
            return false;
    }

    if (stmt->on_false()) {
        FlowType a = stmt->on_true()->flow_type();
        FlowType b = stmt->on_false()->flow_type();
        if (a == b)
            stmt->set_flow_type(a);
        else if (a != Flow_None && b != Flow_None)
            stmt->set_flow_type(Flow_Mixed);
    } else if (stmt->on_true()->flow_type() != Flow_None) {
        // Ideally, we'd take the "on-true" flow type and propagate it upward.
        // But that's not accurate, because it's really mixed with an implicit
        // fallthrough. There's no nice way to handle this and the flow tracker
        // is already way too complex.
        stmt->set_flow_type(Flow_Mixed);
    }

    if (*always_returns)
        sc_->set_always_returns(true);
    return true;
}

bool Semantics::CheckExprStmt(ExprStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckExpr(expr))
        return false;
    if (!expr->HasSideEffects())
        report(expr, 215);
    return true;
}

/*  testsymbols - test for unused local or global variables
 *
 *  "Public" functions are excluded from the check, since these
 *  may be exported to other object modules.
 *  Labels are excluded from the check if the argument 'testlabs'
 *  is 0. Thus, labels are not tested until the end of the function.
 *  Constants may also be excluded (convenient for global constants).
 *
 *  When the nesting level drops below "level", the check stops.
 *
 *  The function returns whether there is an "entry" point for the file.
 *  This flag will only be 1 when browsing the global symbol table.
 */
bool
Semantics::TestSymbol(symbol* sym, bool testconst)
{
    bool entry = false;
    switch (sym->ident) {
        case iFUNCTN:
        {
            if (sym->is_public || strcmp(sym->name(), uMAINFUNC) == 0)
                entry = true; /* there is an entry point */
            if ((sym->usage & uREAD) == 0 && !(sym->native || sym->stock || sym->is_public) &&
                sym->defined)
            {
                /* symbol isn't used ... (and not public/native/stock) */
                report(sym, 203) << sym->name();
                return entry;
            }

            // Functions may be used as callbacks, in which case we don't check
            // whether their arguments were used or not. We can't tell this until
            // the scope is exiting, which is right here, so peek at the arguments
            // for the function and check now.
            auto node = sym->function()->node;
            if (node && node->body()) {
                CheckFunctionReturnUsage(node);
                if (node->scope() && !sym->callback)
                    TestSymbols(node->scope(), true);
            }
            break;
        }
        case iCONSTEXPR:
            if (testconst && (sym->usage & uREAD) == 0) {
                error(sym, 203, sym->name()); /* symbol isn't used: ... */
            }
            break;
        case iMETHODMAP:
        case iENUMSTRUCT:
            // Ignore usage on methodmaps and enumstructs.
            break;
        default:
            /* a variable */
            if (!sym->stock && (sym->usage & (uWRITTEN | uREAD)) == 0 && !sym->is_public) {
                error(sym, 203, sym->name()); /* symbol isn't used (and not stock) */
            } else if (!sym->stock && !sym->is_public && (sym->usage & uREAD) == 0) {
                error(sym, 204, sym->name()); /* value assigned to symbol is never used */
            }
    }
    return entry;
}

bool Semantics::TestSymbols(SymbolScope* root, bool testconst) {
    bool entry = false;
    root->ForEachSymbol([&](symbol* sym) -> void {
        entry |= TestSymbol(sym, testconst);
    });
    return entry;
}

bool Semantics::CheckBlockStmt(BlockStmt* block) {
    ke::SaveAndSet<bool> restore_heap(&pending_heap_allocation_, false);

    bool ok = true;
    for (const auto& stmt : block->stmts()) {
        cc_.reports()->ResetErrorFlag();

        if (ok && !sc_->warned_unreachable() && (sc_->always_returns() ||
            (block->flow_type() != Flow_None && block->flow_type() != Flow_Mixed)))
        {
            report(stmt, 225);
            sc_->set_warned_unreachable();
        }
        ok &= CheckStmt(stmt);

        FlowType flow = stmt->flow_type();
        if (flow != Flow_None && block->flow_type() == Flow_None)
            block->set_flow_type(flow);
    }

    if (block->scope())
        TestSymbols(block->scope(), true);

    // Blocks always taken heap ownership.
    AssignHeapOwnership(block);
    return ok;
}

AutoCollectSemaFlow::AutoCollectSemaFlow(SemaContext& sc, ke::Maybe<bool>* out)
  : sc_(sc),
    out_(out),
    old_value_(sc.always_returns())
{
    sc.set_always_returns(false);
}

AutoCollectSemaFlow::~AutoCollectSemaFlow()
{
    if (out_->isValid())
        out_->get() &= sc_.always_returns();
    else
        out_->init(sc_.always_returns());
    sc_.set_always_returns(old_value_);
}

bool Semantics::CheckBreakStmt(BreakStmt* stmt) {
    sc_->loop_has_break() = true;
    return true;
}

bool Semantics::CheckContinueStmt(ContinueStmt* stmt) {
    sc_->loop_has_continue() = true;
    return true;
}

bool Semantics::CheckReturnStmt(ReturnStmt* stmt) {
    sc_->set_always_returns();
    sc_->loop_has_return() = true;

    symbol* curfunc = sc_->func();

    auto expr = stmt->expr();
    if (!expr) {
        if (curfunc->must_return_value())
            ReportFunctionReturnError(curfunc);
        if (sc_->void_return())
            return true;
        sc_->set_void_return(stmt);
        return true;
    }

    if (Stmt* other = sc_->void_return()) {
        if (!sc_->warned_mixed_returns()) {
            report(other, 78);
            report(stmt, 78);
            sc_->set_warned_mixed_returns();
        }
    }

    if (!CheckExpr(expr))
        return false;

    if (expr->lvalue())
        expr = stmt->set_expr(new RvalueExpr(expr));

    AutoErrorPos aep(expr->pos());

    if (curfunc->tag == types_->tag_void()) {
        report(stmt, 88);
        return false;
    }

    const auto& v = expr->val();
    if (v.ident == iARRAY && !v.sym) {
        /* returning a literal string is not supported (it must be a variable) */
        report(stmt, 39);
        return false;
    }
    /* see if this function already has a sub type (an array attached) */
    auto sub = curfunc->array_return();
    assert(sub == nullptr || sub->ident == iREFARRAY);
    if (sc_->returns_value()) {
        int retarray = (v.ident == iARRAY || v.ident == iREFARRAY);
        /* there was an earlier "return" statement in this function */
        if ((sub == nullptr && retarray) || (sub != nullptr && !retarray)) {
            report(stmt, 79); /* mixing "return array;" and "return value;" */
            return false;
        }
        if (retarray && curfunc->is_public) {
            report(stmt, 90) << curfunc->name(); /* public function may not return array */
            return false;
        }
    } else {
        sc_->set_returns_value();
    }

    /* check tagname with function tagname */
    assert(curfunc != nullptr);
    if (!matchtag_string(v.ident, v.tag))
        matchtag(curfunc->tag, v.tag, TRUE);

    if (v.ident == iARRAY || v.ident == iREFARRAY) {
        if (!CheckArrayReturnStmt(stmt))
            return false;
    }
    return true;
}

bool Semantics::CheckArrayReturnStmt(ReturnStmt* stmt) {
    symbol* curfunc = sc_->func();
    symbol* sub = curfunc->array_return();
    const auto& val = stmt->expr()->val();
    symbol* sym = val.sym;

    auto& array = stmt->array();
    array = {};
    array.ident = iARRAY;

    if (sub) {
        assert(sub->ident == iREFARRAY);
        // this function has an array attached already; check that the current
        // "return" statement returns exactly the same array
        if (sub->dim_count() != val.array_dim_count()) {
            report(stmt, 48); /* array dimensions must match */
            return false;
        }

        for (int i = 0; i < sub->dim_count(); i++) {
            array.dim.emplace_back(sub->dim(i));
            if (val.array_dim(i) != array.dim.back()) {
                report(stmt, 47); /* array sizes must match */
                return false;
            }
        }
    } else {
        // this function does not yet have an array attached; clone the
        // returned symbol beneath the current function
        sub = sym;
        for (int i = 0; i < sub->dim_count(); i++) {
            if (!sub->dim(i)) {
                report(stmt, 128);
                return false;
            }
            array.dim.emplace_back(sub->dim(i));
            if (sub->semantic_tag) {
                array.set_tag(0);
                array.declared_tag = sub->semantic_tag;
            }
        }
        if (!array.has_tag())
            array.set_tag(sub->tag);

        // the address of the array is stored in a hidden parameter; the address
        // of this parameter is 1 + the number of parameters (times the size of
        // a cell) + the size of the stack frame and the return address
        //   base + 0*sizeof(cell)         == previous "base"
        //   base + 1*sizeof(cell)         == function return address
        //   base + 2*sizeof(cell)         == number of arguments
        //   base + 3*sizeof(cell)         == first argument of the function
        //   ...
        //   base + ((n-1)+3)*sizeof(cell) == last argument of the function
        //   base + (n+3)*sizeof(cell)     == hidden parameter with array address
        assert(curfunc != NULL);
        int argcount = (int)curfunc->function()->node->args().size();

        auto dim = array.dim.empty() ? nullptr : &array.dim[0];
        sub = NewVariable(curfunc->nameAtom(), (argcount + 3) * sizeof(cell), iREFARRAY,
                          sGLOBAL, curfunc->tag, dim, array.numdim(),
                          array.enum_struct_tag());
        curfunc->set_array_return(sub);
    }

    auto func_node = sc_->func_node();
    if (func_node->type().numdim() == 0)
        report(stmt, 246) << func_node->name();
    else if (func_node->type().numdim() != array.numdim())
        report(stmt, 413);

    array.set_tag(sub->tag);
    array.has_postdims = true;
    return true;
}

bool Semantics::CheckAssertStmt(AssertStmt* stmt) {
    if (Expr* expr = AnalyzeForTest(stmt->expr())) {
        stmt->set_expr(expr);
        return true;
    }
    return false;
}

bool Semantics::CheckDeleteStmt(DeleteStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckExpr(expr))
        return false;

    const auto& v = expr->val();
    switch (v.ident) {
        case iFUNCTN:
            report(expr, 167) << "functions";
            return false;

        case iARRAY:
        case iREFARRAY:
            report(expr, 167) << "arrays";
            return false;
            break;

        case iACCESSOR:
            if (v.accessor()->getter)
                markusage(v.accessor()->getter, uREAD);
            if (v.accessor()->setter)
                markusage(v.accessor()->setter, uREAD);
            break;
    }

    if (v.tag == 0) {
        report(expr, 167) << "integers";
        return false;
    }

    methodmap_t* map = types_->find(v.tag)->asMethodmap();
    if (!map) {
        report(expr, 115) << "type" << type_to_name(v.tag);
        return false;
    }

    for (methodmap_t* iter = map; iter; iter = iter->parent) {
        if (iter->dtor) {
            map = iter;
            break;
        }
    }

    if (!map || !map->dtor) {
        report(expr, 115) << "methodmap" << map->name;
        return false;
    }

    stmt->set_map(map);
    return true;
}

bool Semantics::CheckExitStmt(ExitStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckExpr(expr))
        return false;
    if (expr->lvalue())
        expr = stmt->set_expr(new RvalueExpr(expr));

    switch (expr->val().ident) {
        case iEXPRESSION:
        case iREFERENCE:
        case iVARIABLE:
        case iCONSTEXPR:
        case iARRAYCHAR:
        case iARRAYCELL: {
            AutoErrorPos aep(expr->pos());
            matchtag(0, expr->val().tag, MATCHTAG_COERCE);
            break;
        }
        default:
            report(expr, 106);
            return false;
    }
    return true;
}

bool Semantics::CheckDoWhileStmt(DoWhileStmt* stmt) {
    {
        ke::SaveAndSet<bool> restore_heap(&pending_heap_allocation_, false);

        if (Expr* expr = AnalyzeForTest(stmt->cond())) {
            stmt->set_cond(expr);
            AssignHeapOwnership(expr);
        }
    }

    auto cond = stmt->cond();

    ke::Maybe<cell> constval;
    if (cond->val().ident == iCONSTEXPR)
        constval.init(cond->val().constval());

    bool has_break = false;
    bool has_return = false;
    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        ke::SaveAndSet<bool> auto_break(&sc_->loop_has_break(), false);
        ke::SaveAndSet<bool> auto_return(&sc_->loop_has_return(), false);

        if (!CheckStmt(stmt->body(), STMT_OWNS_HEAP))
            return false;

        has_break = sc_->loop_has_break();
        has_return = sc_->loop_has_return();
    }

    stmt->set_never_taken(constval.isValid() && !constval.get());
    stmt->set_always_taken(constval.isValid() && constval.get());

    if (stmt->never_taken() && stmt->token() == tWHILE) {
        // Loop is never taken, don't touch the return status.
    } else if ((stmt->token() == tDO || stmt->always_taken()) && !has_break) {
        // Loop is always taken, and has no break statements.
        if (stmt->always_taken() && has_return)
            sc_->set_always_returns(true);

        // Loop body ends in a return and has no break statements.
        if (stmt->body()->flow_type() == Flow_Return)
            stmt->set_flow_type(Flow_Return);
    }

    // :TODO: endless loop warning?
    return true;
}

bool Semantics::CheckForStmt(ForStmt* stmt) {
    bool ok = true;
    if (stmt->init() && !CheckStmt(stmt->init()))
        ok = false;

    auto cond = stmt->cond();
    if (cond) {
        if (Expr* expr = AnalyzeForTest(cond))
            cond = stmt->set_cond(expr);
        else
            ok = false;
    }
    if (stmt->advance()) {
        ke::SaveAndSet<bool> restore(&pending_heap_allocation_, false);
        if (CheckExpr(stmt->advance()))
            AssignHeapOwnership(stmt->advance());
        else
            ok = false;
    }

    ke::Maybe<cell> constval;
    if (cond && cond->val().ident == iCONSTEXPR)
        constval.init(cond->val().constval());

    bool has_break = false;
    bool has_return = false;
    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        ke::SaveAndSet<bool> auto_break(&sc_->loop_has_break(), false);
        ke::SaveAndSet<bool> auto_continue(&sc_->loop_has_continue(), false);
        ke::SaveAndSet<bool> auto_return(&sc_->loop_has_return(), false);

        ok &= CheckStmt(stmt->body(), STMT_OWNS_HEAP);

        has_break = sc_->loop_has_break();
        has_return = sc_->loop_has_return();
        stmt->set_has_continue(sc_->loop_has_continue());
    }

    stmt->set_never_taken(constval.isValid() && !constval.get());
    stmt->set_always_taken(!cond || (constval.isValid() && constval.get()));

    // If the body falls through, then implicitly there is a continue operation.
    auto body = stmt->body();
    if (body->flow_type() != Flow_Break && body->flow_type() != Flow_Return)
        stmt->set_has_continue(true);
    // If there is a non-constant conditional, there is also an implicit continue.
    if (!stmt->always_taken())
        stmt->set_has_continue(true);

    if (stmt->never_taken()) {
        // Loop is never taken, don't touch the return status.
    } else if (stmt->always_taken() && !has_break) {
        if (has_return) {
            // Loop is always taken, and has no break statements, and has a return statement.
            sc_->set_always_returns(true);
        }
        if (body->flow_type() == Flow_Return && !has_break)
            stmt->set_flow_type(Flow_Return);
    }

    if (stmt->scope())
        TestSymbols(stmt->scope(), true);
    return ok;
}

bool Semantics::CheckSwitchStmt(SwitchStmt* stmt) {
    auto expr = stmt->expr();
    bool tag_ok = CheckExpr(expr);
    const auto& v = expr->val();
    if (tag_ok && (v.ident == iARRAY || v.ident == iREFARRAY))
        report(expr, 33) << "-unknown-";

    if (expr->lvalue())
        expr = stmt->set_expr(new RvalueExpr(expr));

    ke::Maybe<bool> always_returns;
    ke::Maybe<FlowType> flow;

    auto update_flow = [&](FlowType other) -> void {
        if (flow) {
            if (*flow == Flow_None || other == Flow_None)
                *flow = Flow_None;
            else if (*flow != other)
                *flow = Flow_Mixed;
        } else {
            flow.init(other);
        }
    };

    std::unordered_set<cell> case_values;
    for (const auto& case_entry : stmt->cases()) {
        for (Expr* expr : case_entry.first) {
            if (!CheckExpr(expr))
                continue;

            int tag;
            cell value;
            if (!expr->EvalConst(&value, &tag)) {
                report(expr, 8);
                continue;
            }
            if (tag_ok) {
                AutoErrorPos aep(expr->pos());
                matchtag(v.tag, tag, MATCHTAG_COERCE);
            }

            if (!case_values.count(value))
                case_values.emplace(value);
            else
                report(expr, 40) << value;
        }

        AutoCollectSemaFlow flow(*sc_, &always_returns);
        if (CheckStmt(case_entry.second))
            update_flow(case_entry.second->flow_type());
    }

    if (stmt->default_case()) {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        if (CheckStmt(stmt->default_case()))
            update_flow(stmt->default_case()->flow_type());
    } else {
        always_returns.init(false);
        update_flow(Flow_None);
    }

    if (*always_returns)
        sc_->set_always_returns(true);

    stmt->set_flow_type(*flow);

    // Return value doesn't really matter for statements.
    return true;
}

void
ReportFunctionReturnError(symbol* sym)
{
    if (sym->function()->is_member_function) {
        // This is a member function, ignore compatibility checks and go
        // straight to erroring.
        report(sym, 400) << sym->name();
        return;
    }

    auto types = CompileContext::get().types();

    // Normally we want to encourage return values. But for legacy code,
    // we allow "public int" to warn instead of error.
    //
    // :TODO: stronger enforcement when function result is used from call
    if (sym->tag == 0) {
        report(sym, 209) << sym->name();
    } else if (types->find(sym->tag)->isEnum() || sym->tag == types->tag_bool() ||
               sym->tag == types->tag_float() || !sym->retvalue_used)
    {
        report(sym, 242) << sym->name();
    } else {
        report(sym, 400) << sym->name();
    }
}

bool
FunctionDecl::IsVariadic() const
{
    return !args_.empty() && args_.back()->type().ident == iVARARGS;
}

bool Semantics::CheckFunctionDecl(FunctionDecl* info) {
    // We could have been analyzed recursively to derive return array sizes.
    if (info->is_analyzed())
        return info->analysis_status();

    assert(!info->is_analyzing());

    info->set_is_analyzing(true);
    info->set_analyzed(CheckFunctionDeclImpl(info));
    info->set_is_analyzing(false);

    return info->analysis_status();
}

bool Semantics::CheckFunctionDeclImpl(FunctionDecl* info) {
    SemaContext sc(*sc_, info->sym(), info);
    ke::SaveAndSet<SemaContext*> push_sc(&sc_, &sc);

    auto& decl = info->decl();
    {
        AutoErrorPos error_pos(info->pos());
        CheckVoidDecl(&decl, FALSE);

        if (decl.opertok)
            check_operatortag(decl.opertok, decl.type.tag(), decl.name->chars());
    }

    if (info->is_public() || info->is_forward()) {
        if (decl.type.numdim() > 0)
            report(info->pos(), 141);
    }

    auto sym = info->sym();
    if (sym->native) {
        if (decl.type.numdim() > 0) {
            report(info->pos(), 83);
            return false;
        }
        return true;
    }

    auto body = info->body();
    if (!body) {
        if (info->is_native() || info->is_forward())
            return true;
        report(info->pos(), 10);
        return false;
    }

    if (sym->deprecated && !sym->stock) {
        const char* ptr = sym->documentation->chars();
        report(info->pos(), 234) << sym->name() << ptr; /* deprecated (probably a public function) */
    }

    bool ok = CheckStmt(body, STMT_OWNS_HEAP);

    sym->returns_value = sc_->returns_value();
    sym->always_returns = sc_->always_returns();

    if (!sym->returns_value) {
        if (sym->tag == types_->tag_void() && sym->function()->forward && !decl.type.tag() &&
            !decl.type.is_new)
        {
            // We got something like:
            //    forward void X();
            //    public X()
            //
            // Switch our decl type to void.
            decl.type.set_tag(types_->tag_void());
        }
    }

    // Make sure that a public return type matches the forward (if any).
    if (sym->function()->forward && info->is_public()) {
        if (sym->tag != decl.type.tag())
            report(info->pos(), 180) << type_to_name(sym->tag) << type_to_name(decl.type.tag());
    }

    // For globals, we test arguments in a later pass, since we need to know
    // which functions get used as callbacks in order to emit a warning. The
    // same is true for return value usage: we don't know how to handle
    // compatibility edge cases until we've discovered all callers.
    if (sym->function()->is_member_function) {
        CheckFunctionReturnUsage(info);
        if (info->scope())
            TestSymbols(info->scope(), true);
    }

    if (sym->is_public)
        cc_.publics().emplace(sym);
    return ok;
}

void Semantics::CheckFunctionReturnUsage(FunctionDecl* info) {
    auto sym = info->sym();
    if (sym->returns_value && sym->always_returns)
        return;

    if (sym->must_return_value())
        ReportFunctionReturnError(sym);

        // Synthesize a return statement.
    std::vector<Stmt*> stmts = {
        info->body(),
        new ReturnStmt(info->end_pos(), nullptr),
    };

    auto new_body = new BlockStmt(info->body()->pos(), stmts);
    new_body->set_flow_type(Flow_Return);
    info->set_body(new_body);
}

void
StmtList::ProcessUses(SemaContext& sc)
{
    for (const auto& stmt : stmts_)
        stmt->ProcessUses(sc);
}

void
VarDeclBase::ProcessUses(SemaContext& sc)
{
    if (init_)
        init_rhs()->MarkAndProcessUses(sc);
}

void
IfStmt::ProcessUses(SemaContext& sc)
{
    cond_->MarkAndProcessUses(sc);
    on_true_->ProcessUses(sc);
    if (on_false_)
        on_false_->ProcessUses(sc);
}

void
ReturnStmt::ProcessUses(SemaContext& sc)
{
    if (expr_)
        expr_->MarkAndProcessUses(sc);
}

void
ExitStmt::ProcessUses(SemaContext& sc)
{
    if (expr_)
        expr_->MarkAndProcessUses(sc);
}

void
DoWhileStmt::ProcessUses(SemaContext& sc)
{
    cond_->MarkAndProcessUses(sc);
    body_->ProcessUses(sc);
}

void
ForStmt::ProcessUses(SemaContext& sc)
{
    if (init_)
        init_->ProcessUses(sc);
    if (cond_)
        cond_->MarkAndProcessUses(sc);
    if (advance_)
        advance_->ProcessUses(sc);
    body_->ProcessUses(sc);
}

void
SwitchStmt::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);

    for (const auto& entry : cases_) {
        for (const auto& expr : entry.first)
            expr->MarkAndProcessUses(sc);
        entry.second->ProcessUses(sc);
    }

    if (default_case_)
        default_case_->ProcessUses(sc);
}

void
FunctionDecl::ProcessUses(SemaContext& outer_sc)
{
    if (!body_)
        return;

    SemaContext sc(outer_sc, sym_, this);

    for (const auto& arg : args_)
        arg->ProcessUses(sc);

    body_->ProcessUses(sc);
}

bool Semantics::CheckPragmaUnusedStmt(PragmaUnusedStmt* stmt) {
    for (const auto& sym : stmt->symbols()) {
        sym->usage |= uREAD;

        switch (sym->ident) {
            case iVARIABLE:
            case iREFERENCE:
            case iARRAY:
            case iREFARRAY:
                sym->usage |= uWRITTEN;
                break;
        }
    }
    return true;
}

bool Semantics::CheckEnumStructDecl(EnumStructDecl* decl) {
    bool ok = true;
    for (const auto& fun : decl->methods())
        ok &= CheckStmt(fun);
    return ok;
}

void
EnumStructDecl::ProcessUses(SemaContext& sc)
{
    for (const auto& fun : methods_)
        fun->ProcessUses(sc);
}

bool Semantics::CheckMethodmapDecl(MethodmapDecl* decl) {
    bool ok = true;
    for (const auto& prop : decl->properties()) {
        if (prop->getter)
            ok &= CheckFunctionDecl(prop->getter);
        if (prop->setter)
            ok &= CheckFunctionDecl(prop->setter);
    }
    for (const auto& method : decl->methods())
        ok &= CheckStmt(method->decl);
    return ok;
}

void Semantics::NeedsHeapAlloc(Expr* expr) {
    expr->set_can_alloc_heap(true);
    pending_heap_allocation_ = true;
}

void Semantics::AssignHeapOwnership(ParseNode* node) {
    if (pending_heap_allocation_) {
        node->set_tree_has_heap_allocs(true);
        pending_heap_allocation_ = false;
    }
}

void
MethodmapDecl::ProcessUses(SemaContext& sc)
{
    for (const auto& prop : properties_) {
        if (prop->getter)
            prop->getter->ProcessUses(sc);
        if (prop->setter)
            prop->setter->ProcessUses(sc);
    }
    for (const auto& method : methods_)
        method->decl->ProcessUses(sc);
}

void Semantics::CheckVoidDecl(const typeinfo_t* type, int variable) {
    if (type->tag() != types_->tag_void())
        return;

    if (variable) {
        error(144);
        return;
    }

    if (type->numdim() > 0) {
        error(145);
        return;
    }
}

void Semantics::CheckVoidDecl(const declinfo_t* decl, int variable) {
    CheckVoidDecl(&decl->type, variable);
}

int argcompare(ArgDecl* a1, ArgDecl* a2) {
    int result = 1;

    if (result)
        result = a1->type().ident == a2->type().ident; /* type()/class */
    if (result)
        result = a1->type().is_const == a2->type().is_const; /* "const" flag */
    if (result)
        result = a1->type().tag() == a2->type().tag();
    if (result)
        result = a1->type().dim == a2->type().dim; /* array dimensions & index tags */
    if (result)
        result = a1->type().declared_tag == a2->type().declared_tag;
    if (result)
        result = !!a1->default_value() == !!a2->default_value(); /* availability of default value */
    if (auto a1_def = a1->default_value()) {
        auto a2_def = a2->default_value();
        if (a1->type().ident == iREFARRAY) {
            if (result)
                result = !!a1_def->array == !!a2_def->array;
            if (result && a1_def->array)
                result = a1_def->array->total_size() == a2_def->array->total_size();
            /* ??? should also check contents of the default array (these troubles
             * go away in a 2-pass compiler that forbids double declarations, but
             * Pawn currently does not forbid them) */
        } else {
            if (result)
                result = a1_def->val.isValid() == a2_def->val.isValid();
            if (result && a1_def->val)
                result = a1_def->val.get() == a2_def->val.get();
        }
        if (result)
            result = a1_def->tag == a2_def->tag;
    }
    return result;
}

bool
IsLegacyEnumTag(SymbolScope* scope, int tag)
{
    Type* type = CompileContext::get().types()->find(tag);
    if (!type->isEnum())
        return false;
    symbol* sym = FindSymbol(scope, type->nameAtom());
    if (!sym)
        return false;
    return sym->data() && (sym->data()->asEnumStruct() || sym->data()->asEnum());
}

void fill_arg_defvalue(CompileContext& cc, ArgDecl* decl) {
    auto def = new DefaultArg();
    def->tag = decl->type().tag();

    if (auto expr = decl->init_rhs()->as<SymbolExpr>()) {
        symbol* sym = expr->sym();
        assert(sym->vclass == sGLOBAL);

        def->sym = sym;
        if (sym->usage & uREAD)
            markusage(sym, uREAD);
    } else {
        auto array = cc.NewDefaultArrayData();
        BuildArrayInitializer(decl, array, 0);

        def->array = array;
        def->array->iv_size = (cell_t)array->iv.size();
        def->array->data_size = (cell_t)array->data.size();
    }
    decl->set_default_value(def);
}

bool Semantics::CheckChangeScopeNode(ChangeScopeNode* node) {
    assert(sc_->scope()->kind() == sGLOBAL || sc_->scope()->kind() == sFILE_STATIC);
    sc_->set_scope(node->scope());
    static_scopes_.emplace(node->scope());
    return true;
}

SymbolScope* Semantics::current_scope() const {
    if (sc_)
        return sc_->scope();
    return cc_.globals();
}

void DeleteStmt::ProcessUses(SemaContext& sc) {
    expr_->MarkAndProcessUses(sc);
    markusage(map_->dtor->target, uREAD);
}
