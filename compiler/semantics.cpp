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
#include "type-checker.h"
#include "value-inl.h"

namespace sp {
namespace cc {

Semantics::Semantics(CompileContext& cc)
  : cc_(cc)
{
    types_ = cc.types();
}

bool Semantics::Analyze(ParseTree* tree) {
    SemaContext sc(this);
    ke::SaveAndSet<SemaContext*> push_sc(&sc_, &sc);

    AutoCountErrors errors;
    if (!CheckStmtList(tree->stmts()) || !errors.ok())
        return false;

    DeduceLiveness();
    DeduceMaybeUsed();

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
        case StmtKind::ConstDecl:
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
        case StmtKind::MemberFunctionDecl:
        case StmtKind::MethodmapMethodDecl:
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
        case StmtKind::EnumFieldDecl:
        case StmtKind::PstructDecl:
        case StmtKind::TypedefDecl:
        case StmtKind::TypesetDecl:
            return true;
        default:
            assert(false);

            report(stmt, 420) << (int)stmt->kind();
            return false;
    }
}

bool Semantics::CheckVarDecl(VarDeclBase* decl) {
    AutoErrorPos aep(decl->pos());

    const auto& type = decl->type();
    bool is_const = decl->type_info().is_const;

    // Constants are checked during binding.
    if (decl->ident() == iCONSTEXPR)
        return true;

    if (type->isPstruct())
        return CheckPstructDecl(decl);

    if (!decl->as<ArgDecl>() && is_const && !decl->init() && !decl->is_public())
        report(decl->pos(), 251);

    // CheckArrayDecl works on enum structs too.
    if (type->isArray() || type->isEnumStruct()) {
        if (!CheckArrayDeclaration(decl))
            return false;
        if (type->isEnumStruct() && IsThisAtom(decl->name()))
            decl->mutable_type_info()->is_const = false;
        if (decl->vclass() == sLOCAL)
            pending_heap_allocation_ = true;
        return true;
    }

    auto init = decl->init();

    // Since we always create an assignment expression, all type checks will
    // be performed by the Analyze(sc) call here.
    //
    // :TODO: write flag when removing ProcessUses
    if (init && !CheckRvalue(init))
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

    auto init = decl->init()->right()->as<StructExpr>();
    if (!init) {
        report(decl->init(), 433);
        return false;
    }

    auto type = decl->type();
    auto ps = type->asPstruct();

    std::vector<bool> visited;
    visited.resize(ps->fields().size());

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
        auto arg = ps->fields()[i];
        if (auto at = arg->type()->as<ArrayType>()) {
            assert(at->inner()->isChar());

            auto expr = new StringExpr(decl->pos(), cc_.atom(""));
            init->fields().push_back(new StructInitFieldExpr(arg->name(), expr, decl->pos()));
        }
    }

    return true;
}

bool Semantics::CheckPstructArg(VarDeclBase* decl, PstructDecl* ps,
                                StructInitFieldExpr* field, std::vector<bool>* visited)
{
    auto arg = ps->FindField(field->name);
    if (!arg) {
        report(field->pos(), 96) << field->name << "struct" << decl->name();
        return false;
    }

    if (visited->at(arg->offset()))
        report(field->value->pos(), 244) << field->name->chars();

    visited->at(arg->offset()) = true;

    Type* actual = nullptr;
    if (auto expr = field->value->as<StringExpr>()) {
        actual = types_->defineArray(types_->type_char(), 0);
    } else if (auto expr = field->value->as<TaggedValueExpr>()) {
        actual = expr->type();
    } else if (auto expr = field->value->as<SymbolExpr>()) {
        actual = expr->decl()->type();
    } else {
        assert(false);
        return false;
    }

    TypeChecker tc(field, arg->type(), actual, TypeChecker::Assignment);
    if (!tc.Coerce())
        return false;
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

bool Expr::EvalConst(cell* value, Type** type) {
    if (val_.ident != iCONSTEXPR) {
        if (!FoldToConstant())
            return false;
        assert(val_.ident == iCONSTEXPR);
    }

    if (value)
        *value = val_.constval();
    if (type)
        *type = val_.type();
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
            return cc::HasSideEffects(e->exprs());
        }
        case ExprKind::ArrayExpr: {
            auto e = to<ArrayExpr>();
            return cc::HasSideEffects(e->exprs());
        }
        case ExprKind::NewArrayExpr: {
            auto e = to<NewArrayExpr>();
            return cc::HasSideEffects(e->exprs());
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

bool Semantics::CheckScalarType(Expr* expr) {
    const auto& val = expr->val();
    if (val.type()->isArray()) {
        if (val.sym)
            report(expr, 33) << val.sym->name();
        else
            report(expr, 29);
        return false;
    }
    if (val.type()->asEnumStruct()) {
        report(expr, 447);
        return false;
    }
    return true;
}

Expr* Semantics::AnalyzeForTest(Expr* expr) {
    if (!CheckRvalue(expr))
        return nullptr;
    if (!CheckScalarType(expr))
        return nullptr;

    auto& val = expr->val();
    if (!val.type()->isInt() && !val.type()->isBool()) {
        UserOperation userop;
        if (find_userop(*sc_, '!', val.type(), 0, 1, &val, &userop)) {
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
            expr->val().set_type(types_->type_bool());
            return expr;
        }
    }

    if (val.ident == iCONSTEXPR) {
        if (!sc_->preprocessing()) {
            if (val.constval())
                report(expr, 206);
            else
                report(expr, 205);
        }
    } else if (auto sym_expr = expr->as<SymbolExpr>()) {
        if (sym_expr->decl()->as<FunctionDecl>())
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
        if (val_.accessor()->getter())
            markusage(val_.accessor()->getter(), uREAD);
        val_.ident = iEXPRESSION;
    } else if (val_.ident == iVARIABLE) {
        if (val_.type()->isReference())
            val_.set_type(val_.type()->inner());
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
    if (!CheckRvalue(expr))
        return false;
    if (!CheckScalarType(expr))
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
            if (find_userop(*sc_, '!', out_val.type(), 0, 1, &out_val, &userop)) {
                expr = unary->set_expr(new CallUserOpExpr(userop, expr));
                out_val = expr->val();
                unary->set_userop();
            } else if (out_val.ident == iCONSTEXPR) {
                out_val.set_constval(!out_val.constval());
            }
            out_val.set_type(types_->type_bool());
            break;
        case '-':
            if (out_val.ident == iCONSTEXPR && out_val.type()->isFloat()) {
                float f = sp::FloatCellUnion(out_val.constval()).f32;
                out_val.set_constval(sp::FloatCellUnion(-f).cell);
            } else if (find_userop(*sc_, '-', out_val.type(), 0, 1, &out_val, &userop)) {
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
    if (!CheckScalarType(expr))
        return false;
    if (!expr->lvalue()) {
        report(incdec, 22);
        return false;
    }

    const auto& expr_val = expr->val();
    if (expr_val.ident != iACCESSOR) {
        if (expr_val.sym && expr_val.sym->is_const()) {
            report(incdec, 22); /* assignment to const argument */
            return false;
        }
    } else {
        if (!expr_val.accessor()->setter()) {
            report(incdec, 152) << expr_val.accessor()->name();
            return false;
        }
        if (!expr_val.accessor()->getter()) {
            report(incdec, 149) << expr_val.accessor()->name();
            return false;
        }
        markusage(expr_val.accessor()->getter(), uREAD);
        markusage(expr_val.accessor()->setter(), uREAD);
    }

    Type* type = expr_val.type();
    if (type->isReference())
        type = type->inner();

    find_userop(*sc_, incdec->token(), type, 0, 1, &expr_val, &incdec->userop());

    // :TODO: more type checks
    auto& val = incdec->val();
    val.ident = iEXPRESSION;
    val.set_type(type);
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
    if (!CheckExpr(left) || !CheckRvalue(right))
        return false;

    int token = expr->token();
    if (token != '=') {
        if (!CheckScalarType(left))
            return false;
        if (!CheckScalarType(right))
            return false;
    }

    if (IsAssignOp(token)) {
        // Mark the left-hand side as written as soon as we can.
        if (Decl* sym = left->val().sym) {
            markusage(sym, uWRITTEN);

            // If it's an outparam, also mark it as read.
            if (sym->vclass() == sARGUMENT &&
                (sym->type()->isReference() ||
                 sym->type()->isArray() ||
                 sym->type()->isEnumStruct()))
            {
                markusage(sym, uREAD);
            }
        } else if (auto* accessor = left->val().accessor()) {
            if (!accessor->setter()) {
                report(expr, 152) << accessor->name();
                return false;
            }
            markusage(accessor->setter(), uREAD);
            if (accessor->getter() && token != '=')
                markusage(accessor->getter(), uREAD);
        }

        if (!CheckAssignmentLHS(expr))
            return false;
        if (token != '=' && !CheckRvalue(left->pos(), left->val()))
            return false;
    } else if (left->lvalue()) {
        if (!CheckRvalue(left->pos(), left->val()))
            return false;
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

        if (left_val.type()->isArray()) {
            const char* ptr = (left_val.sym != nullptr) ? left_val.sym->name()->chars() : "-unknown-";
            report(expr, 33) << ptr; /* array must be indexed */
            return false;
        }
        if (right_val.type()->isArray()) {
            const char* ptr = (right_val.sym != nullptr) ? right_val.sym->name()->chars() : "-unknown-";
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
    val.set_type(left_val.type());

    auto& assignop = expr->assignop();
    if (assignop.sym)
        val.set_type(assignop.sym->type());

    if (oper_tok) {
        auto& userop = expr->userop();
        if (find_userop(*sc_, oper_tok, left_val.type(), right_val.type(), 2, nullptr, &userop)) {
            val.set_type(userop.sym->type());
        } else if (left_val.ident == iCONSTEXPR && right_val.ident == iCONSTEXPR) {
            char boolresult = FALSE;
            matchtag(left_val.type(), right_val.type(), FALSE);
            val.ident = iCONSTEXPR;
            val.set_constval(calc(left_val.constval(), oper_tok, right_val.constval(),
                                  &boolresult));
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            Type* left_type = left_val.type();
            if (left_type->isReference())
                left_type = left_type->inner();

            Type* right_type = right_val.type();
            if (right_type->isReference())
                right_type = right_type->inner();

            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_type, right_type, MATCHTAG_DEDUCE);
        }

        if (IsChainedOp(token) || token == tlEQ || token == tlNE)
            val.set_type(types_->type_bool());
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
    if (auto left_array = left->val().type()->as<ArrayType>()) {
        // array assignment is permitted too (with restrictions)
        if (oper_tok) {
            report(expr, 23);
            return false;
        }

        for (auto iter = left_array; iter; iter = iter->inner()->as<ArrayType>()) {
            if (!iter->size()) {
                report(left, 46);
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

    // may not change "constant" parameters
    if (!expr->initializer() && left_val.sym && left_val.sym->is_const()) {
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

    if (auto left_array = left_val.type()->as<ArrayType>()) {
        TypeChecker tc(expr, left_val.type(), right_val.type(), TypeChecker::Assignment);
        if (!tc.Coerce())
            return false;

        auto right_array = right_val.type()->to<ArrayType>();
        if (right_array->inner()->isArray()) {
            report(expr, 23);
            return false;
        }

        if (right_array->size() == 0) {
            report(expr, 9);
            return false;
        }

        expr->set_array_copy_length(CalcArraySize(right_array));
    } else {
        if (right_val.type()->isArray()) {
            // Hack. Special case array literals assigned to an enum struct,
            // since we don't have the infrastructure to deduce an RHS type
            // yet.
            if (!left_val.type()->isEnumStruct() || !right->as<ArrayExpr>()) {
                report(expr, 6); // must be assigned to an array
                return false;
            }
            return true;
        }

        // Userop tag will be propagated by the caller.
        find_userop(*sc_, 0, right_val.type(), left_val.type(), 2, &left_val, &expr->assignop());
    }

    if (!expr->oper() &&
        !checkval_string(&left_val, &right_val) &&
        !expr->assignop().sym)
    {
        if (left_val.type()->isArray() &&
            ((left_val.type()->isChar() && !right_val.type()->isChar()) ||
             (!left_val.type()->isChar() && right_val.type()->isChar())))
        {
            report(expr, 179) << left_val.type() << right_val.type();
            return false;
        }
        if (left_val.type()->asEnumStruct() || right_val.type()->asEnumStruct()) {
            if (left_val.type() != right_val.type()) {
                report(expr, 134) << left_val.type() << right_val.type();
                return false;
            }

            auto es = left_val.type()->asEnumStruct();
            expr->set_array_copy_length(es->array_size());
        } else if (!left_val.type()->isArray()) {
            matchtag(left_val.type(), right_val.type(), TRUE);
        }
    }
    return true;
}

static inline bool
IsTypeBinaryConstantFoldable(Type* type)
{
    if (type->isEnum() || type->isInt())
        return true;
    return false;
}

bool
BinaryExpr::FoldToConstant()
{
    cell left_val, right_val;
    Type* left_type;
    Type* right_type;

    if (!left_->EvalConst(&left_val, &left_type) || !right_->EvalConst(&right_val, &right_type))
        return false;
    if (IsAssignOp(token_) || userop_.sym)
        return false;

    if (!IsTypeBinaryConstantFoldable(left_type) || !IsTypeBinaryConstantFoldable(right_type))
        return false;

    switch (token_) {
        case '*':
            val_.set_constval(left_val * right_val);
            break;
        case '/':
        case '%':
            if (!right_val) {
                report(pos_, 93);
                return false;
            }
            if (left_val == cell(0x80000000) && right_val == -1) {
                report(pos_, 97);
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

    if ((left = AnalyzeForTest(left)) == nullptr)
        return false;
    if ((right = AnalyzeForTest(right)) == nullptr)
        return false;

    if (left->lvalue())
        left = new RvalueExpr(left);
    if (right->lvalue())
        right = new RvalueExpr(right);

    expr->set_left(left);
    expr->set_right(right);

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
    val.set_type(types_->type_bool());
    return true;
}

bool Semantics::CheckChainedCompareExpr(ChainedCompareExpr* chain) {
    auto first = chain->first();
    if (!CheckRvalue(first))
        return false;
    if (first->lvalue())
        first = chain->set_first(new RvalueExpr(first));

    for (auto& op : chain->ops()) {
        if (!CheckRvalue(op.expr))
            return false;
        if (op.expr->lvalue())
            op.expr = new RvalueExpr(op.expr);
    }

    Expr* left = first;
    bool all_const = (left->val().ident == iCONSTEXPR);
    bool constval = true;

    auto& val = chain->val();
    val.ident = iEXPRESSION;
    val.set_type(types_->type_bool());

    for (auto& op : chain->ops()) {
        Expr* right = op.expr;
        const auto& left_val = left->val();
        const auto& right_val = right->val();

        if (left_val.type()->isArray()) {
            const char* ptr = (left_val.sym != nullptr) ? left_val.sym->name()->chars() : "-unknown-";
            report(left, 33) << ptr; /* array must be indexed */
            return false;
        }
        if (right_val.type()->isArray()) {
            const char* ptr = (right_val.sym != nullptr) ? right_val.sym->name()->chars() : "-unknown-";
            report(right, 33) << ptr; /* array must be indexed */
            return false;
        }

        if (find_userop(*sc_, op.oper_tok, left_val.type(), right_val.type(), 2, nullptr,
                        &op.userop))
        {
            if (!op.userop.sym->type()->isBool()) {
                report(op.pos, 51) << get_token_string(op.token);
                return false;
            }
        } else {
            // For the purposes of tag matching, we consider the order to be irrelevant.
            if (!checkval_string(&left_val, &right_val))
                matchtag_commutative(left_val.type(), right_val.type(), MATCHTAG_DEDUCE);
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

    if (!CheckRvalue(first) || !CheckRvalue(second) || !CheckRvalue(third))
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

    TypeChecker tc(second, left.type(), right.type(), TypeChecker::Generic,
                   TypeChecker::Ternary | TypeChecker::Commutative);
    if (!tc.Check())
        return false;

    // Huge hack: for now, take the larger of two char arrays.
    auto& val = expr->val();
    val = left;
    if (val.type()->isCharArray() && right.type()->isCharArray()) {
        auto left_array = val.type()->to<ArrayType>();
        auto right_array = right.type()->to<ArrayType>();
        if (right_array->size() > left_array->size())
            val = right;
    }

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

    Type* atype = expr->type();
    if (atype->isVoid()) {
        report(expr, 144);
        return false;
    }

    if (!CheckExpr(expr->expr()))
        return false;

    auto& out_val = expr->val();

    out_val = expr->expr()->val();
    expr->set_lvalue(expr->expr()->lvalue());

    Type* ltype = out_val.type();

    auto actual_array =  ltype->as<ArrayType>();
    if (actual_array) {
        // Unwind back to the inner.
        auto iter = actual_array;
        for (;;) {
            if (!iter->inner()->isArray())
                break;
            iter = iter->inner()->to<ArrayType>();
        }
        ltype = iter->inner();
    }

    if (ltype->isObject() || atype->isObject()) {
        matchtag(atype, out_val.type(), MATCHTAG_COERCE);
    } else if (ltype->isFunction() != atype->isFunction()) {
        // Warn: unsupported cast.
        report(expr, 237);
    } else if (ltype->isFunction() && atype->isFunction()) {
        matchtag(atype, out_val.type(), MATCHTAG_COERCE);
    } else if (out_val.type()->isVoid()) {
        report(expr, 89);
    } else if (atype->isEnumStruct() || ltype->isEnumStruct()) {
        report(expr, 95) << atype;
    }
    if (ltype->isReference() && !atype->isReference()) {
        if (atype->isEnumStruct()) {
            report(expr, 136);
            return false;
        }
        atype = types_->defineReference(atype);
    }
    if (actual_array)
        atype = types_->redefineArray(atype, actual_array);
    out_val.set_type(atype);
    return true;
}

void
CastExpr::ProcessUses(SemaContext& sc)
{
    expr_->MarkAndProcessUses(sc);
}

void SymbolExpr::MarkUsed(SemaContext& sc) {
    markusage(decl_, uREAD);
}

// This is a hack. Most code is not prepared to handle iMETHODMAP in type
// checks, so for now, we forbid it by default. Since the '.' operator *is*
// prepared for this, we have a special analysis option to allow returning
// types as values.
bool Semantics::CheckSymbolExpr(SymbolExpr* expr, bool allow_types) {
    AutoErrorPos aep(expr->pos());

    auto decl = expr->decl();
    if (!decl) {
        // This can happen if CheckSymbolExpr is called during name resolution.
        assert(cc_.reports()->total_errors() > 0);
        return false;
    }

    auto& val = expr->val();
    val.ident = decl->ident();
    val.sym = decl;

    // Don't expose the tag of old enumroots.
    Type* type = decl->type();
    if (decl->as<EnumDecl>() && !type->asEnumStruct() && decl->ident() == iCONSTEXPR) {
        report(expr, 174) << decl->name();
        return false;
    }
    val.set_type(type);

    if (auto fun = decl->as<FunctionDecl>()) {
        fun = fun->canonical();
        if (fun->is_native()) {
            report(expr, 76);
            return false;
        }
        if (fun->return_array()) {
            report(expr, 182);
            return false;
        }
        if (!fun->impl()) {
            report(expr, 4) << fun->name();
            return false;
        }

        funcenum_t* fe = funcenum_for_symbol(cc_, fun);

        // New-style "closure".
        val.ident = iEXPRESSION;
        val.set_type(fe->type);

        // Mark as being indirectly invoked. Direct invocations go through
        // BindCallTarget.
        fun->set_is_callback();
    }

    switch (decl->ident()) {
        case iVARIABLE:
            expr->set_lvalue(true);
            break;
        case iFUNCTN:
            // Not an l-value.
            break;
        case iTYPENAME:
            if (!allow_types) {
                report(expr, 174) << decl->name();
                return false;
            }
            break;
        case iCONSTEXPR:
            val.set_constval(decl->ConstVal());
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
        if (!CheckRvalue(expr))
            return false;
    }

    Expr* last = comma->exprs().back();
    if (comma->exprs().size() > 1 && last->lvalue()) {
        last = new RvalueExpr(last);
        comma->exprs().back() = last;
    }

    for (size_t i = 0; i < comma->exprs().size() - 1; i++) {
        auto expr = comma->exprs().at(i);
        if (!expr->HasSideEffects())
            report(expr, 231) << i;
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

    Type* last_type = nullptr;
    for (const auto& expr : array->exprs()) {
        if (!CheckExpr(expr))
            return false;

        const auto& val = expr->val();
        if (val.ident != iCONSTEXPR) {
            report(expr, 8);
            return false;
        }
        if (!last_type) {
            last_type = val.type();
            continue;
        }

        TypeChecker tc(array, last_type, val.type(), TypeChecker::Generic);
        if (!tc.Check())
            return false;
    }

    auto& val = array->val();
    val.ident = iEXPRESSION;
    val.set_type(types_->defineArray(last_type, (int)array->exprs().size()));
    return true;
}

bool Semantics::CheckIndexExpr(IndexExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto base = expr->base();
    auto index = expr->index();
    if (!CheckRvalue(base))
        return false;
    if (base->lvalue() && base->val().ident == iACCESSOR)
        base = expr->set_base(new RvalueExpr(base));

    const auto& base_val = base->val();
    if (!base_val.type()->isArray()) {
        report(index, 28);
        return false;
    }

    ArrayType* array = base_val.type()->to<ArrayType>();

    if (index) {
        if (!CheckRvalue(index))
            return false;
        if (!CheckScalarType(index))
            return false;
        if (index->lvalue())
            index = expr->set_index(new RvalueExpr(index));

        auto idx_type = index->val().type();
        if (!IsValidIndexType(idx_type)) {
            report(index, 77) << idx_type;
            return false;
        }

        const auto& index_val = index->val();
        if (index_val.ident == iCONSTEXPR) {
            if (!array->isCharArray()) {
                /* normal array index */
                if (index_val.constval() < 0 ||
                    (array->size() != 0 && array->size() <= index_val.constval()))
                {
                    report(index, 32) << base_val.sym->name(); /* array index out of bounds */
                    return false;
                }
            } else {
                /* character index */
                if (index_val.constval() < 0 ||
                    (array->size() != 0 && array->size() <= index_val.constval()))
                {
                    report(index, 32) << base_val.sym->name(); /* array index out of bounds */
                    return false;
                }
            }
        }
    }

    auto& out_val = expr->val();
    out_val = base_val;

    if (array->inner()->isArray()) {
        // Note: Intermediate arrays are not l-values.
        out_val.ident = iEXPRESSION;
        out_val.set_type(array->inner());
        return true;
    }

    /* set type to fetch... INDIRECTLY */
    if (array->isCharArray())
        out_val.set_slice(iARRAYCHAR, base_val.sym);
    else
        out_val.set_slice(iARRAYCELL, base_val.sym);
    out_val.set_type(array->inner());

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
    auto sym = expr->decl();
    assert(sym->ident() == iVARIABLE);

    auto& val = expr->val();
    val.ident = sym->ident();
    val.sym = sym;
    val.set_type(sym->type());
    expr->set_lvalue(true);
    return true;
}

bool Semantics::CheckNullExpr(NullExpr* expr) {
    auto& val = expr->val();
    val.set_constval(0);
    val.set_type(types_->type_null());
    return true;
}

bool Semantics::CheckTaggedValueExpr(TaggedValueExpr* expr) {
    auto& val = expr->val();
    val.set_type(expr->type());
    val.set_constval(expr->value());
    return true;
}

bool Semantics::CheckStringExpr(StringExpr* expr) {
    auto& val = expr->val();
    val.ident = iEXPRESSION;
    val.set_type(types_->defineArray(types_->type_char(), (cell)expr->text()->length() + 1));
    return true;
}

bool Semantics::CheckFieldAccessExpr(FieldAccessExpr* expr, bool from_call) {
    AutoErrorPos aep(expr->pos());

    auto base = expr->base();
    if (auto sym_expr = base->as<SymbolExpr>()) {
        if (!CheckSymbolExpr(sym_expr, true))
            return false;
    } else {
        if (!CheckRvalue(base))
            return false;
    }

    int token = expr->token();
    if (token == tDBLCOLON)
        return CheckStaticFieldAccessExpr(expr);

    const auto& base_val = base->val();
    switch (base_val.ident) {
        case iFUNCTN:
            report(expr, 107);
            return false;
        default:
            if (base_val.type()->isArray()) {
                report(expr, 96) << expr->name() << "type" << "array";
                return false;
            }
            break;
    }

    auto& val = expr->val();
    if (base_val.ident == iTYPENAME) {
        auto map = MethodmapDecl::LookupMethodmap(base_val.sym);
        auto member = map ? map->FindMember(expr->name()) : nullptr;
        if (!member || !member->as<MethodmapMethodDecl>()) {
            report(expr, 444) << base_val.sym->name() << expr->name();
            return false;
        }
        auto method = member->as<MethodmapMethodDecl>();
        if (!method->is_static()) {
            report(expr, 176) << method->decl_name() << map->name();
            return false;
        }
        expr->set_resolved(method);
        val.ident = iFUNCTN;
        val.sym = method;
        markusage(method, uREAD);
        return true;
    }

    Type* base_type = base_val.type();
    if (auto es = base_type->asEnumStruct())
        return CheckEnumStructFieldAccessExpr(expr, base_type, es, from_call);
    if (base_type->isReference())
        base_type = base_type->inner();

    auto map = base_type->asMethodmap();
    if (!map) {
        report(expr, 104) << base_val.type();
        return false;
    }

    auto member = map->FindMember(expr->name());
    if (!member) {
        report(expr, 105) << map->name() << expr->name();
        return false;
    }

    if (auto prop = member->as<MethodmapPropertyDecl>()) {
        // This is the only scenario in which we need to compute a load of the
        // base address. Otherwise, we're only accessing the type.
        if (base->lvalue())
            base = expr->set_base(new RvalueExpr(base));
        val.set_type(prop->property_type());
        val.set_accessor(prop);
        expr->set_lvalue(true);
        return true;
    }

    auto method = member->as<MethodmapMethodDecl>();
    if (method->is_static()) {
        report(expr, 177) << method->decl_name() << map->name() << method->decl_name();
        return false;
    }
    expr->set_resolved(method);

    if (!from_call) {
        report(expr, 50);
        return false;
    }

    val.ident = iFUNCTN;
    val.sym = method;
    markusage(method, uREAD);
    return true;
}

void
FieldAccessExpr::ProcessUses(SemaContext& sc)
{
    base_->MarkAndProcessUses(sc);
}

FunctionDecl* Semantics::BindCallTarget(CallExpr* call, Expr* target) {
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

            auto resolved = expr->resolved();
            if (auto method = resolved->as<MethodmapMethodDecl>()) {
                auto map = method->parent()->as<MethodmapDecl>();
                if (map->ctor() == method) {
                    report(call, 84) << method->parent()->name();
                    return nullptr;
                }
            }

            auto method = resolved->as<MemberFunctionDecl>();
            assert(resolved->as<LayoutFieldDecl>() || method);

            auto base = expr->base();
            if (base->lvalue())
                base = expr->set_base(new RvalueExpr(base));
            if (resolved->as<LayoutFieldDecl>() || !method->is_static())
                call->set_implicit_this(base);
            return val.sym->as<FunctionDecl>()->canonical();
        }
        case ExprKind::SymbolExpr: {
            call->set_implicit_this(nullptr);

            auto expr = target->to<SymbolExpr>();
            auto decl = expr->decl();
            if (auto mm = decl->as<MethodmapDecl>()) {
                if (!mm->ctor()) {
                    // Immediately fatal - no function to call.
                    report(target, 172) << decl->name();
                    return nullptr;
                }
                if (mm->nullable()) {
                    // Keep going, this is basically a style thing.
                    report(target, 170) << decl->name();
                    return nullptr;
                }
                return mm->ctor();
            }
            auto fun = decl->as<FunctionDecl>();
            if (!fun) {
                report(target, 12);
                return nullptr;
            }

            fun = fun->canonical();
            if (!fun->is_native() && !fun->impl()) {
                report(target, 4) << decl->name();
                return nullptr;
            }
            return fun;
        }
        default:
            report(target, 12);
            return nullptr;
    }
}

FunctionDecl* Semantics::BindNewTarget(Expr* target) {
    AutoErrorPos aep(target->pos());

    switch (target->kind()) {
        case ExprKind::SymbolExpr: {
            auto expr = target->to<SymbolExpr>();
            auto decl = expr->decl();

            auto mm = MethodmapDecl::LookupMethodmap(decl);
            if (!mm) {
                report(expr, 116) << decl->name();
                return nullptr;
            }

            if (!mm->nullable()) {
                report(expr, 171) << mm->name();
                return nullptr;
            }
            if (!mm->ctor()) {
                report(expr, 172) << mm->name();
                return nullptr;
            }
            return mm->ctor();
        }
    }
    return nullptr;
}

bool Semantics::CheckEnumStructFieldAccessExpr(FieldAccessExpr* expr, Type* type, EnumStructDecl* root,
                                               bool from_call)
{
    expr->set_resolved(FindEnumStructField(type, expr->name()));

    auto field_decl = expr->resolved();
    if (!field_decl) {
        report(expr, 105) << type << expr->name();
        return false;
    }

    auto& val = expr->val();
    if (auto fun = field_decl->as<MemberFunctionDecl>()) {
        if (!from_call) {
            report(expr, 76);
            return false;
        }

        val.ident = iFUNCTN;
        val.sym = fun;
        markusage(val.sym, uREAD);
        return true;
    }

    auto field = field_decl->as<LayoutFieldDecl>();
    assert(field);

    Type* field_type = field->type_info().type;

    val.set_type(field_type);
    if (field_type->isArray()) {
        // Already an r-value.
        val.ident = iEXPRESSION;
    } else {
        // Need LOAD_I to convert to r-value.
        val.ident = iARRAYCELL;
        expr->set_lvalue(true);
    }
    return true;
}

bool Semantics::CheckStaticFieldAccessExpr(FieldAccessExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto base = expr->base();
    const auto& base_val = base->val();
    if (base_val.ident != iTYPENAME) {
        report(expr, 108);
        return false;
    }

    Type* type = base_val.type();
    Decl* field = FindEnumStructField(type, expr->name());
    if (!field) {
        report(expr, 105) << type << expr->name();
        return false;
    }

    auto fd = field->as<LayoutFieldDecl>();
    if (!fd) {
        report(expr, 445) << field->name();
        return false;
    }

    expr->set_resolved(field);

    auto& val = expr->val();
    val.set_constval(fd->offset());
    val.set_type(types_->type_int());
    return true;
}

bool Semantics::CheckSizeofExpr(SizeofExpr* expr) {
    AutoErrorPos aep(expr->pos());

    Expr* child = expr->child();
    if (auto sym = child->as<SymbolExpr>()) {
        if (!CheckSymbolExpr(sym, true))
            return false;
    } else {
        if (!CheckExpr(child))
            return false;
    }

    auto& val = expr->val();
    val.set_type(types_->type_int());

    const auto& cv = child->val();
    switch (cv.ident) {
        case iARRAYCELL:
        case iVARIABLE:
        case iEXPRESSION:
            if (auto es = cv.type()->asEnumStruct()) {
                val.set_constval(es->array_size());
            } else if (auto array = cv.type()->as<ArrayType>()) {
                if (!array->size()) {
                    report(child, 163);
                    return false;
                }
                val.set_constval(array->size());
            } else if (cv.ident == iEXPRESSION) {
                report(child, 72);
                return false;
            } else {
                val.set_constval(1);
                report(expr, 252);
            }
            return true;

        case iARRAYCHAR:
            report(expr, 252);
            val.set_constval(1);
            return true;

        case iTYPENAME: {
            auto es = cv.sym->as<EnumStructDecl>();
            if (!es) {
                report(child, 72);
                return false;
            }
            val.set_constval(es->array_size());
            return true;
        }

        case iCONSTEXPR: {
            auto access = child->as<FieldAccessExpr>();
            if (!access || access->token() != tDBLCOLON) {
                report(child, 72);
                return false;
            }
            auto field = access->resolved()->as<LayoutFieldDecl>();
            if (auto array = field->type()->as<ArrayType>())
                val.set_constval(array->size());
            else if (auto es = field->type()->asEnumStruct())
                val.set_constval(es->array_size());
            else
                val.set_constval(1);
            return true;
        }

        default:
            report(child, 72);
            return false;
    }
}

CallUserOpExpr::CallUserOpExpr(const UserOperation& userop, Expr* expr)
  : EmitOnlyExpr(ExprKind::CallUserOpExpr, expr->pos()),
    userop_(userop),
    expr_(expr)
{
    val_.ident = iEXPRESSION;
    val_.set_type(userop_.sym->type());
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
    FunctionDecl* fun;
    if (call->token() == tNEW)
        fun = BindNewTarget(call->target());
    else
        fun = BindCallTarget(call, call->target());
    if (!fun)
        return false;

    assert(fun->canonical() == fun);

    call->set_fun(fun);

    if (fun->return_type()->isArray() || fun->return_type()->isEnumStruct()) {
        // We need to know the size of the returned array. Recursively analyze
        // the function.
        if (fun->is_analyzing() || !CheckFunctionDecl(fun)) {
            report(call, 411);
            return false;
        }
    }

    markusage(fun, uREAD);

    auto& val = call->val();
    val.ident = iEXPRESSION;
    val.set_type(fun->return_type());
    if (fun->return_array())
        NeedsHeapAlloc(call);

    // We don't have canonical decls yet, so get the one attached to the symbol.
    if (fun->deprecate())
        report(call, 234) << fun->name() << fun->deprecate();

    ParamState ps;

    unsigned int nargs = 0;
    unsigned int argidx = 0;
    auto& arglist = fun->args();
    if (call->implicit_this()) {
        if (arglist.empty()) {
            report(call->implicit_this(), 92);
            return false;
        }
        Expr* param = CheckArgument(call, arglist[0], call->implicit_this(), &ps, 0);
        if (!param)
            return false;
        ps.argv[0] = param;
        nargs++;
        argidx++;
    }

    bool namedparams = false;
    for (const auto& param : call->args()) {
        unsigned int argpos;
        if (auto named = param->as<NamedArgExpr>()) {
            int pos = fun->FindNamedArg(named->name);
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
        auto result = CheckArgument(call, arglist[argidx], param, &ps, argpos);
        if (!result)
            return false;
        ps.argv[argpos] = result;

        nargs++;

        // Don't iterate past terminators (0 or varargs).
        if (!arglist[argidx]->type_info().is_varargs)
            argidx++;
    }

    if (!sc_->func()) {
        report(call, 10);
        return false;
    }

    // Check for missing or invalid extra arguments, and fill in default
    // arguments.
    for (unsigned int argidx = 0; argidx < arglist.size(); argidx++) {
        auto arg = arglist[argidx];
        if (arg->type_info().is_varargs)
            break;
        if (argidx >= ps.argv.size() || !ps.argv[argidx]) {
            auto result = CheckArgument(call, arg, nullptr, &ps, argidx);
            if (!result)
                return false;
            ps.argv[argidx] = result;
        }

        Expr* expr = ps.argv[argidx];
        if (expr->as<DefaultArgExpr>() && !IsReferenceType(iVARIABLE, arg->type())) {
            UserOperation userop;
            if (find_userop(*sc_, 0, arg->default_value()->type, arg->type(), 2, nullptr,
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

Expr* Semantics::CheckArgument(CallExpr* call, ArgDecl* arg, Expr* param,
                               ParamState* ps, unsigned int pos)
{
    while (pos >= ps->argv.size())
        ps->argv.push_back(nullptr);

    unsigned int visual_pos = call->implicit_this() ? pos : pos + 1;

    if (!param || param->as<DefaultArgExpr>()) {
        if (arg->type_info().is_varargs) {
            report(call, 92); // argument count mismatch
            return nullptr;
        }
        if (!arg->default_value()) {
            report(call, 34) << visual_pos; // argument has no default value
            return nullptr;
        }

        if (!param)
            param = new DefaultArgExpr(call->pos(), arg);
        else
            param->as<DefaultArgExpr>()->set_arg(arg);

        if (arg->type()->isReference() ||
            ((arg->type()->isArray() || arg->type()->isEnumStruct()) &&
             !arg->type_info().is_const && arg->default_value()->array))
        {
            NeedsHeapAlloc(param);
        }

        // The rest of the code to handle default values is in DoEmit.
        return param;
    }

    if (param != call->implicit_this()) {
        if (!CheckRvalue(param))
            return nullptr;
    }

    AutoErrorPos aep(param->pos());

    bool handling_this = call->implicit_this() && (pos == 0);

    if (param->val().ident == iACCESSOR) {
        if (!CheckRvalue(param->pos(), param->val()))
            return nullptr;
        param = new RvalueExpr(param);
    }

    const auto* val = &param->val();
    bool lvalue = param->lvalue();
    if (arg->type_info().is_varargs) {
        assert(!handling_this);

        // Always pass by reference.
        if (val->ident == iVARIABLE) {
            if (val->sym->is_const() && !arg->type_info().is_const) {
                // Treat a "const" variable passed to a function with a
                // non-const "variable argument list" as a constant here.
                if (!lvalue) {
                    report(param, 22); // need lvalue
                    return nullptr;
                }
                NeedsHeapAlloc(param);
            } else if (!lvalue) {
                NeedsHeapAlloc(param);
            }
        } else if (val->ident == iCONSTEXPR || val->ident == iEXPRESSION) {
            NeedsHeapAlloc(param);
        }
        if (!checktag_string(arg->type(), val) && !checktag(arg->type(), val->type()))
            report(param, 213) << arg->type() << val->type();
    } else if (arg->type()->isReference()) {
        assert(!handling_this);

        if (!lvalue || val->ident == iARRAYCHAR) {
            report(param, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }
        if (val->sym && val->sym->is_const() && !arg->type_info().is_const) {
            report(param, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }
        checktag(arg->type()->inner(), val->type());
    } else if (arg->type()->isArray()) {
        // If the input type is an index into an array, create an implicit
        // array type to represent the slice.
        Type* type = val->type();
        if ((val->ident == iARRAYCELL || val->ident == iARRAYCHAR) && !type->isEnumStruct())
            type = types_->defineArray(type, 0);

        TypeChecker tc(param, arg->type(), type, TypeChecker::Argument);
        if (!tc.Coerce())
            return nullptr;

        if (val->sym && val->sym->is_const() && !arg->type_info().is_const) {
            report(param, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }
    } else {
        if (lvalue) {
            param = new RvalueExpr(param);
            val = &param->val();
        }

        // Do not allow user operators to transform |this|.
        UserOperation userop;
        if (!handling_this &&
            find_userop(*sc_, 0, arg->type(), val->type(), 2, nullptr, &userop))
        {
            param = new CallUserOpExpr(userop, param);
            val = &param->val();
        }

        TypeChecker tc(param, arg->type(), val->type(), TypeChecker::Argument);
        if (!tc.Coerce())
            return nullptr;
    }
    return param;
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
    if (fun_)
        fun_->set_retvalue_used();
}

bool Semantics::CheckStaticAssertStmt(StaticAssertStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckExpr(expr))
        return false;

    // :TODO: insert coercion to bool.
    cell value;
    Type* type;
    if (!expr->EvalConst(&value, &type)) {
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

bool Semantics::CheckNewArrayExprForArrayInitializer(NewArrayExpr* na) {
    if (na->analyzed())
        return na->analysis_result();

    na->set_analysis_result(false);

    auto& val = na->val();
    val.ident = iEXPRESSION;

    PoolList<int> dims;
    for (auto& expr : na->exprs()) {
        if (!CheckRvalue(expr))
            return false;
        if (expr->lvalue())
            expr = new RvalueExpr(expr);

        const auto& v = expr->val();
        if (IsLegacyEnumType(sc_->scope(), v.type())) {
            report(expr, 153);
            return false;
        }
        if (!IsValidIndexType(v.type())) {
            report(expr, 77) << v.type();
            return false;
        }
        if (v.ident == iCONSTEXPR && v.constval() <= 0) {
            report(expr, 9);
            return false;
        }
        dims.emplace_back(0);
    }
    assert(na->type()->isArray());

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

bool Semantics::IsIncluded(Decl* sym) {
    const auto fileno = cc_.sources()->GetSourceFileIndex(sym->pos());
    return !cc_.sources()->opened_files()[fileno]->is_main_file();
}

bool Semantics::IsIncludedStock(VarDeclBase* sym) {
    return sym->vclass() == sGLOBAL && sym->is_stock() && IsIncluded(sym);
}

/*  testsymbols - test for unused local or global variables
 *
 *  "Public" functions are excluded from the check, since these
 *  may be exported to other object modules.
 *
 *  The function returns whether there is an "entry" point for the file.
 *  This flag will only be 1 when browsing the global symbol table.
 */
bool Semantics::TestSymbol(Decl* sym, bool testconst) {
    bool entry = false;
    switch (sym->ident()) {
        case iFUNCTN:
        {
            auto canonical = sym->as<FunctionDecl>()->canonical();
            if (canonical->is_public() || canonical->name()->str() == uMAINFUNC)
                entry = true; /* there is an entry point */
            if (!(canonical->maybe_used() || canonical->is_live()) &&
                !(canonical->is_native() || canonical->is_stock() || canonical->is_public()) &&
                canonical->impl())
            {
                /* symbol isn't used ... (and not public/native/stock) */
                report(canonical, 203) << canonical->name();
                return entry;
            }

            // Functions may be used as callbacks, in which case we don't check
            // whether their arguments were used or not. We can't tell this until
            // the scope is exiting, which is right here, so peek at the arguments
            // for the function and check now.
            if (canonical->body()) {
                CheckFunctionReturnUsage(canonical);
                if (canonical->scope() && !canonical->is_callback())
                    TestSymbols(canonical->scope(), true);
            }
            break;
        }
        case iCONSTEXPR: {
            auto var = sym->as<VarDeclBase>();
            if (testconst && var && !var->is_read())
                report(var, 203) << var->name(); /* symbol isn't used: ... */
            break;
        }
        case iTYPENAME:
            // Ignore usage on methodmaps and enumstructs.
            break;
        default: {
            auto var = sym->as<VarDeclBase>();
            /* a variable */

            // We ignore variables that are marked as public or stock that was included.
            if (var->is_public() || IsIncludedStock(var))
                break;

            if (!var->is_used()) {
                report(sym, 203) << sym->name(); /* symbol isn't used (and not public/stock) */
            } else if (!var->is_read()) {
                report(sym, 204) << sym->name(); /* value assigned to symbol is never used */
            }
        }
    }
    return entry;
}

bool Semantics::TestSymbols(SymbolScope* root, bool testconst) {
    bool entry = false;
    root->ForEachSymbol([&](Decl* decl) -> void {
        entry |= TestSymbol(decl, testconst);
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

    auto fun = sc_->func();

    auto expr = stmt->expr();
    if (!expr) {
        if (fun->MustReturnValue())
            ReportFunctionReturnError(fun);
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

    if (!CheckRvalue(expr))
        return false;

    if (expr->lvalue())
        expr = stmt->set_expr(new RvalueExpr(expr));

    AutoErrorPos aep(expr->pos());

    if (fun->return_type()->isVoid()) {
        report(stmt, 88);
        return false;
    }

    sc_->set_returns_value();

    const auto& v = expr->val();

    // Check that the return statement matches the declared return type.
    TypeChecker tc(stmt, fun->return_type(), v.type(), TypeChecker::Return);
    if (!tc.Coerce())
        return false;

    if (v.type()->isArray() || v.type()->isEnumStruct()) {
        if (!CheckCompoundReturnStmt(stmt))
            return false;
    }
    return true;
}

bool Semantics::CheckCompoundReturnStmt(ReturnStmt* stmt) {
    FunctionDecl* curfunc = sc_->func();
    assert(curfunc == curfunc->canonical());

    const auto& val = stmt->expr()->val();

    if (auto iter = val.type()->as<ArrayType>()) {
        do {
            if (iter->size() == 0) {
                report(stmt, 128);
                return false;
            }
            iter = iter->inner()->as<ArrayType>();
        } while (iter);
    }

    if (curfunc->is_public()) {
        report(stmt, 90);
        return false;
    }

    if (!curfunc->return_array()) {
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
        auto info = new FunctionDecl::ReturnArrayInfo;
        info->hidden_address = ((cell_t)curfunc->args().size() + 3) * sizeof(cell);

        curfunc->set_return_array(info);
        curfunc->update_return_type(val.type());
    }
    return true;
}

bool Semantics::CheckNativeCompoundReturn(FunctionDecl* info) {
    auto rt = info->return_type();
    if (auto root = rt->as<ArrayType>()) {
        for (auto it = root; it; it = it->inner()->as<ArrayType>()) {
            if (it->size() == 0) {
                report(info, 39);
                return false;
            }
        }
    }

    // For native calls, the implicit arg is first, not last.
    auto rai = new FunctionDecl::ReturnArrayInfo;
    rai->hidden_address = sizeof(cell_t) * 3;
    info->set_return_array(rai);
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
    if (!CheckRvalue(expr))
        return false;

    const auto& v = expr->val();
    switch (v.ident) {
        case iFUNCTN:
            report(expr, 167) << "function";
            return false;

        case iVARIABLE:
            if (v.type()->isArray() || v.type()->isEnumStruct()) {
                report(expr, 167) << v.type();
                return false;
            }
            break;

        case iACCESSOR:
            if (v.accessor()->getter())
                markusage(v.accessor()->getter(), uREAD);
            if (v.accessor()->setter())
                markusage(v.accessor()->setter(), uREAD);
            break;
    }

    Type* type = v.type();
    if (type->isReference())
        type = type->inner();

    if (type->isInt()) {
        report(expr, 167) << "integers";
        return false;
    }

    auto map = type->asMethodmap();
    if (!map) {
        report(expr, 115) << "type" << v.type();
        return false;
    }

    for (auto iter = map; iter; iter = iter->parent()) {
        if (iter->dtor()) {
            map = iter;
            break;
        }
    }

    if (!map || !map->dtor()) {
        report(expr, 115) << "methodmap" << map->name();
        return false;
    }

    markusage(map->dtor(), uREAD);

    stmt->set_map(map);
    return true;
}

bool Semantics::CheckExitStmt(ExitStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckRvalue(expr))
        return false;
    if (expr->lvalue())
        expr = stmt->set_expr(new RvalueExpr(expr));

    if (!IsValueKind(expr->val().ident)) {
        report(expr, 106);
        return false;
    }

    AutoErrorPos aep(expr->pos());

    if (!TypeChecker::DoCoerce(types_->type_int(), expr))
        return false;
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
        if (CheckRvalue(stmt->advance()))
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
    bool tag_ok = CheckRvalue(expr);
    const auto& v = expr->val();
    if (tag_ok && v.type()->isArray())
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
            if (!CheckRvalue(expr))
                continue;

            cell value;
            Type* type;
            if (!expr->EvalConst(&value, &type)) {
                report(expr, 8);
                continue;
            }
            if (tag_ok) {
                AutoErrorPos aep(expr->pos());
                matchtag(v.type(), type, MATCHTAG_COERCE);
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

void ReportFunctionReturnError(FunctionDecl* decl) {
    if (decl->as<MemberFunctionDecl>()) {
        // This is a member function, ignore compatibility checks and go
        // straight to erroring.
        report(decl, 400) << decl->name();
        return;
    }

    // Normally we want to encourage return values. But for legacy code,
    // we allow "public int" to warn instead of error.
    //
    // :TODO: stronger enforcement when function result is used from call
    if (decl->return_type()->isInt()) {
        report(decl, 209) << decl->name();
    } else if (decl->return_type()->isEnum() || decl->return_type()->isBool() ||
               decl->return_type()->isFloat() || !decl->retvalue_used())
    {
        report(decl, 242) << decl->name();
    } else {
        report(decl, 400) << decl->name();
    }
}

bool
FunctionDecl::IsVariadic() const
{
    return !args_.empty() && args_.back()->type_info().is_varargs;
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
    SemaContext sc(*sc_, info);
    ke::SaveAndSet<SemaContext*> push_sc(&sc_, &sc);

    auto& decl = info->decl();
    {
        AutoErrorPos error_pos(info->pos());
        CheckVoidDecl(&decl, FALSE);

        if (decl.opertok)
            check_operatortag(decl.opertok, decl.type.type, decl.name->chars());
    }

    if (info->is_public() || info->is_forward()) {
        if (decl.type.dim_exprs.size() > 0)
            report(info->pos(), 141);
    }

    if (info->is_native()) {
        auto rt = info->return_type();
        if ((rt->isArray() || rt->isEnumStruct()) && !CheckNativeCompoundReturn(info))
            return false;
        return true;
    }

    auto body = info->body();
    if (!body) {
        if (info->is_native() || info->is_forward())
            return true;
        report(info->pos(), 10);
        return false;
    }

    // We never warn about unused member functions.
    if (info->as<MemberFunctionDecl>())
        maybe_used_.emplace_back(info);

    // We never warn about unused stock functions.
    if (info->is_stock())
        maybe_used_.emplace_back(info);

    auto fwd = info->prototype();
    if (fwd && fwd->deprecate() && !info->is_stock())
        report(info->pos(), 234) << info->name() << fwd->deprecate();

    bool ok = CheckStmt(body, STMT_OWNS_HEAP);

    info->set_returns_value(sc_->returns_value());
    info->set_always_returns(sc_->always_returns());

    if (!info->returns_value()) {
        if (fwd && fwd->return_type()->isVoid() && decl.type.type->isInt() &&
            !decl.type.is_new)
        {
            // We got something like:
            //    forward void X();
            //    public X()
            //
            // Switch our decl type to void.
            decl.type.set_type(types_->type_void());
        }
    }

    // Make sure that a public return type matches the forward (if any).
    if (fwd && info->is_public()) {
        if (fwd->return_type() != decl.type.type)
            report(info->pos(), 180) << fwd->return_type() << decl.type.type;
    }

    // For globals, we test arguments in a later pass, since we need to know
    // which functions get used as callbacks in order to emit a warning. The
    // same is true for return value usage: we don't know how to handle
    // compatibility edge cases until we've discovered all callers.
    if (info->as<MemberFunctionDecl>()) {
        CheckFunctionReturnUsage(info);
        if (info->scope())
            TestSymbols(info->scope(), true);
    }

    if (info->is_public())
        cc_.publics().emplace(info->canonical());
    return ok;
}

void Semantics::CheckFunctionReturnUsage(FunctionDecl* info) {
    if (info->returns_value() && info->always_returns())
        return;

    if (info->MustReturnValue())
        ReportFunctionReturnError(info);

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

    SemaContext sc(outer_sc, this);

    for (const auto& arg : args_)
        arg->ProcessUses(sc);

    body_->ProcessUses(sc);
}

bool Semantics::CheckPragmaUnusedStmt(PragmaUnusedStmt* stmt) {
    for (const auto& decl : stmt->symbols()) {
        decl->set_is_read();

        if (decl->ident() == iVARIABLE) {
            decl->set_is_written();
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
        if (prop->getter())
            ok &= CheckFunctionDecl(prop->getter());
        if (prop->setter())
            ok &= CheckFunctionDecl(prop->setter());
    }
    for (const auto& method : decl->methods())
        ok &= CheckStmt(method);
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

void MethodmapDecl::ProcessUses(SemaContext& sc) {
    for (const auto& prop : properties_)
        prop->ProcessUses(sc);
    for (const auto& method : methods_)
        method->ProcessUses(sc);
}

void MethodmapPropertyDecl::ProcessUses(SemaContext& sc) {
    if (getter_)
        getter_->ProcessUses(sc);
    if (setter_)
        setter_->ProcessUses(sc);
}

void Semantics::CheckVoidDecl(const typeinfo_t* type, int variable) {
    if (!type->type->isVoid())
        return;

    if (variable) {
        report(144);
        return;
    }
}

void Semantics::CheckVoidDecl(const declinfo_t* decl, int variable) {
    CheckVoidDecl(&decl->type, variable);
}

int argcompare(ArgDecl* a1, ArgDecl* a2) {
    int result = 1;

    if (result)
        result = a1->type_info().is_const == a2->type_info().is_const; /* "const" flag */
    if (result)
        result = a1->type() == a2->type();
    if (result)
        result = !!a1->default_value() == !!a2->default_value(); /* availability of default value */
    if (auto a1_def = a1->default_value()) {
        auto a2_def = a2->default_value();
        if (a1->type()->isArray()) {
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
            result = a1_def->type == a2_def->type;
    }
    return result;
}

bool IsLegacyEnumType(SymbolScope* scope, Type* type) {
    if (!type->isEnum())
        return false;
    auto decl = FindSymbol(scope, type->declName());
    if (!decl)
        return false;
    if (auto ed = decl->as<EnumDecl>())
        return !ed->mm();
    return false;
}

void fill_arg_defvalue(CompileContext& cc, ArgDecl* decl) {
    auto def = new DefaultArg();
    def->type = decl->type();

    if (auto expr = decl->init_rhs()->as<SymbolExpr>()) {
        Decl* sym = expr->decl();
        assert(sym->vclass() == sGLOBAL || sym->vclass() == sSTATIC);
        assert(sym->as<VarDecl>());

        def->sym = sym->as<VarDecl>();
    } else {
        auto array = cc.NewDefaultArrayData();
        BuildCompoundInitializer(decl, array, 0);

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

// Determine the set of live functions.
void Semantics::DeduceLiveness() {
    std::vector<FunctionDecl*> work;
    std::unordered_set<FunctionDecl*> seen;

    // The root set is all public functions.
    for (const auto& decl : cc_.publics()) {
        assert(!decl->is_native());
        assert(decl->is_public());

        decl->set_is_live();

        seen.emplace(decl);
        work.emplace_back(decl);
    }

    // Traverse referrers to find the transitive set of live functions.
    while (!work.empty()) {
        FunctionDecl* live = ke::PopBack(&work);
        if (!live->refers_to())
            continue;

        for (const auto& other : *live->refers_to()) {
            other->set_is_live();
            if (!seen.count(other)) {
                seen.emplace(other);
                work.emplace_back(other);
            }
        }
    }
}

void Semantics::DeduceMaybeUsed() {
    std::vector<FunctionDecl*> work;
    std::unordered_set<FunctionDecl*> seen;

    while (!maybe_used_.empty()) {
        auto decl = ke::PopBack(&maybe_used_);
        decl->set_maybe_used();
        seen.emplace(decl);
        work.emplace_back(decl);
    }

    while (!work.empty()) {
        FunctionDecl* live = ke::PopBack(&work);
        if (!live->refers_to())
            continue;

        for (const auto& other : *live->refers_to()) {
            other->set_maybe_used();
            if (!seen.count(other)) {
                seen.emplace(other);
                work.emplace_back(other);
            }
        }
    }
}

bool Semantics::CheckRvalue(Expr* expr) {
    if (!CheckExpr(expr))
        return false;
    return CheckRvalue(expr->pos(), expr->val());
}

bool Semantics::CheckRvalue(const token_pos_t& pos, const value& val) {
    if (auto accessor = val.accessor()) {
        if (!accessor->getter()) {
            report(pos, 149) << accessor->name();
            return false;
        }
    }
    return true;
}

bool Semantics::IsThisAtom(sp::Atom* atom) {
    if (!this_atom_)
        this_atom_ = cc_.atom("this");
    return atom == this_atom_;
}

void DeleteStmt::ProcessUses(SemaContext& sc) {
    expr_->MarkAndProcessUses(sc);
    markusage(map_->dtor(), uREAD);
}

} // namespace cc
} // namespace sp
