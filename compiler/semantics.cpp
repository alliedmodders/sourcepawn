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
#include <vector>

#include <amtl/am-raii.h>
#include "array-helpers.h"
#include "code-generator.h"
#include "errors.h"
#include "expressions.h"
#include "ir.h"
#include "lexer.h"
#include "parse-node.h"
#include "sctracker.h"
#include "symbols.h"
#include "type-checker.h"
#include "value-inl.h"

namespace sp {
namespace cc {

Semantics::Semantics(CompileContext& cc, std::shared_ptr<ir::Module> mod)
  : cc_(cc),
    types_(cc.types()),
    mod_(std::move(mod))
{}

bool Semantics::Analyze(ParseTree* tree) {
    global_sc_.emplace(this);
    ke::SaveAndSet<SemaContext*> push_sc(&sc_, &global_sc_.value());

    AutoCountErrors errors;
    if (!CheckStmtList(tree->stmts()) || !errors.ok())
        return false;

    DeduceLiveness();
    DeduceMaybeUsed();

    // Remove functions that are not live.
    ke::EraseIf(&mod_->functions(), [](ir::Function* fun) -> bool {
        return !fun->is_live();
    });

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

    assert(errors.ok());

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

    std::vector<uint32_t> temp_slots;
    ke::SaveAndSet<std::vector<uint32_t>*> push_temp_slots(&temp_slots_, &temp_slots);

    auto cleanup = ke::MakeScopeGuard([&, this]() {
        if (flags & STMT_OWNS_HEAP)
            AssignHeapOwnership(stmt);
        while (!temp_slots.empty())
            sc_->FreeTempSlot(ke::PopBack(&temp_slots));
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
    // Always map the variable so that symbol binding works.
    ir::Value* init;
    bool ok = CheckVarDeclCommon(decl, &init);

    // :TODO: remove ident
    // Constants are checked during binding.
    if (decl->ident() == iCONSTEXPR)
        return true;

    auto def = new ir::Variable(decl, init);
    if (fun_) {
        assert(sc_->local_vars().find(decl) == sc_->local_vars().end());
        ir_->add(def);
        sc_->local_vars().emplace(decl, def);
    } else {
        assert(global_vars_.find(decl) == global_vars_.end());
        mod_->globals().emplace_back(def);
        global_vars_.emplace(decl, def);
    }
    return ok;
}

bool Semantics::CheckVarDeclCommon(VarDeclBase* decl, ir::Value** out_init) {
    AutoErrorPos aep(decl->pos());

    *out_init = nullptr;

    auto& ti = *decl->mutable_type_info();
    if (!ti.dim_exprs.empty()) {
        if (!ResolveArrayType(this, decl))
            return false;
    }

    auto type = decl->type();
    if (type->isArray() && (!ti.has_postdims || decl->implicit_dynamic_array())) {
        if (decl->vclass() == sGLOBAL)
            report(decl, 162);
        else if (decl->vclass() == sSTATIC)
            report(decl, 165);
    }

    // :TODO: check arrays dont have pstructs
    // :TODO: check no arg if pstruct
    // :TODO: supply init
    if (type->isPstruct())
        return CheckPstructDecl(decl);

    if (decl->as<ArgDecl>()) {
        if (ti.is_varargs)
            markusage(decl, uREAD);
    } else {
        if (decl->is_public())
            decl->set_is_read();

        if (ti.is_const && !decl->init() && !decl->is_public() && decl->ident() != iCONSTEXPR)
            report(decl->pos(), 251);
    }

    if (type->isArray() || type->isEnumStruct()) {
        if (!CheckArrayDeclaration(decl, out_init))
            return false;
        if (decl->vclass() == sLOCAL)
            pending_heap_allocation_ = true;
    } else if (auto rhs = decl->init_rhs()) {
        ir::Value* init;
        if ((init = CheckRvalue(rhs)) == nullptr)
            return false;

        if (decl->vclass() != sLOCAL) {
            if (!init->as<ir::Const>()) {
                assert(false);
#if 0
                if (decl->vclass() == sARGUMENT && init_rhs->is(ExprKind::SymbolExpr))
                    return true;
#endif
                report(init->pn()->pos(), 8);
            }
        }
        *out_init = init;
    }
    return true;
}

bool Semantics::CheckPstructDecl(VarDeclBase* decl) {
    decl->mutable_type_info()->is_const = true;

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
            (void)at;
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
    if (field->value->as<StringExpr>()) {
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

ir::Value* Semantics::CheckExpr(Expr* expr, ExprFlags flags) {
    AutoErrorPos aep(expr->pos());
    switch (expr->kind()) {
        case ExprKind::UnaryExpr:
            return CheckUnaryExpr(expr->to<UnaryExpr>());
        case ExprKind::IncDecExpr:
            return CheckIncDecExpr(expr->to<IncDecExpr>(), flags);
        case ExprKind::BinaryExpr:
            return CheckBinaryExpr(expr->to<BinaryExpr>());
        case ExprKind::LogicalExpr:
            return CheckLogicalExpr(expr->to<LogicalExpr>());
        case ExprKind::ChainedCompareExpr:
            return CheckChainedCompareExpr(expr->to<ChainedCompareExpr>());
        case ExprKind::TernaryExpr:
            return CheckTernaryExpr(expr->to<TernaryExpr>());
        case ExprKind::CastExpr:
            return CheckCastExpr(expr->to<CastExpr>(), flags);
        case ExprKind::SymbolExpr:
            return CheckSymbolExpr(expr->to<SymbolExpr>(), flags);
        case ExprKind::CommaExpr:
            return CheckCommaExpr(expr->to<CommaExpr>(), flags);
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
            return CheckFieldAccessExpr(expr->to<FieldAccessExpr>(), flags);
        case ExprKind::CallExpr:
            return CheckCallExpr(expr->to<CallExpr>());
        case ExprKind::NewArrayExpr:
            return CheckNewArrayExpr(expr->to<NewArrayExpr>());
        case ExprKind::TaggedValueExpr:
            return CheckTaggedValueExpr(expr->to<TaggedValueExpr>());
        case ExprKind::SizeofExpr:
            return CheckSizeofExpr(expr->to<SizeofExpr>());
        case ExprKind::NamedArgExpr:
            return CheckExpr(expr->to<NamedArgExpr>()->expr(), flags);
        default:
            assert(false);
            report(expr, 420) << (int)expr->kind();
            return nullptr;
    }
}

CompareOp::CompareOp(const token_pos_t& pos, int token, Expr* expr)
  : pos(pos),
    token(token),
    expr(expr),
    oper_tok(GetOperToken(token))
{
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

bool Semantics::CheckScalarType(Expr* expr, QualType type) {
    if (type->isArray() ||
        type->isEnumStruct() ||
        type->isReference() ||
        type->isVoid())
    {
        report(expr, 454) << type;
        return false;
    }
    return true;
}

ir::Value* Semantics::AnalyzeForTest(Expr* expr) {
    ir::Value* val = CheckRvalue(expr);
    if (!val)
        return nullptr;
    if (!CheckScalarType(expr, val->type()))
        return nullptr;

    if (!val->type()->isInt() && !val->type()->isBool()) {
        if (auto op = MaybeCallUserOp(expr, '!', val, nullptr))
            val = op;
    }

    if (auto cv = val->as<ir::Const>()) {
        if (!sc_->preprocessing()) {
            if (cv->value())
                report(expr, 206);
            else
                report(expr, 205);
        }
    } else if (val->as<ir::FunctionRef>()) {
        report(expr, 249);
    }

    return val;
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

void RvalueExpr::ProcessUses(SemaContext& sc) {
    expr_->MarkAndProcessUses(sc);
}

ir::Value* Semantics::CheckUnaryExpr(UnaryExpr* unary) {
    AutoErrorPos aep(unary->pos());

    ir::Value* val = CheckRvalue(unary->expr());
    if (!val)
        return nullptr;
    if (!CheckScalarType(unary, val->type()))
        return nullptr;

    // :TODO: check for invalid types

    UserOperation userop;
    switch (unary->token()) {
        case '~': {
            if (val->type()->isFloat()) {
                report(unary, 453) << "~" << val->type();
                return nullptr;
            }

            if (auto cv = val->as<ir::Const>())
                return new ir::Const(unary, cv->type(), ~cv->value());

            return new ir::UnaryOp(unary, val->type(), val);
        }
        case '!': {
            auto type = types_->get_bool();

            if (auto op = MaybeCallUserOp(unary, '!', val, nullptr))
                return op;

            if (auto cv = val->as<ir::Const>())
                return new ir::Const(unary, type, !cv->value());

            return new ir::UnaryOp(unary, types_->get_bool(), val);
        }
        case '-': {
            // Since array initializers need constexprs, and we don't lex '-' as
            // part of a number (yet), constant fold floats as a convenience hack.
            auto cv = val->as<ir::Const>();
            if (cv && cv->type()->isFloat()) {
                float f = sp::FloatCellUnion(cv->value()).f32;
                cell_t new_value = sp::FloatCellUnion(-f).cell;
                return new ir::Const(unary, val->type(), new_value);
            }

            if (auto op = MaybeCallUserOp(unary, '-', val, nullptr))
                return op;

            if (cv)
                return new ir::Const(unary, val->type(), -cv->value());

            return new ir::UnaryOp(unary, val->type(), val);
        }
        default:
            assert(false);
            return nullptr;
    }
}

void UnaryExpr::ProcessUses(SemaContext& sc) {
    expr_->MarkAndProcessUses(sc);
}

ir::Value* Semantics::CheckIncDecExpr(IncDecExpr* incdec, ExprFlags flags) {
    AutoErrorPos aep(incdec->pos());

    auto val = CheckExpr(incdec->expr(), ExprFlags::DEFAULT);
    if (!val)
        return nullptr;
    auto lval = BindLvalue(val, uREAD | uWRITTEN);
    if (!lval)
        return nullptr;

    auto type = BuildRvalueType(lval->type());
    if (!CheckScalarType(incdec, type))
        return nullptr;

    bool used = !(flags & ExprFlags::RESULT_UNUSED);

    if (UserOp userop = FindUserOp(incdec, incdec->token(), type, QualType()); userop.target) {
        assert(!userop.swapparams);
        return new ir::IncDecUserOp(incdec, type, lval, userop.target, used);
    }

    return new ir::IncDec(incdec, type, lval, used);
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

static inline bool CanConstFoldType(QualType type) {
    return type->isInt() ||
           type->isChar() ||
           type->isBool() ||
           type->isEnum();
}

ir::Value* Semantics::CheckBinaryExpr(BinaryExpr* expr) {
    AutoErrorPos aep(expr->pos());

    int token = expr->token();
    int oper_token = GetOperToken(token);

    auto left = CheckExpr(expr->left(), ExprFlags::DEFAULT);
    if (!left)
        return nullptr;
    auto right = CheckRvalue(expr->right());
    if (!right)
        return nullptr;

    auto temp_slot = ir::Function::kInvalidSlot;
    ir::Lvalue* lval = nullptr;
    if (IsAssignOp(token)) {
        uint8_t flags = uWRITTEN;
        if (token != '=')
            flags |= uREAD;
        if ((lval = BindLvalue(left, flags)) == nullptr)
            return nullptr;

        if (!CheckAssignmentLHS(expr, lval))
            return nullptr;

        if (token != '=') {
            if (!lval->IsAddressable() || lval->HasComplexAddressCalculation() ||
                lval->HasSideEffects())
            {
                temp_slot = AllocTempSlot();
                left = new ir::Load(expr->left(), lval->type(),
                                    new ir::StackRef(expr->left(), lval->type(), temp_slot));
            } else {
                // Using the same lval twice is kind of hacky, but should be
                // safe as address calculation is constant with respect to the
                // rest of the statement.
                left = BuildRvalue(expr->left(), lval);
            }
        }
    } else {
        if ((left = BindRvalue(expr->left(), left)) == nullptr)
            return nullptr;
    }

    ir::Value* out = nullptr;

    if (token != '=') {
        if (!CheckScalarType(expr->left(), left->type()))
            return nullptr;
        if (!CheckScalarType(expr->right(), right->type()))
            return nullptr;

        out = MaybeCallUserOp(expr, oper_token, left, right);
    }

    if (!out)
        TypeChecker::DoCoerce(expr->pos(), *left->type(), *right->type(), TypeChecker::Commutative);

    if (token != '=') {
        auto left_cv = left->as<ir::Const>();
        auto right_cv = right->as<ir::Const>();
        if (left_cv && CanConstFoldType(left_cv->type()) &&
            right_cv && CanConstFoldType(right_cv->type()))
        {
            char is_bool = false;
            cell result = calc(left_cv->value(), oper_token, right_cv->value(), &is_bool);
            auto result_type = is_bool ? types_->get_bool() : left_cv->type();
            out = new ir::Const(expr, result_type, result);
        }

        if (!out) {
            QualType type;
            if (IsChainedOp(oper_token) || oper_token == tlEQ || oper_token == tlNE)
                type = types_->get_bool();
            else
                type = left->type();
            out = new ir::BinaryOp(expr, type, oper_token, left, right);
        }
    } else {
        out = right;
    }

    if (IsAssignOp(token)) {
        auto type = BuildRvalueType(lval->type());
        if (temp_slot == ir::Function::kInvalidSlot)
            out = new ir::Store(expr, type, lval, out);
        else
            out = new ir::StoreWithTemp(expr, type, lval, out, temp_slot);
    }

    return out;
}

bool Semantics::CheckAssignmentLHS(BinaryExpr* expr, ir::Lvalue* lval) {
    if (lval->type()->isCharArray()) {
        // This is a special case, assigned to a packed character in a cell
        // is permitted.
        return true;
    }

    // :TODO: is this needed? TypeChecker should cover it.
    if (auto left_array = lval->type()->as<ArrayType>()) {
        for (auto iter = left_array; iter; iter = iter->inner()->as<ArrayType>()) {
            if (!iter->size()) {
                report(expr->left(), 46);
                return false;
            }
        }
        return true;
    }

    // may not change "constant" parameters
    if (lval->type().is_const()) {
        report(expr, 22);
        return false;
    }
    if (auto prop = lval->as<ir::PropertyRef>()) {
        if (!prop->decl()->setter()) {
            report(expr, 152) << prop->decl()->name();
            return false;
        }
    }
    return true;
}

static inline void CheckSelfAssignment(BinaryExpr* expr, ir::Lvalue* lval, ir::Value* rval) {
    auto left_var = lval->as<ir::VariableRef>();
    if (!left_var)
        return;

    auto load = rval->as<ir::Load>();
    if (!load)
        return;

    auto right_var = load->lval()->as<ir::VariableRef>();
    if (!right_var)
        return;

    if (left_var->var() == right_var->var())
        report(expr, 226) << left_var->var()->decl()->name();
}

bool Semantics::CheckAssignmentRHS(BinaryExpr* expr, ir::Lvalue* lval, ir::Value* rval) {
    CheckSelfAssignment(expr, lval, rval);

    if (auto left_array = lval->type()->as<ArrayType>()) {
        (void)left_array;
        TypeChecker tc(expr, lval->type(), rval->type(), TypeChecker::Assignment);
        if (!tc.Coerce())
            return false;

        auto right_array = rval->type()->to<ArrayType>();
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
        if (rval->type()->isArray()) {
            // Hack. Special case array literals assigned to an enum struct,
            // since we don't have the infrastructure to deduce an RHS type
            // yet.
            if (!lval->type()->isEnumStruct() || !expr->right()->as<ArrayExpr>()) {
                report(expr, 6); // must be assigned to an array
                return false;
            }
            return true;
        }

#if 0
        // :TODO: assignment operator overload
#endif
    }

#if 0
    if (!expr->oper() &&
        !(lval->type()->isCharArray() || rval->type()->isCharArray()) /*
        :TODO: !expr->assignop().sym*/)
    {
        // :TODO: needed?
        if (left_val.type()->isArray() &&
            ((left_val.type()->isChar() && !right_val.type()->isChar()) ||
             (!left_val.type()->isChar() && right_val.type()->isChar())))
        {
            report(expr, 179) << left_val.type() << right_val.type();
            return false;
        }
        if (lval->type()->asEnumStruct() || rval->type()->asEnumStruct()) {
            if (lval->type() != rval->type()) {
                report(expr, 134) << left_val.type() << right_val.type();
                return false;
            }

            auto es = left_val.type()->asEnumStruct();
            expr->set_array_copy_length(es->array_size());
        } else if (!left_val.type()->isArray()) {
            TypeChecker::DoCoerce(expr->pos(), left_val.type(), right_val.type());
        }
    }
#endif
    return true;
}

static inline bool IsTypeBinaryConstantFoldable(QualType type) {
    if (type->isEnum() || type->isInt())
        return true;
    return false;
}

bool Expr::EvalConst(Expr* expr, cell* value, QualType* type) {
    if (auto tve = expr->as<TaggedValueExpr>()) {
        if (value)
            *value = tve->value();
        if (type)
            *type = QualType(tve->type());
        return true;
    }
    if (auto bin = expr->as<BinaryExpr>())
        return bin->ConstantFold(value, type);
    if (auto cast = expr->as<CastExpr>())
        return cast->ConstantFold(value, type);
    return false;
}

bool BinaryExpr::ConstantFold(cell* value, QualType* type) {
    cell left_val, right_val;
    QualType left_type, right_type;

    if (!Expr::EvalConst(left_, &left_val, &left_type) ||
        !Expr::EvalConst(right_, &right_val, &right_type))
    {
        return false;
    }

    // If we went through sema we could drop this...
    if (!IsTypeBinaryConstantFoldable(left_type) || !IsTypeBinaryConstantFoldable(right_type))
        return false;

    if (left_type != right_type)
        return false;

    *type = left_type;

    switch (token_) {
        case '*':
            *value = left_val * right_val;
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
                *value = left_val / right_val;
            else
                *value = left_val % right_val;
            break;
        case '+':
            *value = left_val + right_val;
            break;
        case '-':
            *value = left_val - right_val;
            break;
        case tSHL:
            *value = left_val << right_val;
            break;
        case tSHR:
            *value = left_val >> right_val;
            break;
        case tSHRU:
            *value = uint32_t(left_val) >> uint32_t(right_val);
            break;
        case '&':
            *value = left_val & right_val;
            break;
        case '^':
            *value = left_val ^ right_val;
            break;
        case '|':
            *value = left_val | right_val;
            break;
        default:
            return false;
    }
    return true;
}

ir::Value* Semantics::CheckLogicalExpr(LogicalExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto left = AnalyzeForTest(expr->left());
    if (!left)
        return nullptr;
    auto right = AnalyzeForTest(expr->right());
    if (!right)
        return nullptr;

    auto bool_type = types_->get_bool();

    auto left_cv = left->as<ir::Const>();
    auto right_cv = right->as<ir::Const>();
    if (left_cv && right_cv) {
        if (expr->token() == tlOR)
            return new ir::Const(expr, bool_type, left_cv->value() || right_cv->value());
        if (expr->token() == tlAND)
            return new ir::Const(expr, bool_type, left_cv->value() && right_cv->value());
        assert(false);
    }
    return new ir::BinaryOp(expr, bool_type, expr->token(), left, right);
}

ir::Value* Semantics::CheckChainedCompareExpr(ChainedCompareExpr* chain) {
    auto first = CheckRvalue(chain->first());
    if (!first)
        return nullptr;

    ir::Value* left = first;
    ir::Value* out = nullptr;
    for (const auto& chain_op : chain->ops()) {
        auto right = CheckRvalue(chain_op.expr);
        if (!right)
            return nullptr;

        assert(!right->HasSideEffects());

        auto op = MaybeCallUserOp(chain, chain_op.token, left, right);
        if (op) {
            if (!op->type()->isBool()) {
                report(chain_op.pos, 51) << get_token_string(chain_op.token);
                return nullptr;
            }
        } else {
            // :TODO: type check
            // :TODO: Compare struct should be Expr
            op = new ir::BinaryOp(chain, types_->get_bool(), chain_op.token, left, right);
        }
        if (!out)
            out = op;
        else
            out = new ir::BinaryOp(chain, types_->get_bool(), tlAND, out, op);

        left = right;
    }

    return out;
}

void
ChainedCompareExpr::ProcessUses(SemaContext& sc)
{
    first_->MarkAndProcessUses(sc);
    for (const auto& op : ops_)
        op.expr->MarkAndProcessUses(sc);
}

ir::Value* Semantics::CheckTernaryExpr(TernaryExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto first = CheckRvalue(expr->first());
    if (!first)
        return nullptr;
    auto second = CheckRvalue(expr->second());
    if (!second)
        return nullptr;
    auto third = CheckRvalue(expr->third());
    if (!third)
        return nullptr;

    if (auto cv = first->as<ir::Const>())
        report(expr->first(), cv->value() ? 206 : 205);

    TypeChecker tc(expr->second(), second->type(), third->type(), TypeChecker::Generic,
                   TypeChecker::Ternary | TypeChecker::Commutative);
    if (!tc.Check())
        return nullptr;

    // Huge hack: for now, take the larger of two char arrays.
    auto type = second->type();
    if (second->type()->isCharArray() && third->type()->isCharArray()) {
        auto second_array = second->type()->to<ArrayType>();
        auto third_array = third->type()->to<ArrayType>();
        if (third_array->size() > second_array->size())
            type = third->type();
    }
    return new ir::TernaryOp(expr, type, first, second, third);
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

ir::Value* Semantics::CheckCastExpr(CastExpr* expr, ExprFlags flags) {
    AutoErrorPos aep(expr->pos());

    QualType to_type = QualType(expr->type());
    if (to_type->isVoid()) {
        report(expr, 144);
        return nullptr;
    }

    ir::Value* val = CheckExpr(expr->expr(), flags);
    if (!val)
        return nullptr;

    if (auto lval = val->as<ir::Lvalue>()) {
        if ((flags & ExprFlags::WANT_RVALUE) == ExprFlags::WANT_RVALUE)
            val = BindRvalue(expr->expr(), lval);
    }

    QualType from_type = val->type();
    auto actual_array = from_type->as<ArrayType>();
    if (actual_array) {
        // Unwind back to the inner.
        auto iter = actual_array;
        for (;;) {
            if (!iter->inner()->isArray())
                break;
            iter = iter->inner()->to<ArrayType>();
        }
        from_type = QualType(iter->inner());
    }

    assert(!val->as<ir::Lvalue>());

    if (from_type->isObject() || to_type->isObject()) {
        //TypeChecker::DoCoerce(expr->pos(), to_type, from_type);
        assert(false);
    } else if (from_type->isFunction() != to_type->isFunction()) {
        // Warn: unsupported cast.
        report(expr, 237);
    } else if (from_type->isFunction() && to_type->isFunction()) {
        //TypeChecker::DoCoerce(expr->pos(), to_type, from_type);
        assert(false);
    } else if (from_type->isVoid()) {
        report(expr, 89);
        return nullptr;
    } else if (to_type->isEnumStruct() || from_type->isEnumStruct()) {
        report(expr, 95) << to_type;
        return nullptr;
    }
    if (from_type->isReference() && !to_type->isReference()) {
        if (to_type->isEnumStruct()) {
            report(expr, 136);
            return nullptr;
        }
        to_type = QualType(types_->defineReference(*to_type));
    }
    if (actual_array) {
        to_type = QualType(types_->redefineArray(to_type.ptr(), actual_array));
        if (from_type.is_const())
            to_type = QualType(to_type, true);
    }

    if (auto cv = val->as<ir::Const>()) {
        if (to_type->hasTrivialViewAsCast() && from_type->hasTrivialViewAsCast())
            return new ir::Const(expr, to_type, cv->value());
    }

    return new ir::CastOp(expr, to_type, val);
}

bool CastExpr::ConstantFold(cell* value, QualType* out_type) {
    QualType from_type;
    if (!Expr::EvalConst(expr(), value, &from_type))
        return false;

    QualType to_type(type());
    if (!from_type->hasTrivialViewAsCast() || !to_type->hasTrivialViewAsCast())
        return false;

    *out_type = QualType(to_type);
    return true;
}

void CastExpr::ProcessUses(SemaContext& sc) {
    expr_->MarkAndProcessUses(sc);
}

void SymbolExpr::MarkUsed(SemaContext& sc) {
    markusage(decl_, uREAD);
}

// This is a hack. Most code is not prepared to handle iMETHODMAP in type
// checks, so for now, we forbid it by default. Since the '.' operator *is*
// prepared for this, we have a special analysis option to allow returning
// types as values.
ir::Value* Semantics::CheckSymbolExpr(SymbolExpr* expr, ExprFlags flags) {
    AutoErrorPos aep(expr->pos());

    auto decl = expr->decl();
    if (!decl) {
        // This can happen if CheckSymbolExpr is called during name resolution.
        assert(cc_.reports()->total_errors() > 0);
        return nullptr;
    }

    // Don't expose the tag of old enumroots.
    Type* type = decl->type();
    if (decl->as<EnumDecl>() && !type->asEnumStruct() && decl->ident() == iCONSTEXPR) {
        report(expr, 174) << decl->name();
        return nullptr;
    }

    if (auto fun = decl->as<FunctionDecl>()) {
        fun = fun->canonical();
        if (fun->is_native()) {
            report(expr, 76);
            return nullptr;
        }
        if (!fun->impl()) {
            report(expr, 4) << fun->name();
            return nullptr;
        }

        auto ir_fun = BuildFunction(decl->to<FunctionDecl>());
        if (ir_fun->return_array()) {
            report(expr, 182);
            return nullptr;
        }

        funcenum_t* fe = funcenum_for_symbol(cc_, fun);

        // New-style "closure". TODO: when we get rid of funcenum_t, this won't
        // be necessary.
        type = fe->type;

        // Mark as being indirectly invoked. Direct invocations go through
        // BindCallTarget.
        fun->set_is_callback();
 }

    switch (decl->ident()) {
        case iVARIABLE: {
            auto var_decl = decl->as<VarDeclBase>();
            assert(var_decl);

            ir::Variable* var = nullptr;
            if (var_decl->vclass() == sGLOBAL ||
                (var_decl->vclass() == sSTATIC && !var_decl->is_local_static()))
            {
                auto it = global_vars_.find(var_decl);
                assert(it != global_vars_.end());
                var = it->second;
            } else {
                auto it = sc_->local_vars().find(var_decl);
                assert(it != sc_->local_vars().end());
                var = it->second;
            }

            return new ir::VariableRef(expr, QualType(type), var);
        }
        case iFUNCTN: {
            auto fun = BuildFunction(decl->to<FunctionDecl>());
            return BuildFunctionRef(expr, fun);
        }
        case iTYPENAME:
            if (!(flags & ExprFlags::ALLOW_TYPES)) {
                report(expr, 174) << decl->name();
                return nullptr;
            }
            return new ir::TypeRef(expr, QualType(type));
        case iCONSTEXPR:
            return new ir::Const(expr, QualType(type), decl->ConstVal());
        default:
            // Should not be a symbol.
            assert(false);
            return nullptr;
    }
}

ir::Value* Semantics::CheckCommaExpr(CommaExpr* comma, ExprFlags flags) {
    AutoErrorPos aep(comma->pos());

    // A single value acts as a passthrough.
    if (comma->exprs().size() == 1)
        return CheckExpr(comma->exprs()[0], flags);

    std::vector<ir::Value*> values;
    for (size_t i = 0; i < comma->exprs().size(); i++) {
        auto expr = comma->exprs().at(i);

        // Don't bother converting ignored results to rvalues.
        ir::Value* val;
        if (i == comma->exprs().size() - 1)
            val = CheckRvalue(expr);
        else
            val = CheckRvalue(expr, ExprFlags::RESULT_UNUSED);
        if (!val)
            return nullptr;
        values.emplace_back(val);
    }

    for (size_t i = 0; i < comma->exprs().size() - 1; i++) {
        auto expr = comma->exprs().at(i);
        if (!expr->HasSideEffects())
            report(expr, 231) << i;
    }

    return new ir::CommaOp(comma, values.back()->type(), values);
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

ir::Value* Semantics::CheckArrayExpr(ArrayExpr* array) {
    AutoErrorPos aep(array->pos());

    std::vector<ir::Value*> values;

    QualType last_type;
    for (const auto& expr : array->exprs()) {
        auto val = CheckRvalue(expr);
        if (!val)
            return nullptr;

        values.emplace_back(val);

        auto cv = val->as<ir::Const>();
        if (!cv) {
            report(expr, 8);
            return nullptr;
        }
        if (!last_type) {
            last_type = val->type();
            continue;
        }

        TypeChecker tc(array, last_type, val->type(), TypeChecker::Generic);
        if (!tc.Check())
            return nullptr;
    }

    NeedsHeapAlloc(array);

    auto type = types_->defineArray(last_type.ptr(), (int)values.size());
    return new ir::ArrayInitializer(array, QualType(type), std::move(values));
}

ir::Value* Semantics::CheckIndexExpr(IndexExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto base = CheckRvalue(expr->base());
    if (!base)
        return nullptr;

    ArrayType* array = base->type()->as<ArrayType>();
    if (!array) {
        report(expr, 28);
        return nullptr;
    }

    auto index = CheckRvalue(expr->index());
    if (!index)
        return nullptr;

    if (!CheckScalarType(expr, index->type()))
        return nullptr;

    auto idx_type = index->type();
    if (!IsValidIndexType(idx_type.ptr())) {
        report(expr->index(), 77) << idx_type;
        return nullptr;
    }

    if (auto cv = index->as<ir::Const>()) {
        auto val = cv->value();
        if (val < 0 || (array->size() != 0 && array->size() <= val)) {
            report(expr->index(), 32);
            return nullptr;
        }
    }

    return new ir::IndexOp(expr, QualType(array->inner()), base, index);
}

void
IndexExpr::ProcessUses(SemaContext& sc)
{
    base_->MarkAndProcessUses(sc);
    expr_->MarkAndProcessUses(sc);
}

ir::Value* Semantics::CheckThisExpr(ThisExpr* expr) {
    auto decl = expr->decl();
    auto it = sc_->local_vars().find(decl);
    assert(it != sc_->local_vars().end());
    auto var = it->second;

    var->set_read();

    // |this| is never an l-value.
    auto ref = new ir::VariableRef(expr, QualType(decl->type()), var);
    return BindRvalue(expr, ref);
}

ir::Value* Semantics::CheckNullExpr(NullExpr* expr) {
    return new ir::Const(expr, types_->get_null(), 0);
}

ir::Value* Semantics::CheckTaggedValueExpr(TaggedValueExpr* expr) {
    return new ir::Const(expr, QualType(expr->type()), expr->value());
}

ir::Value* Semantics::CheckStringExpr(StringExpr* expr) {
    auto type = types_->defineArray(types_->type_char(), (cell)expr->text()->length() + 1);
    return new ir::CharArrayLiteral(expr, QualType(type));
}

ir::Value* Semantics::CheckFieldAccessExpr(FieldAccessExpr* expr, ExprFlags flags) {
    AutoErrorPos aep(expr->pos());

    ir::Value* base = CheckRvalue(expr->base(), ExprFlags::ALLOW_TYPES);
    if (!base)
        return nullptr;

    int token = expr->token();
    if (token == tDBLCOLON)
        return CheckStaticFieldAccessExpr(expr, base, flags);

    if (base->type()->isFunction()) {
        report(expr, 107);
        return nullptr;
    }
    if (base->type()->isArray()) {
        report(expr, 96) << expr->name() << "type" << "array";
        return nullptr;
    }

    if (auto type_ref = base->as<ir::TypeRef>()) {
        auto map = type_ref->type()->asMethodmap();
        auto member = map ? map->FindMember(expr->name()) : nullptr;
        if (!member || !member->as<MethodmapMethodDecl>()) {
            report(expr, 444) << type_ref->type().ptr() << expr->name();
            return nullptr;
        }
        auto method = member->as<MethodmapMethodDecl>();
        if (!method->is_static()) {
            report(expr, 176) << method->decl_name() << map->name();
            return nullptr;
        }

        auto fun = BuildFunction(method);
        return BuildFunctionRef(expr, fun);
    }

    QualType base_type = base->type();
    if (auto es = base_type->asEnumStruct())
        return CheckEnumStructFieldAccessExpr(expr, base, es, flags);
    if (base_type->isReference())
        base_type = QualType(base_type->inner());

    auto map = base_type->asMethodmap();
    if (!map) {
        report(expr, 104) << base_type;
        return nullptr;
    }

    auto member = map->FindMember(expr->name());
    if (!member) {
        report(expr, 105) << map->name() << expr->name();
        return nullptr;
    }

    if (auto prop = member->as<MethodmapPropertyDecl>())
        return new ir::PropertyRef(expr, QualType(prop->property_type()), base, prop);

    auto method = member->as<MethodmapMethodDecl>();
    if (method->is_static()) {
        report(expr, 177) << method->decl_name() << map->name() << method->decl_name();
        return nullptr;
    }
    expr->set_resolved(method);

    if (!(flags & ExprFlags::ALLOW_BOUND_FUNCTIONS)) {
        report(expr, 50);
        return nullptr;
    }

    auto fun = BuildFunction(method);
    return new ir::BoundFunction(expr, QualType(method->return_type()), base, fun);
}

void
FieldAccessExpr::ProcessUses(SemaContext& sc)
{
    base_->MarkAndProcessUses(sc);
}

ir::FunctionRef* Semantics::BindCallTarget(CallExpr* call, Expr* target) {
    AutoErrorPos aep(target->pos());

    switch (target->kind()) {
        case ExprKind::FieldAccessExpr: {
            auto expr = target->to<FieldAccessExpr>();
            auto ir = CheckFieldAccessExpr(expr, ExprFlags::ALLOW_BOUND_FUNCTIONS);
            if (!ir)
                return nullptr;

            auto ref = ir->as<ir::FunctionRef>();
            if (!ref) {
                report(target, 12);
                return nullptr;
            }

            auto fun = ref->fun();

            // The static accessor (::) is offsetof(), so it can't return functions.
            assert(expr->token() == '.');

            if (auto method = fun->decl()->as<MethodmapMethodDecl>()) {
                auto map = method->parent()->as<MethodmapDecl>();
                if (map->ctor() == method) {
                    report(call, 84) << method->parent()->name();
                    return nullptr;
                }
            }
            return ref;
        }
        case ExprKind::SymbolExpr: {
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
                return BuildFunctionRef(expr, mm->ctor());
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
            return BuildFunctionRef(expr, fun);
        }
        default:
            report(target, 12);
            return nullptr;
    }
}

ir::FunctionRef* Semantics::BindNewTarget(Expr* target) {
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
            return BuildFunctionRef(expr, mm->ctor());
        }
    }
    return nullptr;
}

ir::Value* Semantics::CheckEnumStructFieldAccessExpr(FieldAccessExpr* expr, ir::Value* base,
                                                     EnumStructDecl* root, ExprFlags flags)
{
    auto type = base->type();

    expr->set_resolved(FindEnumStructField(*type, expr->name()));

    auto field_decl = expr->resolved();
    if (!field_decl) {
        report(expr, 105) << type << expr->name();
        return nullptr;
    }

    if (auto fun = field_decl->as<MemberFunctionDecl>()) {
        if (!(flags & ExprFlags::ALLOW_BOUND_FUNCTIONS)) {
            report(expr, 76);
            return nullptr;
        }

        auto ir_fun = BuildFunction(fun);

        // :TODO: real type
        return new ir::BoundFunction(expr, QualType(fun->return_type()), base, ir_fun);
    }

    auto field = field_decl->as<LayoutFieldDecl>();
    assert(field);

    QualType field_type = QualType(field->type_info().type);
    return new ir::FieldRef(expr, field_type, base, field);
}

ir::Value* Semantics::CheckStaticFieldAccessExpr(FieldAccessExpr* expr, ir::Value* base,
                                                 ExprFlags flags)
{
    AutoErrorPos aep(expr->pos());

    auto type_ref = base->as<ir::TypeRef>();
    if (!type_ref) {
        report(expr, 108);
        return nullptr;
    }

    Decl* field = FindEnumStructField(*base->type(), expr->name());
    if (!field) {
        report(expr, 105) << base->type() << expr->name();
        return nullptr;
    }

    auto fd = field->as<LayoutFieldDecl>();
    if (!fd) {
        report(expr, 445) << field->name();
        return nullptr;
    }

    expr->set_resolved(field);

    if ((flags & ExprFlags::SIZEOF) == ExprFlags::SIZEOF)
        return new ir::TypeRef(expr, QualType(fd->type()));

    return new ir::Const(expr, types_->get_int(), fd->offset());
}

ir::Value* Semantics::CheckSizeofExpr(SizeofExpr* expr) {
    AutoErrorPos aep(expr->pos());

    Expr* child = expr->child();
    ir::Value* val = CheckExpr(child, ExprFlags::ALLOW_TYPES | ExprFlags::SIZEOF);
    if (!val)
        return nullptr;

    QualType type;
    if (auto type_ref = val->as<ir::TypeRef>())
        type = type_ref->type();
    else
        type = val->type();
    if (type->isReference())
        type = QualType(type->inner());

    switch (type->kind()) {
        case TypeKind::Builtin:
        case TypeKind::Methodmap:
        case TypeKind::Function:
        case TypeKind::Object:
        case TypeKind::FunctionSignature:
            if (type->isVoid()) {
                report(child, 72);
                return nullptr;
            }
            return new ir::Const(child, types_->get_int(), 1);

        case TypeKind::EnumStruct:
            return new ir::Const(child, types_->get_int(), type->asEnumStruct()->array_size());

        case TypeKind::Array: {
            auto array = type->as<ArrayType>();
            if (!array->size()) {
                report(child, 163);
                return nullptr;
            }
            return new ir::Const(child, types_->get_int(), array->size());
        }

        default:
            report(child, 72);
            return nullptr;
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

ir::Value* Semantics::CheckCallExpr(CallExpr* call) {
    AutoErrorPos aep(call->pos());

    // Note: we do not Analyze the call target. We leave this to the
    // implementation of BindCallTarget.
    ir::FunctionRef* ref;
    if (call->token() == tNEW)
        ref = BindNewTarget(call->target());
    else
        ref = BindCallTarget(call, call->target());
    if (!ref)
        return nullptr;

    FunctionDecl* decl = ref->fun()->decl();
    if (decl->return_type()->isArray() || decl->return_type()->isEnumStruct()) {
        // We need to know the size of the returned array. Recursively analyze
        // the function.
        if (decl->is_analyzing() || !CheckFunctionDecl(decl)) {
            report(call, 411);
            return nullptr;
        }
    }

    fun_->AddReferenceTo(ref->fun());

    // We don't have canonical decls yet, so get the one attached to the symbol.
    if (decl->deprecate())
        report(call, 234) << decl->name() << decl->deprecate();

    ParamState ps;

    unsigned int implicit_args = 0;
    if (auto bf = ref->as<ir::BoundFunction>()) {
        implicit_args++;
        ps.argv.emplace_back(bf->val());
    }

    unsigned int argidx = implicit_args;
    unsigned int nargs = implicit_args;

    bool namedparams = false;
    auto& arglist = decl->args();
    for (const auto& param : call->args()) {
        unsigned int argpos;
        if (auto named = param->as<NamedArgExpr>()) {
            int pos = decl->FindNamedArg(named->name());
            if (pos < 0) {
                report(call, 17) << named->name();
                break;
            }
            argpos = pos;
            argidx = pos;
        } else {
            if (namedparams) {
                report(call, 44); // positional parameters must precede named parameters
                return nullptr;
            }
            argpos = nargs;
            if (argidx >= arglist.size()) {
                report(param->pos(), 92);
                return nullptr;
            }
        }

        if (argpos >= SP_MAX_CALL_ARGUMENTS) {
            report(call, 45); // too many function arguments
            return nullptr;
        }
        if (argpos < ps.argv.size() && ps.argv[argpos]) {
            report(call, 58); // argument already set
            return nullptr;
        }

        // Add the argument to |argv| and perform type checks.
        auto result = CheckArgument(call, arglist[argidx], param, &ps, argpos);
        if (!result)
            return nullptr;
        ps.argv[argpos] = result;

        nargs++;

        // Don't iterate past terminators (0 or varargs).
        if (!arglist[argidx]->type_info().is_varargs)
            argidx++;
    }

    if (!sc_->func()) {
        report(call, 10);
        return nullptr;
    }

    // Check for missing or invalid extra arguments, and fill in default
    // arguments.
    for (unsigned int iter = implicit_args; iter < arglist.size(); iter++) {
        auto arg = arglist[iter];
        if (arg->type_info().is_varargs)
            break;
        //if (ref->as<ir::BoundFunction>() && iter == 0)
        //    continue;
        if (iter >= ps.argv.size() || !ps.argv[iter]) {
            auto result = CheckArgument(call, arg, nullptr, &ps, iter);
            if (!result)
                return nullptr;
            ps.argv[iter] = result;
        }

#if 0
        Expr* expr = ps.argv[iter];
        if (expr->as<DefaultArgExpr>() && !IsReferenceType(iVARIABLE, arg->type())) {
            assert(false);
        }
#endif
    }

    if (decl->return_type()->isArray())
        NeedsHeapAlloc(call);

    return new ir::CallOp(call, QualType(decl->return_type()), ref, ps.argv);
}

ir::Value* Semantics::CheckArgument(CallExpr* call, ArgDecl* arg, Expr* param,
                                    ParamState* ps, unsigned int pos)
{
    while (pos >= ps->argv.size())
        ps->argv.push_back(nullptr);

    unsigned int visual_pos = pos + 1;

#if 0
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
#endif

    auto flags = ExprFlags::DEFAULT;
    if (!(arg->type()->isReference() ||
          (arg->type()->isArray() && !arg->type_info().is_const)))
    {
        flags |= ExprFlags::WANT_RVALUE;
    }

    ir::Value* val = CheckExpr(param, flags);
    if (!val)
        return nullptr;

    AutoErrorPos aep(param->pos());

    if (param->val().ident == iACCESSOR) {
#if 0
        if (val = CheckRvalue(val); !val)
            return nullptr;
#endif
        assert(false);
    }

    if (arg->type_info().is_varargs) {
        // Varargs are always passed by reference.
        if (auto lval = val->as<ir::Lvalue>()) {
            if (!BindLvalue(lval, uREAD))
                return nullptr;

            // If the value is already a reference we can load it. Or, if it's
            // not addressable, we also need to load it.
            //
            // If it's not a reference and it's addressable, we can use the
            // specialized AddressOf operation.
            if (lval->type()->isReferenceType()) {
                val = BuildRvalue(param, lval);
            } else if (!lval->IsAddressable()) {
                val = new ir::TempValueRef(param, val->type(), BuildRvalue(param, lval),
                                           AllocTempSlot());
            } else {
                val = new ir::AddressOf(param, lval->type(), lval);
            }
        } else if (!val->type()->isReferenceType()) {
            val = new ir::TempValueRef(param, val->type(), val, AllocTempSlot());
        }

        if (!checktag_string(arg->type(), val->type().ptr()) && !checktag(arg->type(), val->type().ptr()))
            report(param, 213) << arg->type() << val->type();
    } else if (arg->type()->isReference()) {
        auto lval = val->as<ir::Lvalue>();
        if (!lval) {
            report(param, 35) << visual_pos;
            return nullptr;
        }
        if (!BindLvalue(lval, uREAD))
            return nullptr;
        if (lval->as<ir::IndexOp>() && lval->type()->isChar()) {
            report(param, 35) << visual_pos;
            return nullptr;
        }
        if (lval->type().is_const() && !arg->type_info().is_const) {
            report(param, 35) << visual_pos;
            return nullptr;
        }

        checktag(arg->type()->inner(), *val->type());

        val = new ir::AddressOf(param, lval->type(), lval);
    } else if (arg->type()->isArray()) {
        // If the input type is an index into an array, create an implicit
        // array type to represent the slice. Note that this only works for
        // one-dimensional arrays.
        if (auto index_op = val->as<ir::IndexOp>()) {
            auto formal = arg->type()->to<ArrayType>();
            if (!formal->inner()->isArray() && !index_op->type()->isArray()) {
                auto type = QualType(types_->defineArray(index_op->type().ptr(), 0));
                val = new ir::SliceArray(index_op->expr(), type, index_op);
            }
        }
        if (auto lval = val->as<ir::Lvalue>())
            val = BindRvalue(param, lval);

        TypeChecker tc(param, QualType(arg->type()), val->type(), TypeChecker::Argument);
        if (!tc.Coerce())
            return nullptr;

        if (val->type().is_const() && !arg->type_info().is_const) {
            report(param, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }
    } else {
        if (auto lval = val->as<ir::Lvalue>(); lval)
            val = BindRvalue(param, lval);

#if 0
        // Do not allow user operators to transform |this|.
        UserOperation userop;
        if (!handling_this &&
            find_userop(*sc_, 0, arg->type(), val->type(), 2, nullptr, &userop))
        {
            param = new CallUserOpExpr(userop, param);
            val = &param->val();
        }

        TypeChecker tc(param, QualType(arg->type()), val->type(), TypeChecker::Argument);
        if (!tc.Coerce())
            return nullptr;
#endif
    }
    return val;
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
    auto val = CheckRvalue(stmt->expr());
    if (!val)
        return false;

    // :TODO: insert coercion to bool.
    auto cv = val->as<ir::Const>();
    if (!cv) {
        report(stmt->expr(), 8);
        return false;
    }

    if (cv->value())
        return true;

    std::string message;
    if (stmt->text())
        message += ": " + std::string(stmt->text()->chars(), stmt->text()->length());

    report(stmt, 70) << message;
    return false;
}

ir::Value* Semantics::CheckNewArrayExpr(NewArrayExpr* expr) {
    // We can't handle random refarrays floating around yet, so forbid this.
    report(expr, 142);
    return nullptr;
}

ir::Value* Semantics::BuildImplicitDynamicInitializer(VarDeclBase* decl) {
    // If any one rank was dynamic, the entire array is considered dynamic. For
    // new-style fixed arrays we've thrown an error at this point. For old
    // style, we need to synthesize an initializer.
    //
    // Rewrite dim_exprs into an array initializer. If an initializer
    // already exists, leave it, because it's illegal and we want to error
    // in the semantic pass.
    //
    // Note that these declarations use old tag-based syntax, and therefore
    // do not work with enum structs, which create implicit dimensions.
    auto type = decl->type();
    ArrayType* array = type->to<ArrayType>();
    assert(!array->innermost()->isEnumStruct());

    auto dim_vals = decl->implicit_dims();
    return new ir::NewArray(decl, QualType(array), std::move(*dim_vals));
}

ir::Value* Semantics::CheckNewArrayExprForArrayInitializer(NewArrayExpr* na) {
    std::vector<ir::Value*> ir_dims;

    for (auto& expr : na->exprs()) {
        auto val = CheckRvalue(expr, ExprFlags::DEFAULT);
        if (!val)
            return nullptr;

        if (IsLegacyEnumType(sc_->scope(), val->type().ptr())) {
            report(expr, 153);
            return nullptr;
        }
        if (!IsValidIndexType(val->type().ptr())) {
            report(expr, 77) << val->type();
            return nullptr;
        }

        auto cv = val->as<ir::Const>();
        if (cv && cv->value() <= 0) {
            report(expr, 9);
            return nullptr;
        }
        ir_dims.emplace_back(val);
    }

    return new ir::NewArray(na, QualType(na->type()), std::move(ir_dims));
}

void
NewArrayExpr::ProcessUses(SemaContext& sc)
{
    for (const auto& expr : exprs_)
        expr->MarkAndProcessUses(sc);
}

bool Semantics::CheckIfStmt(IfStmt* stmt) {
    ir::Value* cond = AnalyzeForTest(stmt->cond());
    if (!cond)
        return false;

    // Note: unlike loop conditions, we don't factor in constexprs here, it's
    // too much work and way less common than constant loop conditions.
    ir::InsnBlock* on_true = nullptr;
    ir::InsnBlock* on_false = nullptr;

    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);

        ir::NodeListBuilder builder(&ir_);
        if (!CheckStmt(stmt->on_true(), STMT_OWNS_HEAP))
            return false;
        on_true = builder.Finish();
    }
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        if (stmt->on_false()) {
            ir::NodeListBuilder builder(&ir_);
            if (!CheckStmt(stmt->on_false(), STMT_OWNS_HEAP))
                return false;
            on_false = builder.Finish();
        }
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

    ir_->emplace<ir::If>(stmt, cond, on_true, on_false);
    return true;
}

bool Semantics::CheckExprStmt(ExprStmt* stmt) {
    auto val = CheckRvalue(stmt->expr(), ExprFlags::RESULT_UNUSED);
    if (!val)
        return false;

    if (!val->HasSideEffects())
        report(stmt, 215);

    ir_->emplace<ir::ValueInsn>(stmt, val);
    return true;
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
            if (!var->is_used() && !var->is_public()) {
                report(sym, 203) << sym->name(); /* symbol isn't used (and not public) */
            } else if (!var->is_public() && !var->is_read()) {
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
    return true;
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
    ir_->emplace<ir::Break>(stmt);
    return true;
}

bool Semantics::CheckContinueStmt(ContinueStmt* stmt) {
    sc_->loop_has_continue() = true;
    ir_->emplace<ir::Continue>(stmt);
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

        ir_->emplace<ir::Return>(stmt, nullptr);

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

    auto val = CheckRvalue(expr);
    if (!val)
        return false;

    AutoErrorPos aep(expr->pos());

    if (fun->return_type()->isVoid()) {
        report(stmt, 88);
        return false;
    }

    sc_->set_returns_value();

    // Check that the return statement matches the declared return type.
    TypeChecker tc(stmt, QualType(fun->return_type()), val->type(), TypeChecker::Return);
    if (!tc.Coerce())
        return false;

    if (val->type()->isArray() || val->type()->isEnumStruct()) {
        if (!CheckCompoundReturnStmt(stmt, val))
            return false;
    }

    ir_->emplace<ir::Return>(stmt, val);
    return true;
}

bool Semantics::CheckCompoundReturnStmt(ReturnStmt* stmt, ir::Value* val) {
    FunctionDecl* curfunc = sc_->func();
    assert(curfunc == curfunc->canonical());

    if (auto iter = val->type()->as<ArrayType>()) {
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

    if (!fun_->return_array()) {
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
        auto info = new ir::Function::ReturnArrayInfo;
        info->hidden_address = ((cell_t)curfunc->args().size() + 3) * sizeof(cell);

        fun_->set_return_array(info);
        curfunc->update_return_type(val->type().ptr());
    }
    return true;
}

bool Semantics::CheckAssertStmt(AssertStmt* stmt) {
    auto val = AnalyzeForTest(stmt->expr());
    if (!val)
        return false;

    ir_->emplace<ir::Assert>(stmt, val);
    return true;
}

bool Semantics::CheckDeleteStmt(DeleteStmt* stmt) {
    auto val = CheckExpr(stmt->expr(), ExprFlags::DEFAULT);
    if (!val)
        return false;

    // Only grab an l-value if it can be set.
    auto lval = BindLvalue(val, uREAD);
    if (lval && !BindLvalue(lval, uMAYBE_WRITTEN))
        val = BuildRvalue(stmt->expr(), lval);

    auto type = val->type();
    auto map = type->asMethodmap();
    if (!map) {
        report(stmt, 115) << "type" << type;
        return false;
    }

    for (auto iter = map; iter; iter = iter->parent()) {
        if (iter->dtor()) {
            map = iter;
            break;
        }
    }

    if (!map || !map->dtor()) {
        report(stmt, 115) << "methodmap" << map->name();
        return false;
    }

    auto dtor = BuildFunction(map->dtor());
    if (!dtor)
        return false;

    ir_->emplace<ir::Delete>(stmt, val, dtor);
    return true;
}

bool Semantics::CheckExitStmt(ExitStmt* stmt) {
    auto val = CheckRvalue(stmt->expr());
    if (!val)
        return false;

    AutoErrorPos aep(stmt->pos());
    if (!TypeChecker::DoCoerce(stmt->expr()->pos(), types_->get_int(), val->type()))
        return false;

    ir_->emplace<ir::Exit>(stmt, val);
    return true;
}

bool Semantics::CheckDoWhileStmt(DoWhileStmt* stmt) {
    ir::Value* cond;
    {
        ke::SaveAndSet<bool> restore_heap(&pending_heap_allocation_, false);

        if ((cond = AnalyzeForTest(stmt->cond())) == nullptr)
            return false;
#if 0
        AssignHeapOwnership(stmt->cond());
#endif
    }

    ke::Maybe<cell> constval;
    if (auto cv = cond->as<ir::Const>())
        constval.init(cv->value());

    ir::InsnBlock* body;
    bool has_break = false;
    bool has_return = false;
    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        ke::SaveAndSet<bool> auto_break(&sc_->loop_has_break(), false);
        ke::SaveAndSet<bool> auto_return(&sc_->loop_has_return(), false);

        ir::NodeListBuilder builder(&ir_);
        if (!CheckStmt(stmt->body(), STMT_OWNS_HEAP))
            return false;
        body = builder.Finish();

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
    ir_->emplace<ir::DoWhile>(stmt, cond, body);
    return true;
}

bool Semantics::CheckForStmt(ForStmt* stmt) {
    bool ok = true;

    ir::InsnBlock* init = nullptr;
    if (stmt->init()) {
        ir::NodeListBuilder builder(&ir_);
        if (CheckStmt(stmt->init()))
            init = builder.Finish();
        else
            ok = false;
    }

    ir::Value* cond = nullptr;
    if (stmt->cond()) {
        if ((cond = AnalyzeForTest(stmt->cond())) == nullptr)
            ok = false;
    }

    ir::InsnBlock* advance = nullptr;
    if (stmt->advance()) {
        ir::NodeListBuilder builder(&ir_);
        if (CheckStmt(stmt->advance(), STMT_OWNS_HEAP))
            advance = builder.Finish();
        else
            ok = false;
    }

    ke::Maybe<cell> constval;
    if (cond) {
        if (auto cv = cond->as<ir::Const>())
            constval.init(cv->value());
    }

    ir::InsnBlock* body_ir;
    bool has_break = false;
    bool has_return = false;
    ke::Maybe<bool> always_returns;
    {
        AutoCollectSemaFlow flow(*sc_, &always_returns);
        ke::SaveAndSet<bool> auto_break(&sc_->loop_has_break(), false);
        ke::SaveAndSet<bool> auto_continue(&sc_->loop_has_continue(), false);
        ke::SaveAndSet<bool> auto_return(&sc_->loop_has_return(), false);

        ir::NodeListBuilder builder(&ir_);
        if (CheckStmt(stmt->body(), STMT_OWNS_HEAP))
            body_ir = builder.Finish();
        else
            ok = false;

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

    ir_->emplace<ir::ForLoop>(stmt, init, cond, advance, body_ir);
    return ok;
}

bool Semantics::CheckSwitchStmt(SwitchStmt* stmt) {
    auto cond = CheckRvalue(stmt->expr());
    if (!cond)
        return false;

#if 0
    const auto& v = expr->val();
    if (tag_ok && v.type()->isArray())
        report(expr, 33) << "-unknown-";
#endif

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

    std::unordered_set<cell_t> case_values;
    std::vector<ir::Switch::Case> out_cases;;
    for (const auto& case_entry : stmt->cases()) {
        PoolArray<cell_t> entry_values(case_entry.first.size());
        for (size_t i = 0; i < case_entry.first.size(); i++) {
            Expr* expr = case_entry.first[i];
            auto val = CheckRvalue(expr);
            if (!val)
                return false;

            auto cv = val->as<ir::Const>();
            if (!cv) {
                report(expr, 8);
                return false;
            }

            AutoErrorPos aep(expr->pos());
            TypeChecker::DoCoerce(expr->pos(), cond->type(), cv->type());

            if (!case_values.count(cv->value()))
                case_values.emplace(cv->value());
            else
                report(expr, 40) << cv->value();

            entry_values[i] = cv->value();
        }

        AutoCollectSemaFlow flow(*sc_, &always_returns);

        ir::NodeListBuilder builder(&ir_);
        if (!CheckStmt(case_entry.second))
            return false;
        out_cases.emplace_back(std::move(entry_values), builder.Finish());

        update_flow(case_entry.second->flow_type());
    }

    ir::InsnBlock* default_case = nullptr;
    if (stmt->default_case()) {
        AutoCollectSemaFlow flow(*sc_, &always_returns);

        ir::NodeListBuilder builder(&ir_);
        if (!CheckStmt(stmt->default_case()))
            return false;
        default_case = builder.Finish();

        update_flow(stmt->default_case()->flow_type());
    } else {
        always_returns.init(false);
        update_flow(Flow_None);
    }

    if (*always_returns)
        sc_->set_always_returns(true);

    stmt->set_flow_type(*flow);

    ir_->emplace<ir::Switch>(stmt, cond, std::move(out_cases), default_case);
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

    auto fun = BuildFunction(info);
    if (!fun)
        return false;

    ke::SaveAndSet<ir::Function*> push_fun(&fun_, fun);

    // :TODO: hack.
    sc.set_fun(fun);

    bool ok = true;
    for (const auto& arg : fun->argv()) {
        assert(sc_->local_vars().find(arg->decl()) == sc_->local_vars().end());
        sc_->local_vars().emplace(arg->decl(), arg);

        ok &= CheckFunctionArgument(arg);
    }

    if (info->body()) {
        ir::NodeListBuilder builder(&ir_);
        ok = CheckStmt(info->body(), STMT_OWNS_HEAP);
        fun->set_body(builder.Finish());
    }

    auto fwd = info->prototype();
    if (fwd && fwd->deprecate() && !info->is_stock() && fwd != info)
        report(info->pos(), 234) << info->name() << fwd->deprecate();

#if 0
    if (info->is_native()) {
        if (decl.type.dim_exprs.size() > 0) {
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

    // We never warn about unused member functions.
    if (info->as<MemberFunctionDecl>())
        maybe_used_.emplace_back(info);

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
#endif
    return ok;
}

ir::Function* Semantics::BuildFunction(FunctionDecl* decl) {
    auto canonical = decl->canonical();

    ir::Function* fun = nullptr;
    if (auto iter = functions_.find(canonical); iter != functions_.end()) {
        fun = iter->second;
    } else {
        SemaContext arg_sc(*global_sc_, decl);
        ke::SaveAndSet<SemaContext*> push_sc(&sc_, &arg_sc);

        // Build arguments.
        PoolArray<ir::Argument*> argv(canonical->args().size());
        for (size_t i = 0; i < canonical->args().size(); i++) {
            const auto& arg = canonical->args().at(i);

            ir::Value* init;
            if (!CheckVarDeclCommon(arg, &init))
                return nullptr;

            argv[i] = new ir::Argument(arg, init);
        }

        fun = new ir::Function(canonical);
        fun->set_argv(std::move(argv));

        functions_.emplace(canonical, fun);
        mod_->functions().emplace_back(fun);
    }

    if (fun_)
        fun_->AddReferenceTo(fun);

    return fun;
}

ir::FunctionRef* Semantics::BuildFunctionRef(Expr* expr, ir::Function* fun) {
#if 0
    // :TODO: proper type
#endif
    return new ir::FunctionRef(expr, QualType(fun->decl()->return_type()), fun);
}

ir::FunctionRef* Semantics::BuildFunctionRef(Expr* expr, FunctionDecl* decl) {
    auto fun = BuildFunction(decl);
    return BuildFunctionRef(expr, fun);
}

bool Semantics::CheckFunctionArgument(ir::Argument* arg) {
    assert(!arg->arg_decl()->init());
    return true;
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
    if (auto rhs = init_rhs())
        rhs->MarkAndProcessUses(sc);
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
    if (pending_heap_allocation_)
        ir_->set_has_heap_allocs();
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
        assert(sym->vclass() == sGLOBAL);
        assert(sym->as<VarDecl>());

        def->sym = sym->as<VarDecl>();
    } else {
        assert(false);
#if 0
        auto array = cc.NewDefaultArrayData();
        BuildCompoundInitializer(decl, array, 0);

        def->array = array;
        def->array->iv_size = (cell_t)array->iv.size();
        def->array->data_size = (cell_t)array->data.size();
#endif
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
    std::vector<ir::Function*> work;
    std::unordered_set<ir::Function*> seen;

    // The root set is all public functions.
    for (const auto& fun : mod_->functions()) {
        if (fun->decl()->is_public())
            fun->set_is_live();

        seen.emplace(fun);
        work.emplace_back(fun);
    }

    // Traverse referrers to find the transitive set of live functions.
    while (!work.empty()) {
        auto live = ke::PopBack(&work);
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

ir::Value* Semantics::CheckRvalue(Expr* expr, ExprFlags flags) {
    auto val = CheckExpr(expr, flags | ExprFlags::WANT_RVALUE);
    if (!val)
        return nullptr;
    return BindRvalue(expr, val);
}

ir::Value* Semantics::BuildRvalue(Expr* expr, ir::Lvalue* lval) {
    QualType type = lval->type();
    if (lval->as<ir::VariableRef>())
        type = BuildRvalueType(type);

    assert(!type->isReference());
    return new ir::Load(expr, type, lval);
}

QualType Semantics::BuildRvalueType(QualType type) {
    if (type->isReference())
        return QualType(type->inner());
    return type;
}

static inline std::string_view GetOverloadName(int token) {
    switch (token) {
        case '=': return "=";
        case '*': return "*";
        case taMULT: return "*";
        case '/': return "/";
        case taDIV: return "/";
        case '%': return "%";
        case taMOD: return "%";
        case '+': return "+";
        case taADD: return "+";
        case '-': return "-";
        case taSUB: return "-";
        case '<': return "<";
        case '>': return ">";
        case tlLE: return "<=";
        case tlGE: return ">=";
        case tlEQ: return "==";
        case tlNE: return "!=";
        case tINC: return "++";
        case tDEC: return "--";
        case '!': return "!";
        default: return {};
    }
}


static inline bool MatchOperator(int token, FunctionDecl* fun, QualType type1, QualType type2) {
    // If token is '=', type2 is the return type.
    size_t numparam = (type2 && token != '=') ? 2 : 1;

    const auto& args = fun->args();
    if (args.size() != numparam)
        return false;

    assert(numparam == 1 || numparam == 2);
    QualType types[2] = { type1, type2 };

    for (size_t i = 0; i < numparam; i++) {
        if (args[i]->type_info().is_varargs)
            return false;
        if (QualType(args[i]->type_info().type) != types[i])
            return false;
    }

    if (token == '=' && QualType(fun->return_type()) != type2)
        return false;
    return true;
}

auto Semantics::FindUserOp(Expr* expr, int token, QualType type1, QualType type2) -> UserOp {
    // No operator overloading allowed in the preprocessor.
    if (cc_.in_preprocessor())
        return {};

    // :TODO: use TypeChecker to do this instead
    if (type1->isReference())
        type1 = QualType(type1->inner());
    if (type2 && type2->isReference())
        type2 = QualType(type2->inner());

    if (type1->isInt() && (!type2 || type2->isInt()))
        return {};

    auto opername = GetOverloadName(token);
    if (opername.empty())
        return {};

    auto atom = cc_.atom(opername);
    Decl* chain = FindSymbol(*sc_, atom);
    if (!chain)
        return {};

    FunctionDecl* decl = nullptr;
    bool swapparams = false;
    bool is_commutative = IsOperTokenCommutative(token);
    for (auto iter = chain; iter; iter = iter->next) {
        auto fun = iter->as<FunctionDecl>();
        if (!fun)
            continue;
        fun = fun->canonical();

        bool matched = MatchOperator(token, fun, type1, type2);
        bool swapped = false;
        if (!matched && is_commutative && type1 != type2 && token) {
            matched = MatchOperator(token, fun, type2, type1);
            swapped = true;
        }
        if (matched) {
            decl = fun;
            swapparams = swapped;
            break;
        }
    }

    if (!decl)
        return {};

    // we don't want to use the redefined operator in the function that
    // redefines the operator itself, otherwise the snippet below gives
    // an unexpected recursion:
    //    fixed:operator+(fixed:a, fixed:b)
    //        return a + b
    if (decl == sc_->func())
        report(expr, 408);

    auto target = BuildFunction(decl);
    if (!target)
        return {};
    return {target, swapparams};
}

ir::Value* Semantics::MaybeCallUserOp(Expr* expr, int token, ir::Value* first,
                                      ir::Value* second)
{
    // No overloads for int,int.
    QualType type1 = first->type();
    QualType type2;
    if (second)
        type2 = second->type();

    auto userop = FindUserOp(expr, token, type1, type2);
    if (!userop.target)
        return nullptr;

    return new ir::CallUserOp(expr, QualType(userop.target->decl()->return_type()), userop.target,
                              first, second, userop.swapparams);
}

static void MarkUsage(ir::VariableRef* ref, uint8_t usage) {
    auto var = ref->var();
    if (usage & (uWRITTEN | uMAYBE_WRITTEN))
        var->set_written();
    if (usage & uREAD)
        var->set_read();
}

ir::Value* Semantics::BindRvalue(Expr* expr, ir::Value* val) {
    auto lval = val->as<ir::Lvalue>();
    if (!lval)
        return val;

    if (!BindLvalue(lval, uREAD))
        return nullptr;

    return BuildRvalue(expr, lval);
}

ir::Lvalue* Semantics::BindLvalue(ir::Value* val, uint8_t usage) {
    ir::Lvalue* lval = val->as<ir::Lvalue>();
    if (!lval) {
        report(val->pn(), 455);
        return nullptr;
    }
    if (!BindLvalue(lval, usage))
        return nullptr;
    return lval;
}

bool Semantics::BindLvalue(ir::Lvalue* lval, uint8_t usage) {
    if (lval->type().is_const()) {
        if (usage & uWRITTEN) {
            report(lval->pn(), 22);
            return false;
        }
        if (usage & uMAYBE_WRITTEN)
            return false;
    }

    switch (lval->kind()) {
        case IrKind::VariableRef: {
            auto ref = lval->to<ir::VariableRef>();
            MarkUsage(ref, usage);
            return true;
        }
        case IrKind::IndexOp: {
            auto op = lval->to<ir::IndexOp>();
            // Mark base inner variables as written.
            if (auto ref = op->base()->as<ir::VariableRef>())
                MarkUsage(ref, usage);
            return true;
        }
        case IrKind::FieldRef: {
            auto op = lval->to<ir::FieldRef>();
            // Mark base inner variables as written.
            if (auto ref = op->base()->as<ir::VariableRef>())
                MarkUsage(ref, usage);
            return true;
        }
        case IrKind::PropertyRef: {
            auto op = lval->to<ir::PropertyRef>();
            // Mark base inner variables as written.
            if (auto ref = op->val()->as<ir::VariableRef>())
                MarkUsage(ref, usage);
            if (usage & uREAD) {
                if (!op->decl()->getter()) {
                    report(op->pn(), 149) << op->decl()->name();
                    return false;
                }
                auto getter = BuildFunction(op->decl()->getter());
                if (!getter)
                    return false;
                op->BindGetter(getter);
            }
            if (usage & (uWRITTEN | uMAYBE_WRITTEN)) {
                if (!op->decl()->setter()) {
                    if (usage & uWRITTEN)
                        report(op->pn(), 152) << op->decl()->name();
                    return false;
                }
                auto setter = BuildFunction(op->decl()->setter());
                if (!setter)
                    return false;
                op->BindSetter(setter);
            }
            return true;
        }
        default:
            assert(false);
            return false;
    }
}

uint32_t Semantics::AllocTempSlot() {
    assert(temp_slots_);
    uint32_t slot = sc_->AllocTempSlot();
    temp_slots_->emplace_back(slot);
    return slot;
}

uint32_t SemaContext::AllocTempSlot() {
    if (!free_local_slots_.empty())
        return ke::PopBack(&free_local_slots_);

    uint32_t next_slot = fun_->num_slots();
    fun_->set_num_slots(next_slot + 1);
    return next_slot;
}

void SemaContext::FreeTempSlot(uint32_t slot) {
    assert(std::find(free_local_slots_.begin(), free_local_slots_.end(), slot) == free_local_slots_.end());
    free_local_slots_.emplace_back(slot);
}

} // namespace cc
} // namespace sp
