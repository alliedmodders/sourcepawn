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

#include <string>
#include <unordered_set>

#include <amtl/am-raii.h>
#include "array-helpers.h"
#include "code-generator.h"
#include "coercion-rules.h"
#include "errors.h"
#include "constant-fold.h"
#include "lexer.h"
#include "parse-node.h"
#include "sctracker.h"
#include "semantics-inl.h"
#include "symbols.h"

namespace sp {
namespace cc {

static bool AreSliceElementsCompatible(Type* t1, Type* t2) {
    if (t1 == t2)
        return true;
    auto size1 = t1->maybe_lit_size();
    auto size2 = t2->maybe_lit_size();
    if (size1 && size2)
        return *size1 == *size2;
    return false;
}

Semantics::Semantics(CompileContext& cc)
  : cc_(cc)
{
    types_ = cc.types();
}

bool Semantics::Analyze(ParseTree* tree) {
    SemaContext sc(this);
    ke::SaveRestore<SemaContext*> push_sc(sc_, &sc);

    AutoCountErrors errors;
    if (!CheckStmtList(tree->stmts()) || !errors.ok())
        return false;

    DeduceLiveness();
    DeduceMaybeUsed();

    if (!globals_to_init_.empty())
        GenerateInitFunctions(tree);

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

    return true;
}

void Semantics::GenerateInitFunctions(ParseTree* tree) {
    std::vector<FunctionDecl*> file_ctors;

    unsigned int count = 0;
    std::vector<VarDeclBase*> vars;
    std::optional<uint32_t> prev_file_index;
    for (const auto& var : globals_to_init_) {
        uint32_t file_index = cc_.sources()->GetSourceFileIndex(var->pos());
        if (!prev_file_index || *prev_file_index != file_index) {
            if (!vars.empty()) {
                file_ctors.emplace_back(GenerateInitFunction(vars, ++count));
                vars.clear();
            }
            prev_file_index = {file_index};
        }
        vars.emplace_back(var);
    }

    if (!vars.empty())
        file_ctors.emplace_back(GenerateInitFunction(vars, ++count));

    assert(!file_ctors.empty());

    if (file_ctors.size() == 1) {
        file_ctors[0]->set_name(cc_.atom(".ctor"));
        tree->global_ctors() = PoolArray<FunctionDecl*>(file_ctors);
        return;
    }

    declinfo_t decl{};
    decl.name = cc_.atom(".ctor");
    decl.type.type = types_->type_void();

    auto fun = new FunctionDecl(token_pos_t{}, decl);
    auto ft = types_->defineFunction(QualType(types_->type_void(), false), {}, false,
                                     FunctionType::Typed);
    fun->set_function_type(ft);

    std::vector<Stmt*> stmts;
    for (const auto& file_ctor : file_ctors) {
        auto call = new CallExpr(fun->pos(), '(', file_ctor, {});
        call->val().set_expr(types_->type_void());
        stmts.emplace_back(new ExprStmt(fun->pos(), call));
    }
    fun->set_body(new BlockStmt(fun->pos(), stmts));
    fun->set_is_live();
    fun->set_is_global_ctor();

    file_ctors.insert(file_ctors.begin(), fun);

    tree->global_ctors() = PoolArray<FunctionDecl*>(file_ctors);
}

FunctionDecl* Semantics::GenerateInitFunction(const std::vector<VarDeclBase*>& vars,
                                              uint32_t suffix)
{
    auto name = cc_.atom(".ctor." + std::to_string(suffix));

    declinfo_t decl{};
    decl.name = name;
    decl.type.type = types_->type_void();

    auto fun = new FunctionDecl(vars[0]->pos(), decl);
    auto ft = types_->defineFunction(QualType(types_->type_void(), false), {}, false,
                                     FunctionType::Typed);
    fun->set_function_type(ft);
    auto init = new GlobalInitStmt(fun->pos(), vars);
    fun->set_body(init);
    fun->set_is_live();
    fun->set_is_global_ctor();
    return fun;
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

bool Semantics::CheckStmt(Stmt* stmt) {
    AutoErrorPos aep(stmt->pos());

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
        case StmtKind::BlockStmt:
            return CheckBlockStmt(stmt->to<BlockStmt>());
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
            return CheckFunctionDecl(stmt->to<FunctionDecl>());
        case StmtKind::EnumStructDecl:
            return CheckEnumStructDecl(stmt->to<EnumStructDecl>());
        case StmtKind::ClassDecl:
            return CheckClassDecl(stmt->to<ClassDecl>());
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
        case StmtKind::GlobalInitStmt:
            return true;
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

    if (decl->type_info().is_auto) {
        if (!CheckInferredVarDecl(decl))
            return false;
    } else {
        if (!CheckTypedVarDecl(decl))
            return false;
        if (decl->type()->isPstruct())
            return true;
    }

    auto vclass = decl->vclass();
    auto init_rhs = decl->init_rhs();
    if (decl->init() && init_rhs && vclass != sLOCAL && !decl->type()->isComposite()) {
        if (!CheckExpr(init_rhs) || init_rhs->val().ident != iCONSTEXPR) {
            if (vclass == sARGUMENT && (init_rhs->is(ExprKind::SymbolExpr) || init_rhs->is(ExprKind::SizeofExpr)))
                return true;

            // Make a special exception for int64 / double lits (they can't
            // be folded by FoldToConstant).
            if (!((vclass == sGLOBAL || vclass == sSTATIC) &&
                  (init_rhs->as<Number64Expr>() || init_rhs->as<DoubleExpr>())))
            {
                report(init_rhs->pos(), 8);
            }
        }
    }

    if (decl->init() && (vclass == sGLOBAL || vclass == sSTATIC))
        globals_to_init_.emplace_back(decl);

    return true;
}

bool Semantics::CheckTypedVarDecl(VarDeclBase* decl) {
    const auto& type = decl->type();
    bool is_const = decl->type_info().is_const;

    // Constants are checked during binding.
    if (decl->as<ConstDecl>())
        return true;

    if (type->isPstruct())
        return CheckPstructDecl(decl);

    if (!decl->as<ArgDecl>() && is_const && !decl->init() && !decl->is_public())
        report(decl->pos(), 251);

    if (type->isArray()) {
        if (!CheckArrayDeclaration(decl))
            return false;
    } else if (type->isEnumStruct()) {
        if (!CheckEnumStructVarDecl(decl))
            return false;
        if (IsThisAtom(decl->name()))
            decl->mutable_type_info()->is_const = false;
    } else if (type->isClass()) {
        if (!decl->init()) {
            report(decl->pos(), 478);
            return false;
        }
        auto init = decl->init();
        if (init && !CheckRvalue(init))
            return false;
    } else {
        // Since we always create an assignment expression, all type checks will
        // be performed by the Analyze(sc) call here.
        auto init = decl->init();
        if (init && !CheckRvalue(init))
            return false;
    }

    return true;
}

static bool IsArrayLiteralExpr(Expr* expr) {
    return expr->is(ExprKind::StringExpr) ||
           expr->is(ExprKind::ArrayExpr) ||
           expr->is(ExprKind::StructExpr);
}

static typeinfo_t ErrorTypeinfo() {
    typeinfo_t ti;
    ti.type = CompileContext::get().types()->type_int();
    ti.resolved = true;
    return ti;
}

bool Semantics::CheckInferredVarDecl(VarDeclBase* decl) {
    AutoErrorPos aep(decl->pos());

    if (!decl->init()) {
        report(decl->pos(), 6);
        return false;
    }

    if (IsArrayLiteralExpr(decl->init_rhs())) {
        report(decl->pos(), 20);
        *decl->mutable_type_info() = ErrorTypeinfo();
        return false;
    }

    // Analyze the RHS to determine its type.
    Expr* init_rhs = decl->init_rhs();
    if (!CheckExpr(init_rhs)) {
        *decl->mutable_type_info() = ErrorTypeinfo();
        return false;
    }

    QualType rhs_type = init_rhs->val().type();

    if (rhs_type->isVoid()) {
        report(decl->pos(), 144);
        return false;
    }
    if (rhs_type->isNull()) {
        report(decl->pos(), 19);
        return false;
    }
    if (IsArrayLiteralExpr(init_rhs)) {
        report(decl->pos(), 20);
        return false;
    }

    // Assign the inferred type.
    auto* ti = decl->mutable_type_info();
    ti->type = rhs_type.unqualified();
    ti->resolved = true;
    ti->is_auto = false;

    // Make sure we don't double-eval the RHS in case that triggers weirdness.
    BinaryExprState state(decl->init());
    state.rhs_resolved = true;
    if (!CheckBinaryExprImpl(state))
        return false;

    return true;
}

bool Semantics::CheckEnumStructVarDecl(VarDeclBase* decl) {
    Expr* init = decl->init_rhs();
    if (!init)
        return true;

    // Handle array literal initializer — validate against enum struct fields.
    if (init->as<ArrayExpr>()) {
        AutoErrorPos aep(init->pos());
        return ValidateEnumStructInitializer(decl->type()->asEnumStruct(), init);
    }

    if (init->as<StructExpr>()) {
        report(init->pos(), 428);
        return false;
    }

    // Non-literal initialization (e.g. from a function result).
    if (!CheckRvalue(init))
        return false;
    if (init->lvalue())
        decl->init()->set_right(new RvalueExpr(init));

    auto ck = FindConversion(init->val().type(), *decl->type(), CvtContext::Assignment);
    if (ck == ConversionKind::NeedsCast) {
        report(init->pos(), 462) << init->val().type() << decl->type();
        return false;
    }
    if (!HasImplicitConversion(ck)) {
        ReportConversionDiagnostic(init->pos(), decl->type(), init->val().type());
        return false;
    }
    return true;
}

bool Semantics::ValidateEnumStructInitializer(EnumStructDecl* es, Expr* init) {
    ArrayExpr* array = init->as<ArrayExpr>();
    if (!array) {
        report(init->pos(), 47);
        return false;
    }

    const auto& field_list = es->fields();
    auto field_iter = field_list.begin();

    for (size_t i = 0; i < array->exprs().size(); i++) {
        Expr* expr = array->exprs().at(i);
        if (field_iter == field_list.end()) {
            report(expr->pos(), 91);
            return false;
        }

        auto field = *field_iter;
        field_iter++;

        const auto& type = field->type_info();
        if (type.type->isArray()) {
            if (!CheckArrayInitialization(this, type, expr))
                continue;
        } else {
            AutoErrorPos pos(expr->pos());

            if (!CheckExpr(expr))
                continue;

            const auto& v = expr->val();
            if (v.ident != iCONSTEXPR) {
                report(8);
                continue;
            }

            ConversionKind ck = FindConversion(v.type(), type.type, CvtContext::Assignment);
            if (!HasImplicitConversion(ck)) {
                ReportConversionDiagnostic(expr->pos(), type.type, v.type());
                continue;
            }
            if (!IsNopConversion(ck)) {
                Expr* converted = BuildConversion(expr, ck, type.type);
                assert(converted);

                array->exprs()[i] = converted;
            }
        }
    }

    if (array->ellipses()) {
        report(array->pos(), 80);
        return false;
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

        if ([[maybe_unused]] auto at = arg->type()->as<ArrayType>()) {
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
        actual = *expr->decl()->type();
    } else {
        assert(false);
        return false;
    }

    if (arg->type()->isBool() && actual->isInt())
        return true;

    if (!CheckCoercion(field, arg->type(), QualType(actual), CvtContext::Argument))
        return false;
    return true;
}

bool Semantics::CheckExpr(Expr* expr, uint32_t flags) {
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
        case ExprKind::Number64Expr:
            return CheckNumber64Expr(expr->to<Number64Expr>());
        case ExprKind::DoubleExpr:
            return CheckDoubleExpr(expr->to<DoubleExpr>());
        case ExprKind::SizeofExpr:
            return CheckSizeofExpr(expr->to<SizeofExpr>());
        case ExprKind::RvalueExpr:
        case ExprKind::SliceExpr:
        case ExprKind::SimpleCastExpr:
            return true;
        case ExprKind::NamedArgExpr:
            return CheckWrappedExpr(expr, expr->to<NamedArgExpr>()->expr);
        case ExprKind::FunctionExpr:
            return CheckFunctionExpr(expr->to<FunctionExpr>());
        case ExprKind::StructInitFieldExpr:
            return CheckWrappedExpr(expr, expr->to<StructInitFieldExpr>()->value);
        case ExprKind::DefaultArgExpr:
        case ExprKind::SpreadArgsExpr:
            return true;
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
    return true;
}

CompareOp::CompareOp(const token_pos_t& pos, int token, Expr* expr)
  : pos(pos),
    token(token),
    expr(expr)
{}

bool Expr::EvalConst(cell* value, Type** type) {
    if (val_.ident != iCONSTEXPR) {
        if (!FoldToConstant())
            return false;
        assert(val_.ident == iCONSTEXPR);
    }

    if (value)
        *value = val_.const_cell();
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
            return e->expr()->HasSideEffects();
        }
        case ExprKind::BinaryExpr: {
            auto e = to<BinaryExpr>();
            return IsAssignOp(e->token()) || e->left()->HasSideEffects() ||
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
                if (op.expr->HasSideEffects())
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
        case ExprKind::NamedArgExpr:
            return to<NamedArgExpr>()->expr->HasSideEffects();
        case ExprKind::StructInitFieldExpr:
            return to<StructInitFieldExpr>()->value->HasSideEffects();
        case ExprKind::SimpleCastExpr:
            return to<SimpleCastExpr>()->from()->HasSideEffects();
        case ExprKind::SliceExpr: {
            auto e = to<SliceExpr>();
            return e->expr()->HasSideEffects() || e->index()->HasSideEffects();
        }
        case ExprKind::StructExpr: {
            auto e = to<StructExpr>();
            for (const auto& field : e->fields()) {
                if (field->value->HasSideEffects())
                    return true;
            }
            return false;
        }
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
            return to<RvalueExpr>()->lval()->HasSideEffects();
        case ExprKind::CallExpr: // Not intelligent yet.
        case ExprKind::IncDecExpr:
            return true;
        case ExprKind::NullExpr:
        case ExprKind::SizeofExpr:
        case ExprKind::StringExpr:
        case ExprKind::SymbolExpr:
        case ExprKind::TaggedValueExpr:
        case ExprKind::Number64Expr:
        case ExprKind::DoubleExpr:
        case ExprKind::ThisExpr:
        case ExprKind::DefaultArgExpr:
        case ExprKind::SpreadArgsExpr:
        case ExprKind::FunctionExpr:
            return false;
        default:
            assert(false);
            return true;
    }
}

bool Semantics::CheckScalarType(Expr* expr) {
    const auto& val = expr->val();
    if (val.type()->isArray()) {
        if (val.sym())
            report(expr, 456) << val.type();
        else
            report(expr, 29);
        return false;
    }
    if (val.type()->asEnumStruct()) {
        report(expr, 447);
        return false;
    }
    if (val.type()->isVoid()) {
        report(expr, 466);
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
    if (val.type()->isWideType())
        return BuildSimpleCast(expr, BuiltinType::Bool);
    if (val.type()->isVoid()) {
        report(expr, 466);
        return nullptr;
    }

    if (val.ident == iCONSTEXPR) {
        if (!sc_->preprocessing()) {
            if (val.const_i32())
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

bool Semantics::AnalyzeForConst(Expr* expr, cell* value, Type** type) {
    if (!CheckExpr(expr))
        return false;

    auto& val = expr->val();
    if (val.ident != iCONSTEXPR) {
        report(expr, 8);
        return false;
    }

    if (value)
        *value = val.const_cell();
    if (type)
        *type = val.type();
    return true;
}

RvalueExpr::RvalueExpr(Expr* lval)
  : EmitOnlyExpr(ExprKind::RvalueExpr, lval->pos()),
    lval_(lval)
{
    assert(lval_->lvalue());
    assert(!lval->as<RvalueExpr>());

    val_ = lval_->val();
    if (val_.ident == iACCESSOR) {
        if (val_.accessor()->getter())
            markusage(val_.accessor()->getter(), uREAD);
        val_.ident = iEXPRESSION;
    }
    if (val_.type()->isReference()) {
        val_.set_type(val_.type()->inner());
    }
}

SliceExpr::SliceExpr(Expr* expr, Expr* index, Type* type)
  : EmitOnlyExpr(ExprKind::SliceExpr, expr->pos()),
    expr_(expr),
    index_(index)
{
    val_.ident = iEXPRESSION;
    val_.set_type(type);
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

    switch (unary->token()) {
        case '~':
            if (out_val.ident == iCONSTEXPR)
                out_val.set_constval(~out_val.const_i32());
            break;
        case '!':
            if (out_val.ident == iCONSTEXPR)
                out_val.set_constval(!out_val.const_i32());
            out_val.set_type(types_->type_bool());
            break;
        case '-':
            if (out_val.ident == iCONSTEXPR && out_val.type()->isFloat()) {
                float f = out_val.const_float();
                out_val.set_const_float(-f);
            } else if (out_val.ident == iCONSTEXPR) {
                /* the negation of a fixed point number is just an integer negation */
                out_val.set_constval(-out_val.const_i32());
            } else {
                // Special case for -INT_MIN, since we can't eat the '-' during lexing.
                if (auto num64 = Number64Expr::ToInt64(expr); num64) {
                    int64_t value = -*num64;
                    if (value >= INT_MIN && value <= INT_MAX) {
                        out_val.set_constval(value);
                        out_val.set_type(types_->type_int());
                    }
                }
            }
            break;
        default:
            assert(false);
    }

    if (out_val.ident != iCONSTEXPR)
        out_val.ident = iEXPRESSION;
    return true;
}

bool Semantics::CheckIncDecExpr(IncDecExpr* incdec, uint32_t flags) {
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
        if (expr_val.sym() && expr_val.sym()->is_const()) {
            report(incdec, 22); /* assignment to const argument */
            return false;
        }
        markusage(expr_val, uWRITTEN);
        if (!(flags & EXPR_DISCARD_RESULT))
            markusage(expr_val, uREAD);
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

    // :TODO: more type checks
    auto& val = incdec->val();
    val.ident = iEXPRESSION;
    val.set_type(type);
    return true;
}

BinaryExpr::BinaryExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
  : BinaryExprBase(ExprKind::BinaryExpr, pos, token, left, right)
{
}


static inline bool CanPromoteToInt64(Type* type) {
    return type->isInt() || type->isAny();
}

bool Semantics::CheckBinaryExprImpl(BinaryExprState& state) {
    if (!CheckExpr(state.left))
        return false;

    if (state.expr->token() == '=') {
        if (!state.rhs_resolved && !CheckRvalue(state.right, state.left->val().type()))
            return false;
    } else {
        if (!CheckRvalue(state.right))
            return false;
    }

    int token = state.expr->token();
    int op_token = NormalizeBinaryToken(token);

    if (IsAssignOp(token)) {
        // Mark the left-hand side as written as soon as we can.
        markusage(state.left->val(), uWRITTEN);
        if (Decl* sym = state.left->val().sym()) {
            // If it's an outparam, also mark it as read.
            if (sym->vclass() == sARGUMENT &&
                (sym->type()->isReference() ||
                 sym->type()->isArray() ||
                 sym->type()->isEnumStruct()))
            {
                markusage(sym, uREAD);
            }
        } else if (auto* accessor = state.left->val().accessor()) {
            if (!accessor->setter()) {
                report(state.expr, 152) << accessor->name();
                return false;
            }
            markusage(accessor->setter(), uREAD);
            if (accessor->getter() && token != '=')
                markusage(accessor->getter(), uREAD);
        }

        if (!CheckAssignmentLHS(state))
            return false;
        if (token != '=' && !CheckRvalueAccess(state.left))
            return false;
    } else if (state.left->lvalue()) {
        if (!CheckRvalueAccess(state.left))
            return false;
        state.left = state.expr->set_left(new RvalueExpr(state.left));
    }

    // RHS is always loaded. Note we do this after validating the left-hand side,
    // so ValidateAssignment has an original view of RHS.
    if (state.right->lvalue())
        state.right = state.expr->set_right(new RvalueExpr(state.right));

    auto left_type = state.left->val().type();
    if (left_type->isReference())
        left_type = left_type->inner();

    auto right_type = state.right->val().type();
    assert(!right_type->isReference());

    auto& val = state.expr->val();

    Type* assign_type;
    std::optional<BinaryOperator> op;
    if (token != '=') {
        op = FindBinaryOperator(op_token, left_type, right_type);
        if (!op) {
            report(state.expr, 461) << get_token_string(token) << left_type << right_type;
            return false;
        }

        if (op->left.ck == ConversionKind::TagMismatch)
            report(state.left, 213) << op->left.type << left_type;
        if (op->right.ck == ConversionKind::TagMismatch)
            report(state.right, 213) << op->right.type << right_type;

        if (!op->right.IsNop())
            state.right = state.expr->set_right(BuildConversion(state.right, op->right));
        if (!op->left.IsNop() && !IsAssignOp(token))
            state.left = state.expr->set_left(BuildConversion(state.left, op->left));

        if (IsCompare(token))
            assign_type = types_->type_bool();
        else
            assign_type = op->left.type;
    } else {
        assign_type = right_type;
    }

    if (IsAssignOp(token)) {
        // Check that there is a valid conversion from the right-hand side to the left.
        ConversionKind ck;
        if (auto constant_ck = FindConstantConversion(state.right, assign_type, left_type,
                                                      CvtContext::Assignment))
        {
            ck = *constant_ck;
        } else {
            ck = FindConversion(assign_type, left_type, CvtContext::Assignment);
        }
        if (ck == ConversionKind::NeedsCast) {
            report(state.expr, 462) << assign_type << left_type;
            return false;
        }
        if (!HasImplicitConversion(ck)) {
            ReportConversionDiagnostic(state.right, left_type, assign_type);
            return false;
        }

        if (ck == ConversionKind::TagMismatch)
            report(state.expr, 213) << left_type << assign_type;

        if (op) {
            // We can't currently handle a left-side conversion for assignment. Can
            // this even happen yet?
            //
            // We also can't encode a conversion of the intermediate result of the compound
            // assignment. Can this happen either? We'd need the result of the operation to have an
            // implicit numeric conversion, which seems impossible.
            if (!op->left.IsNop() || !IsNopConversion(ck)) {
                report(state.expr, 462) << assign_type << left_type;
                return false;
            }
        } else {
            // This is a non-compound assignment with a conversion, so update the right-hand side.
            if (!IsNopConversion(ck))
                state.right = state.expr->set_right(BuildConversion(state.right, ck, left_type));
        }
        val.set_expr(left_type);
    } else {
        val.set_expr(assign_type);
    }

    auto* left_val = &state.left->val();
    auto* right_val = &state.right->val();

    if (left_val->ident == iCONSTEXPR && right_val->ident == iCONSTEXPR &&
        val.type()->coercesFromInt())
    {
        char boolresult = FALSE;
        CheckCoercion(state.expr, left_val->type(), right_val->type(), CvtContext::Operator);
        val.ident = iCONSTEXPR;
        cell folded = calc(left_val->const_i32(), op_token, right_val->const_i32(),
                           &boolresult);

        // If a constant operation overflows, promote it to the next sized up
        // integer.
        if (val.type()->isInt16() &&
            (folded < std::numeric_limits<int16_t>::min() ||
             folded > std::numeric_limits<int16_t>::max()))
        {
            val.set_expr(types_->type_int());
        }
        if (val.type()->isInt8() &&
            (folded < std::numeric_limits<int8_t>::min() ||
             folded > std::numeric_limits<int8_t>::max()))
        {
            val.set_expr(types_->type_int());
        }
        val.set_constval(folded);
    }

    return true;
}

static inline bool IsContextInsideClass(SemaContext& sc, LayoutDecl* cls) {
    if (auto mf = sc.func()->as<MemberFunctionDecl>())
        return mf->parent() == cls;
    return false;
}

static inline bool CheckPrivateMemberAccess(ParseNode* node, Decl* member, LayoutDecl* cls, SemaContext& sc) {
    bool is_private = false;
    if (auto lmd = member->as<LayoutMemberDecl>())
        is_private = lmd->is_private();
    else if (auto fun = member->as<MemberFunctionDecl>())
        is_private = fun->is_private();

    if (is_private && !IsContextInsideClass(sc, cls)) {
        Atom* name = member->name();
        if (auto fun = member->as<MemberFunctionDecl>())
            name = fun->decl_name();
        report(node, 480) << name << cls->name();
    }
    return !is_private || IsContextInsideClass(sc, cls);
}

static bool CheckAccessorAccess(SemaContext& sc, Expr* node, PropertyDecl* prop,
                                MemberFunctionDecl* accessor)
{
    if (accessor->is_private() && !IsContextInsideClass(sc, prop->parent()))
        report(node, 480) << prop->name() << prop->parent()->name();
    return !accessor->is_private() || IsContextInsideClass(sc, prop->parent());
}

bool Semantics::CheckAssignmentLHS(BinaryExprState& state) {
    if (!state.left->lvalue()) {
        report(state.expr, 22);
        return false;
    }

    const auto& left_val = state.left->val();

    // may not change "constant" parameters
    if (!state.expr->initializer() && left_val.sym() && left_val.sym()->is_const()) {
        report(state.expr, 22);
        return false;
    }

    if (auto accessor = left_val.accessor()) {
        if (!accessor->setter()) {
            report(state.expr, 152) << accessor->name();
            return false;
        }
        if (!CheckAccessorAccess(*sc_, state.expr, accessor, accessor->setter()))
            return false;
    }
    return true;
}

bool Semantics::CheckBinaryExpr(BinaryExpr* expr) {
    AutoErrorPos aep(expr->pos());

    BinaryExprState state(expr);
    return CheckBinaryExprImpl(state);
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
            val.set_constval((left_val.const_i32() || right_val.const_i32()));
        else if (expr->token() == tlAND)
            val.set_constval((left_val.const_i32() && right_val.const_i32()));
        else
            assert(false);
    } else {
        val.ident = iEXPRESSION;
    }
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

    bool is_first = true;
    for (auto& op : chain->ops()) {
        Expr* right = op.expr;
        auto left_type = left->val().type();
        auto right_type = right->val().type();

        auto binop = FindBinaryOperator(op.token, left_type, right_type);
        if (!binop) {
            report(op.pos, 461) << get_token_string(op.token) << left_type << right_type;
            return false;
        }

        // For subsequent comparisons, the left operand has already been evaluated
        // (it was the right operand of the previous comparison). We cannot apply
        // coercions to it.
        if (!is_first && !binop->left.IsNop()) {
            report(op.pos, 461) << get_token_string(op.token) << left_type << right_type;
            return false;
        }

        if (binop->left.ck == ConversionKind::TagMismatch)
            report(left, 213) << binop->left.type << left_type;
        if (binop->right.ck == ConversionKind::TagMismatch)
            report(right, 213) << binop->right.type << right_type;

        if (!binop->right.IsNop())
            op.expr = BuildConversion(op.expr, binop->right);
        if (is_first && !binop->left.IsNop())
            first = chain->set_first(BuildConversion(first, binop->left));

        if (right->val().ident != iCONSTEXPR)
            all_const = false;

        // Fold constants as we go.
        if (all_const) {
            const auto& left_val = left->val();
            const auto& right_val = right->val();
            switch (op.token) {
                case tlLE:
                    constval &= left_val.const_i32() <= right_val.const_i32();
                    break;
                case tlGE:
                    constval &= left_val.const_i32() >= right_val.const_i32();
                    break;
                case '>':
                    constval &= left_val.const_i32() > right_val.const_i32();
                    break;
                case '<':
                    constval &= left_val.const_i32() < right_val.const_i32();
                    break;
                default:
                    assert(false);
                    break;
            }
        }

        left = op.expr;
        is_first = false;
    }

    if (all_const)
        val.set_constval(constval ? 1 : 0);
    return true;
}

bool Semantics::CheckTernaryExpr(TernaryExpr* expr, Type* target) {
    AutoErrorPos aep(expr->pos());

    auto first = expr->first();
    auto second = expr->second();
    auto third = expr->third();

    if (!CheckRvalue(first))
        return false;

    if (target) {
        if (!CheckRvalue(second, target))
            return false;
        if (!CheckRvalue(third, target))
            return false;
    } else {
        if (!CheckRvalue(second) || !CheckRvalue(third))
            return false;
    }

    if (first->lvalue()) {
        first = expr->set_first(new RvalueExpr(first));
    } else if (first->val().ident == iCONSTEXPR) {
        report(first, first->val().const_i32() ? 206 : 205);
    }

    if (first->val().type()->isWideType())
        first = expr->set_first(BuildSimpleCast(first, BuiltinType::Bool));

    if (second->lvalue())
        second = expr->set_second(new RvalueExpr(second));
    if (third->lvalue())
        third = expr->set_third(new RvalueExpr(third));

    QualType out_type = second->val().qualified();

    if (second->val().type() != third->val().type()) {
        if (second->val().type()->isArray() && third->val().type()->isArray()) {
            auto left_array = second->val().type()->to<ArrayType>();
            auto right_array = third->val().type()->to<ArrayType>();
            int size = (left_array->size() == right_array->size()) ? left_array->size() : 0;

            // If sizes aren't equal, decay the result type to be unsized.
            if (!size)
                out_type = types_->defineArray(left_array->inner(), 0);

            if (left_array->is_flat()) {
                auto type = types_->defineArray(left_array->inner(), size);
                auto slice = new SliceExpr(second, nullptr, type);
                second = expr->set_second(slice);
            }
            if (right_array->is_flat()) {
                auto type = types_->defineArray(right_array->inner(), size);
                auto slice = new SliceExpr(third, nullptr, type);
                third = expr->set_third(slice);
            }
        }
    }

    const auto& left = second->val();
    const auto& right = third->val();

    {
        auto left_to_right = FindConversion(left.type(), right.type(), CvtContext::Operator);
        auto right_to_left = FindConversion(right.type(), left.type(), CvtContext::Operator);

        // Ternary allows char arrays of different sizes, as long as one
        // fits in the other.
        if (!HasImplicitConversion(left_to_right) && !HasImplicitConversion(right_to_left) &&
            left.type()->isCharArray() && right.type()->isCharArray())
        {
            auto left_array = left.type()->to<ArrayType>();
            auto right_array = right.type()->to<ArrayType>();
            if (!left_array->size() || !right_array->size() ||
                left_array->size() >= right_array->size())
            {
                left_to_right = ConversionKind::None;
            }
            if (!left_array->size() || !right_array->size() ||
                right_array->size() >= left_array->size())
            {
                right_to_left = ConversionKind::None;
            }
        }

        bool use_left_to_right = false;

        if (HasImplicitConversion(left_to_right) && HasImplicitConversion(right_to_left)) {
            use_left_to_right = static_cast<uint32_t>(left_to_right) >=
                                static_cast<uint32_t>(right_to_left);
        } else if (HasImplicitConversion(left_to_right)) {
            use_left_to_right = true;
        } else if (!HasImplicitConversion(right_to_left)) {
            ReportConversionDiagnostic(second, left.type(), right.type());
            return false;
        }

        if (use_left_to_right)
            second = expr->set_second(BuildConversion(second, left_to_right, right.type()));
        else
            third = expr->set_third(BuildConversion(third, right_to_left, left.type()));

        auto ck = use_left_to_right ? left_to_right : right_to_left;
        if (ck == ConversionKind::TagMismatch)
            report(second->pos(), 213) << left.type() << right.type();
    }

    second = expr->set_second(CoerceNull(second, right.type()));
    third = expr->set_third(CoerceNull(third, left.type()));

    auto& val = expr->val();
    val.set_expr(out_type);
    return true;
}


static inline bool IsValidIntWidthChange(Type* from, Type* to) {
    // allow double to/from int64, but not intptr, which is not guaranteed
    // to be 64-bit.
    if ((from->isInt64() && to->isDouble()) || (from->isDouble() && to->isInt64()))
        return true;

    if (from->isWideInt()) {
        return to->isInt() ||
               to->isInt16() ||
               to->isInt8() ||
               to->isWideInt();
    }
    if (to->isWideInt()) {
        return from->isInt() ||
               from->isAny() ||
               from->isInt16() ||
               from->isInt8();
    }
    return false;
}

static inline bool CastNeedsRvalue(const ExprVal& out_val, Type* to_type) {
    if (out_val.ident == iACCESSOR)
        return true;
    if (out_val.type()->isWideInt() || to_type->isWideInt())
        return true;
    if (out_val.type()->isChar())
        return true;
    if (out_val.type()->podLoadSize() != to_type->podLoadSize())
        return true;
    return false;
}

bool Semantics::CheckCastExpr(CastExpr* expr) {
    AutoErrorPos aep(expr->pos());

    Type* to_type = expr->type();
    if (to_type->isVoid()) {
        report(expr, 144);
        return false;
    }

    auto inner = expr->expr();
    if (auto array = inner->as<ArrayExpr>()) {
        Type* target_array = types_->defineArray(to_type, (int)array->exprs().size());
        if (!CheckRvalue(array, target_array))
            return false;
    } else {
        if (!CheckExpr(inner))
            return false;
    }

    auto& out_val = expr->val();
    out_val = inner->val();

    Type* from_type = out_val.type();
    if (from_type == to_type) {
        if (expr->lvalue())
            out_val.ident = iADDRESS;
        return true;
    }

    auto actual_array =  from_type->as<ArrayType>();
    if (actual_array) {
        // Unwind back to the inner.
        auto iter = actual_array;
        for (;;) {
            if (!iter->inner()->isArray())
                break;
            iter = iter->inner()->to<ArrayType>();
        }
        from_type = iter->inner();
    }

    if (from_type->isObject()) {
        report(expr, 477) << from_type;
    } else if (to_type->isObject()) {
        report(expr, 477) << to_type;
    } else if (from_type->isFunctionLike() != to_type->isFunctionLike()) {
        // Warn: unsupported cast.
        Type* func_type = to_type->isFunctionLike() ? to_type : from_type;
        if (!func_type->isLegacyFunction()) {
            report(expr, 460) << from_type << to_type;
            return false;
        }
        report(expr, 237);
    } else if (from_type->isFunctionLike() && to_type->isFunctionLike()) {
        inner = TryConversion(inner, to_type, CvtContext::Assignment);
        if (!inner)
            return false;
        expr->set_expr(inner);
        out_val = inner->val();
    } else if (out_val.type()->isVoid()) {
        report(expr, 89);
    } else if (to_type->isEnumStruct() || from_type->isEnumStruct()) {
        report(expr, 95) << to_type;
    }
    if (from_type->isReference() && !to_type->isReference()) {
        if (to_type->isEnumStruct()) {
            report(expr, 136);
            return false;
        }
        to_type = types_->defineReference(to_type);
    }

    ArrayType* to_array_type = nullptr;
    if (actual_array)
        to_array_type = types_->redefineArray(to_type, actual_array);

    if (actual_array) {
        Type* target_elem = to_type;
        if (auto target_array = to_type->as<ArrayType>()) {
            auto iter = target_array;
            for (;;) {
                if (!iter->inner()->isArray())
                    break;
                iter = iter->inner()->to<ArrayType>();
            }
            target_elem = iter->inner();
        }
        if (!AreSliceElementsCompatible(from_type, target_elem)) {
            report(expr, 460) << expr->expr()->val().type() << to_array_type;
            return false;
        }
    }
    if (actual_array && from_type->isInt64()) {
        report(expr, 460) << actual_array << to_array_type;
        return false;
    }

    if (actual_array)
        to_type = to_array_type;

    if (out_val.type()->isWideInt() || to_type->isWideInt()) {
        if (!IsValidIntWidthChange(out_val.type(), to_type)) {
            report(expr, 460) << out_val.type() << to_type;
            return false;
        }
    }

    if (to_type->isFloat() != out_val.type()->isFloat()) {
        auto other_type = out_val.type()->isFloat() ? to_type : out_val.type();
        if (other_type->podLoadSize() != 4) {
            report(expr, 460) << out_val.type() << to_type;
            return false;
        }
    }

    // Reject any view_as involving double except int64 <-> double (which is
    // already allowed by the isWideInt check above).
    if (to_type->isDouble() != out_val.type()->isDouble() &&
        !to_type->isInt64() && !out_val.type()->isInt64())
    {
        report(expr, 460) << out_val.type() << to_type;
        return false;
    }

    if (CastNeedsRvalue(out_val, to_type)) {
        if (inner->lvalue())
            expr->set_expr(new RvalueExpr(inner));
        out_val.ident = iEXPRESSION;
    }

    if (expr->lvalue())
        out_val.ident = iADDRESS;

    out_val.set_type(to_type);
    return true;
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
    switch (decl->kind()) {
        case StmtKind::VarDecl:
        case StmtKind::ArgDecl:
            val.set_variable(decl->as<VarDeclBase>(), decl->type());
            return true;
        case StmtKind::ConstDecl:
        case StmtKind::EnumFieldDecl:
            val.set_constval(decl->ConstVal());
            break;
        case StmtKind::FunctionDecl:
        case StmtKind::MemberFunctionDecl:
            val.set_function(decl->as<FunctionDecl>());
            break;
        case StmtKind::ClassDecl:
        case StmtKind::EnumStructDecl:
        case StmtKind::MethodmapDecl:
            val.set_typename(decl);
            break;
        case StmtKind::EnumDecl: {
            auto es = decl->as<EnumDecl>();
            if (!es->mm()) {
                report(expr, 174) << decl->name();
                return false;
            }
            val.set_typename(decl);
            break;
        }
        case StmtKind::UpvarDecl:
            val.set_upvar(decl->as<UpvarDecl>(), decl->type());
            return true;
        default:
            assert(false);
    }

    QualType type = decl->type();
    val.set_type(type);

    if (auto fun = decl->as<FunctionDecl>()) {
        fun = fun->canonical();
        if (fun->is_native()) {
            report(expr, 76);
            return false;
        }
        if (fun->return_array() || fun->return_type()->isArray()) {
            report(expr, 182);
            return false;
        }
        if (!fun->impl()) {
            report(expr, 4) << fun->name();
            return false;
        }

        val.ident = iEXPRESSION;
        val.set_type(fun->type());

        // Mark as being indirectly invoked. Direct invocations go through
        // BindCallTarget.
        fun->set_is_callback();
        markusage(fun, uREAD);
    }

    if (val.ident == iTYPENAME) {
        if (!allow_types) {
            report(expr, 174) << decl->name();
            return false;
        }
    }
    return true;
}

bool Semantics::CheckCommaExpr(CommaExpr* comma) {
    AutoErrorPos aep(comma->pos());

    size_t index = 0;
    for (auto& expr : comma->exprs()) {
        if (!CheckRvalue(expr))
            return false;
        if (expr->lvalue())
            expr = new RvalueExpr(expr);
        if (!expr->HasSideEffects())
            report(expr, 231) << index;
        index++;
    }

    const auto& last = comma->exprs().back();
    comma->val().set_expr(last->val().qualified());
    return true;
}


bool Semantics::CheckArrayExpr(ArrayExpr* array, Type* target) {
    AutoErrorPos aep(array->pos());

    if (!target) {
        report(array->pos(), 142);
        return false;
    }

    // Handle enum struct target — validate {x, y, ...} against struct fields.
    if (auto es = target->asEnumStruct()) {
        if (!ValidateEnumStructInitializer(es, array))
            return false;
        array->val().set_expr(target);
        return true;
    }

    auto array_target = target->as<ArrayType>();
    if (!array_target) {
        report(array->pos(), 142);
        return false;
    }

    Type* formal_elt = array_target->inner();

    for (const auto& entry : array->exprs()) {
        if (entry->as<ArrayExpr>()) {
            if (!CheckRvalue(entry, formal_elt))
                return false;
        } else {
            if (!CheckExpr(entry))
                return false;

            const auto& val = entry->val();
            if (val.ident != iCONSTEXPR) {
                report(entry, 8);
                return false;
            }

            if (!CheckCoercion(entry, formal_elt, val.type(), CvtContext::Assignment))
                return false;
        }
    }

    auto& val = array->val();
    val.ident = iEXPRESSION;
    val.set_type(types_->defineArray(formal_elt, (int)array->exprs().size()));
    return CheckRvalueAccess(array);
}

bool Semantics::CheckIndexExpr(IndexExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto base = expr->base();
    auto index = expr->index();
    if (!CheckRvalue(base))
        return false;
    if (base->lvalue())
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
                if (index_val.const_i32() < 0 ||
                    (array->size() != 0 && array->size() <= index_val.const_i32()))
                {
                    report(index, 32);
                    return false;
                }
            } else {
                /* character index */
                if (index_val.const_i32() < 0 ||
                    (array->size() != 0 && array->size() <= index_val.const_i32()))
                {
                    report(index, 32);
                    return false;
                }
            }
        }
    }

    auto& out_val = expr->val();
    out_val = base_val;

    out_val.set_slice(iARRAYELEM, QualType(array->inner()));
    return true;
}

bool Semantics::CheckThisExpr(ThisExpr* expr) {
    auto sym = expr->decl();
    assert(sym->as<ArgDecl>());

    auto& val = expr->val();
    val.set_variable(sym, sym->type());
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

bool Semantics::CheckNumber64Expr(Number64Expr* expr) {
    auto num64 = expr->ToInt64();
    if (!num64) {
        report(expr, 135);
        return false;
    }

    auto& val = expr->val();
    val.ident = iEXPRESSION;
    val.set_type(types_->type_int64());
    return true;
}

bool Semantics::CheckDoubleExpr(DoubleExpr* expr) {
    auto& val = expr->val();
    val.ident = iEXPRESSION;
    val.set_type(types_->type_double());
    return true;
}

bool Semantics::CheckStringExpr(StringExpr* expr, Type* target) {
    auto& val = expr->val();
    val.ident = iEXPRESSION;

    auto arr = target ? target->as<ArrayType>() : nullptr;
    if (arr && arr->size() > 0 && arr->inner()->isChar()) {
        size_t needed = arr->size();
        size_t current = expr->text()->length() + 1;
        if (current < needed) {
            std::string new_str = expr->text()->str();
            new_str.append((needed - 1) - new_str.length(), '\0');
            expr->set_text(cc_.atom(new_str));
        }
    }

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
        Decl* typename_decl = base_val.typename_decl();
        auto layout = typename_decl->as<LayoutDecl>();
        if (!layout) {
            report(expr, 444) << typename_decl->name() << expr->name();
            return false;
        }

        auto member = layout->FindMember(expr->name());
        if (!member || !member->as<MemberFunctionDecl>()) {
            report(expr, 444) << typename_decl->name() << expr->name();
            return false;
        }
        auto method = member->as<MemberFunctionDecl>();
        if (!method->is_static()) {
            report(expr, 176) << method->decl_name() << typename_decl->name();
            return false;
        }
        expr->set_resolved(method);
        val.set_function(method);
        markusage(method, uREAD);
        return true;
    }

    Type* base_type = base_val.type();
    if (auto es = base_type->asEnumStruct()) {
        if (base->lvalue())
            base = expr->set_base(new RvalueExpr(base));
        return CheckEnumStructFieldAccessExpr(expr, base_type, es, from_call);
    }
    if (base_type->isReference())
        base_type = base_type->inner();

    if (auto cls = base_type->asClass()) {
        if (base->lvalue())
            base = expr->set_base(new RvalueExpr(base));
        return CheckClassFieldAccessExpr(expr, base_type, cls, from_call);
    }

    auto map = base_type->asMethodmap();
    if (!map) {
        if (base_val.type()->isFunctionLike())
            report(expr, 104) << "function";
        else
            report(expr, 104) << base_val.type();
        return false;
    }

    auto member = map->FindMember(expr->name());
    if (!member) {
        report(expr, 105) << map->name() << expr->name();
        return false;
    }

    if (auto prop = member->as<PropertyDecl>()) {
        // This is the only scenario in which we need to compute a load of the
        // base address. Otherwise, we're only accessing the type.
        if (base->lvalue())
            base = expr->set_base(new RvalueExpr(base));
        val.set_type(prop->property_type());
        val.set_accessor(prop);
        return true;
    }

    auto method = member->as<MemberFunctionDecl>();
    if (method->is_static()) {
        report(expr, 177) << method->decl_name() << map->name() << method->decl_name();
        return false;
    }
    expr->set_resolved(method);

    if (!from_call) {
        report(expr, 50);
        return false;
    }

    val.set_function(method);
    markusage(method, uREAD);
    return true;
}

CallTarget Semantics::BindCallTarget(CallExpr* call, Expr* target) {
    AutoErrorPos aep(target->pos());

    switch (target->kind()) {
        case ExprKind::FieldAccessExpr: {
            auto expr = target->to<FieldAccessExpr>();
            if (!CheckFieldAccessExpr(expr, true))
                return {};

            auto& val = expr->val();
            if (val.ident != iFUNCTN) {
                report(target, 12);
                return {};
            }

            // The static accessor (::) is offsetof(), so it can't return functions.
            assert(expr->token() == '.');

            auto resolved = expr->resolved();
            if (auto method = resolved->as<MemberFunctionDecl>()) {
                if (auto map = method->parent()->as<MethodmapDecl>()) {
                    if (map->ctor() == method) {
                        report(call, 84) << method->parent()->name();
                        return {};
                    }
                }
            }

            auto method = resolved->as<MemberFunctionDecl>();
            assert(resolved->as<LayoutFieldDecl>() || method);

            auto base = expr->base();
            if (base->lvalue())
                base = expr->set_base(new RvalueExpr(base));
            if (resolved->as<LayoutFieldDecl>() || !method->is_static())
                call->set_implicit_this(base);
            return val.fun()->canonical();
        }
        case ExprKind::SymbolExpr: {
            call->set_implicit_this(nullptr);

            auto expr = target->to<SymbolExpr>();
            auto decl = expr->decl();
            if (auto mm = decl->as<MethodmapDecl>()) {
                if (!mm->ctor()) {
                    // Immediately fatal - no function to call.
                    report(target, 172) << decl->name();
                    return {};
                }
                if (mm->nullable()) {
                    // Keep going, this is basically a style thing.
                    report(target, 170) << decl->name();
                    return {};
                }
                return mm->ctor();
            }
            if (auto fun = decl->as<FunctionDecl>()) {
                fun = fun->canonical();
                if (!(fun->is_native() || fun->is_builtin()) && !fun->impl()) {
                    report(target, 4) << decl->name();
                    return {};
                }
                return fun;
            }
            [[fallthrough]];
        }
        default: {
            if (!CheckRvalue(target))
                return {};

            if (target->lvalue())
                target = new RvalueExpr(target);

            if (auto ft = target->val().type()->as<FunctionType>()) {
                if (ft->conv() == FunctionType::Legacy)
                    report(target, 33);
                return target;
            }

            report(target, 12);
            return {};
        }
    }
}

auto Semantics::BindNewTarget(Expr* target) -> std::optional<CallCtor> {
    AutoErrorPos aep(target->pos());

    switch (target->kind()) {
        case ExprKind::SymbolExpr: {
            auto expr = target->to<SymbolExpr>();
            auto decl = expr->decl();

            if (auto class_decl = decl->as<ClassDecl>()) {
                auto class_type = class_decl->type();
                if (class_decl->ctor())
                    return CallCtor{class_decl->ctor(), class_type.unqualified()};
                return CallCtor{nullptr, class_type.unqualified()};
            }

            auto mm = MethodmapDecl::LookupMethodmap(decl);
            if (!mm) {
                report(expr, 116) << decl->name();
                return {};
            }

            if (!mm->nullable()) {
                report(expr, 171) << mm->name();
                return {};
            }
            if (!mm->ctor()) {
                report(expr, 172) << mm->name();
                return {};
            }
            return CallCtor{mm->ctor(), nullptr};
        }
    }
    return {};
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

        val.set_function(fun);
        markusage(fun, uREAD);
        return true;
    }

    auto field = field_decl->as<LayoutFieldDecl>();
    assert(field);

    val.set_field(field, field->type());
    return true;
}

bool Semantics::CheckClassFieldAccessExpr(FieldAccessExpr* expr, Type* type, ClassDecl* decl,
                                           bool from_call)
{
    Decl* member = FindClassField(type, expr->name());
    if (!member) {
        report(expr, 105) << type << expr->name();
        return false;
    }

    if (!CheckPrivateMemberAccess(expr, member, decl, *sc_))
        return false;

    // Only set resolved() for properties and methods. Regular fields carry
    // their info in val() via set_field(), and EmitFieldAccessExpr asserts
    // if resolved() is a LayoutFieldDecl.
    if (!member->as<LayoutFieldDecl>())
        expr->set_resolved(member);

    auto& val = expr->val();
    if (auto prop = member->as<PropertyDecl>()) {
        if (expr->base()->lvalue())
            expr->set_base(new RvalueExpr(expr->base()));
        val.set_type(prop->property_type());
        val.set_accessor(prop);
        return true;
    }

    if (auto fun = member->as<MemberFunctionDecl>()) {
        if (!from_call) {
            report(expr, 76);
            return false;
        }

        val.set_function(fun);
        markusage(fun, uREAD);
        return true;
    }

    auto field = member->as<LayoutFieldDecl>();
    assert(field);

    val.set_field(field, field->type());
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
    val.set_expr(types_->type_int());
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
        case iARRAYELEM:
        case iVARIABLE:
        case iEXPRESSION:
        case iFIELD:
            if (cv.type()->asEnumStruct()) {
                val.set_expr(QualType(types_->type_int()));
            } else if (auto array = cv.type()->as<ArrayType>()) {
                if (!array->size()) {
                    report(child, 163);
                    return false;
                }
                val.set_constval(array->size());
            } else if (cv.ident == iEXPRESSION) {
                if (auto access = child->as<FieldAccessExpr>()) {
                    if (access->token() == tDBLCOLON) {
                        auto field = access->resolved()->as<LayoutFieldDecl>();
                        if (auto array = field->type()->as<ArrayType>())
                            val.set_constval(array->size());
                        else if (field->type()->asEnumStruct())
                            val.set_expr(QualType(types_->type_int()));
                        else
                            val.set_constval(1);
                        return true;
                    }
                }
                report(child, 72);
                return false;
            } else {
                if (cv.type()->isIntPtr()) {
                    report(expr, 449) << cv.type();
                    return false;
                }
                val.set_constval(1);
                report(expr, 252);
            }
            return true;

        case iTYPENAME: {
            auto es = cv.typename_decl()->as<EnumStructDecl>();
            if (!es) {
                report(child, 72);
                return false;
            }
            val.set_expr(QualType(types_->type_int()));
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
            else if (field->type()->asEnumStruct())
                val.set_expr(QualType(types_->type_int()));
            else
                val.set_constval(1);
            return true;
        }

        default:
            report(child, 72);
            return false;
    }
}

DefaultArgExpr::DefaultArgExpr(const token_pos_t& pos, ArgDecl* arg)
  : Expr(ExprKind::DefaultArgExpr, pos),
    arg_(arg)
{
    // Leave val bogus, it doesn't participate in anything, and we can't
    // accurately construct it.
}

static inline bool IsValidInt64RefArg(Type* param) {
    if (param->isWideInt())
        return true;
    if (param->isReference() && param->inner()->isWideInt())
        return true;
    return false;
}

bool Semantics::CheckCallExpr(CallExpr* call) {
    AutoErrorPos aep(call->pos());

    FunctionDecl* fun = nullptr;
    Expr* target = nullptr;
    Type* ctor_type = nullptr;

    if (call->token() == tNEW) {
        auto ctor = BindNewTarget(call->target());
        if (!ctor)
            return false;
        fun = ctor->first;
        ctor_type = ctor->second;
    } else {
        auto result = BindCallTarget(call, call->target());
        if (auto target_fun = std::get_if<FunctionDecl*>(&result)) {
            fun = *target_fun;
        } else if (auto target_expr = std::get_if<Expr*>(&result)) {
            target = *target_expr;
        } else {
            return false;
        }
    }

    // If we have no function and no target, it means we're getting a
    // constructor call with no user constructor function.
    if (fun) {
        assert(fun->canonical() == fun);
        call->set_callee(fun);

        if (fun->return_type()->isArray() || fun->return_type()->isEnumStruct()) {
            if (fun->is_analyzing() || !CheckFunctionDecl(fun)) {
                report(call, 411);
                return false;
            }
        }

        markusage(fun, uREAD);

        if (fun->deprecate())
            report(call, 234) << fun->name() << fun->deprecate();
    } else if (target) {
        call->set_target(target);
        call->set_callee(target->val().type()->to<FunctionType>());
    }

    auto& val = call->val();

    if (ctor_type) {
        call->set_ctor_type(ctor_type);

        if (!fun) {
            if (!call->args().empty()) {
                report(call->pos(), 92);
                return false;
            }
            val.set_expr(ctor_type);
            return true;
        }
    }

    // Note: must read function_type() after CheckFunctionDecl, since
    // recursive analysis can update the return type.
    FunctionType* ft = call->callee_type();

    ParamState ps;

    // The |this| argument is even more implicit for NEWOBJ, since it's supplied
    // by the VM. Thus, skip analysis of argument 0 if we have a ctor_type.
    unsigned int nargs = ctor_type ? 1 : 0;
    unsigned int first_argidx = ctor_type ? 1 : 0;
    unsigned int argidx = first_argidx;

    if (call->implicit_this()) {
        if (ft->nargs() == 0) {
            report(call->implicit_this(), 92);
            return false;
        }
        Expr* param = CheckArgument(call, ft, ft->arg_type(0), call->implicit_this(), &ps, 0);
        if (!param)
            return false;
        ps.argv[0] = param;
        nargs++;
        argidx++;
    }

    bool namedparams = false;
    for (const auto& entry : call->args()) {
        unsigned int argpos;

        Expr* param = entry;
        if (auto named = param->as<NamedArgExpr>()) {
            if (!fun) {
                report(call, 421);
                continue;
            }
            int pos = fun->FindNamedArg(named->name);
            if (pos < 0) {
                report(call, 17) << named->name;
                break;
            }
            argpos = pos;
            argidx = pos;
            param = named->expr;
        } else {
            if (namedparams) {
                report(call, 44); // positional parameters must precede named parameters
                return false;
            }
            argpos = nargs;
            if (!ft->variadic() && argidx >= ft->nargs()) {
                report(param, 92);
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
        auto formal = argidx < ft->nargs() ? ft->arg_type(argidx) : QualType{};
        auto result = CheckArgument(call, ft, formal, param, &ps, argpos);
        if (!result)
            return false;

        ps.argv[argpos] = result;
        nargs++;

        // Detect an edge case at compile-time to avoid a very confusing runtime
        // error. If the argument of the callee is captured, and we're passing a
        // stack array, the VM will throw a runtime error due to an escaping
        // slice. Warn about this at compile time.
        if (fun && argidx < fun->args().size()) {
            auto formal_param = fun->args()[argidx];
            if (auto* array = formal_param->type()->as<ArrayType>()) {
                if (formal_param->is_captured() && !array->is_fixed()) {
                    report(param, 487);
                    return false;
                }
            }
        }

        // Don't iterate past the varargs position.
        if (!ft->variadic() || argidx < ft->nargs())
            argidx++;
    }

    if (!sc_->func()) {
        report(call, 10);
        return false;
    }

    // Check for missing or invalid extra arguments, and fill in default
    // arguments.
    for (unsigned int argidx = first_argidx; argidx < ft->nargs(); argidx++) {
        if (argidx >= ps.argv.size() || !ps.argv[argidx]) {
            auto result = CheckArgument(call, ft, ft->arg_type(argidx), nullptr, &ps, argidx);
            if (!result)
                return false;
            ps.argv[argidx] = result;
        }
    }

    // Copy newly deduced argument information.
    if (call->args().size() == ps.argv.size()) {
        for (size_t i = 0; i < ps.argv.size(); i++)
            call->args()[i] = ps.argv[i];
    } else {
        new (&call->args()) PoolArray<Expr*>(ps.argv);
    }

    if (ctor_type)
        val.set_expr(ctor_type);
    else
        val.set_expr(ft->return_type());
    return true;
}

// Note: currently formal is null for variadic arguments. We don't really
// bother checking legacy vararg types anymore.
Expr* Semantics::CheckArgument(CallExpr* call, FunctionType* ft, QualType formal,
                               Expr* param, ParamState* ps, unsigned int pos)
{
    while (pos >= ps->argv.size())
        ps->argv.push_back(nullptr);

    unsigned int visual_pos = call->implicit_this() ? pos : pos + 1;

    if (!param || param->as<DefaultArgExpr>()) {
        if (!formal) {
            report(call, 92); // argument count mismatch
            return nullptr;
        }
        auto fun = call->fun();
        if (!fun || !fun->args()[pos]->init_rhs()) {
            report(call, 34) << visual_pos; // argument has no default value
            return nullptr;
        }
        auto arg = fun->args()[pos];

        if (!param)
            param = new DefaultArgExpr(call->pos(), arg);
        else
            param->as<DefaultArgExpr>()->set_arg(arg);

        if (param->val().type())
            assert(!param->val().type()->isInt64());

        // The rest of the code to handle default values is in DoEmit.
        return param;
    }

    if (param->as<SpreadArgsExpr>()) {
        if (!ft->variadic()) {
            report(param, 474);
            return nullptr;
        }
        if (!sc_->func() || !sc_->func()->IsVariadic()) {
            report(param, 475);
            return nullptr;
        }
        if (param != call->args().back()) {
            report(param, 476);
            return nullptr;
        }
        return param;
    }

    if (param != call->implicit_this()) {
        if (formal) {
            if (!CheckRvalue(param, *formal))
                return nullptr;
        } else {
            if (!CheckExpr(param))
                return nullptr;
        }
    }

    AutoErrorPos aep(param->pos());

    if (param->val().ident == iACCESSOR) {
        if (!CheckRvalueAccess(param))
            return nullptr;
        param = new RvalueExpr(param);
    }

#ifndef NDEBUG
    bool handling_this = call->implicit_this() && (pos == 0);
#endif

    const auto* val = &param->val();
    bool lvalue = param->lvalue();
    if (!formal) {
        // We don't pass down a type for variadic arguments.
        assert(!handling_this);

        // Always pass by reference.
        if (val->ident == iVARIABLE) {
            if (val->sym()->is_const() && !formal.is_const()) {
                // Treat a "const" variable passed to a function with a
                // non-const "variable argument list" as a constant here.
                if (!lvalue) {
                    report(param, 22); // need lvalue
                    return nullptr;
                }
            }
        }
        if (val->type()->isVoid()) {
            report(param, 466);
            return nullptr;
        }

        Type* type = val->type();
        if (type->isInt64() || (type->isReference() && type->inner()->isInt64())) {
            // Hack: allow this since we don't have typed varargs right now.
        } else if (type->isArray()) {
            // Arrays are allowed in varargs.
        } else {
            // Varargs have no specific type to coerce against.
        }
        if (auto slice = ParamNeedsSliceWrapper(param, nullptr))
            param = slice;
        if (param->lvalue() && val->type()->isNonFlatArray()) {
            param = new RvalueExpr(param);
            val = &param->val();
        }
    } else if (formal->isReference()) {
        assert(!handling_this);

        if (!lvalue ||
            (val->ident == iARRAYELEM &&
             (val->type()->maybe_lit_size().value_or(4) != 4)))
        {
            report(param, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }
        if (val->sym() && val->sym()->is_const() && !formal.is_const()) {
            report(param, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }

        if (formal->inner()->isWideInt()) {
            if (!IsValidInt64RefArg(val->type())) {
                report(param, 134) << *formal << val->type();
                return nullptr;
            }
        } else {
            if (IsValidInt64RefArg(val->type())) {
                report(param, 134) << *formal << val->type();
                return nullptr;
            }
            CheckCoercion(param, formal->inner(), QualType(val->type()), CvtContext::Argument);
        }
    } else if (auto to_array = formal->as<ArrayType>()) {
        if (auto slice = ParamNeedsSliceWrapper(param, to_array))
            param = slice;
        if (param->lvalue())
            param = new RvalueExpr(param);

        val = &param->val();

        auto type = val->type();
        if (!CheckCoercion(param, *formal, QualType(type), CvtContext::Argument))
            return nullptr;

        if (auto array = param->as<ArrayExpr>()) {
            if (to_array->is_flat()) {
                auto flat_type = types_->defineFlatArray(to_array->inner(), to_array->size());
                array->val().set_type(flat_type);
            }
        }

        if (val->sym() && val->sym()->is_const() && !formal.is_const()) {
            report(param, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }
    } else {
        if (lvalue) {
            param = new RvalueExpr(param);
            val = &param->val();
        }

        if (val->type()->isInt() && formal->isInt64()) {
            param = BuildSimpleCast(param, BuiltinType::Int64);
            val = &param->val();
        }

        if (!(param = TryConversion(param, *formal, CvtContext::Argument)))
            return nullptr;
        val = &param->val();
    }

    if ((call->fun() && call->fun()->is_native()) || !formal) {
        if (!val->type()->isAllowedInNativeCall())
            ReportInvalidNativeArgument(param, val->type());
    }

    if (formal && param)
        param = CoerceNull(param, *formal);
    return param;
}

bool Semantics::CheckStaticAssertStmt(StaticAssertStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckExpr(expr))
        return false;

    // :TODO: insert coercion to bool.
    cell value;
    if (!AnalyzeForConst(expr, &value))
        return false;

    if (value)
        return true;

    std::string message;
    if (stmt->text())
        message += ": " + std::string(stmt->text()->chars(), stmt->text()->length());

    report(expr, 70) << message;
    return false;
}

bool Semantics::CheckNewArrayExpr(NewArrayExpr* expr) {
    return CheckNewArrayExprForArrayInitializer(expr);
}

bool Semantics::CheckNewArrayExprForArrayInitializer(NewArrayExpr* na) {
    if (na->analyzed())
        return na->analysis_result();

    na->set_analysis_result(false);

    auto& val = na->val();
    val.ident = iEXPRESSION;

    PoolList<int> dims;
    bool seen_null = false;
    for (auto& expr : na->exprs()) {
        if (!expr) {
            seen_null = true;
            dims.emplace_back(0);
            continue;
        }
        if (seen_null) {
            report(na, 185);
            return false;
        }
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
        if (v.ident == iCONSTEXPR && v.const_i32() <= 0) {
            report(expr, 9);
            return false;
        }
        dims.emplace_back(0);
    }
    assert(na->type()->isArray());

    val.set_type(na->type());
    na->set_analysis_result(true);
    return true;
}

bool Semantics::CheckIfStmt(IfStmt* stmt) {
    if (Expr* expr = AnalyzeForTest(stmt->cond()))
        stmt->set_cond(expr);

    // Note: unlike loop conditions, we don't factor in constexprs here, it's
    // too much work and way less common than constant loop conditions.

    if (!CheckStmt(stmt->on_true()))
        return false;
    if (stmt->on_false() && !CheckStmt(stmt->on_false()))
        return false;

    if (stmt->on_false()) {
        FlowType a = stmt->on_true()->flow_type();
        FlowType b = stmt->on_false()->flow_type();
        if (a == b)
            stmt->set_flow_type(a);
    }
    return true;
}

bool Semantics::CheckExprStmt(ExprStmt* stmt) {
    auto expr = stmt->expr();
    if (!CheckRvalue(expr, nullptr, EXPR_DISCARD_RESULT))
        return false;
    if (expr->lvalue())
        expr = stmt->set_expr(new RvalueExpr(expr));

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
    if (auto fun = sym->as<FunctionDecl>()) {
        auto canonical = fun->canonical();
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
    } else if (auto var = sym->as<VarDeclBase>()) {
        if (var->as<ConstDecl>()) {
            if (testconst && !var->is_read())
                report(var, 203) << var->name(); /* symbol isn't used: ... */
            return false;
        }

        // We ignore variables that are marked as public or stock that was included.
        if (var->is_public() || IsIncludedStock(var))
            return false;

        if (!var->is_used()) {
            report(sym, 203) << sym->name(); /* symbol isn't used (and not public/stock) */
        } else if (!var->is_read()) {
            report(sym, 204) << sym->name(); /* value assigned to symbol is never used */
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

    bool ok = true;
    for (const auto& stmt : block->stmts()) {
        cc_.reports()->ResetErrorFlag();

        if (ok && !sc_->warned_unreachable() && block->flow_type() == Flow_Return) {
            report(stmt, 225);
            sc_->set_warned_unreachable();
        }
        ok &= CheckStmt(stmt);

        if (FlowType flow = stmt->flow_type(); flow != Flow_None) {
            if (block->flow_type() == Flow_None)
                block->set_flow_type(flow);
        }
    }

    if (block->scope())
        TestSymbols(block->scope(), true);

    return ok;
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

    bool already_returned = sc_->returns_value();
    sc_->set_returns_value();

    if (fun->return_type()->isInt64() && CanPromoteToInt64(expr->val().type())) {
        expr = stmt->set_expr(BuildSimpleCast(expr, BuiltinType::Int64));
        return true;
    }

    // Check that the return statement matches the declared return type.
    // If a return statement has already been checked, the function's return type
    // is now fixed. We use Assignment to prevent returning a flat array to a
    // dynamic array return type, while the first return uses Return to allow
    // updating the return type.
    CvtContext why = already_returned ? CvtContext::Assignment : CvtContext::Return;
    if ((expr = TryConversion(expr, fun->return_type(), why)) == nullptr)
        return false;
    stmt->set_expr(expr);

    if (expr->val().type()->isEnumStruct() || expr->val().type()->isFixedArray()) {
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
        auto info = new FunctionDecl::ReturnArrayInfo;
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

    auto rai = new FunctionDecl::ReturnArrayInfo;
    info->set_return_array(rai);
    return true;
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

bool Semantics::CheckDoWhileStmt(DoWhileStmt* stmt) {
    if (Expr* expr = AnalyzeForTest(stmt->cond())) {
        stmt->set_cond(expr);
    }

    auto cond = stmt->cond();

    ke::Maybe<cell> constval;
    if (cond->val().ident == iCONSTEXPR)
        constval.init(cond->val().const_i32());

    bool has_break = false;
    bool has_return = false;
    {
        ke::SaveAndSet<bool> auto_break(&sc_->loop_has_break(), false);
        ke::SaveAndSet<bool> auto_return(&sc_->loop_has_return(), false);

        if (!CheckStmt(stmt->body()))
            return false;

        has_break = sc_->loop_has_break();
        has_return = sc_->loop_has_return();
    }

    stmt->set_never_taken(constval.isValid() && !constval.get());
    stmt->set_always_taken(constval.isValid() && constval.get());

    if (stmt->never_taken() && stmt->token() == tWHILE) {
        // Loop is never taken, don't touch the return status.
    } else if (stmt->always_taken() && !has_break) {
        if (has_return) {
            // Loop body ends in a return and has no break statements.
            stmt->set_flow_type(Flow_Return);
        }
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
        if (!CheckRvalue(stmt->advance()))
            ok = false;
    }

    ke::Maybe<cell> constval;
    if (cond && cond->val().ident == iCONSTEXPR)
        constval.init(cond->val().const_i32());

    bool has_break = false;
    bool has_return = false;
    {
        ke::SaveAndSet<bool> auto_break(&sc_->loop_has_break(), false);
        ke::SaveAndSet<bool> auto_continue(&sc_->loop_has_continue(), false);
        ke::SaveAndSet<bool> auto_return(&sc_->loop_has_return(), false);

        ok &= CheckStmt(stmt->body());

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
            stmt->set_flow_type(Flow_Return);
        }
    }

    if (stmt->scope())
        TestSymbols(stmt->scope(), true);
    return ok;
}

bool Semantics::CheckSwitchStmt(SwitchStmt* stmt) {
    auto expr = stmt->expr();
    bool tag_ok = CheckRvalue(expr);
    if (expr->lvalue())
        expr = stmt->set_expr(new RvalueExpr(expr));

    const auto& v = expr->val();
    if (tag_ok && !(v.type()->coercesToInt() || v.type()->isFloat()))
        report(450) << v.type() << types_->type_int();

    ke::Maybe<FlowType> flow;
    auto update_flow = [&](FlowType other) -> void {
        if (flow) {
            if (*flow != other)
                *flow = Flow_None;
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
            if (!AnalyzeForConst(expr, &value, &type))
                continue;
            if (tag_ok)
                CheckSwitchCaseType(expr, v.type(), type);

            if (!case_values.count(value))
                case_values.emplace(value);
            else
                report(expr, 40) << value;
        }

        if (CheckStmt(case_entry.second))
            update_flow(case_entry.second->flow_type());
    }

    if (stmt->default_case()) {
        if (CheckStmt(stmt->default_case()))
            update_flow(stmt->default_case()->flow_type());
    } else {
        update_flow(Flow_None);
    }

    stmt->set_flow_type(*flow);

    // Return value doesn't really matter for statements.
    return true;
}

void Semantics::CheckSwitchCaseType(Expr* expr, Type* formal, Type* actual) {
    CheckCoercion(expr, formal, actual, CvtContext::Assignment);
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

void Semantics::ReportInvalidNativeArgument(ParseNode* node, Type* type) {
    if (type->isFunctionLike())
        report(node, 43);
    else
        report(node, 48) << type;
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

    if (info->is_public() || info->is_forward()) {
        if (info->return_type()->isArray())
            report(info->pos(), 141);
    }

    auto canonical = info->canonical();
    for (const auto& arg : info->args()) {
        if (!arg->init())
            continue;

        if (canonical != info) {
            report(arg, 471);
            continue;
        }

        if (!CheckVarDecl(arg))
            continue;

        auto type = arg->type();
        if (!type->isArray() && !type->isEnumStruct()) {
            // Note: arrays and enum structs were checked earlier in ArrayValidator.
            const auto& rhs = arg->init_rhs();
            if (rhs->val().ident != iCONSTEXPR && !rhs->is(ExprKind::SizeofExpr))
                report(rhs, 8);
        }
    }

    if (info->is_native()) {
        auto rt = info->return_type();
        if ((rt->isArray() || rt->isEnumStruct()) && !CheckNativeCompoundReturn(info))
            return false;

        for (const auto& arg : info->args()) {
            if (arg->type() && !arg->type()->isAllowedInNativeCall())
                ReportInvalidNativeArgument(arg, *arg->type());
        }
        return true;
    }

    auto body = info->body();
    if (!body) {
        if (info->is_native() || info->is_forward() || info->is_builtin())
            return true;
        report(info->pos(), 10);
        return false;
    }

    if (!info->GenerateSharedClass(sc))
        return false;

    info->AddUpvarsForSharedObjects();

    // We never warn about unused member functions.
    if (info->as<MemberFunctionDecl>())
        maybe_used_.emplace_back(info);

    // We never warn about unused stock functions.
    if (info->is_stock())
        maybe_used_.emplace_back(info);

    auto fwd = info->prototype();
    if (fwd && fwd->deprecate() && !info->is_stock())
        report(info->pos(), 234) << info->name() << fwd->deprecate();

    bool ok = CheckStmt(body);

    // All type information is available, validate upvar types now.
    ok &= info->CheckUpvarTypes();

    // This must be after evaluating the body, since we won't know the types of
    // let statements until after type deduction.
    info->UpdateSharedClassFieldTypes();

    info->set_returns_value(sc_->returns_value());

    // Make sure that a public return type matches the forward (if any).
    if (fwd && info->is_public()) {
        if (fwd->return_type() != info->return_type())
            report(info->pos(), 180) << fwd->return_type() << info->return_type();
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

void FunctionDecl::AddUpvarsForSharedObjects() {
    std::unordered_map<VarDeclBase*, UpvarDecl*> seen;
    for (const auto& [var, upvar_decl] : upvar_decls_) {
        if (!var->is_shared())
            continue;

        auto owner = upvar_decl->enclosure();
        auto shared_obj = owner->shared_object();

        UpvarDecl* shared_obj_upvar = nullptr;

        auto iter = seen.find(shared_obj);
        if (iter != seen.end())
            shared_obj_upvar = iter->second;
        else
            shared_obj_upvar = AddUpvar(upvar_decl->pos(), owner, shared_obj);

        upvar_decl->set_shared_obj_upvar_index(shared_obj_upvar->upvar_index());
    }
}

// We don't allow copy capture of arrays that are >2D, since we have never
// supported copying arrays of more than one dimension.
bool CheckArrayCapture(VarDeclBase* var, Type* type) {
    if (var->is_shared())
        return true;
    auto* array = type->as<ArrayType>();
    if (!array)
        return true;
    if (array->is_fixed() && !array->is_flat() && array->inner()->isArray()) {
        report(var->pos(), 481) << var->name()->chars();
        return false;
    }
    return true;
}

bool FunctionDecl::CheckUpvarTypes() {
    bool ok = true;
    for (const auto& [var, upvar_decl] : upvar_decls_) {
        Type* var_type = var->type_info().type;
        assert(var_type);

        ok &= CheckArrayCapture(var, var_type);
    }
    return ok;
}

void Semantics::CheckFunctionReturnUsage(FunctionDecl* info) {
    if (info->returns_value() && info->body()->flow_type() == Flow_Return)
        return;

    if (info->MustReturnValue())
        ReportFunctionReturnError(info);
}

bool Semantics::CheckFunctionExpr(FunctionExpr* expr) {
    auto fun = expr->decl();
    if (!CheckFunctionDecl(fun))
        return false;

    closures_.emplace_back(fun);

    auto& v = expr->val();
    v.set_expr(fun->type());
    return true;
}

bool Semantics::CheckPragmaUnusedStmt(PragmaUnusedStmt* stmt) {
    for (const auto& decl : stmt->symbols()) {
        decl->set_is_read();

        if (decl->as<VarDecl>()) {
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

bool Semantics::CheckClassDecl(ClassDecl* decl) {
    bool ok = true;
    for (const auto& prop : decl->properties()) {
        if (prop->getter())
            ok &= CheckFunctionDecl(prop->getter());
        if (prop->setter())
            ok &= CheckFunctionDecl(prop->setter());
    }
    for (const auto& fun : decl->methods())
        ok &= CheckStmt(fun);
    return ok;
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

    // Also add all closures/inner functions that were checked.
    for (const auto& decl : closures_) {
        decl->set_is_live();
        if (!seen.count(decl)) {
            seen.emplace(decl);
            work.emplace_back(decl);
        }
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

bool Semantics::CheckRvalue(Expr* expr, Type* target, uint32_t flags) {
    switch (expr->kind()) {
        case ExprKind::ArrayExpr:
            return CheckArrayExpr(expr->to<ArrayExpr>(), target);
        case ExprKind::TernaryExpr:
            return CheckTernaryExpr(expr->to<TernaryExpr>(), target);
        case ExprKind::StringExpr:
            return CheckStringExpr(expr->to<StringExpr>(), target);
        default:
            break;
    }

    if (!CheckExpr(expr, flags))
        return false;
    return CheckRvalueAccess(expr);
}

bool Semantics::CheckRvalueAccess(Expr* expr) {
    if (auto accessor = expr->val().accessor()) {
        if (!accessor->getter()) {
            report(expr, 149) << accessor->name();
            return false;
        }
        if (!CheckAccessorAccess(*sc_, expr, accessor, accessor->getter()))
            return false;
    }
    return true;
}

bool Semantics::IsThisAtom(sp::Atom* atom) {
    if (!this_atom_)
        this_atom_ = cc_.atom("this");
    return atom == this_atom_;
}

Expr* Semantics::BuildConversion(Expr* from, const Conversion& cv) {
    return BuildConversion(from, cv.ck, cv.type);
}

Expr* Semantics::BuildConversion(Expr* from, ConversionKind ck, Type* to) {
    switch (ck) {
        case ConversionKind::Numeric:
            assert(to->isBuiltin());
            return BuildSimpleCast(from, to->builtin_type());
        case ConversionKind::FuncToLegacy:
        case ConversionKind::LegacyToFunc:
            if (from->lvalue())
                from = new RvalueExpr(from);
            return new SimpleCastExpr(from, to);
        case ConversionKind::CoerceNull:
            return CoerceNull(from, to);
        default:
            return from;
    }
}

Expr* Semantics::BuildSimpleCast(Expr* from, BuiltinType type) {
    if (from->lvalue())
        from = new RvalueExpr(from);

    // Half-assed constant folding for int->int64 casts. We don't do this for
    // intptr since the width is not known at compile time.
    Expr* to;
    if (from->val().ident == iCONSTEXPR && from->val().type()->isInt() &&
        type == BuiltinType::Int64)
    {
        to = new Number64Expr(from->pos(), from->val().const_i32());
    } else {
        to = new SimpleCastExpr(from, types_->GetBuiltin(type));
    }

    to->val().set_expr(types_->GetBuiltin(type));
    return to;
}

Expr* Semantics::CoerceNull(Expr* expr, Type* formal) {
    if (expr->val().type()->isNull() && !formal->isHeapItem()) {
        expr->val().set_type(types_->type_int());
        expr->val().set_constval(0);
    }
    return expr;
}

static inline bool CanImplicitSliceArgument(const ExprVal& val, ArrayType* to) {
    if (to && to->is_flat())
        return false;
    if (val.type()->isFlatArray()) {
        if (to && !AreSliceElementsCompatible(val.type()->inner(), to->inner()))
            return false;
        return true;
    }
    if (val.type()->isEnumStruct()) {
        if (to && !to->inner()->isAny())
            return false;
        return true;
    }
    if (to && to->inner()->isArray() && !to->inner()->isEnumStruct())
        return false;
    if (val.ident == iARRAYELEM) {
        if (val.type()->isEnumStruct() || val.type()->isArray())
            return false;
        if (to && !AreSliceElementsCompatible(val.type(), to->inner()))
            return false;
        return true;
    }
    return false;
}

SliceExpr* Semantics::ParamNeedsSliceWrapper(Expr* param, ArrayType* to) {
    if (!CanImplicitSliceArgument(param->val(), to))
        return nullptr;

    Expr* base = nullptr;
    Expr* index_expr = nullptr;
    Type* inner_type = nullptr;
    int size = 0;

    if (param->val().type()->isFlatArray()) {
        base = param;
        inner_type = param->val().type()->inner();
        size = param->val().type()->to<ArrayType>()->size();
    } else if (param->val().type()->isEnumStruct()) {
        base = param;
        inner_type = types_->type_any();
    } else {
        assert(param->as<IndexExpr>());
        IndexExpr* index = param->as<IndexExpr>();
        if (!index)
            return nullptr;
        base = index->base();
        index_expr = index->index();
        inner_type = param->val().type();
    }

    Type* type = types_->defineArray(inner_type, size);
    auto slice = new SliceExpr(base, index_expr, type);
    return slice;
}

bool IsValidIndexType(Type* type) {
    return type->isInt() || type->isAny() || type->isChar() || type->isEnum();
}

bool HasTagOnInheritanceChain(Type* type, Type* other) {
    auto map = type->asMethodmap();
    if (!map)
        return false;
    for (; map; map = map->parent()) {
        if (*map->type() == other)
            return true;
    }
    return false;
}

bool FunctionDecl::GenerateSharedClass(SemaContext& sc) {
    if (shared_var_list_.empty())
        return true;

    auto& cc = sc.cc();

    // Generate a unique class name.
    std::string class_name =
        ke::StringPrintf("__shared_%s_%d", name_->chars(),
                         sc.sema()->next_shared_class_count());
    auto class_atom = cc.atom(class_name);
    shared_class_ = new ClassDecl(pos(), class_atom);

    // Create fields for every captured shared variable.
    std::vector<LayoutFieldDecl*> fields;
    for (auto var : shared_var_list_) {
        declinfo_t field_info{};
        field_info.name = var->name();
        field_info.type = var->type_info();
        fields.push_back(new LayoutFieldDecl(pos(), field_info, shared_class_));
        shared_vars_.emplace(var, fields.back());
    }
    new (&shared_class_->fields()) PoolArray<LayoutFieldDecl*>(fields);

    // Note: we don't need EnterNames or Bind since this isn't exposed anywhere.
    shared_class_->EnterTypes(sc);

    // Create a hidden local variable of the shared class type.
    auto hidden_name = cc.atom(class_name + "_inst");
    typeinfo_t hidden_type{};
    hidden_type.type = shared_class_->type().unqualified();
    hidden_type.is_const = false;

    shared_object_ = new VarDecl(pos(), hidden_name, hidden_type, sLOCAL, VARDECL_DEFAULT, nullptr);
    prebody().push_back(shared_object_);

    // Synthesize "new SharedClass()" and attach as initializer.
    {
        auto target = new SymbolExpr(pos(), shared_class_->name());
        target->set_decl(shared_class_);

        auto call = new CallExpr(pos(), tNEW, target, {});
        call->set_ctor_type(shared_class_->type().unqualified());
        call->val().set_expr(shared_class_->type().unqualified());
        shared_object_->set_init(call);
    }

    return true;
}

void FunctionDecl::UpdateSharedClassFieldTypes() {
    for (auto var : shared_var_list_) {
        auto field = GetSharedVarField(var);
        field->mutable_type_info() = var->type_info();
    }
}

} // namespace cc
} // namespace sp
