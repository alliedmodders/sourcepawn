// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2005
//
#include "semantics.h"

#include <string>
#include <unordered_set>

#include <amtl/am-raii.h>
#include "array-helpers.h"
#include "code-generator.h"
#include "coercion-rules.h"
#include "constant-fold.h"
#include "errors.h"
#include "lexer.h"
#include "parse-node.h"
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
        file_ctors[0]->set_name(cc_.atom(".init"));
        tree->global_ctors() = PoolArray<FunctionDecl*>(file_ctors);
        return;
    }

    declinfo_t decl{};
    decl.name = cc_.atom(".init");
    decl.type.type = types_->type_void();

    auto fun = new FunctionDecl(token_pos_t{}, decl);
    auto ft = types_->defineFunction(QualType(types_->type_void(), false), {}, false,
                                     FunctionType::Typed);
    fun->set_function_type(ft);

    std::vector<Stmt*> stmts;
    for (const auto& file_ctor : file_ctors) {
        auto call = new CallExpr(fun->pos(), '(', file_ctor, {});
        ExprVal void_val;
        void_val.set_expr(types_->type_void());
        auto call_ir = new ir::Call(call, nullptr, {}, void_val);
        auto stmt = new ExprStmt(fun->pos(), call);
        stmt->set_sema_expr(call_ir);
        stmts.emplace_back(stmt);
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
    auto name = cc_.atom(".init." + std::to_string(suffix));

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
        if (decl->type()->isPstruct()) {
            if (decl->init_rhs())
                decl->set_sema_init_rhs(new ir::Struct(decl->init_rhs(), {}));
            return true;
        }
    }

    auto vclass = decl->vclass();
    auto init_rhs = decl->init_rhs();
    if (decl->init() && init_rhs && vclass != sLOCAL && !decl->type()->isComposite()) {
        ir::Value* checked_rhs = CheckExpr(init_rhs);
        if (!checked_rhs || !checked_rhs->is(IrKind::Constant)) {
            if (vclass == sARGUMENT && (init_rhs->is(ExprKind::SymbolExpr) || init_rhs->is(ExprKind::SizeofExpr))) {
                decl->set_sema_init_rhs(checked_rhs);
                return true;
            }

            report(init_rhs->pos(), 8);
        }
    }

    if (decl->init() && (vclass == sGLOBAL || vclass == sSTATIC))
        globals_to_init_.emplace_back(decl);

    assert(!decl->init() || decl->sema_init_rhs());
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
    } else {
        if (type->isClass() && !decl->init()) {
            report(decl->pos(), 478);
            return false;
        }
        auto init = decl->init();
        if (init) {
            ir::Value* node = CheckRvalue(init);
            if (!node)
                return false;
            decl->set_sema_init_rhs(node->to<ir::Binary>()->right());
        }
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
    ir::Value* checked_rhs = CheckExpr(init_rhs);
    if (!checked_rhs) {
        *decl->mutable_type_info() = ErrorTypeinfo();
        return false;
    }

    QualType rhs_type = checked_rhs->val().type();

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
    state.right = checked_rhs;
    ir::Value* bin = CheckBinaryExprImpl(state);
    if (!bin)
        return false;

    // We're guaranteed the result is ir::Binary here, and not constant folded,
    // because the token is '=' which is not foldable.
    decl->set_sema_init_rhs(bin->to<ir::Binary>()->right());
    return true;
}

bool Semantics::CheckEnumStructVarDecl(VarDeclBase* decl) {
    Expr* init = decl->init_rhs();
    if (!init)
        return true;

    // Handle array literal initializer — validate against enum struct fields.
    if (init->as<ArrayExpr>()) {
        AutoErrorPos aep(init->pos());
        ir::Value* node =
           ValidateEnumStructInitializer(decl->type()->asEnumStruct(), init, nullptr);
        if (!node)
            return false;
        decl->set_sema_init_rhs(node);
        return true;
    }

    if (init->as<StructExpr>()) {
        report(init->pos(), 428);
        return false;
    }

    // Non-literal initialization (e.g. from a function result).
    ir::Value* node = CheckRvalue(init);
    if (!node)
        return false;
    if (auto lval = node->as<ir::Lvalue>())
        node = new ir::Rvalue(lval);
    decl->set_sema_init_rhs(node);

    auto ck = FindConversion(node->val().type(), *decl->type(), CvtContext::Assignment);
    if (ck == ConversionKind::NeedsCast) {
        report(init->pos(), 462) << node->val().type() << decl->type();
        return false;
    }
    if (!HasImplicitConversion(ck)) {
        ReportConversionDiagnostic(init->pos(), decl->type(), node->val().type());
        return false;
    }
    return true;
}

ir::Value* Semantics::ValidateEnumStructInitializer(EnumStructDecl* es, Expr* init,
                                                    Type* expr_type)
{
    auto array = init->as<ArrayExpr>();
    if (!array) {
        report(init->pos(), 47);
        return nullptr;
    }

    const auto& field_list = es->fields();
    auto field_iter = field_list.begin();

    std::vector<ir::Value*> elements;
    for (size_t i = 0; i < array->exprs().size(); i++) {
        Expr* expr = array->exprs().at(i);
        if (field_iter == field_list.end()) {
            report(expr->pos(), 91);
            continue;
        }

        auto field = *field_iter;
        field_iter++;

        const auto& type = field->type_info();
        if (type.type->isArray()) {
            ir::Value* elem = nullptr;
            if (!CheckArrayInitialization(this, type, expr, &elem) || !elem)
                continue;
            elements.emplace_back(elem);
        } else {
            AutoErrorPos pos(expr->pos());

            ir::Value* expr_ir = CheckExpr(expr);
            if (!expr_ir)
                continue;

            const auto& v = expr_ir->val();
            if (!expr_ir->is(IrKind::Constant)) {
                report(8);
                continue;
            }

            ConversionKind ck = FindConversion(v.type(), type.type, CvtContext::Assignment);
            if (!HasImplicitConversion(ck)) {
                ReportConversionDiagnostic(expr->pos(), type.type, v.type());
                continue;
            }
            if (!IsNopConversion(ck)) {
                ir::Value* converted = BuildConversion(expr_ir, ck, type.type);
                assert(converted);
                elements.emplace_back(converted);
            } else {
                elements.emplace_back(expr_ir);
            }
        }
    }

    if (array->ellipses()) {
        report(array->pos(), 80);
        return nullptr;
    }
    if (elements.size() != array->exprs().size())
        return nullptr;
    ExprVal val = {};
    if (expr_type)
        val.set_expr(expr_type);
    return new ir::Array(array, elements, array->ellipses(), val);
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

bool Semantics::CheckPstructArg(VarDeclBase* decl, PstructDecl* ps, StructInitFieldExpr* field,
                                std::vector<bool>* visited)
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
    } else if (auto expr = field->value->as<NumberExpr>()) {
        actual = expr->type();
    } else if (auto expr = field->value->as<SymbolExpr>()) {
        actual = *expr->decl()->type();
    } else {
        assert(false);
        return false;
    }

    if (arg->type()->isBool() && actual->isInt())
        return true;

    return CheckCoercion(field->pos(), arg->type(), QualType(actual), CvtContext::Argument);
}

ir::Value* Semantics::CheckExpr(Expr* expr, uint32_t flags) {
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
            return CheckSymbolExpr(expr->to<SymbolExpr>(), !!(flags & EXPR_ALLOW_TYPE_SYMS));
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
        case ExprKind::NumberExpr: {
            auto e = expr->to<NumberExpr>();
            return new ir::Constant(e, e->val());
        }
        case ExprKind::SizeofExpr:
            return CheckSizeofExpr(expr->to<SizeofExpr>());
        case ExprKind::DefaultArgExpr:
            return new ir::DefaultArg(expr);
        case ExprKind::SpreadArgsExpr:
            return new ir::SpreadArgs(expr);
        case ExprKind::NamedArgExpr: {
            ir::Value* inner = CheckExpr(expr->to<NamedArgExpr>()->expr);
            if (!inner)
                return nullptr;
            return new ir::NamedArg(expr, inner, inner->val());
        }
        case ExprKind::FunctionExpr:
            return CheckFunctionExpr(expr->to<FunctionExpr>());
        case ExprKind::StructInitFieldExpr: {
            ir::Value* inner = CheckExpr(expr->to<StructInitFieldExpr>()->value);
            if (!inner)
                return nullptr;
            return new ir::StructInitField(expr, inner, inner->val());
        }
        default:
            assert(false);
            report(expr, 420) << (int)expr->kind();
            return nullptr;
    }
}

CompareOp::CompareOp(const token_pos_t& pos, int token, Expr* expr)
  : pos(pos),
    token(token),
    expr(expr)
{}

static bool HasSideEffects(ir::Value* node);

static bool HasSideEffects(const PoolArray<ir::Value*>& nodes) {
    for (auto n : nodes) {
        if (HasSideEffects(n))
            return true;
    }
    return false;
}

static bool HasSideEffects(ir::Value* node) {
    if (node->is(IrKind::Accessor))
        return true;

    switch (node->kind()) {
        case IrKind::Unary:
            return HasSideEffects(node->to<ir::Unary>()->expr());
        case IrKind::Binary: {
            auto e = node->to<ir::Binary>();
            return IsAssignOp(e->token()) || HasSideEffects(e->left()) ||
                   HasSideEffects(e->right());
        }
        case IrKind::Logical: {
            auto e = node->to<ir::Logical>();
            return HasSideEffects(e->left()) || HasSideEffects(e->right());
        }
        case IrKind::ChainedCompare: {
            auto e = node->to<ir::ChainedCompare>();
            if (HasSideEffects(e->first()))
                return true;
            for (const auto& op : e->ops()) {
                if (HasSideEffects(op.expr))
                    return true;
            }
            return false;
        }
        case IrKind::Ternary: {
            auto e = node->to<ir::Ternary>();
            return HasSideEffects(e->first()) || HasSideEffects(e->second()) ||
                   HasSideEffects(e->third());
        }
        case IrKind::Cast:
            return HasSideEffects(node->to<ir::Cast>()->expr());
        case IrKind::LvalueCast:
            return HasSideEffects(node->to<ir::LvalueCast>()->expr());
        case IrKind::NamedArg:
            return HasSideEffects(node->to<ir::NamedArg>()->expr());
        case IrKind::StructInitField:
            return HasSideEffects(node->to<ir::StructInitField>()->value());
        case IrKind::SimpleCast:
            return HasSideEffects(node->to<ir::SimpleCast>()->from());
        case IrKind::Slice: {
            auto e = node->to<ir::Slice>();
            return HasSideEffects(e->base()) || (e->index() && HasSideEffects(e->index()));
        }
        case IrKind::Struct: {
            auto e = node->to<ir::Struct>();
            for (auto field : e->fields()) {
                if (HasSideEffects(field))
                    return true;
            }
            return false;
        }
        case IrKind::Comma:
            return HasSideEffects(node->to<ir::Comma>()->exprs());
        case IrKind::Array:
            return HasSideEffects(node->to<ir::Array>()->elements());
        case IrKind::NewArray:
            return HasSideEffects(node->to<ir::NewArray>()->dims());
        case IrKind::Index: {
            auto e = node->to<ir::Index>();
            return HasSideEffects(e->base()) || (e->index() && HasSideEffects(e->index()));
        }
        case IrKind::StaticFieldRef:
            return HasSideEffects(node->to<ir::StaticFieldRef>()->base());
        case IrKind::MethodRef:
            return HasSideEffects(node->to<ir::MethodRef>()->base());
        case IrKind::FieldRef:
            return HasSideEffects(node->to<ir::FieldRef>()->base());
        case IrKind::Accessor:
            return HasSideEffects(node->to<ir::Accessor>()->base());
        case IrKind::Rvalue:
            return HasSideEffects(node->to<ir::Rvalue>()->expr());
        case IrKind::Call:  // Not intelligent yet.
        case IrKind::IncDec:
            return true;
        case IrKind::Typename:
        case IrKind::FunctionRef:
        case IrKind::Variable:
        case IrKind::Upvar:
        case IrKind::String:
        case IrKind::Constant:
        case IrKind::Sizeof:
        case IrKind::DefaultArg:
        case IrKind::SpreadArgs:
        case IrKind::Function:
            return false;
        default:
            assert(false);
            return true;
    }
}

bool Semantics::CheckScalarType(ir::Value* node) {
    const auto& val = node->val();
    if (val.type()->isArray()) {
        if (node->is(IrKind::Variable))
            report(node, 456) << val.type();
        else
            report(node, 29);
        return false;
    }
    if (val.type()->asEnumStruct()) {
        report(node, 447);
        return false;
    }
    if (val.type()->isVoid()) {
        report(node, 466);
        return false;
    }
    return true;
}

ir::Value* Semantics::AnalyzeForTest(Expr* expr) {
    ir::Value* node = CheckRvalue(expr);
    if (!node)
        return nullptr;
    if (!CheckScalarType(node))
        return nullptr;

    auto& val = node->val();
    if (val.type()->isWideType())
        return BuildSimpleCast(node, BuiltinType::Bool);
    if (val.type()->isVoid()) {
        report(expr, 466);
        return nullptr;
    }

    if (auto* c = node->as<ir::Constant>()) {
        if (!sc_->preprocessing()) {
            if (c->get_i32())
                report(expr, 206);
            else
                report(expr, 205);
        }
    } else if (auto sym_expr = expr->as<SymbolExpr>()) {
        if (sym_expr->decl()->as<FunctionDecl>())
            report(expr, 249);
    }

    if (auto lval = node->as<ir::Lvalue>())
        return new ir::Rvalue(lval);

    return node;
}

ir::Constant* Semantics::AnalyzeForConst(ir::Value* node) {
    auto* c = node->as<ir::Constant>();
    if (!c) {
        report(node, 8);
        return nullptr;
    }
    return c;
}

ir::Value* Semantics::CheckExprForConst(Expr* expr) {
    ir::Value* node = CheckExpr(expr);
    if (!node)
        return nullptr;
    return AnalyzeForConst(node) ? node : nullptr;
}

ir::Value* Semantics::CheckUnaryExpr(UnaryExpr* unary) {
    AutoErrorPos aep(unary->pos());

    ir::Value* operand = CheckRvalue(unary->expr());
    if (!operand)
        return nullptr;
    if (!CheckScalarType(operand))
        return nullptr;

    if (auto lval = operand->as<ir::Lvalue>())
        operand = new ir::Rvalue(lval);

    ExprVal out_val = operand->val();
    std::optional<ConstVal> folded;

    // :TODO: check for invalid types

    switch (unary->token()) {
        case '~':
            if (!out_val.type()->coercesToInt() && !out_val.type()->isInt64() &&
                !out_val.type()->isIntPtr())
            {
                report(unary, 462) << out_val.type() << types_->type_int();
                return nullptr;
            }
            if (auto* c = operand->as<ir::Constant>()) {
                Type* t = c->val().type();
                if (c->is_int64())
                    folded = ConstVal(t, ~c->get_int64());
                else if (t->coercesToInt())
                    folded = ConstVal(t, ~c->get_cell());
            }
            break;
        case '!': {
            auto ck = FindConversion(out_val.type(), types_->type_bool(), CvtContext::Explicit);
            if (!HasImplicitConversion(ck)) {
                ReportConversionDiagnostic(operand, types_->type_bool(), out_val.qualified());
                return nullptr;
            }
            operand = BuildConversion(operand, ck, types_->type_bool());
            out_val = operand->val();

            if (auto* c = operand->as<ir::Constant>())
                folded = ConstVal(types_->type_bool(), c->get_i32() ? 0 : 1);
            break;
        }
        case '-':
            if (auto* c = operand->as<ir::Constant>()) {
                Type* t = c->val().type();
                if (c->is_float()) {
                    folded = ConstVal(t, -c->get_float());
                } else if (c->is_int64()) {
                    // Negate the int64. If the result still fits in int, narrow
                    // to int as a special case for -INT_MIN.
                    int64_t value = -c->get_int64();
                    if (value >= INT_MIN && value <= INT_MAX)
                        folded = ConstVal(types_->type_int(), value);
                    else
                        folded = ConstVal(t, value);
                } else if (c->is_double()) {
                    folded = ConstVal(t, -c->get_double());
                } else {
                    folded = ConstVal(t, -c->get_cell());
                }
            }
            break;
        default:
            assert(false);
    }

    if (folded)
        return new ir::Constant(unary, *folded);

    return new ir::Unary(unary, unary->token(), operand, out_val);
}

ir::Value* Semantics::CheckIncDecExpr(IncDecExpr* incdec, uint32_t flags) {
    AutoErrorPos aep(incdec->pos());

    ir::Value* operand = CheckExpr(incdec->expr());
    if (!operand)
        return nullptr;
    if (!CheckScalarType(operand))
        return nullptr;
    if (!operand->lvalue()) {
        report(incdec, 22);
        return nullptr;
    }

    const auto& expr_val = operand->val();
    if (auto* acc = operand->as<ir::Accessor>()) {
        auto prop = acc->accessor();
        if (!prop->setter()) {
            report(incdec, 152) << prop->name();
            return nullptr;
        }
        if (!prop->getter()) {
            report(incdec, 149) << prop->name();
            return nullptr;
        }
        markusage(prop->getter(), uREAD);
        markusage(prop->setter(), uREAD);
    } else {
        if (auto* var = operand->as<ir::Variable>()) {
            if (var->decl()->is_const()) {
                report(incdec, 22); /* assignment to const argument */
                return nullptr;
            }
        }
        markusage(operand->to<ir::Lvalue>(), uWRITTEN);
        if (!(flags & EXPR_DISCARD_RESULT))
            markusage(operand->to<ir::Lvalue>(), uREAD);
    }

    Type* type = expr_val.type();
    if (type->isReference())
        type = type->inner();

    ExprVal val;
    val.set_expr(type);

    // :TODO: more type checks
    return new ir::IncDec(incdec, incdec->token(), incdec->prefix(), operand->to<ir::Lvalue>(),
                          val);
}

BinaryExpr::BinaryExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
  : BinaryExprBase(ExprKind::BinaryExpr, pos, token, left, right)
{
}


static inline bool CanPromoteToInt64(Type* type) {
    return type->isInt() || type->isAny();
}

ir::Value* Semantics::CheckBinaryExprImpl(BinaryExprState& state) {
    if (!(state.left = CheckExpr(state.expr->left())))
        return nullptr;

    if (state.expr->token() == '=') {
        if (!state.rhs_resolved && !(state.right = CheckRvalue(state.expr->right(), state.left->val().type())))
            return nullptr;
    } else {
        if (!(state.right = CheckRvalue(state.expr->right())))
            return nullptr;
    }

    int token = state.expr->token();
    int op_token = NormalizeBinaryToken(token);

    if (IsAssignOp(token)) {
        // Mark the left-hand side as written as soon as we can.
        if (auto* lhs = state.left->as<ir::Lvalue>())
            markusage(lhs, uWRITTEN);
        if (auto* var = state.left->as<ir::Variable>()) {
            // If it's an outparam, also mark it as read.
            auto sym = var->decl();
            if (sym->vclass() == sARGUMENT &&
                (sym->type()->isReference() ||
                 sym->type()->isArray() ||
                 sym->type()->isEnumStruct()))
            {
                markusage(sym, uREAD);
            }
        } else if (auto* acc = state.left->as<ir::Accessor>()) {
            auto accessor = acc->accessor();
            if (!accessor->setter()) {
                report(state.expr, 152) << accessor->name();
                return nullptr;
            }
            markusage(accessor->setter(), uREAD);
            if (accessor->getter() && token != '=')
                markusage(accessor->getter(), uREAD);
        }

        if (!CheckAssignmentLHS(state))
            return nullptr;
        if (token != '=' && !CheckRvalueAccess(state.left))
            return nullptr;
    } else if (state.left->lvalue()) {
        if (!CheckRvalueAccess(state.left))
            return nullptr;
        state.left = new ir::Rvalue(state.left->to<ir::Lvalue>());
    }

    // RHS is always loaded. Note we do this after validating the left-hand side,
    // so ValidateAssignment has an original view of RHS.
    if (auto lval = state.right->as<ir::Lvalue>())
        state.right = new ir::Rvalue(lval);

    auto left_type = state.left->val().type();
    if (left_type->isReference())
        left_type = left_type->inner();

    auto right_type = state.right->val().type();
    assert(!right_type->isReference());

    ExprVal val;

    Type* assign_type;
    std::optional<BinaryOperator> op;
    if (token != '=') {
        op = FindBinaryOperator(op_token, left_type, right_type);
        if (!op) {
            report(state.expr, 461) << get_token_string(token) << left_type << right_type;
            return nullptr;
        }

        if (op->left.ck == ConversionKind::TagMismatch)
            report(state.left, 213) << op->left.type << left_type;
        if (op->right.ck == ConversionKind::TagMismatch)
            report(state.right, 213) << op->right.type << right_type;

        if (!op->right.IsNop())
            state.right = BuildConversion(state.right, op->right);
        if (!op->left.IsNop() && !IsAssignOp(token))
            state.left = BuildConversion(state.left, op->left);

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
            return nullptr;
        }
        if (!HasImplicitConversion(ck)) {
            ReportConversionDiagnostic(state.right, left_type, assign_type);
            return nullptr;
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
                return nullptr;
            }
        } else {
            // This is a non-compound assignment with a conversion, so update the right-hand side.
            if (!IsNopConversion(ck))
                state.right = BuildConversion(state.right, ck, left_type);
        }
        val.set_expr(left_type);
    } else {
        val.set_expr(assign_type);
    }

    // Finally, do a constant folding pass.
    if (auto folded = TryFoldBinary(state.expr, state.left, state.right, val.type()))
        return new ir::Constant(state.expr, *folded);
    return new ir::Binary(state.expr, state.expr->token(), state.left, state.right, val);
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

template <typename Node>
static bool CheckAccessorAccess(SemaContext& sc, Node* node, PropertyDecl* prop,
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

    // may not change "constant" parameters
    if (!state.expr->initializer()) {
        if (auto* var = state.left->as<ir::Variable>()) {
            if (var->decl()->is_const()) {
                report(state.expr, 22);
                return false;
            }
        }
    }

    if (auto* acc = state.left->as<ir::Accessor>()) {
        auto accessor = acc->accessor();
        if (!accessor->setter()) {
            report(state.expr, 152) << accessor->name();
            return false;
        }
        if (!CheckAccessorAccess(*sc_, state.expr, accessor, accessor->setter()))
            return false;
    }
    return true;
}

ir::Value* Semantics::CheckBinaryExpr(BinaryExpr* expr) {
    AutoErrorPos aep(expr->pos());

    BinaryExprState state(expr);
    return CheckBinaryExprImpl(state);
}


ir::Value* Semantics::CheckLogicalExpr(LogicalExpr* expr) {
    AutoErrorPos aep(expr->pos());

    ir::Value* left = AnalyzeForTest(expr->left());
    if (!left)
        return nullptr;
    ir::Value* right = AnalyzeForTest(expr->right());
    if (!right)
        return nullptr;

    if (auto lval = left->as<ir::Lvalue>())
        left = new ir::Rvalue(lval);
    if (auto lval = right->as<ir::Lvalue>())
        right = new ir::Rvalue(lval);

    if (auto* l = left->as<ir::Constant>()) {
        if (auto* r = right->as<ir::Constant>()) {
            cell v = 0;
            if (expr->token() == tlOR)
                v = (l->get_i32() || r->get_i32()) ? 1 : 0;
            else if (expr->token() == tlAND)
                v = (l->get_i32() && r->get_i32()) ? 1 : 0;
            else
                assert(false);
            return new ir::Constant(expr, ConstVal(types_->type_bool(), v));
        }
    }

    return new ir::Logical(expr, expr->token(), left, right, ExpressionVal(types_->type_bool()));
}

ir::Value* Semantics::CheckChainedCompareExpr(ChainedCompareExpr* chain) {
    ir::Value* first = CheckRvalue(chain->first());
    if (!first)
        return nullptr;
    if (auto lval = first->as<ir::Lvalue>())
        first = new ir::Rvalue(lval);

    struct ChainedOpIr {
        int token;
        token_pos_t pos;
        ir::Value* expr;
    };
    std::vector<ChainedOpIr> ops;
    for (auto& op : chain->ops()) {
        ir::Value* e = CheckRvalue(op.expr);
        if (!e)
            return nullptr;
        if (auto lval = e->as<ir::Lvalue>())
            e = new ir::Rvalue(lval);
        ops.push_back({op.token, op.pos, e});
    }

    ir::Value* left = first;
    bool all_const = (left->is(IrKind::Constant) && left->val().type()->isInt());
    bool constval = true;

    bool is_first = true;
    for (auto& op : ops) {
        ir::Value* right = op.expr;
        auto left_type = left->val().type();
        auto right_type = right->val().type();

        auto binop = FindBinaryOperator(op.token, left_type, right_type);
        if (!binop) {
            report(op.pos, 461) << get_token_string(op.token) << left_type << right_type;
            return nullptr;
        }

        // For subsequent comparisons, the left operand has already been evaluated
        // (it was the right operand of the previous comparison). We cannot apply
        // coercions to it.
        if (!is_first && !binop->left.IsNop()) {
            report(op.pos, 461) << get_token_string(op.token) << left_type << right_type;
            return nullptr;
        }

        if (binop->left.ck == ConversionKind::TagMismatch)
            report(left, 213) << binop->left.type << left_type;
        if (binop->right.ck == ConversionKind::TagMismatch)
            report(right, 213) << binop->right.type << right_type;

        if (!binop->right.IsNop())
            op.expr = BuildConversion(op.expr, binop->right);
        if (is_first && !binop->left.IsNop())
            first = BuildConversion(first, binop->left);

        if (!right->is(IrKind::Constant) || !right->val().type()->isInt())
            all_const = false;

        // Fold constants as we go.
        if (all_const) {
            auto* lv = left->to<ir::Constant>();
            auto* rv = right->to<ir::Constant>();
            switch (op.token) {
                case tlLE:
                    constval &= lv->get_i32() <= rv->get_i32();
                    break;
                case tlGE:
                    constval &= lv->get_i32() >= rv->get_i32();
                    break;
                case '>':
                    constval &= lv->get_i32() > rv->get_i32();
                    break;
                case '<':
                    constval &= lv->get_i32() < rv->get_i32();
                    break;
                default:
                    assert(false);
                    break;
            }
        }

        left = op.expr;
        is_first = false;
    }

    std::vector<ir::ChainedCompare::Op> irops;
    for (auto& op : ops)
        irops.push_back({op.token, op.expr});
    if (all_const)
        return new ir::Constant(chain, ConstVal(types_->type_bool(), constval ? 1 : 0));

    ExprVal val;
    val.set_type(types_->type_bool());
    return new ir::ChainedCompare(chain, first, std::move(irops), val);
}

ir::Value* Semantics::CheckTernaryExpr(TernaryExpr* expr, Type* target) {
    AutoErrorPos aep(expr->pos());

    ir::Value* second = CheckRvalue(expr->second(), target);
    if (!second)
        return nullptr;
    ir::Value* third = CheckRvalue(expr->third(), target);
    if (!third)
        return nullptr;

    ir::Value* first_ir = AnalyzeForTest(expr->first());
    if (!first_ir)
        return nullptr;

    if (auto lval = second->as<ir::Lvalue>())
        second = new ir::Rvalue(lval);
    if (auto lval = third->as<ir::Lvalue>())
        third = new ir::Rvalue(lval);

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
                second = new ir::Slice(second, nullptr, type);
            }
            if (right_array->is_flat()) {
                auto type = types_->defineArray(right_array->inner(), size);
                third = new ir::Slice(third, nullptr, type);
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
            return nullptr;
        }

        if (use_left_to_right)
            second = BuildConversion(second, left_to_right, right.type());
        else
            third = BuildConversion(third, right_to_left, left.type());

        auto ck = use_left_to_right ? left_to_right : right_to_left;
        if (ck == ConversionKind::TagMismatch)
            report(second, 213) << left.type() << right.type();
    }

    second = CoerceNull(second, right.type());
    third = CoerceNull(third, left.type());

    if (auto taken = FoldToConstantBool(first_ir))
        return *taken ? second : third;

    ExprVal out_val;
    out_val.set_expr(out_type);
    return new ir::Ternary(expr, first_ir, second, third, out_val);
}


static inline bool IsValidIntWidthChange(Type* from, Type* to) {
    // allow double to/from int64, but not intptr, which is not guaranteed
    // to be 64-bit.
    if ((from->isInt64() && to->isDouble()) || (from->isDouble() && to->isInt64()))
        return true;
    if (from->isWideInt())
        return to->isInt() || to->isInt16() || to->isInt8() || to->isWideInt();
    if (to->isWideInt())
        return from->isInt() || from->isAny() || from->isInt16() || from->isInt8();
    return false;
}

static inline bool CastNeedsRvalue(ir::Value* inner, Type* to_type) {
    if (inner->is(IrKind::Accessor))
        return true;
    auto type = inner->val().type();
    if (type->isWideInt() || to_type->isWideInt())
        return true;
    if (type->isChar())
        return true;
    if (type->podLoadSize() != to_type->podLoadSize())
        return true;
    return false;
}

ir::Value* Semantics::CheckCastExpr(CastExpr* expr) {
    AutoErrorPos aep(expr->pos());

    Type* to_type = expr->type();
    if (to_type->isVoid()) {
        report(expr, 144);
        return nullptr;
    }

    auto inner = expr->expr();
    ir::Value* inner_ir = nullptr;
    if (auto array = inner->as<ArrayExpr>()) {
        Type* target_array = types_->defineArray(to_type, (int)array->exprs().size());
        if (!(inner_ir = CheckRvalue(array, target_array)))
            return nullptr;
    } else {
        if (!(inner_ir = CheckExpr(inner)))
            return nullptr;
    }

    ExprVal out_val = inner_ir->val();
    ir::Value* operand = inner_ir;

    std::optional<ConstVal> const_result;
    if (auto* c = inner_ir->as<ir::Constant>())
        const_result = c->value();

    Type* from_type = out_val.type();
    if (from_type == to_type) {
        if (auto* c = inner_ir->as<ir::Constant>())
            return new ir::Constant(expr, c->value());
        if (operand->lvalue())
            return new ir::LvalueCast(expr, operand, out_val);
        return new ir::Cast(expr, operand, out_val);
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
            return nullptr;
        }
        report(expr, 237);
    } else if (from_type->isFunctionLike() && to_type->isFunctionLike()) {
        ir::Value* converted = TryConversion(operand, QualType(to_type), CvtContext::Assignment);
        if (!converted)
            return nullptr;
        operand = converted;
        out_val = converted->val();
        if (auto* c = converted->as<ir::Constant>())
            const_result = c->value();
        else
            const_result = {};
    } else if (out_val.type()->isVoid()) {
        report(expr, 89);
    } else if (to_type->isEnumStruct() || from_type->isEnumStruct()) {
        report(expr, 95) << to_type;
    }
    if (from_type->isReference() && !to_type->isReference()) {
        if (to_type->isEnumStruct()) {
            report(expr, 136);
            return nullptr;
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
            report(expr, 460) << operand->val().type() << to_array_type;
            return nullptr;
        }
    }
    if (actual_array && from_type->isInt64()) {
        report(expr, 460) << actual_array << to_array_type;
        return nullptr;
    }

    if (actual_array)
        to_type = to_array_type;

    if (out_val.type()->isWideInt() || to_type->isWideInt()) {
        if (!IsValidIntWidthChange(out_val.type(), to_type)) {
            report(expr, 460) << out_val.type() << to_type;
            return nullptr;
        }
    }

    if (to_type->isFloat() != out_val.type()->isFloat()) {
        auto other_type = out_val.type()->isFloat() ? to_type : out_val.type();
        if (other_type->podLoadSize() != 4) {
            report(expr, 460) << out_val.type() << to_type;
            return nullptr;
        }
    }

    // Reject any view_as involving double except int64 <-> double (which is
    // already allowed by the isWideInt check above).
    if (to_type->isDouble() != out_val.type()->isDouble() &&
        !to_type->isInt64() && !out_val.type()->isInt64())
    {
        report(expr, 460) << out_val.type() << to_type;
        return nullptr;
    }

    if (CastNeedsRvalue(inner_ir, to_type)) {
        if (inner_ir->lvalue())
            operand = new ir::Rvalue(inner_ir->to<ir::Lvalue>());
        const_result = std::nullopt;
    }

    out_val.set_type(to_type);

    if (auto folded = TryFoldCast(inner_ir, to_type))
        return new ir::Constant(expr, *folded);
    if (const_result) {
        const_result->type = to_type;
        return new ir::Constant(expr, *const_result);
    }
    if (operand->lvalue())
        return new ir::LvalueCast(expr, operand, out_val);
    return new ir::Cast(expr, operand, out_val);
}

// This is a hack. Most code is not prepared to handle iMETHODMAP in type
// checks, so for now, we forbid it by default. Since the '.' operator *is*
// prepared for this, we have a special analysis option to allow returning
// types as values.
ir::Value* Semantics::CheckSymbolExpr(SymbolExpr* expr, bool allow_types) {
    AutoErrorPos aep(expr->pos());

    auto decl = expr->decl();
    if (!decl) {
        // This can happen if CheckSymbolExpr is called during name resolution.
        assert(cc_.reports()->total_errors() > 0);
        return nullptr;
    }

    ExprVal val = {};
    switch (decl->kind()) {
        case StmtKind::VarDecl:
        case StmtKind::ArgDecl:
            return new ir::Variable(expr, decl->as<VarDeclBase>());
        case StmtKind::ConstDecl:
        case StmtKind::EnumFieldDecl: {
            ConstVal const_val = decl->const_value();
            const_val.type = decl->type().unqualified();
            return new ir::Constant(expr, const_val);
        }
        case StmtKind::FunctionDecl:
        case StmtKind::MemberFunctionDecl: {
            auto fun = decl->as<FunctionDecl>()->canonical();
            if (fun->is_native()) {
                report(expr, 76);
                return nullptr;
            }
            if (fun->return_array() || fun->return_type()->isArray()) {
                report(expr, 182);
                return nullptr;
            }
            if (!fun->impl()) {
                report(expr, 4) << fun->name();
                return nullptr;
            }

            // Mark as being indirectly invoked. Direct invocations go
            // through BindCallTarget.
            fun->set_is_callback();
            markusage(fun, uREAD);
            return new ir::FunctionRef(expr, fun);
        }
        case StmtKind::ClassDecl:
        case StmtKind::EnumStructDecl:
        case StmtKind::MethodmapDecl:
            break;
        case StmtKind::EnumDecl: {
            auto es = decl->as<EnumDecl>();
            if (!es->mm() && !allow_types) {
                report(expr, 174) << decl->name();
                return nullptr;
            }
            break;
        }
        case StmtKind::UpvarDecl:
            return new ir::Upvar(expr, decl->as<UpvarDecl>(), decl->type());
        default:
            assert(false);
    }

    // Everything other than type names returns earlier in the switch.
    if (!allow_types) {
        report(expr, 174) << decl->name();
        return nullptr;
    }
    return new ir::Typename(expr, decl);
}

ir::Value* Semantics::CheckCommaExpr(CommaExpr* comma) {
    AutoErrorPos aep(comma->pos());

    size_t index = 0;
    std::vector<ir::Value*> exprs;
    for (auto& expr : comma->exprs()) {
        ir::Value* e = CheckRvalue(expr);
        if (!e)
            return nullptr;
        if (auto lval = e->as<ir::Lvalue>())
            e = new ir::Rvalue(lval);
        if (!HasSideEffects(e))
            report(e, 231) << index;
        exprs.push_back(e);
        index++;
    }

    return new ir::Comma(comma, std::move(exprs));
}


ir::Value* Semantics::CheckArrayExpr(ArrayExpr* array, Type* target) {
    AutoErrorPos aep(array->pos());

    if (!target) {
        report(array->pos(), 142);
        return nullptr;
    }

    // Handle enum struct target — validate {x, y, ...} against struct fields.
    if (auto es = target->asEnumStruct())
        return ValidateEnumStructInitializer(es, array, target);

    auto array_target = target->as<ArrayType>();
    if (!array_target) {
        report(array->pos(), 142);
        return nullptr;
    }

    Type* formal_elt = array_target->inner();

    std::vector<ir::Value*> elements;
    for (auto& entry : array->exprs()) {
        ir::Value* enode;
        if (entry->as<ArrayExpr>()) {
            if (!(enode = CheckRvalue(entry, formal_elt)))
                return nullptr;
        } else {
            if (!(enode = CheckExpr(entry)))
                return nullptr;

            const auto& val = enode->val();
            if (!enode->is(IrKind::Constant)) {
                report(entry, 8);
                return nullptr;
            }

            if (!CheckCoercion(enode, formal_elt, val.type(), CvtContext::Assignment))
                return nullptr;
        }
        elements.emplace_back(enode);
    }

    ExprVal val;
    if (array_target->is_flat())
        val.set_type(types_->defineFlatArray(formal_elt, array_target->size()));
    else
        val.set_type(types_->defineArray(formal_elt, (int)array->exprs().size()));
    return new ir::Array(array, elements, array->ellipses(), val);
}

ir::Value* Semantics::CheckIndexExpr(IndexExpr* expr) {
    AutoErrorPos aep(expr->pos());

    auto checked_base = CheckRvalue(expr->base());
    if (!checked_base)
        return nullptr;
    if (auto lval = checked_base->as<ir::Lvalue>())
        checked_base = new ir::Rvalue(lval);

    ir::Value* checked_index = nullptr;

    const auto& base_val = checked_base->val();
    if (!base_val.type()->isArray()) {
        report(expr->index(), 28);
        return nullptr;
    }

    ArrayType* array = base_val.type()->to<ArrayType>();

    if (expr->index()) {
        if (!(checked_index = CheckRvalue(expr->index())))
            return nullptr;
        if (!CheckScalarType(checked_index))
            return nullptr;
        if (auto lval = checked_index->as<ir::Lvalue>())
            checked_index = new ir::Rvalue(lval);

        auto idx_type = checked_index->val().type();
        if (!IsValidIndexType(idx_type)) {
            report(checked_index, 77) << idx_type;
            return nullptr;
        }

        if (auto* idx = checked_index->as<ir::Constant>()) {
            if (!array->isCharArray()) {
                /* normal array index */
                if (idx->get_i32() < 0 ||
                    (array->size() != 0 && array->size() <= idx->get_i32()))
                {
                    report(expr->index(), 32);
                    return nullptr;
                }
            } else {
                /* character index */
                if (idx->get_i32() < 0 ||
                    (array->size() != 0 && array->size() <= idx->get_i32()))
                {
                    report(expr->index(), 32);
                    return nullptr;
                }
            }
        }
    }

    return new ir::Index(expr, checked_base, checked_index, array->inner());
}

ir::Value* Semantics::CheckThisExpr(ThisExpr* expr) {
    auto sym = expr->decl();
    assert(sym->as<ArgDecl>());

    return new ir::Variable(expr, sym);
}

ir::Value* Semantics::CheckNullExpr(NullExpr* expr) {
    return new ir::Constant(expr, ConstVal(types_->type_null(), 0));
}

ir::Value* Semantics::CheckStringExpr(StringExpr* expr, Type* target) {
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

    ExprVal val;
    val.set_type(types_->defineArray(types_->type_char(), (cell)expr->text()->length() + 1));
    return new ir::String(expr, val);
}

ir::Value* Semantics::CheckFieldAccessExpr(FieldAccessExpr* expr, bool from_call) {
    AutoErrorPos aep(expr->pos());

    ir::Value* base = nullptr;
    if (auto sym_expr = expr->base()->as<SymbolExpr>()) {
        if (!(base = CheckSymbolExpr(sym_expr, true)))
            return nullptr;
    } else {
        if (!(base = CheckRvalue(expr->base())))
            return nullptr;
    }

    int token = expr->token();
    if (token == tDBLCOLON)
        return CheckStaticFieldAccessExpr(expr, base);

    const auto& base_val = base->val();
    if (base_val.type()->isArray()) {
        report(expr, 96) << expr->name() << "type" << "array";
        return nullptr;
    }

    if (auto* tn = base->as<ir::Typename>()) {
        Decl* typename_decl = tn->decl();
        auto layout = typename_decl->as<LayoutDecl>();
        if (!layout) {
            report(expr, 444) << typename_decl->name() << expr->name();
            return nullptr;
        }

        auto member = layout->FindMember(expr->name());
        if (!member || !member->as<MemberFunctionDecl>()) {
            report(expr, 444) << typename_decl->name() << expr->name();
            return nullptr;
        }
        auto method = member->as<MemberFunctionDecl>();
        if (!method->is_static()) {
            report(expr, 176) << method->decl_name() << typename_decl->name();
            return nullptr;
        }
        markusage(method, uREAD);
        return new ir::MethodRef(expr, token, base, method);
    }

    Type* base_type = base_val.type();
    if (auto es = base_type->asEnumStruct()) {
        if (auto lval = base->as<ir::Lvalue>())
            base = new ir::Rvalue(lval);
        return CheckEnumStructFieldAccessExpr(expr, base, base_type, es, from_call);
    }
    if (base_type->isReference())
        base_type = base_type->inner();

    if (auto cls = base_type->asClass()) {
        if (auto lval = base->as<ir::Lvalue>())
            base = new ir::Rvalue(lval);
        return CheckClassFieldAccessExpr(expr, base, base_type, cls, from_call);
    }

    auto map = base_type->asMethodmap();
    if (!map) {
        if (base_val.type()->isFunctionLike())
            report(expr, 104) << "function";
        else
            report(expr, 104) << base_val.type();
        return nullptr;
    }

    auto member = map->FindMember(expr->name());
    if (!member) {
        report(expr, 105) << map->name() << expr->name();
        return nullptr;
    }

    if (auto prop = member->as<PropertyDecl>()) {
        // This is the only scenario in which we need to compute a load of the
        // base address. Otherwise, we're only accessing the type.
        if (auto lval = base->as<ir::Lvalue>())
            base = new ir::Rvalue(lval);
        return new ir::Accessor(expr, token, base, prop);
    }

    auto method = member->as<MemberFunctionDecl>();
    if (method->is_static()) {
        report(expr, 177) << method->decl_name() << map->name() << method->decl_name();
        return nullptr;
    }

    if (!from_call) {
        report(expr, 50);
        return nullptr;
    }

    markusage(method, uREAD);
    return new ir::MethodRef(expr, token, base, method);
}

auto Semantics::BindCallTarget(CallExpr* call, Expr* target) -> CallBinding {
    AutoErrorPos aep(target->pos());

    switch (target->kind()) {
        case ExprKind::FieldAccessExpr: {
            auto expr = target->to<FieldAccessExpr>();
            ir::Value* checked = CheckFieldAccessExpr(expr, true);
            if (!checked)
                return {};

            if (!checked->is(IrKind::MethodRef)) {
                report(target, 12);
                return {};
            }

            auto method_ref = checked->to<ir::MethodRef>();

            auto method = method_ref->decl();
            if (auto map = method->parent()->as<MethodmapDecl>()) {
                if (map->ctor() == method) {
                    report(call, 84) << method->parent()->name();
                    return {};
                }
            }

            auto base = method_ref->base();
            if (auto lval = base->as<ir::Lvalue>())
                base = new ir::Rvalue(lval);
            ir::Value* this_arg = nullptr;
            if (method_ref->token() == '.' && !method->is_static()) {
                call->set_implicit_this(target);
                this_arg = base;
            }
            return {method->canonical(), this_arg};
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
                return {mm->ctor()};
            }
            if (auto fun = decl->as<FunctionDecl>()) {
                fun = fun->canonical();
                if (!(fun->is_native() || fun->is_builtin()) && !fun->impl()) {
                    report(target, 4) << decl->name();
                    return {};
                }
                return {fun};
            }
            [[fallthrough]];
        }
        default: {
            ir::Value* node = CheckRvalue(target);
            if (!node)
                return {};

            if (auto lval = node->as<ir::Lvalue>())
                node = new ir::Rvalue(lval);

            if (auto ft = node->val().type()->as<FunctionType>()) {
                if (ft->conv() == FunctionType::Legacy)
                    report(target, 33);
                return {node};
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

ir::Value* Semantics::CheckEnumStructFieldAccessExpr(FieldAccessExpr* expr, ir::Value* base,
                                                      Type* type, EnumStructDecl* root, bool from_call)
{
    Decl* field_decl = FindEnumStructField(type, expr->name());
    if (!field_decl) {
        report(expr, 105) << type << expr->name();
        return nullptr;
    }

    if (auto fun = field_decl->as<MemberFunctionDecl>()) {
        if (!from_call) {
            report(expr, 76);
            return nullptr;
        }

        markusage(fun, uREAD);
        return new ir::MethodRef(expr, expr->token(), base, fun);
    }

    auto field = field_decl->as<LayoutFieldDecl>();
    assert(field);

    return new ir::FieldRef(expr, expr->token(), base, field);
}

ir::Value* Semantics::CheckClassFieldAccessExpr(FieldAccessExpr* expr, ir::Value* base,
                                                 Type* type, ClassDecl* decl, bool from_call)
{
    Decl* member = FindClassField(type, expr->name());
    if (!member) {
        report(expr, 105) << type << expr->name();
        return nullptr;
    }

    if (!CheckPrivateMemberAccess(expr, member, decl, *sc_))
        return nullptr;

    if (auto prop = member->as<PropertyDecl>()) {
        if (auto lval = base->as<ir::Lvalue>())
            base = new ir::Rvalue(lval);
        return new ir::Accessor(expr, expr->token(), base, prop);
    }

    if (auto fun = member->as<MemberFunctionDecl>()) {
        if (!from_call) {
            report(expr, 76);
            return nullptr;
        }

        markusage(fun, uREAD);
        return new ir::MethodRef(expr, expr->token(), base, fun);
    }

    auto field = member->as<LayoutFieldDecl>();
    assert(field);

    return new ir::FieldRef(expr, expr->token(), base, field);
}

ir::Value* Semantics::CheckStaticFieldAccessExpr(FieldAccessExpr* expr, ir::Value* base) {
    AutoErrorPos aep(expr->pos());

    if (!base->is(IrKind::Typename)) {
        report(expr, 108);
        return nullptr;
    }
    const auto& base_val = base->val();

    Type* type = base_val.type();
    Decl* field = FindEnumStructField(type, expr->name());
    if (!field) {
        report(expr, 105) << type << expr->name();
        return nullptr;
    }

    auto fd = field->as<LayoutFieldDecl>();
    if (!fd) {
        report(expr, 445) << field->name();
        return nullptr;
    }

    return new ir::StaticFieldRef(expr, base, fd, types_->type_int());
}

ir::Value* Semantics::CheckSizeofExpr(SizeofExpr* expr) {
    AutoErrorPos aep(expr->pos());

    Expr* child = expr->child();
    ir::Value* child_ir = nullptr;
    if (auto sym = child->as<SymbolExpr>()) {
        if (!(child_ir = CheckSymbolExpr(sym, true)))
            return nullptr;
    } else {
        if (!(child_ir = CheckExpr(child)))
            return nullptr;
    }

    Type* int_type = types_->type_int();

    if (auto* access = child_ir->as<ir::StaticFieldRef>()) {
        auto field = access->field();
        if (auto array = field->type()->as<ArrayType>())
            return new ir::Constant(expr, ConstVal(int_type, array->size()));
        if (field->type()->asEnumStruct())
            return new ir::Sizeof(expr, child_ir, ExpressionVal(int_type));
        return new ir::Constant(expr, ConstVal(int_type, 1));
    }

    if (auto* tn = child_ir->as<ir::Typename>()) {
        if (!tn->decl()->as<EnumStructDecl>()) {
            report(child, 72);
            return nullptr;
        }
        return new ir::Sizeof(expr, child_ir, ExpressionVal(int_type));
    }

    const auto& cv = child_ir->val();
    if (child_ir->is(IrKind::Index) || child_ir->is(IrKind::Variable) ||
        child_ir->is(IrKind::FieldRef) ||
        (!child_ir->lvalue() && !child_ir->is(IrKind::Constant)))
    {
        if (cv.type()->asEnumStruct())
            return new ir::Sizeof(expr, child_ir, ExpressionVal(int_type));
        if (auto array = cv.type()->as<ArrayType>()) {
            if (!array->size()) {
                report(child, 163);
                return nullptr;
            }
            return new ir::Constant(expr, ConstVal(int_type, array->size()));
        }
        if (!child_ir->lvalue()) {
            report(child, 72);
            return nullptr;
        }
        if (cv.type()->isIntPtr()) {
            report(expr, 449) << cv.type();
            return nullptr;
        }
        report(expr, 252);
        return new ir::Constant(expr, ConstVal(int_type, 1));
    }

    report(child, 72);
    return nullptr;
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

ir::Value* Semantics::CheckCallExpr(CallExpr* call) {
    AutoErrorPos aep(call->pos());

    FunctionDecl* fun = nullptr;
    ir::Value* target = nullptr;
    Type* ctor_type = nullptr;
    ir::Value* this_arg = nullptr;

    if (call->token() == tNEW) {
        auto ctor = BindNewTarget(call->target());
        if (!ctor)
            return nullptr;
        fun = ctor->first;
        ctor_type = ctor->second;
    } else {
        CallBinding binding = BindCallTarget(call, call->target());
        if (auto target_fun = std::get_if<FunctionDecl*>(&binding.target)) {
            fun = *target_fun;
            this_arg = binding.this_arg;
        } else if (auto target_expr = std::get_if<ir::Value*>(&binding.target)) {
            target = *target_expr;
        } else {
            return nullptr;
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
                return nullptr;
            }
        }

        markusage(fun, uREAD);

        if (fun->deprecate())
            report(call, 234) << fun->name() << fun->deprecate();
    } else if (target) {
        call->set_callee(target->val().type()->to<FunctionType>());
    }

    ExprVal out_val = {};

    if (ctor_type) {
        call->set_ctor_type(ctor_type);

        if (!fun) {
            if (!call->args().empty()) {
                report(call->pos(), 92);
                return nullptr;
            }
            out_val.set_expr(ctor_type);
            return new ir::Call(call, nullptr, {}, out_val);
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
            return nullptr;
        }
        ir::Value* checked_this = ProcessArgument(call, ft, ft->arg_type(0), this_arg, &ps, 0);
        if (!checked_this)
            return nullptr;
        ps.argv[0] = checked_this;
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
                return nullptr;
            }
            argpos = nargs;
            if (!ft->variadic() && argidx >= ft->nargs()) {
                report(param, 92);
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
        auto formal = argidx < ft->nargs() ? ft->arg_type(argidx) : QualType{};
        auto result = CheckArgument(call, ft, formal, param, &ps, argpos);
        if (!result)
            return nullptr;

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
                    return nullptr;
                }
            }
        }

        // Don't iterate past the varargs position.
        if (!ft->variadic() || argidx < ft->nargs())
            argidx++;
    }

    if (!sc_->func()) {
        report(call, 10);
        return nullptr;
    }

    // Check for missing or invalid extra arguments, and fill in default
    // arguments.
    for (unsigned int argidx = first_argidx; argidx < ft->nargs(); argidx++) {
        if (argidx >= ps.argv.size() || !ps.argv[argidx]) {
            auto result = CheckArgument(call, ft, ft->arg_type(argidx), nullptr, &ps, argidx);
            if (!result)
                return nullptr;
            ps.argv[argidx] = result;
        }
    }

    if (ctor_type)
        out_val.set_expr(ctor_type);
    else
        out_val.set_expr(ft->return_type());
    return new ir::Call(call, target, ps.argv, out_val);
}

// Note: currently formal is null for variadic arguments. We don't really
// bother checking legacy vararg types anymore.
ir::Value* Semantics::CheckArgument(CallExpr* call, FunctionType* ft, QualType formal,
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

        auto arg_ir = new ir::DefaultArg(param);
        if (arg_ir->val().type())
            assert(!arg_ir->val().type()->isInt64());

        // The rest of the code to handle default values is in DoEmit.
        return arg_ir;
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
        auto callee = call->fun();
        if (!callee || !callee->is_native()) {
            report(param, 51);
            return nullptr;
        }
        return new ir::SpreadArgs(param);
    }

    ir::Value* arg = nullptr;
    if (formal) {
        if (!(arg = CheckRvalue(param, *formal)))
            return nullptr;
    } else {
        if (!(arg = CheckExpr(param)))
            return nullptr;
    }

    return ProcessArgument(call, ft, formal, arg, ps, pos);
}

ir::Value* Semantics::ProcessArgument(CallExpr* call, FunctionType* ft, QualType formal,
                                      ir::Value* arg, ParamState* ps, unsigned int pos)
{
    while (pos >= ps->argv.size())
        ps->argv.push_back(nullptr);

    unsigned int visual_pos = call->implicit_this() ? pos : pos + 1;

    AutoErrorPos aep(arg->pos());

    if (arg->is(IrKind::Accessor)) {
        if (!CheckRvalueAccess(arg))
            return nullptr;
        arg = new ir::Rvalue(arg->to<ir::Lvalue>());
    }

#ifndef NDEBUG
    bool handling_this = call->implicit_this() && (pos == 0);
#endif

    const auto* val = &arg->val();
    bool lvalue = arg->lvalue();
    if (!formal) {
        // We don't pass down a type for variadic arguments.
        assert(!handling_this);

        // Always pass by reference.
        if (auto* var = arg->as<ir::Variable>()) {
            if (var->decl()->is_const() && !formal.is_const()) {
                // Treat a "const" variable passed to a function with a
                // non-const "variable argument list" as a constant here.
                if (!lvalue) {
                    report(arg, 22); // need lvalue
                    return nullptr;
                }
            }
        }
        if (val->type()->isVoid()) {
            report(arg, 466);
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
        if (auto slice = ParamNeedsSlice(arg, nullptr))
            arg = slice;
        if (arg->lvalue() && val->type()->isNonFlatArray()) {
            arg = new ir::Rvalue(arg->to<ir::Lvalue>());
            val = &arg->val();
        }
    } else if (formal->isReference()) {
        assert(!handling_this);

        if (!lvalue ||
            (arg->is(IrKind::Index) &&
             (val->type()->maybe_lit_size().value_or(4) != 4)))
        {
            report(arg, 35) << visual_pos; // argument type mismatch
            return nullptr;
        }
        if (auto* var = arg->as<ir::Variable>()) {
            if (var->decl()->is_const() && !formal.is_const()) {
                report(arg, 35) << visual_pos; // argument type mismatch
                return nullptr;
            }
        }

        if (formal->inner()->isWideInt()) {
            if (!IsValidInt64RefArg(val->type())) {
                report(arg, 134) << *formal << val->type();
                return nullptr;
            }
        } else {
            if (IsValidInt64RefArg(val->type())) {
                report(arg, 134) << *formal << val->type();
                return nullptr;
            }
            CheckCoercion(arg, formal->inner(), QualType(val->type()), CvtContext::Argument);
        }
    } else if (auto to_array = formal->as<ArrayType>()) {
        if (auto slice = ParamNeedsSlice(arg, to_array))
            arg = slice;
        if (auto lval = arg->as<ir::Lvalue>())
            arg = new ir::Rvalue(lval);

        val = &arg->val();

        auto type = val->type();
        if (!CheckCoercion(arg, *formal, QualType(type), CvtContext::Argument))
            return nullptr;

        ir::Value* target = arg->is(IrKind::Rvalue) ? arg->to<ir::Rvalue>()->expr() : arg;
        if (auto* var = target->as<ir::Variable>()) {
            if (var->decl()->is_const() && !formal.is_const()) {
                report(arg, 35) << visual_pos; // argument type mismatch
                return nullptr;
            }
        }
    } else {
        if (lvalue) {
            arg = new ir::Rvalue(arg->to<ir::Lvalue>());
            val = &arg->val();
        }

        if (val->type()->isInt() && formal->isInt64()) {
            arg = BuildSimpleCast(arg, BuiltinType::Int64);
            val = &arg->val();
        }

        if (!(arg = TryConversion(arg, formal, CvtContext::Argument)))
            return nullptr;
        val = &arg->val();
    }

    if ((call->fun() && call->fun()->is_native()) || !formal) {
        if (!val->type()->isAllowedInNativeCall())
            ReportInvalidNativeArgument(arg, val->type());
    }

    if (formal)
        arg = CoerceNull(arg, *formal);
    return arg;
}

bool Semantics::CheckStaticAssertStmt(StaticAssertStmt* stmt) {
    auto node = CheckExpr(stmt->expr());
    if (!node)
        return false;

    auto ck = FindConversion(node->val().type(), types_->type_bool(), CvtContext::Argument);
    if (!HasImplicitConversion(ck)) {
        ReportConversionDiagnostic(node, types_->type_bool(), node->val().qualified());
        return false;
    }
    node = BuildConversion(node, ck, types_->type_bool());

    ir::Constant* val = AnalyzeForConst(node);
    if (!val)
        return false;

    if (val->get_cell())
        return true;

    std::string message;
    if (stmt->text())
        message += ": " + std::string(stmt->text()->chars(), stmt->text()->length());

    report(node, 70) << message;
    return false;
}

ir::Value* Semantics::CheckNewArrayExpr(NewArrayExpr* expr) {
    return CheckNewArrayExprForArrayInitializer(expr);
}

ir::Value* Semantics::CheckNewArrayExprForArrayInitializer(NewArrayExpr* na) {
    if (na->analyzed())
        return na->sema_result();

    na->set_sema_result(nullptr);

    ExprVal out_val = {};

    PoolList<int> dims;
    std::vector<ir::Value*> dim_nodes;
    bool seen_null = false;
    for (auto& expr : na->exprs()) {
        if (!expr) {
            seen_null = true;
            dims.emplace_back(0);
            dim_nodes.emplace_back(nullptr);
            continue;
        }
        if (seen_null) {
            report(na, 185);
            return nullptr;
        }
        ir::Value* dim = CheckRvalue(expr);
        if (!dim)
            return nullptr;
        if (auto lval = dim->as<ir::Lvalue>())
            dim = new ir::Rvalue(lval);
        dim_nodes.emplace_back(dim);

        const auto& v = dim->val();
        if (IsLegacyEnumType(sc_->scope(), v.type())) {
            report(expr, 153);
            return nullptr;
        }
        if (!IsValidIndexType(v.type())) {
            report(expr, 77) << v.type();
            return nullptr;
        }
        if (auto* dc = dim->as<ir::Constant>()) {
            if (dc->get_i32() <= 0) {
                report(expr, 9);
                return nullptr;
            }
        }
        dims.emplace_back(0);
    }
    assert(na->type()->isArray());

    out_val.set_type(na->type());
    auto node = new ir::NewArray(na, dim_nodes, out_val);
    na->set_sema_result(node);
    return node;
}

bool Semantics::CheckIfStmt(IfStmt* stmt) {
    stmt->set_sema_cond(AnalyzeForTest(stmt->cond()));

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
    auto node = CheckRvalue(stmt->expr(), nullptr, EXPR_DISCARD_RESULT);
    if (!node)
        return false;
    if (auto lval = node->as<ir::Lvalue>())
        node = new ir::Rvalue(lval);

    if (!HasSideEffects(node))
        report(node, 215);
    stmt->set_sema_expr(node);
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

    ir::Value* node = CheckRvalue(expr);
    if (!node)
        return false;

    if (auto lval = node->as<ir::Lvalue>())
        node = new ir::Rvalue(lval);

    AutoErrorPos aep(node->pos());

    if (fun->return_type()->isVoid()) {
        report(stmt, 88);
        return false;
    }

    bool already_returned = sc_->returns_value();
    sc_->set_returns_value();

    if (fun->return_type()->isInt64() && CanPromoteToInt64(node->val().type())) {
        node = BuildSimpleCast(node, BuiltinType::Int64);
        stmt->set_sema_expr(node);
        return true;
    }

    // Check that the return statement matches the declared return type.
    // If a return statement has already been checked, the function's return type
    // is now fixed. We use Assignment to prevent returning a flat array to a
    // dynamic array return type, while the first return uses Return to allow
    // updating the return type.
    CvtContext why = already_returned ? CvtContext::Assignment : CvtContext::Return;
    if ((node = TryConversion(node, fun->return_type(), why)) == nullptr)
        return false;
    stmt->set_sema_expr(node);

    if (node->val().type()->isEnumStruct() || node->val().type()->isFixedArray()) {
        if (!CheckCompoundReturnStmt(stmt))
            return false;
    }
    return true;
}

bool Semantics::CheckCompoundReturnStmt(ReturnStmt* stmt) {
    FunctionDecl* curfunc = sc_->func();
    assert(curfunc == curfunc->canonical());

    const auto& val = stmt->sema_expr()->val();

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
    auto node = CheckRvalue(stmt->expr());
    if (!node)
        return false;
    auto expr = node->pn();

    const auto& v = node->val();
    auto* target = node->is(IrKind::Rvalue) ? node->to<ir::Rvalue>()->expr() : node;
    if (target->is(IrKind::Variable)) {
        if (v.type()->isArray() || v.type()->isEnumStruct()) {
            report(expr, 167) << v.type();
            return false;
        }
    } else if (auto* acc = target->as<ir::Accessor>()) {
        if (acc->accessor()->getter())
            markusage(acc->accessor()->getter(), uREAD);
        if (acc->accessor()->setter())
            markusage(acc->accessor()->setter(), uREAD);
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
    stmt->set_sema_expr(node);
    return true;
}

bool Semantics::CheckDoWhileStmt(DoWhileStmt* stmt) {
    ir::Value* cond_ir = AnalyzeForTest(stmt->cond());
    stmt->set_sema_cond(cond_ir);

    ke::Maybe<cell> constval;
    if (auto* c = ir::Value::As<ir::Constant>(cond_ir))
        constval.init(c->get_i32());

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

    ir::Value* cond_ir = nullptr;
    if (stmt->cond()) {
        cond_ir = AnalyzeForTest(stmt->cond());
        if (!cond_ir)
            ok = false;
        stmt->set_sema_cond(cond_ir);
    }

    ir::Value* advance_ir = nullptr;
    if (stmt->advance()) {
        if (!(advance_ir = CheckRvalue(stmt->advance())))
            ok = false;
    }
    stmt->set_sema_advance(advance_ir);

    ke::Maybe<cell> constval;
    if (auto* c = ir::Value::As<ir::Constant>(cond_ir))
        constval.init(c->get_i32());

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
    stmt->set_always_taken(!stmt->cond() || (constval.isValid() && constval.get()));

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
    ir::Value* checked_expr = CheckRvalue(expr);
    bool tag_ok = checked_expr != nullptr;
    if (checked_expr) {
        if (auto lval = checked_expr->as<ir::Lvalue>())
            checked_expr = new ir::Rvalue(lval);
        stmt->set_sema_expr(checked_expr);

        const auto& v = checked_expr->val();
        if (!(v.type()->coercesToInt() || v.type()->isFloat())) {
            report(450) << v.type() << types_->type_int();
            tag_ok = false;
        }
    }

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
    for (size_t i = 0; i < stmt->cases().size(); i++) {
        const auto& case_entry = stmt->cases()[i];

        std::vector<ir::Value*> checked_case_exprs;
        for (Expr* expr : case_entry.first) {
            ir::Value* checked = CheckRvalue(expr);
            checked_case_exprs.push_back(checked);
            if (!checked)
                continue;

            if (!tag_ok)
                continue;

            const auto& v = checked_expr->val();
            ConversionKind ck = FindConversion(checked->val().type(), v.type(),
                                               CvtContext::Assignment);
            if (!IsNopConversion(ck)) {
                report(expr, 450) << v.type() << checked->val().type();
                continue;
            }
            if (ck == ConversionKind::TagMismatch)
                report(expr, 213) << v.type() << checked->val().type();

            if (!checked->is(IrKind::Constant)) {
                report(expr, 8);
                continue;
            }

            cell value = checked->to<ir::Constant>()->get_cell();
            if (!case_values.count(value))
                case_values.emplace(value);
            else
                report(expr, 40) << value;
        }
        stmt->set_sema_case_exprs(i, checked_case_exprs);

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

void Semantics::ReportInvalidNativeArgument(ir::Value* arg, Type* type) {
    if (type->isFunctionLike())
        report(arg, 43);
    else
        report(arg, 48) << type;
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
            ir::Value* rhs_ir = CheckExpr(rhs);
            if ((!rhs_ir || !rhs_ir->is(IrKind::Constant)) && !rhs->is(ExprKind::SizeofExpr))
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

ir::Value* Semantics::CheckFunctionExpr(FunctionExpr* expr) {
    auto fun = expr->decl();
    if (!CheckFunctionDecl(fun))
        return nullptr;

    closures_.emplace_back(fun);

    ExprVal out_val;
    out_val.set_expr(fun->type());
    return new ir::Function(expr, out_val);
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

ir::Value* Semantics::CheckRvalue(Expr* expr, Type* target, uint32_t flags) {
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
    ir::Value* r = CheckExpr(expr, flags);
    if (!r)
        return nullptr;
    return CheckRvalueAccess(r);
}

ir::Value* Semantics::CheckRvalueAccess(ir::Value* expr) {
    if (auto* acc = expr->as<ir::Accessor>()) {
        auto accessor = acc->accessor();
        if (!accessor->getter()) {
            report(expr, 149) << accessor->name();
            return nullptr;
        }
        if (!CheckAccessorAccess(*sc_, expr, accessor, accessor->getter()))
            return nullptr;
    }
    return expr;
}

bool Semantics::IsThisAtom(sp::Atom* atom) {
    if (!this_atom_)
        this_atom_ = cc_.atom("this");
    return atom == this_atom_;
}


ir::Value* Semantics::CoerceNull(ir::Value* expr, Type* formal) {
    if (expr->val().type()->isNull() && !formal->isHeapItem()) {
        return new ir::Constant(expr->pn(), ConstVal(types_->type_int(), 0));
    }
    return expr;
}

ir::Value* Semantics::BuildSimpleCast(ir::Value* from, BuiltinType type) {
    if (auto lval = from->as<ir::Lvalue>())
        from = new ir::Rvalue(lval);

    Type* to = types_->GetBuiltin(type);

    // Fold constant conversions into a Constant rather than building a cast.
    if (auto* c = from->as<ir::Constant>()) {
        if (type == BuiltinType::Float && c->val().type()->coercesToInt()) {
            return new ir::Constant(from->pn(), ConstVal(to, float(c->get_cell())));
        }
        if (c->val().type()->isInt() && type == BuiltinType::Int64)
            return new ir::Constant(from->pn(), ConstVal(to, int64_t(c->get_i32())));
        if (c->val().type()->isInt() && type == BuiltinType::IntPtr)
            return new ir::Constant(from->pn(), ConstVal(to, c->get_i32()));
    }

    return new ir::SimpleCast(from->pn(), from, to);
}

ir::Value* Semantics::BuildConversion(ir::Value* from, const Conversion& cv) {
    return BuildConversion(from, cv.ck, cv.type);
}

ir::Value* Semantics::BuildConversion(ir::Value* from, ConversionKind ck, Type* to) {
    switch (ck) {
        case ConversionKind::Numeric:
            assert(to->isBuiltin());
            return BuildSimpleCast(from, to->builtin_type());
        case ConversionKind::FuncToLegacy:
        case ConversionKind::LegacyToFunc:
        {
            if (auto lval = from->as<ir::Lvalue>())
                from = new ir::Rvalue(lval);
            return new ir::SimpleCast(from->pn(), from, to);
        }
        case ConversionKind::CoerceNull:
            return CoerceNull(from, to);
        default:
            return from;
    }
}

static inline bool CanImplicitSliceArgument(ir::Value* param, ArrayType* to) {
    const auto& val = param->val();
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
    if (param->is(IrKind::Index)) {
        if (val.type()->isEnumStruct() || val.type()->isArray())
            return false;
        if (to && !AreSliceElementsCompatible(val.type(), to->inner()))
            return false;
        return true;
    }
    return false;
}

ir::Value* Semantics::ParamNeedsSlice(ir::Value* param, ArrayType* to) {
    if (!CanImplicitSliceArgument(param, to))
        return nullptr;

    ir::Value* base = nullptr;
    ir::Value* index_expr = nullptr;
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
        auto index = param->as<ir::Index>();
        if (!index)
            return nullptr;
        base = index->base();
        index_expr = index->index();
        inner_type = param->val().type();
    }

    Type* type = types_->defineArray(inner_type, size);
    return new ir::Slice(base, index_expr, type);
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
        ExprVal shared_val;
        shared_val.set_expr(shared_class_->type().unqualified());
        auto call_ir = new ir::Call(call, nullptr, {}, shared_val);
        shared_object_->set_init(call);
        shared_object_->set_sema_init_rhs(call_ir);
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
