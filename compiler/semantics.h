// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
// Copyright (c) ITB CompuPhase, 1997-2006
//
#pragma once

#include <unordered_set>
#include <variant>
#include <vector>

#include "coercion-rules.h"
#include "compile-context.h"
#include "errors.h"
#include "ir-node.h"
#include "sc.h"
#include "scopes.h"
#include "parse-node.h"

namespace sp {
namespace cc {

class AutoCreateScope;
class Semantics;

class SemaContext
{
  public:
    explicit SemaContext(Semantics* sema)
      : cc_(CompileContext::get()),
        sema_(sema),
        func_(nullptr)
    {
        cc_prev_sc_ = cc_.sema();
        cc_.set_sema(this);
        scope_ = cc_.globals();
    }

    SemaContext(SemaContext& parent, FunctionDecl* func)
      : cc_(parent.cc_),
        outer_(&parent),
        sema_(parent.sema()),
        scope_(parent.scope_),
        func_(func),
        preprocessing_(parent.preprocessing())
    {
        cc_prev_sc_ = cc_.sema();
        cc_.set_sema(this);
        if (parent.func() != nullptr) {
            while (scope_ && !scope_->IsGlobalOrFileStatic())
                scope_ = scope_->parent();
        }
    }

    ~SemaContext() {
        cc_.set_sema(cc_prev_sc_);
    }

    CompileContext& cc() { return cc_; }

    bool BindType(const token_pos_t& pos, TypenameInfo* ti);
    bool BindType(const token_pos_t& pos, typeinfo_t* ti);
    bool BindType(const token_pos_t& pos, Atom* atom, bool is_label, Type** type);

    Stmt* void_return() const { return void_return_; }
    void set_void_return(Stmt* stmt) { void_return_ = stmt; }

    bool warned_mixed_returns() const { return warned_mixed_returns_; }
    void set_warned_mixed_returns() { warned_mixed_returns_ = true; }

    bool returns_value() const { return returns_value_; }
    void set_returns_value() { returns_value_ = true; }

    bool& loop_has_break() { return loop_has_break_; }
    bool& loop_has_continue() { return loop_has_continue_; }
    bool& loop_has_return() { return loop_has_return_; }

    bool warned_unreachable() const { return warned_unreachable_; }
    void set_warned_unreachable() { warned_unreachable_ = true; }

    FunctionDecl* func() const { return func_; }
    Semantics* sema() const { return sema_; }
    SemaContext* outer() const { return outer_; }

    std::vector<VarDeclBase*>& shared_locals() { return shared_locals_; }

    SymbolScope* ScopeForAdd();

    // Currently, this only refers to local/argument scopes, and not global
    // scope. They will be linked together when reparse goes away.
    SymbolScope* scope() const { return scope_; }
    void set_scope(SymbolScope* scope) { scope_ = scope; }

    AutoCreateScope* scope_creator() const { return scope_creator_; }
    void set_scope_creator(AutoCreateScope* scope) { scope_creator_ = scope; }

    void set_preprocessing() { preprocessing_ = true; }
    bool preprocessing() const { return preprocessing_; }

    std::unordered_set<SymbolScope*>& static_scopes() { return static_scopes_; }

  private:
    CompileContext& cc_;
    SemaContext* outer_ = nullptr;
    Semantics* sema_ = nullptr;
    SymbolScope* scope_ = nullptr;
    AutoCreateScope* scope_creator_ = nullptr;
    FunctionDecl* func_ = nullptr;
    Stmt* void_return_ = nullptr;
    bool warned_mixed_returns_ = false;
    bool returns_value_ = false;
    bool loop_has_break_ = false;
    bool loop_has_continue_ = false;
    bool loop_has_return_ = false;
    bool warned_unreachable_ = false;
    bool preprocessing_ = false;
    SemaContext* cc_prev_sc_ = nullptr;
    std::unordered_set<SymbolScope*> static_scopes_;
    std::vector<VarDeclBase*> shared_locals_;
};

class Semantics final
{
    friend class ArrayTypeResolver;
    friend class ArrayValidator;
    friend class BinaryExprChecker;
    friend class ConstDecl;
    friend class EnumDecl;
    friend class FunctionDecl;
    friend class Parser;

  public:
    explicit Semantics(CompileContext& cc);

    bool Analyze(ParseTree* tree);

    CompileContext& cc() { return cc_; }
    bool CheckCoercion(const token_pos_t& pos, QualType formal, QualType actual,
                       CvtContext why);
    ir::Value* ParamNeedsSlice(ir::Value* param, ArrayType* to);
    bool CheckCoercion(ir::Value* node, QualType formal, QualType actual,
                       CvtContext why);
    ir::Value* TryConversion(ir::Value* expr, QualType formal, CvtContext why);
    SymbolScope* current_scope() const;
    SemaContext* context() { return sc_; }
    void set_context(SemaContext* sc) { sc_ = sc; }

    int next_fun_expr_count() { return fun_expr_count_++; }
    int next_shared_class_count() { return shared_class_count_++; }

  private:

    void GenerateInitFunctions(ParseTree* tree);
    FunctionDecl* GenerateInitFunction(const std::vector<VarDeclBase*>& vars, uint32_t suffix);

    bool CheckStmt(Stmt* stmt);
    bool CheckStmtList(StmtList* list);
    bool CheckBlockStmt(BlockStmt* stmt);
    bool CheckChangeScopeNode(ChangeScopeNode* node);
    bool CheckMethodmapDecl(MethodmapDecl* info);
    bool CheckEnumStructDecl(EnumStructDecl* info);
    bool CheckEnumStructVarDecl(VarDeclBase* decl);
    ir::Value* ValidateEnumStructInitializer(EnumStructDecl* es, Expr* init);
    bool CheckClassDecl(ClassDecl* info);
    bool CheckFunctionDecl(FunctionDecl* info);
    bool CheckFunctionDeclImpl(FunctionDecl* info);
    void CheckFunctionReturnUsage(FunctionDecl* info);
    bool CheckPragmaUnusedStmt(PragmaUnusedStmt* stmt);
    bool CheckSwitchStmt(SwitchStmt* stmt);
    bool CheckForStmt(ForStmt* stmt);
    bool CheckDoWhileStmt(DoWhileStmt* stmt);
    bool CheckBreakStmt(BreakStmt* stmt);
    bool CheckContinueStmt(ContinueStmt* stmt);
    bool CheckDeleteStmt(DeleteStmt* stmt);
    bool CheckStaticAssertStmt(StaticAssertStmt* stmt);
    bool CheckReturnStmt(ReturnStmt* stmt);
    bool CheckCompoundReturnStmt(ReturnStmt* stmt);
    bool CheckNativeCompoundReturn(FunctionDecl* info);
    void ReportInvalidNativeArgument(ParseNode* node, Type* type);
    void ReportInvalidNativeArgument(ir::Value* arg, Type* type);
    bool CheckExprStmt(ExprStmt* stmt);
    bool CheckIfStmt(IfStmt* stmt);
    bool CheckConstDecl(ConstDecl* decl);
    bool CheckVarDecl(VarDeclBase* decl);
    bool CheckTypedVarDecl(VarDeclBase* decl);
    bool CheckInferredVarDecl(VarDeclBase* decl);
    bool CheckConstDecl(VarDecl* decl);
    bool CheckPstructDecl(VarDeclBase* decl);
    bool CheckPstructArg(VarDeclBase* decl, PstructDecl* ps, StructInitFieldExpr* field,
                         std::vector<bool>* visited);

    // Expressions.
    enum ExprFlags {
        EXPR_DEFAULT = 0,
        EXPR_DISCARD_RESULT = (1 << 0),
        EXPR_ALLOW_TYPE_SYMS = (1 << 1),
    };

    ir::Value* CheckExpr(Expr* expr, uint32_t flags = EXPR_DEFAULT);
    ir::Value* CheckNewArrayExpr(NewArrayExpr* expr);
    ir::Value* CheckArrayExpr(ArrayExpr* expr, Type* target = nullptr);
    ir::Value* CheckStringExpr(StringExpr* expr, Type* target = nullptr);
    ir::Value* CheckNullExpr(NullExpr* expr);
    ir::Value* CheckThisExpr(ThisExpr* expr);
    ir::Value* CheckCommaExpr(CommaExpr* expr);
    ir::Value* CheckIndexExpr(IndexExpr* expr);
    ir::Value* CheckCallExpr(CallExpr* expr);
    ir::Value* CheckSymbolExpr(SymbolExpr* expr, bool allow_types);
    ir::Value* CheckSizeofExpr(SizeofExpr* expr);
    ir::Value* CheckCastExpr(CastExpr* expr);
    ir::Value* CheckIncDecExpr(IncDecExpr* expr, uint32_t flags);
    ir::Value* CheckTernaryExpr(TernaryExpr* expr, Type* target = nullptr);
    ir::Value* CheckChainedCompareExpr(ChainedCompareExpr* expr);
    ir::Value* CheckLogicalExpr(LogicalExpr* expr);
    ir::Value* CheckBinaryExpr(BinaryExpr* expr);
    ir::Value* CheckUnaryExpr(UnaryExpr* expr);
    ir::Value* CheckFieldAccessExpr(FieldAccessExpr* expr, bool from_call);
    ir::Value* CheckStaticFieldAccessExpr(FieldAccessExpr* expr, ir::FieldAccess* field_ir);
    ir::Value* CheckEnumStructFieldAccessExpr(FieldAccessExpr* expr, ir::FieldAccess* field_ir,
                                               Type* type, EnumStructDecl* root, bool from_call);
    ir::Value* CheckClassFieldAccessExpr(FieldAccessExpr* expr, ir::FieldAccess* field_ir,
                                          Type* type, ClassDecl* decl, bool from_call);
    ir::Value* CheckFunctionExpr(FunctionExpr* expr);

    ir::Value* CheckRvalueAccess(ir::Value* expr);

    bool AddImplicitDynamicInitializer(VarDeclBase* decl);
    ir::Value* BuildConversion(ir::Value* from, ConversionKind ck, Type* to);
    ir::Value* BuildConversion(ir::Value* from, const Conversion& cv);
    ir::Value* CheckRvalue(Expr* expr, Type* target = nullptr, uint32_t flags = 0);
    ir::Value* BuildSimpleCast(ir::Value* from, BuiltinType type);
    ir::Value* CoerceNull(ir::Value* expr, Type* formal);
    std::optional<ConversionKind> FindConstantConversion(ir::Value* source, Type* from_type,
                                                         Type* to, CvtContext why);
    bool CheckCoercionImpl(ir::Value* node, const token_pos_t& pos, QualType formal,
                           QualType actual, CvtContext why, ConversionKind ck);
    void ReportConversionDiagnostic(const token_pos_t& pos, QualType formal, QualType actual);
    void ReportConversionDiagnostic(ir::Value* node, QualType formal, QualType actual);

    struct ParamState {
        std::vector<ir::Value*> argv;
    };

    bool CheckArrayDeclaration(VarDeclBase* decl);
    ir::Value* CheckNewArrayExprForArrayInitializer(NewArrayExpr* expr);
    ir::Value* CheckArgument(CallExpr* call, FunctionType* ft, QualType formal, Expr* param,
                             ParamState* ps, unsigned int argpos);
    ir::Value* ProcessArgument(CallExpr* call, FunctionType* ft, QualType formal,
                               ir::Value* arg, ParamState* ps, unsigned int argpos);
    using CallCtor = std::pair<FunctionDecl*, Type*>;
    std::optional<CallCtor> BindNewTarget(Expr* target);

    struct CallBinding
    {
        CallTarget target;
        ir::Value* this_arg = nullptr;
    };

    CallBinding BindCallTarget(CallExpr* call, Expr* target);

    ir::Value* AnalyzeForTest(Expr* expr);
    const ExprVal* AnalyzeForConst(ir::Value* node);

    // Helper for CheckExpr + AnalyzeForConst.
    ir::Value* CheckExprForConst(Expr* expr);

    void DeduceLiveness();
    void DeduceMaybeUsed();
    bool TestSymbol(Decl* sym, bool testconst);
    bool TestSymbols(SymbolScope* scope, bool testconst);

    void CheckVoidDecl(const typeinfo_t* type, int variable);
    void CheckVoidDecl(const declinfo_t* decl, int variable);

    bool CheckScalarType(ir::Value* node);
    bool IsThisAtom(sp::Atom* atom);

    bool IsIncluded(Decl* expr);
    bool IsIncludedStock(VarDeclBase* expr);

    struct BinaryExprState {
        BinaryExpr* expr;
        ir::Value* left = nullptr;
        ir::Value* right = nullptr;
        bool rhs_resolved = false;

        BinaryExprState(BinaryExpr* expr)
          : expr(expr)
        {}
    };
    ir::Value* CheckBinaryExprImpl(BinaryExprState& state);
    bool CheckAssignmentLHS(BinaryExprState& state);

    struct BinaryOperator {
        Conversion left;
        Conversion right;
    };
    std::optional<BinaryOperator> FindBinaryOperator(int token, Type* left_type, Type* right_type);
    std::optional<BinaryOperator> FindEqualityOperator(Type* left_type, Type* right_type);

  private:
    CompileContext& cc_;
    TypeManager* types_ = nullptr;
    std::vector<VarDeclBase*> globals_to_init_;
    tr::unordered_set<SymbolScope*> static_scopes_;
    tr::vector<FunctionDecl*> maybe_used_;
    tr::vector<FunctionDecl*> closures_;
    SemaContext* sc_ = nullptr;
    sp::Atom* this_atom_ = nullptr;
    int fun_expr_count_ = 0;
    int shared_class_count_ = 0;
};

class AutoEnterScope final
{
  public:
    // Create a new scope.
    AutoEnterScope(SemaContext& sc, ScopeKind kind);

    // Use existing scope.
    AutoEnterScope(SemaContext& sc, SymbolScope* scope);

    ~AutoEnterScope();

  private:
    SemaContext& sc_;
    SymbolScope* prev_;
};

class AutoCreateScope final
{
  public:
    // Create a new scope.
    AutoCreateScope(SemaContext& sc, ScopeKind kind, SymbolScope** where);
    ~AutoCreateScope();

    SymbolScope* prev() const { return prev_; }
    ScopeKind kind() const { return kind_; }

  private:
    SemaContext& sc_;
    ScopeKind kind_;
    SymbolScope** where_;
    SymbolScope* prev_;
    AutoCreateScope* prev_creator_;
    std::vector<SymbolScope*> pending_;
};

void ReportFunctionReturnError(FunctionDecl* decl);
bool TestSymbols(SymbolScope* root, int testconst);
void check_void_decl(const typeinfo_t* type, int variable);
void check_void_decl(const declinfo_t* decl, int variable);
int argcompare(ArgDecl* a1, ArgDecl* a2);
bool IsLegacyEnumType(SymbolScope* scope, Type* type);
bool IsValidIndexType(Type* type);
bool HasTagOnInheritanceChain(Type* type, Type* other);

} // namespace cc
} // namespace sp
