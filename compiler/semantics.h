// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) AlliedModders LLC 2021
//  Copyright (c) ITB CompuPhase, 1997-2006
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
#pragma once

#include <unordered_set>
#include <vector>

#include "compile-context.h"
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
        sema_(parent.sema()),
        scope_(parent.scope_),
        func_(func),
        preprocessing_(parent.preprocessing())
    {
        cc_prev_sc_ = cc_.sema();
        cc_.set_sema(this);
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

    // Indicates that the current context only ever exits its control flow
    // through a return statement. For example,
    //
    //     for (;;) {
    //         if (a) return;
    //         if (b) break;
    //     }
    //
    // .. can exit through "break". If this line is omitted, "always_returns"
    // will be true. If the loop had a conditional, this would imply an implicit
    // break statement.
    //
    // This is different from Stmt::flow_type, which indicates whether the
    // statement unconditionally ends in a certain keyword.
    bool always_returns() const { return always_returns_; }
    void set_always_returns() { always_returns_ = true; }
    void set_always_returns(bool value) { always_returns_ = value; }

    bool& loop_has_break() { return loop_has_break_; }
    bool& loop_has_continue() { return loop_has_continue_; }
    bool& loop_has_return() { return loop_has_return_; }

    bool warned_unreachable() const { return warned_unreachable_; }
    void set_warned_unreachable() { warned_unreachable_ = true; }

    FunctionDecl* func() const { return func_; }
    Semantics* sema() const { return sema_; }

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
    Semantics* sema_ = nullptr;
    SymbolScope* scope_ = nullptr;
    AutoCreateScope* scope_creator_ = nullptr;
    FunctionDecl* func_ = nullptr;
    Stmt* void_return_ = nullptr;
    bool warned_mixed_returns_ = false;
    bool returns_value_ = false;
    bool always_returns_ = false;
    bool loop_has_break_ = false;
    bool loop_has_continue_ = false;
    bool loop_has_return_ = false;
    bool warned_unreachable_ = false;
    bool preprocessing_ = false;
    SemaContext* cc_prev_sc_ = nullptr;
    std::unordered_set<SymbolScope*> static_scopes_;
};

class Semantics final
{
    friend class ArrayTypeResolver;
    friend class ArrayValidator;
    friend class ConstDecl;
    friend class EnumDecl;
    friend class FunctionDecl;
    friend class Parser;

  public:
    explicit Semantics(CompileContext& cc);

    bool Analyze(ParseTree* tree);

    CompileContext& cc() { return cc_; }
    SymbolScope* current_scope() const;
    SemaContext* context() { return sc_; }
    void set_context(SemaContext* sc) { sc_ = sc; }

  private:
    enum StmtFlags {
        STMT_DEFAULT = 0x0,
        STMT_OWNS_HEAP = 0x1
    };

    bool CheckStmt(Stmt* stmt, StmtFlags = STMT_DEFAULT);
    bool CheckStmtList(StmtList* list);
    bool CheckBlockStmt(BlockStmt* stmt);
    bool CheckChangeScopeNode(ChangeScopeNode* node);
    bool CheckMethodmapDecl(MethodmapDecl* info);
    bool CheckEnumStructDecl(EnumStructDecl* info);
    bool CheckFunctionDecl(FunctionDecl* info);
    bool CheckFunctionDeclImpl(FunctionDecl* info);
    void CheckFunctionReturnUsage(FunctionDecl* info);
    bool CheckPragmaUnusedStmt(PragmaUnusedStmt* stmt);
    bool CheckSwitchStmt(SwitchStmt* stmt);
    bool CheckForStmt(ForStmt* stmt);
    bool CheckDoWhileStmt(DoWhileStmt* stmt);
    bool CheckBreakStmt(BreakStmt* stmt);
    bool CheckContinueStmt(ContinueStmt* stmt);
    bool CheckExitStmt(ExitStmt* stmt);
    bool CheckDeleteStmt(DeleteStmt* stmt);
    bool CheckAssertStmt(AssertStmt* stmt);
    bool CheckStaticAssertStmt(StaticAssertStmt* stmt);
    bool CheckReturnStmt(ReturnStmt* stmt);
    bool CheckCompoundReturnStmt(ReturnStmt* stmt);
    bool CheckNativeCompoundReturn(FunctionDecl* info);
    bool CheckExprStmt(ExprStmt* stmt);
    bool CheckIfStmt(IfStmt* stmt);
    bool CheckConstDecl(ConstDecl* decl);
    bool CheckVarDecl(VarDeclBase* decl);
    bool CheckConstDecl(VarDecl* decl);
    bool CheckPstructDecl(VarDeclBase* decl);
    bool CheckPstructArg(VarDeclBase* decl, PstructDecl* ps, StructInitFieldExpr* field,
                         std::vector<bool>* visited);

    // Expressions.
    bool CheckExpr(Expr* expr);
    bool CheckNewArrayExpr(NewArrayExpr* expr);
    bool CheckArrayExpr(ArrayExpr* expr);
    bool CheckStringExpr(StringExpr* expr);
    bool CheckTaggedValueExpr(TaggedValueExpr* expr);
    bool CheckNullExpr(NullExpr* expr);
    bool CheckThisExpr(ThisExpr* expr);
    bool CheckCommaExpr(CommaExpr* expr);
    bool CheckIndexExpr(IndexExpr* expr);
    bool CheckCallExpr(CallExpr* expr);
    bool CheckSymbolExpr(SymbolExpr* expr, bool allow_types);
    bool CheckSizeofExpr(SizeofExpr* expr);
    bool CheckCastExpr(CastExpr* expr);
    bool CheckIncDecExpr(IncDecExpr* expr);
    bool CheckTernaryExpr(TernaryExpr* expr);
    bool CheckChainedCompareExpr(ChainedCompareExpr* expr);
    bool CheckLogicalExpr(LogicalExpr* expr);
    bool CheckBinaryExpr(BinaryExpr* expr);
    bool CheckUnaryExpr(UnaryExpr* expr);
    bool CheckFieldAccessExpr(FieldAccessExpr* expr, bool from_call);
    bool CheckStaticFieldAccessExpr(FieldAccessExpr* expr);
    bool CheckEnumStructFieldAccessExpr(FieldAccessExpr* expr, Type* type, EnumStructDecl* root,
                                        bool from_call);
    bool CheckRvalue(Expr* expr);
    bool CheckRvalue(const token_pos_t& pos, const value& val);

    bool CheckAssignmentLHS(BinaryExpr* expr);
    bool CheckAssignmentRHS(BinaryExpr* expr);
    bool AddImplicitDynamicInitializer(VarDeclBase* decl);

    struct ParamState {
        std::vector<Expr*> argv;
    };

    bool CheckArrayDeclaration(VarDeclBase* decl);
    bool CheckNewArrayExprForArrayInitializer(NewArrayExpr* expr);
    Expr* CheckArgument(CallExpr* call, ArgDecl* arg, Expr* expr,
                        ParamState* ps, unsigned int argpos);
    bool CheckWrappedExpr(Expr* outer, Expr* inner);
    FunctionDecl* BindNewTarget(Expr* target);
    FunctionDecl* BindCallTarget(CallExpr* call, Expr* target);

    void NeedsHeapAlloc(Expr* expr);
    void AssignHeapOwnership(ParseNode* node);

    Expr* AnalyzeForTest(Expr* expr);

    void DeduceLiveness();
    void DeduceMaybeUsed();
    bool TestSymbol(Decl* sym, bool testconst);
    bool TestSymbols(SymbolScope* scope, bool testconst);

    void CheckVoidDecl(const typeinfo_t* type, int variable);
    void CheckVoidDecl(const declinfo_t* decl, int variable);

    bool CheckScalarType(Expr* expr);
    bool IsThisAtom(sp::Atom* atom);

    bool IsIncluded(Decl* expr);
    bool IsIncludedStock(VarDeclBase* expr);

  private:
    CompileContext& cc_;
    TypeManager* types_ = nullptr;
    tr::unordered_set<SymbolScope*> static_scopes_;
    tr::vector<FunctionDecl*> maybe_used_;
    SemaContext* sc_ = nullptr;
    bool pending_heap_allocation_ = false;
    sp::Atom* this_atom_ = nullptr;
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

class AutoCollectSemaFlow final
{
  public:
    AutoCollectSemaFlow(SemaContext& sc, ke::Maybe<bool>* out);
    ~AutoCollectSemaFlow();

  private:
    SemaContext& sc_;
    ke::Maybe<bool>* out_;
    bool old_value_;
};

void ReportFunctionReturnError(FunctionDecl* decl);
bool TestSymbols(SymbolScope* root, int testconst);
void check_void_decl(const typeinfo_t* type, int variable);
void check_void_decl(const declinfo_t* decl, int variable);
bool check_operatortag(int opertok, Type* result_type, const char* opername);
int argcompare(ArgDecl* a1, ArgDecl* a2);
void fill_arg_defvalue(CompileContext& cc, ArgDecl* decl);
bool IsLegacyEnumType(SymbolScope* scope, Type* type);

} // namespace cc
} // namespace sp
