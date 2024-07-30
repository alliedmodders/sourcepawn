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

#include <optional>
#include <unordered_set>
#include <vector>

#include "compile-context.h"
#include "ir.h"
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
        func_(nullptr),
        fun_(nullptr)
    {
        cc_prev_sc_ = cc_.sema();
        cc_.set_sema(this);
        scope_ = cc_.globals();
    }
    SemaContext(SemaContext& parent, FunctionDecl* decl)
      : cc_(parent.cc_),
        sema_(parent.sema()),
        scope_(parent.scope_),
        func_(decl),
        fun_(nullptr),
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

    void set_fun(ir::Function* fun) { fun_ = fun; }

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
    FlowType& flow_type() { return flow_type_; }

    bool warned_unreachable() const { return warned_unreachable_; }
    void set_warned_unreachable() { warned_unreachable_ = true; }

    FunctionDecl* func() const { return func_; }
    ir::Function* fun() const { return fun_; }
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
    std::unordered_map<VarDeclBase*, ir::Variable*>& local_vars() { return local_vars_; }

    uint32_t AllocTempSlot();
    void FreeTempSlot(uint32_t slot);

  private:
    CompileContext& cc_;
    Semantics* sema_ = nullptr;
    SymbolScope* scope_ = nullptr;
    AutoCreateScope* scope_creator_ = nullptr;
    FunctionDecl* func_ = nullptr;
    ir::Function* fun_ = nullptr;
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
    FlowType flow_type_ = Flow_None;
    std::unordered_set<SymbolScope*> static_scopes_;
    std::unordered_map<VarDeclBase*, ir::Variable*> local_vars_;
    std::vector<uint32_t> free_local_slots_;
    std::vector<std::vector<uint32_t>> temp_slots_;
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
    Semantics(CompileContext& cc, std::shared_ptr<ir::Module> mod);

    bool Analyze(ParseTree* tree);

    CompileContext& cc() { return cc_; }
    SymbolScope* current_scope() const;
    SemaContext* context() { return sc_; }
    void set_context(SemaContext* sc) { sc_ = sc; }

    enum class ExprFlags : unsigned int {
        DEFAULT = 0,
        RESULT_UNUSED = 0x1,
        ALLOW_TYPES = 0x2,
        WANT_RVALUE = 0x4,
        ALLOW_BOUND_FUNCTIONS = 0x8,
    };

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
    bool CheckFunctionArgument(ir::Argument* arg);
    void CheckFunctionReturnUsage(FunctionDecl* info);
    bool CheckVarDecl(VarDeclBase* decl);
    bool CheckVarDeclCommon(VarDeclBase* decl, ir::Value** init);
    bool CheckPstructDecl(VarDeclBase* decl);
    bool CheckPstructArg(VarDeclBase* decl, PstructDecl* ps, StructInitFieldExpr* field,
                         std::vector<bool>* visited);

    // Ported to IR.
    bool CheckAssertStmt(AssertStmt* stmt);
    bool CheckBreakStmt(BreakStmt* stmt);
    bool CheckCompoundReturnStmt(ReturnStmt* stmt);
    bool CheckContinueStmt(ContinueStmt* stmt);
    bool CheckDeleteStmt(DeleteStmt* stmt);
    bool CheckDoWhileStmt(DoWhileStmt* stmt);
    bool CheckExitStmt(ExitStmt* stmt);
    bool CheckExprStmt(ExprStmt* stmt);
    bool CheckForStmt(ForStmt* stmt);
    bool CheckIfStmt(IfStmt* stmt);
    bool CheckPragmaUnusedStmt(PragmaUnusedStmt* stmt);
    bool CheckReturnStmt(ReturnStmt* stmt);
    bool CheckSwitchStmt(SwitchStmt* stmt);
    bool CheckStaticAssertStmt(StaticAssertStmt* stmt);

    // Expressions.
    ir::Value* CheckExpr(Expr* expr, ExprFlags flags);
    ir::Value* CheckNewArrayExpr(NewArrayExpr* expr);
    ir::Value* CheckArrayExpr(ArrayExpr* expr);
    ir::Value* CheckStringExpr(StringExpr* expr);
    ir::Value* CheckTaggedValueExpr(TaggedValueExpr* expr);
    ir::Value* CheckNullExpr(NullExpr* expr);
    ir::Value* CheckThisExpr(ThisExpr* expr);
    ir::Value* CheckCommaExpr(CommaExpr* expr, ExprFlags flags);
    ir::Value* CheckIndexExpr(IndexExpr* expr);
    ir::Value* CheckCallExpr(CallExpr* expr);
    ir::Value* CheckSymbolExpr(SymbolExpr* expr, ExprFlags flags);
    ir::Value* CheckSizeofExpr(SizeofExpr* expr);
    ir::Value* CheckCastExpr(CastExpr* expr, ExprFlags flags);
    ir::Value* CheckIncDecExpr(IncDecExpr* expr, ExprFlags flags);
    ir::Value* CheckTernaryExpr(TernaryExpr* expr);
    ir::Value* CheckChainedCompareExpr(ChainedCompareExpr* expr);
    ir::Value* CheckLogicalExpr(LogicalExpr* expr);
    ir::Value* CheckBinaryExpr(BinaryExpr* expr);
    ir::Value* CheckUnaryExpr(UnaryExpr* expr);
    ir::Value* CheckFieldAccessExpr(FieldAccessExpr* expr, ExprFlags flags);
    ir::Value* CheckStaticFieldAccessExpr(FieldAccessExpr* expr, ir::Value* base);
    ir::Value* CheckEnumStructFieldAccessExpr(FieldAccessExpr* expr, ir::Value* base,
                                              EnumStructDecl* root, ExprFlags);
    ir::Value* CheckRvalue(Expr* expr, ExprFlags flags = ExprFlags::DEFAULT);

    // Check an ir::Value as conforming to an l/r-value, binding IR as needed.
    //
    // If |out_usage| is non-null, errors are not reported. Instead the effective
    // usage is returned and the l-value is bound as possible.
    ir::Value* BindRvalue(Expr* expr, ir::Value* val);
    ir::Lvalue* BindLvalue(ir::Value* val, uint8_t usage);
    bool BindLvalue(ir::Lvalue* val, uint8_t usage);

    // Manually build an r-value. These are infallible. Note that BuildRvalue
    // can ONLY be called if BindLvalue has been called. Failure to do so will
    // cause codegen assertions.
    ir::Value* BuildRvalue(Expr* expr, ir::Lvalue* val);
    QualType BuildRvalueType(QualType type);

    ir::FunctionRef* BuildFunctionRef(Expr* expr, ir::Function* fun);
    ir::FunctionRef* BuildFunctionRef(Expr* expr, FunctionDecl* decl);

    bool CheckAssignmentLHS(BinaryExpr* expr, ir::Lvalue* lval);
    bool CheckAssignmentRHS(BinaryExpr* expr, ir::Lvalue* lval, ir::Value* rval);
    ir::Value* BuildImplicitDynamicInitializer(VarDeclBase* decl);

    struct ParamState {
        std::vector<ir::Value*> argv;
    };

    bool CheckArrayDeclaration(VarDeclBase* decl, ir::Value** new_init);
    ir::Value* CheckNewArrayExprForArrayInitializer(NewArrayExpr* expr);
    ir::Value* CheckArgument(CallExpr* call, ArgDecl* arg, Expr* expr, ParamState* ps,
                             unsigned int argpos);
    ir::FunctionRef* BindNewTarget(Expr* target);
    ir::FunctionRef* BindCallTarget(CallExpr* call, Expr* target);

    void NeedsHeapAlloc(Expr* expr);
    void AssignHeapOwnership(ParseNode* node);

    ir::Value* AnalyzeForTest(Expr* expr);
    ir::Function* BuildFunction(FunctionDecl* decl);

    struct UserOp {
        ir::Function* target = nullptr;
        bool swapparams = false;
    };
    UserOp FindUserOp(Expr* expr, int token, QualType first, QualType second);
    ir::Value* MaybeCallUserOp(Expr* expr, int token, ir::Value* first, ir::Value* second);

    void DeduceLiveness();
    void DeduceMaybeUsed();
    bool TestSymbol(Decl* sym, bool testconst);
    bool TestSymbols(SymbolScope* scope, bool testconst);

    void CheckVoidDecl(const typeinfo_t* type, int variable);
    void CheckVoidDecl(const declinfo_t* decl, int variable);

    bool CheckScalarType(Expr* expr);
    bool CheckScalarType(Expr* expr, QualType type);

    uint32_t AllocTempSlot();

  private:
    CompileContext& cc_;
    TypeManager* types_ = nullptr;
    tr::unordered_set<SymbolScope*> static_scopes_;
    tr::vector<FunctionDecl*> maybe_used_;
    std::optional<SemaContext> global_sc_;
    SemaContext* sc_ = nullptr;
    ir::Function* fun_ = nullptr;
    std::shared_ptr<ir::Module> mod_;
    std::unordered_map<FunctionDecl*, ir::Function*> functions_;
    std::unordered_map<VarDeclBase*, ir::Variable*> global_vars_;
    bool pending_heap_allocation_ = false;
    ir::NodeListBuilder* ir_ = nullptr;
    std::vector<uint32_t>* temp_slots_ = nullptr;
};

KE_DEFINE_ENUM_OPERATORS(Semantics::ExprFlags)

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
