// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
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

#pragma once

#include <amtl/am-deque.h>
#include <amtl/am-maybe.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>

#include "expressions.h"
#include "lexer.h"
#include "pool-allocator.h"
#include "sc.h"
#include "scvars.h"
#include "shared/string-pool.h"

struct UserOperation
{
    UserOperation() {}

    symbol* sym = nullptr;
    void (*oper)() = nullptr;
    int paramspassed;
    bool savepri;
    bool savealt;
    bool swapparams;
};

typedef void (*OpFunc)();

class Expr;
class ArrayExpr;
class BinaryExpr;
class BlockStmt;
class DefaultArgExpr;
class NewArrayExpr;
class StringExpr;
class StructExpr;
class SymbolExpr;
class TaggedValueExpr;
struct StructInitField;

class CodegenContext;
class SemaContext;

class ParseNode : public PoolObject
{
  public:
    explicit ParseNode(const token_pos_t& pos)
      : pos_(pos)
    {}

    virtual bool Bind(SemaContext& sc) {
        return true;
    }
    virtual bool BindLval(SemaContext& sc) {
        return Bind(sc);
    }
    virtual bool Analyze(SemaContext& sc) = 0;
    virtual bool HasSideEffects() {
        return false;
    }

    const token_pos_t& pos() const {
        return pos_;
    }

  protected:
    void error(const token_pos_t& pos, int number, ...);

  private:
    // Hide this symbol. Calls to error(pos... will get more accurate as we
    // make adjustments.
    void error(int number, ...) = delete;

  protected:
    token_pos_t pos_;
};

enum FlowType {
    Flow_None,
    Flow_Break,
    Flow_Continue,
    Flow_Return,
    Flow_Mixed
};

class Stmt : public ParseNode
{
  public:
    explicit Stmt(const token_pos_t& pos)
      : ParseNode(pos)
    {}

    void Emit(CodegenContext& cg);

    // Process any child nodes whose value is consumed.
    virtual void ProcessUses(SemaContext& sc) = 0;

    // Return the last statement in a linear statement chain.
    virtual Stmt* GetLast() { return this; }

    // Helper to immediately bind, analyze, and emit. This will be removed once
    // the two-phase process is eliminated.
    void Process();

    virtual bool IsExprStmt() { return false; }
    virtual BlockStmt* AsBlockStmt() { return nullptr; }

    FlowType flow_type() const { return flow_type_; }
    void set_flow_type(FlowType type) { flow_type_ = type; }

    bool IsTerminal() const { return flow_type() != Flow_None; }

  private:
    virtual void DoEmit(CodegenContext& cg) = 0;

  private:
    FlowType flow_type_ = Flow_None;
};

class StmtList : public Stmt
{
  public:
    explicit StmtList(const token_pos_t& pos)
      : Stmt(pos)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;
    void ProcessUses(SemaContext& sc) override;

    Stmt* GetLast() override {
        return stmts_.empty() ? this : stmts_.back();
    }

    PoolList<Stmt*>& stmts() {
        return stmts_;
    }

  protected:
    PoolList<Stmt*> stmts_;
};

class BlockStmt : public StmtList
{
  public:
    explicit BlockStmt(const token_pos_t& pos)
      : StmtList(pos),
        scope_(nullptr)
    {}

    static BlockStmt* WrapStmt(Stmt* stmt);

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;
    BlockStmt* AsBlockStmt() override { return this; }

    symbol* scope() const { return scope_; }
    void set_scope(symbol* scope) { scope_ = scope; }

  private:
    symbol* scope_;
};

class LoopControlStmt : public Stmt
{
  public:
    explicit LoopControlStmt(const token_pos_t& pos, int token)
      : Stmt(pos),
        token_(token)
    {
        set_flow_type(token_ == tBREAK ? Flow_Break : Flow_Continue);
    }

    bool Analyze(SemaContext& sc) override { return true; }
    void DoEmit(CodegenContext& cg) override;
    void ProcessUses(SemaContext& sc) override {}

    int token() const { return token_; }

  private:
    int token_;
};

class StaticAssertStmt : public Stmt
{
  public:
    explicit StaticAssertStmt(const token_pos_t& pos, int val, PoolString* text)
      : Stmt(pos),
        val_(val),
        text_(text)
    {}

    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext&) override {}
    void ProcessUses(SemaContext& sc) override {}

  private:
    int val_;
    PoolString* text_;
};

class Decl : public Stmt
{
  public:
    Decl(const token_pos_t& pos, sp::Atom* name)
      : Stmt(pos),
        name_(name)
    {}

    bool Analyze(SemaContext& sc) override;

    // Most decls don't emit anything.
    void DoEmit(CodegenContext&) override {}

    sp::Atom* name() const {
        return name_;
    }

  protected:
    sp::Atom* name_;
};

class ErrorDecl final : public Decl
{
  public:
    ErrorDecl()
      : Decl(token_pos_t{}, nullptr)
    {}

    bool Bind(SemaContext& sc) override {
        return false;
    }
    void DoEmit(CodegenContext&) override {}
    void ProcessUses(SemaContext& sc) override {}
};

class VarDecl : public Decl
{
  public:
    VarDecl(const token_pos_t& pos, sp::Atom* name, const typeinfo_t& type, int vclass,
            bool is_public, bool is_static, bool is_stock, Expr* initializer);

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext&) override;
    void ProcessUses(SemaContext& sc) override;

    Expr* init_rhs() const;
    int vclass() const {
        return vclass_;
    }
    const typeinfo_t& type() const {
        return type_;
    }
    typeinfo_t* mutable_type() {
        return &type_;
    }
    void set_init(Expr* expr);
    bool autozero() const {
        return autozero_;
    }
    void set_no_autozero() {
        autozero_ = false;
    }
    symbol* sym() const { return sym_; }

  private:
    bool AnalyzePstruct();
    bool AnalyzePstructArg(const pstruct_t* ps, const StructInitField& field,
                           std::vector<bool>* visited);
    void EmitPstruct();
    void EmitGlobal();
    void EmitLocal();

  protected:
    typeinfo_t type_;
    int vclass_; // This will be implied by scope, when we get there.
    BinaryExpr* init_ = nullptr;
    bool is_public_ : 1;
    bool is_static_ : 1;
    bool is_stock_ : 1;
    bool autozero_ : 1;
    symbol* sym_ = nullptr;
};

class ConstDecl : public VarDecl
{
  public:
    ConstDecl(const token_pos_t& pos, sp::Atom* name, const typeinfo_t& type, int vclass,
              int tag, int value)
      : VarDecl(pos, name, type, vclass, false, false, false, nullptr),
        expr_tag_(tag),
        value_(value)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;

  private:
    int expr_tag_;
    int value_;
};

struct EnumField {
    EnumField(const token_pos_t& pos, sp::Atom* name, Expr* value)
      : pos(pos), name(name), value(value)
    {}
    token_pos_t pos;
    sp::Atom* name;
    Expr* value;
};

class EnumDecl : public Decl
{
  public:
    explicit EnumDecl(const token_pos_t& pos, int vclass, sp::Atom* label, sp::Atom* name,
                      int increment, int multiplier)
      : Decl(pos, name),
        vclass_(vclass),
        label_(label),
        increment_(increment),
        multiplier_(multiplier)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    PoolList<EnumField>& fields() {
        return fields_;
    }
    int increment() const {
        return increment_;
    }
    int multiplier() const {
        return multiplier_;
    }

  private:
    int vclass_;
    sp::Atom* label_;
    PoolList<EnumField> fields_;
    int increment_;
    int multiplier_;
};

struct StructField {
    StructField(const token_pos_t& pos, sp::Atom* name, const typeinfo_t& typeinfo)
      : pos(pos), name(name), type(typeinfo)
    {}

    token_pos_t pos;
    sp::Atom* name;
    typeinfo_t type;
};

class StructDecl : public Decl
{
  public:
    explicit StructDecl(const token_pos_t& pos, sp::Atom* name)
      : Decl(pos, name)
    {}

    void ProcessUses(SemaContext& sc) override {}

    PoolList<StructField>& fields() {
        return fields_;
    }

  protected:
    PoolList<StructField> fields_;
};

class TypedefDecl : public Decl
{
  public:
    explicit TypedefDecl(const token_pos_t& pos, sp::Atom* name, functag_t* type)
      : Decl(pos, name),
        type_(type)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

  private:
    functag_t* type_;
};

// Unsafe typeset - only supports function types. This is a transition hack for SP2.
class TypesetDecl : public Decl
{
  public:
    explicit TypesetDecl(const token_pos_t& pos, sp::Atom* name)
      : Decl(pos, name)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    PoolList<functag_t*>& types() {
        return types_;
    }

  private:
    PoolList<functag_t*> types_;
};

// "Pawn Struct", or p-struct, a hack to effect a replacement for register_plugin()
// when SourceMod was first being prototyped. Theoretically these could be retooled
// as proper structs.
class PstructDecl : public StructDecl
{
  public:
    explicit PstructDecl(const token_pos_t& pos, sp::Atom* name)
      : StructDecl(pos, name)
    {}

    bool Bind(SemaContext& sc) override;
};

class UsingDecl : public Decl
{
  public:
    explicit UsingDecl(const token_pos_t& pos)
      : Decl(pos, nullptr)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}
};

class Expr : public ParseNode
{
  public:
    explicit Expr(const token_pos_t& pos)
      : ParseNode(pos)
    {}

    // Flatten a series of binary expressions into a single list.
    virtual void FlattenLogical(int token, std::vector<Expr*>* out);

    // Fold the expression into a constant. The expression must have been
    // bound and analyzed. False indicates the expression is non-constant.
    //
    // If an expression folds constants during analysis, it can return false
    // here. ExprToConst handles both cases.
    virtual bool FoldToConstant() {
        return false;
    }

    // Process any child nodes whose value is consumed.
    virtual void ProcessUses(SemaContext& sc) = 0;
    // Process any child nodes whose value is not consumed.
    virtual void ProcessDiscardUses(SemaContext& sc) { ProcessUses(sc); }

    // Evaluate as a constant. Returns false if non-const. This is a wrapper
    // around FoldToConstant().
    bool EvalConst(cell* value, int* tag);

    virtual void EmitTest(bool jump_on_true, int target);
    virtual symbol* BindCallTarget(SemaContext& sc, int token, Expr** implicit_this) {
        return nullptr;
    }
    virtual symbol* BindNewTarget(SemaContext& sc) {
        return nullptr;
    }

    // Mark the node's value as consumed.
    virtual void MarkUsed(SemaContext&) {}

    virtual bool AnalyzeForInitializer(SemaContext& sc) {
        return Analyze(sc);
    }
    virtual Expr* AnalyzeForTest(SemaContext& sc);

    void Emit();

    const value& val() const {
        return val_;
    }
    bool lvalue() const {
        return lvalue_;
    }

    void MarkAndProcessUses(SemaContext& sc) {
        MarkUsed(sc);
        ProcessUses(sc);
    }

    // Casts.
    virtual ArrayExpr* AsArrayExpr() { return nullptr; }
    virtual BinaryExpr* AsBinaryExpr() { return nullptr; }
    virtual DefaultArgExpr* AsDefaultArgExpr() {  return nullptr; }
    virtual SymbolExpr* AsSymbolExpr() { return nullptr; }
    virtual StructExpr* AsStructExpr() { return nullptr; }
    virtual StringExpr* AsStringExpr() { return nullptr; }
    virtual TaggedValueExpr* AsTaggedValueExpr() { return nullptr; }
    virtual NewArrayExpr* AsNewArrayExpr() { return nullptr; }

  protected:
    virtual void DoEmit() = 0;

  protected:
    value val_ = {};
    bool lvalue_ = 0;
};

class ErrorExpr final : public Expr
{
  public:
    ErrorExpr() : Expr(token_pos_t{})
    {}

    // Errors never bind.
    bool Bind(SemaContext& sc) override {
        return false;
    }
    bool Analyze(SemaContext& sc) override {
        return false;
    }
    void DoEmit() override {}
    void ProcessUses(SemaContext&) override {}
};

class IsDefinedExpr final : public Expr
{
  public:
    IsDefinedExpr(const token_pos_t& pos, sp::Atom* name)
      : Expr(pos),
        name_(name)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext&) override {}

  private:
    cell value_ = 0;
    sp::Atom* name_;
};

class UnaryExpr final : public Expr
{
  public:
    UnaryExpr(const token_pos_t& pos, int token, Expr* expr)
      : Expr(pos),
        token_(token),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        return expr_->Bind(sc);
    }
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void EmitTest(bool jump_on_true, int target) override;
    bool HasSideEffects() override;
    void ProcessUses(SemaContext& sc) override;

  private:
    int token_;
    Expr* expr_;
    bool userop_ = false;
};

class BinaryExprBase : public Expr
{
  public:
    BinaryExprBase(const token_pos_t& pos, int token, Expr* left, Expr* right);

    bool Bind(SemaContext& sc) override;
    bool HasSideEffects() override;
    void ProcessUses(SemaContext& sc) override;

    int token() const {
        return token_;
    }
    Expr* left() const {
        return left_;
    }
    Expr* right() const {
        return right_;
    }

  protected:
    int token_;
    Expr* left_;
    Expr* right_;
};

class BinaryExpr final : public BinaryExprBase
{
  public:
    BinaryExpr(const token_pos_t& pos, int token, Expr* left, Expr* right);

    bool HasSideEffects() override;
    bool Analyze(SemaContext& sc) override;
    bool FoldToConstant() override;
    void DoEmit() override;

    BinaryExpr* AsBinaryExpr() override {
        return this;
    }
    OpFunc oper() const {
        return oper_;
    }
    const UserOperation& userop() const {
        return userop_;
    }
    void set_initializer() {
        initializer_ = true;
    }

    static void EmitInner(OpFunc oper, const UserOperation& userop, Expr* left, Expr* right);

  private:
    bool ValidateAssignmentLHS();
    bool ValidateAssignmentRHS();

  private:
    UserOperation userop_;
    UserOperation assignop_;
    OpFunc oper_ = nullptr;
    cell array_copy_length_ = 0;
    bool initializer_ = false;
};

class LogicalExpr final : public BinaryExprBase
{
  public:
    LogicalExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
      : BinaryExprBase(pos, token, left, right)
    {}

    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void EmitTest(bool jump_on_true, int target) override;
    void FlattenLogical(int token, std::vector<Expr*>* out) override;
};

struct CompareOp
{
    CompareOp(const token_pos_t& pos, int token, Expr* expr);

    token_pos_t pos;
    int token;
    Expr* expr;
    OpFunc oper;
    UserOperation userop = {};
};

class ChainedCompareExpr final : public Expr
{
  public:
    explicit ChainedCompareExpr(const token_pos_t& pos, Expr* first)
      : Expr(pos),
        first_(first)
    {}

    PoolList<CompareOp>& ops() {
        return ops_;
    }

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    bool HasSideEffects() override;
    void ProcessUses(SemaContext& sc) override;
    void EmitTest(bool jump_on_true, int target) override;

  private:
    Expr* first_;
    PoolList<CompareOp> ops_;
};

class TernaryExpr final : public Expr
{
  public:
    TernaryExpr(const token_pos_t& pos, Expr* first, Expr* second, Expr* third)
      : Expr(pos),
        first_(first),
        second_(second),
        third_(third)
    {}

    bool Bind(SemaContext& sc) override {
        bool ok = first_->Bind(sc);
        ok &= second_->Bind(sc);
        ok &= third_->Bind(sc);
        return ok;
    }
    bool Analyze(SemaContext& sc) override;
    bool FoldToConstant() override;
    void DoEmit() override;
    bool HasSideEffects() override {
        return first_->HasSideEffects() || second_->HasSideEffects() || third_->HasSideEffects();
    }
    void ProcessUses(SemaContext& sc) override;
    void ProcessDiscardUses(SemaContext& sc) override;

  private:
    void EmitImpl(ke::Maybe<cell>* branch1, ke::Maybe<cell>* branch2);

  private:
    Expr* first_;
    Expr* second_;
    Expr* third_;
};

class IncDecExpr : public Expr
{
  public:
    IncDecExpr(const token_pos_t& pos, int token, Expr* expr)
      : Expr(pos),
        token_(token),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        return expr_->Bind(sc);
    }
    bool Analyze(SemaContext& sc) override;
    bool HasSideEffects() override {
        return true;
    }
    void ProcessUses(SemaContext& sc) override;

  protected:
    int token_;
    Expr* expr_;
    UserOperation userop_;
};

class PreIncExpr final : public IncDecExpr
{
  public:
    PreIncExpr(const token_pos_t& pos, int token, Expr* expr)
      : IncDecExpr(pos, token, expr)
    {}

    void DoEmit() override;
};

class PostIncExpr final : public IncDecExpr
{
  public:
    PostIncExpr(const token_pos_t& pos, int token, Expr* expr)
      : IncDecExpr(pos, token, expr)
    {}

    void DoEmit() override;
};

class CastExpr final : public Expr
{
  public:
    CastExpr(const token_pos_t& pos, int token, int tag, Expr* expr)
      : Expr(pos),
        token_(token),
        tag_(tag),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        return expr_->Bind(sc);
    }
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    bool HasSideEffects() override {
        return expr_->HasSideEffects();
    }
    void ProcessUses(SemaContext& sc) override;

  private:
    int token_;
    int tag_;
    Expr* expr_;
};

class SizeofExpr final : public Expr
{
  public:
    SizeofExpr(const token_pos_t& pos, sp::Atom* ident, sp::Atom* field, int suffix_token, int array_levels)
      : Expr(pos),
        ident_(ident),
        field_(field),
        suffix_token_(suffix_token),
        array_levels_(array_levels)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext& sc) override {}

  private:
    sp::Atom* ident_;
    sp::Atom* field_;
    int suffix_token_;
    int array_levels_;
    symbol* sym_ = nullptr;
};

class SymbolExpr final : public Expr
{
  public:
    SymbolExpr(const token_pos_t& pos, sp::Atom* name)
      : Expr(pos),
        name_(name),
        sym_(nullptr)
    {
    }

    bool Bind(SemaContext& sc) override;
    bool BindLval(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void MarkUsed(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}
    symbol* BindCallTarget(SemaContext& sc, int token, Expr** implicit_this) override;
    symbol* BindNewTarget(SemaContext& sc) override;
    SymbolExpr* AsSymbolExpr() override {
        return this;
    }

    bool AnalyzeWithOptions(SemaContext& sc, bool allow_types);

    symbol* sym() const {
        return sym_;
    }

  private:
    bool DoBind(SemaContext& sc, bool is_lval);

  private:
    sp::Atom* name_;
    symbol* sym_;
};

struct ParsedArg {
    ParsedArg()
      : name(nullptr),
        expr(nullptr)
    {}
    ParsedArg(sp::Atom* name, Expr* expr)
      : name(name),
        expr(expr)
    {}

    sp::Atom* name;
    Expr* expr;
};

struct ComputedArg {
    Expr* expr = nullptr;
    arginfo* arg = nullptr;
};

class CallExpr final : public Expr
{
  public:
    CallExpr(const token_pos_t& pos, int token, Expr* target)
      : Expr(pos),
        token_(token),
        target_(target)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void MarkUsed(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    bool HasSideEffects() override {
        return true;
    }

    PoolList<ParsedArg>& args() {
        return args_;
    }

  private:
    bool ProcessArg(arginfo* arg, Expr* param, unsigned int pos);

    int token_;
    Expr* target_;
    PoolList<ParsedArg> args_;
    symbol* sym_ = nullptr;
    Expr* implicit_this_ = nullptr;
    PoolList<ComputedArg> argv_;
};

class EmitOnlyExpr : public Expr
{
  public:
    explicit EmitOnlyExpr(const token_pos_t& pos)
      : Expr(pos)
    {}

    bool Bind(SemaContext& sc) override {
        assert(false);
        return true;
    }
    bool Analyze(SemaContext& sc) override {
        assert(false);
        return true;
    }
};

class CallUserOpExpr final : public EmitOnlyExpr
{
  public:
    CallUserOpExpr(const UserOperation& userop, Expr* expr);

    bool HasSideEffects() override {
        return true;
    }
    void DoEmit() override;
    void ProcessUses(SemaContext& sc) override;

  private:
    UserOperation userop_;
    Expr* expr_;
};

class DefaultArgExpr final : public EmitOnlyExpr
{
  public:
    DefaultArgExpr(const token_pos_t& pos, arginfo* arg);

    DefaultArgExpr* AsDefaultArgExpr() override {
        return this;
    }
    void DoEmit() override;
    void ProcessUses(SemaContext& sc) override {}

  private:
    arginfo* arg_;
};

class FieldAccessExpr final : public Expr
{
  public:
    FieldAccessExpr(const token_pos_t& pos, int tok, Expr* base, sp::Atom* name)
      : Expr(pos),
        token_(tok),
        base_(base),
        name_(name)
    {}

    bool Bind(SemaContext& sc) override {
        return base_->Bind(sc);
    }
    symbol* BindCallTarget(SemaContext& sc, int token, Expr** implicit_this) override;
    bool Analyze(SemaContext& sc) override;
    bool HasSideEffects() override;
    void DoEmit() override;
    void ProcessUses(SemaContext& sc) override;

    bool AnalyzeWithOptions(SemaContext& sc, bool from_call);

  private:
    bool AnalyzeStaticAccess();
    bool AnalyzeEnumStructAccess(Type* type, symbol* root, bool from_call);

  private:
    int token_;
    Expr* base_;
    sp::Atom* name_;
    methodmap_method_t* method_ = nullptr;
    symbol* field_ = nullptr;
};

class IndexExpr final : public Expr
{
  public:
    IndexExpr(const token_pos_t& pos, Expr* base, Expr* expr)
      : Expr(pos),
        base_(base),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        bool ok = base_->Bind(sc);
        ok &= expr_->Bind(sc);
        return ok;
    }
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext& sc) override;
    bool HasSideEffects() override {
        return base_->HasSideEffects() || expr_->HasSideEffects();
    }

  private:
    Expr* base_;
    Expr* expr_;
};

class RvalueExpr final : public EmitOnlyExpr
{
  public:
    explicit RvalueExpr(Expr* expr);

    void DoEmit() override;
    void ProcessUses(SemaContext& sc) override;

  private:
    Expr* expr_;
};

class CommaExpr final : public Expr
{
  public:
    explicit CommaExpr(const token_pos_t& pos)
      : Expr(pos)
    {}

    PoolList<Expr*>& exprs() {
        return exprs_;
    }

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void EmitTest(bool jump_on_true, int target) override;
    void ProcessUses(SemaContext& sc) override;
    void ProcessDiscardUses(SemaContext& sc) override;
    bool HasSideEffects() override {
        return has_side_effects_;
    }

  private:
    PoolList<Expr*> exprs_;
    bool has_side_effects_ = false;
};

class ThisExpr final : public Expr
{
  public:
    explicit ThisExpr(const token_pos_t& pos)
      : Expr(pos),
        sym_(nullptr)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext&) override {}

  private:
    symbol* sym_;
};

class NullExpr final : public Expr
{
  public:
    explicit NullExpr(const token_pos_t& pos)
      : Expr(pos)
    {}

    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext&) override {}
};


class TaggedValueExpr : public Expr
{
  public:
    TaggedValueExpr(const token_pos_t& pos, int tag, cell value)
      : Expr(pos),
        tag_(tag),
        value_(value)
    {}

    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext&) override {}
    TaggedValueExpr* AsTaggedValueExpr() override {
        return this;
    }

    int tag() const {
        return tag_;
    }
    cell value() const {
        return value_;
    }

  protected:
    int tag_;
    cell value_;
};

class NumberExpr final : public TaggedValueExpr
{
  public:
    NumberExpr(const token_pos_t& pos, cell value)
      : TaggedValueExpr(pos, 0, value)
    {}
};

class FloatExpr final : public TaggedValueExpr
{
  public:
    FloatExpr(const token_pos_t& pos, cell value)
      : TaggedValueExpr(pos, sc_rationaltag, value)
    {}
};

class StringExpr final : public Expr
{
  public:
    StringExpr(const token_pos_t& pos, const char* str, size_t len)
      : Expr(pos),
        text_(new PoolString(str, len))
    {}

    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext&) override {}
    StringExpr* AsStringExpr() override {
        return this;
    }

    PoolString* text() const {
        return text_;
    }

  private:
    PoolString* text_;
};

class NewArrayExpr final : public Expr
{
  public:
    explicit NewArrayExpr(const token_pos_t& pos, int tag)
      : Expr(pos),
        tag_(tag)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    bool AnalyzeForInitializer(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext& sc) override;

    NewArrayExpr* AsNewArrayExpr() override {
        return this;
    }

    int tag() {
        return tag_;
    }
    PoolList<Expr*>& exprs() {
        return exprs_;
    }
    void set_no_autozero() {
        autozero_ = false;
    }
    void set_already_analyzed() {
        already_analyzed_ = true;
    }

  private:
    int tag_;
    PoolList<Expr*> exprs_;
    bool autozero_ = true;
    bool already_analyzed_ = false;
};

class ArrayExpr final : public Expr
{
  public:
    explicit ArrayExpr(const token_pos_t& pos)
      : Expr(pos)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit() override;
    void ProcessUses(SemaContext&) override {}

    ArrayExpr* AsArrayExpr() override {
        return this;
    }

    PoolList<Expr*>& exprs() {
        return exprs_;
    }
    bool ellipses() const {
        return ellipses_;
    }
    void set_ellipses() {
        ellipses_ = true;
    }

  private:
    bool ellipses_ = false;
    PoolList<Expr*> exprs_;
};

struct StructInitField {
    StructInitField(sp::Atom* name, Expr* value)
        : name(name), value(value)
    {}
    sp::Atom* name;
    Expr* value;
};

class StructExpr final : public Expr
{
  public:
    explicit StructExpr(const token_pos_t& pos)
        : Expr(pos)
    {}

    bool Analyze(SemaContext& sc) override {
        return true;
    }
    void ProcessUses(SemaContext&) override {}
    void DoEmit() override {
        assert(false);
    }
    StructExpr* AsStructExpr() override {
        return this;
    }

    PoolList<StructInitField>& fields() {
        return fields_;
    }

  private:
    PoolList<StructInitField> fields_;
};

class IfStmt : public Stmt
{
  public:
    explicit IfStmt(const token_pos_t& pos, Expr* cond, Stmt* on_true, Stmt* on_false)
      : Stmt(pos),
        cond_(cond),
        on_true_(on_true),
        on_false_(on_false)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;
    void ProcessUses(SemaContext& sc) override;

  private:
    Expr* cond_;
    Stmt* on_true_;
    Stmt* on_false_;
};

class ExprStmt : public Stmt
{
  public:
    ExprStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }
    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;
    bool IsExprStmt() override { return true; }

    void ProcessUses(SemaContext& sc) override {
        expr_->ProcessDiscardUses(sc);
    }

  private:
    Expr* expr_;
};

class ReturnStmt : public Stmt
{
  public:
    explicit ReturnStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(pos),
        expr_(expr)
    {
        set_flow_type(Flow_Return);
    }

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

  private:
    bool CheckArrayReturn(SemaContext& sc);
    void EmitArrayReturn(CodegenContext& cg);

  private:
    Expr* expr_;
    typeinfo_t array_;
};

class AssertStmt : public Stmt
{
  public:
    explicit AssertStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }
    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

    void ProcessUses(SemaContext& sc) override {
        expr_->MarkAndProcessUses(sc);
    }

  private:
    Expr* expr_;
};

class DeleteStmt : public Stmt
{
  public:
    explicit DeleteStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }
    bool Analyze(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

    void ProcessUses(SemaContext& sc) override {
        expr_->MarkAndProcessUses(sc);
    }

  private:
    Expr* expr_;
    methodmap_t* map_;
};

class ExitStmt : public Stmt
{
  public:
    explicit ExitStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

  private:
    Expr* expr_;
};

class DoWhileStmt : public Stmt
{
  public:
    explicit DoWhileStmt(const token_pos_t& pos, int token, Expr* cond, Stmt* body)
      : Stmt(pos),
        token_(token),
        cond_(cond),
        body_(body)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

  private:
    int token_;
    Expr* cond_;
    Stmt* body_;
    bool always_taken_ = false;
    bool never_taken_ = false;
};

class ForStmt : public Stmt
{
  public:
    explicit ForStmt(const token_pos_t& pos, Stmt* init, Expr* cond, Expr* advance, Stmt* body)
      : Stmt(pos),
        scope_(nullptr),
        init_(init),
        cond_(cond),
        advance_(advance),
        body_(body)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

  private:
    symbol* scope_;
    Stmt* init_;
    Expr* cond_;
    Expr* advance_;
    Stmt* body_;
    bool always_taken_ = false;
    bool never_taken_ = false;
    bool has_continue_ = false;
};

class SwitchStmt : public Stmt
{
  public:
    explicit SwitchStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(pos),
        expr_(expr),
        default_case_(nullptr)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

    void AddCase(PoolList<Expr*>&& exprs, Stmt* stmt) {
        cases_.emplace_back(std::move(exprs), stmt);
    }

    Stmt* default_case() const { return default_case_; }
    void set_default_case(Stmt* stmt) { default_case_ = stmt; }

  private:
    Expr* expr_;
    Stmt* default_case_;

    typedef std::pair<PoolList<Expr*>, Stmt*> Case;

    PoolList<Case> cases_;
};

class PragmaUnusedStmt : public Stmt
{
  public:
    PragmaUnusedStmt(const token_pos_t& pos)
      : Stmt(pos)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}
    void DoEmit(CodegenContext& cg) override {}

    PoolList<sp::Atom*>& names() { return names_; }

  private:
    PoolList<sp::Atom*> names_;
    PoolList<symbol*> symbols_;
};

struct FunctionArg {
    VarDecl* decl;
};

class FunctionInfo : public PoolObject
{
  public:
    explicit FunctionInfo(const token_pos_t& pos, const declinfo_t& decl);

    void AddArg(VarDecl* arg);
    bool IsVariadic() const;

    bool Bind(SemaContext& sc);
    bool Analyze(SemaContext& sc);
    void ProcessUses(SemaContext& sc);
    void Emit(CodegenContext& cg);

    void set_is_public() { is_public_ = true; }
    void set_is_static() { is_static_ = true; }
    void set_is_stock() { is_stock_ = true; }
    void set_is_forward() { is_forward_ = true; }
    void set_end_pos(const token_pos_t& end_pos) { end_pos_ = end_pos; }
    void set_alias(sp::Atom* alias) { alias_ = alias; }

    const ke::Maybe<int>& this_tag() const { return this_tag_; }
    void set_this_tag(int this_tag) {
        if (this_tag != -1)
            this_tag_.init(this_tag);
    }

    Stmt* body() const { return body_; }
    void set_body(Stmt* body) { body_ = body; }

    sp::Atom* name() const { return name_; }
    void set_name(sp::Atom* name) { name_ = name; }

    void set_is_native() { is_native_ = true; }
    bool is_native() const { return is_native_; }

    PoolList<FunctionArg>& args() { return args_; }
    const declinfo_t& decl() const { return decl_; }
    symbol* sym() const { return sym_; }
    const token_pos_t& pos() const { return pos_; }

  private:
    bool AnalyzeArgs(SemaContext& sc);

    sp::Atom* NameForOperator();

  private:
    token_pos_t pos_;
    token_pos_t end_pos_;
    declinfo_t decl_;
    sp::Atom* name_ = nullptr;
    bool is_public_ = false;
    bool is_static_ = false;
    bool is_stock_ = false;
    bool is_forward_ = false;
    bool is_native_ = false;
    Stmt* body_ = nullptr;
    PoolList<FunctionArg> args_;
    symbol* sym_ = nullptr;
    symbol* scope_ = nullptr;
    ke::Maybe<int> this_tag_;
    sp::Atom* alias_ = nullptr;
};

class FunctionDecl : public Decl
{
  public:
    explicit FunctionDecl(const token_pos_t& pos, FunctionInfo* info)
      : Decl(pos, info->name()),
        info_(info)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

    void set_deprecate(const std::string& deprecate) { deprecate_ = new PoolString(deprecate); }

    FunctionInfo* info() const { return info_; }
    symbol* sym() const { return info_->sym(); }

  private:
    FunctionInfo* info_;
    PoolString* deprecate_;
};

struct EnumStructField {
    token_pos_t pos;
    declinfo_t decl;
};

class EnumStructDecl : public Decl
{
  public:
    explicit EnumStructDecl(const token_pos_t& pos, sp::Atom* name)
      : Decl(pos, name)
    {}

    bool Bind(SemaContext& sc) override;
    bool Analyze(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void DoEmit(CodegenContext& cg) override;

    PoolList<FunctionDecl*>& methods() { return methods_; }
    PoolList<EnumStructField>& fields() { return fields_; }

  private:
    sp::Atom* DecorateInnerName(const token_pos_t& pos, sp::Atom* field_name);

  private:
    PoolList<FunctionDecl*> methods_;
    PoolList<EnumStructField> fields_;
};
