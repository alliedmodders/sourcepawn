// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
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
#pragma once

#include <amtl/am-deque.h>
#include <amtl/am-maybe.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>

#include "ast-types.h"
#include "expressions.h"
#include "lexer.h"
#include "pool-allocator.h"
#include "sc.h"
#include "scvars.h"
#include "shared/string-pool.h"
#include "symbols.h"

struct UserOperation
{
    UserOperation() {}

    symbol* sym = nullptr;
    int oper = 0;
    int paramspassed;
    bool savepri;
    bool savealt;
    bool swapparams;
};

typedef void (*OpFunc)();

class Expr;
class SymbolScope;
struct StructInitField;
struct structarg_t;

class SemaContext;

class ParseNode : public PoolObject
{
  public:
    explicit ParseNode(AstKind kind, const token_pos_t& pos)
      : kind_(kind),
        pos_(pos),
        tree_has_heap_allocs_(false)
    {}

    virtual bool Bind(SemaContext& sc) {
        return true;
    }
    virtual bool BindLval(SemaContext& sc) {
        return Bind(sc);
    }
    virtual bool HasSideEffects() {
        return false;
    }

    const token_pos_t& pos() const {
        return pos_;
    }

    AstKind kind() const { return kind_; }
    bool is(AstKind k) const { return kind() == k; }

    bool tree_has_heap_allocs() const { return tree_has_heap_allocs_; }
    void set_tree_has_heap_allocs(bool b) { tree_has_heap_allocs_ = b; }

    template <class T> T* as() {
        if (T::is_a(this))
            return reinterpret_cast<T*>(this);
        return nullptr;
    }
    template <class T> T* to() {
        assert(T::is_a(this));
        return reinterpret_cast<T*>(this);
    }

  protected:
    void error(const token_pos_t& pos, int number, ...);

  private:
    // Hide this symbol. Calls to error(pos... will get more accurate as we
    // make adjustments.
    void error(int number, ...) = delete;

  protected:
    AstKind kind_;
    token_pos_t pos_;
    bool tree_has_heap_allocs_ : 1;
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
    explicit Stmt(AstKind kind, const token_pos_t& pos)
      : ParseNode(kind, pos)
    {}

    // Create symbolic information for any names global to the current name
    // context.
    virtual bool EnterNames(SemaContext& sc) { return true; }

    // Process any child nodes whose value is consumed.
    virtual void ProcessUses(SemaContext& sc) = 0;

    // Return the last statement in a linear statement chain.
    virtual Stmt* GetLast() { return this; }

    FlowType flow_type() const { return flow_type_; }
    void set_flow_type(FlowType type) { flow_type_ = type; }

    bool IsTerminal() const { return flow_type() != Flow_None; }

  private:
    AstKind kind_;
    FlowType flow_type_ = Flow_None;
};

class ChangeScopeNode : public Stmt
{
  public:
    explicit ChangeScopeNode(const token_pos_t& pos, SymbolScope* scope, const std::string& file)
      : Stmt(AstKind::ChangeScopeNode, pos),
        scope_(scope),
        file_(new PoolString(file))
    {}

    virtual bool EnterNames(SemaContext& sc) override;
    virtual bool Bind(SemaContext& sc) override;
    virtual void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ChangeScopeNode; }

    SymbolScope* scope() const { return scope_; }
    PoolString* file() const { return file_; }

  private:
    SymbolScope* scope_;
    PoolString* file_;
};

class StmtList : public Stmt
{
  public:
    explicit StmtList(AstKind kind, const token_pos_t& pos, const std::vector<Stmt*>& stmts)
      : Stmt(kind, pos),
        stmts_(stmts)
    {}
    explicit StmtList(const token_pos_t& pos, const std::vector<Stmt*>& stmts)
      : Stmt(AstKind::StmtList, pos),
        stmts_(stmts)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    Stmt* GetLast() override {
        return stmts_.empty() ? this : stmts_.back();
    }

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::StmtList; }

    PoolArray<Stmt*>& stmts() {
        return stmts_;
    }

  protected:
    PoolArray<Stmt*> stmts_;
};

class ParseTree : public StmtList
{
  public:
    explicit ParseTree(const token_pos_t& pos, const std::vector<Stmt*>& stmts)
      : StmtList(AstKind::ParseTree, pos, stmts)
    {}

    bool ResolveNames(SemaContext& sc);

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ParseTree; }
};

class BlockStmt : public StmtList
{
  public:
    explicit BlockStmt(const token_pos_t& pos, const std::vector<Stmt*>& stmts)
      : StmtList(AstKind::BlockStmt, pos, stmts),
        scope_(nullptr)
    {}

    static BlockStmt* WrapStmt(Stmt* stmt);

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::BlockStmt; }

    bool Bind(SemaContext& sc) override;

    SymbolScope* scope() const { return scope_; }
    void set_scope(SymbolScope* scope) { scope_ = scope; }

  private:
    SymbolScope* scope_;
};

class BreakStmt : public Stmt
{
  public:
    explicit BreakStmt(const token_pos_t& pos)
      : Stmt(AstKind::BreakStmt, pos)
    {
        set_flow_type(Flow_Break);
    }

    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::BreakStmt; }
};

class ContinueStmt : public Stmt
{
  public:
    explicit ContinueStmt(const token_pos_t& pos)
      : Stmt(AstKind::ContinueStmt, pos)
    {
        set_flow_type(Flow_Continue);
    }

    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ContinueStmt; }
};

class StaticAssertStmt : public Stmt
{
  public:
    explicit StaticAssertStmt(const token_pos_t& pos, Expr* expr, PoolString* text)
      : Stmt(AstKind::StaticAssertStmt, pos),
        expr_(expr),
        text_(text)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::StaticAssertStmt; }

    Expr* expr() const { return expr_; }
    PoolString* text() const { return text_; }

  private:
    Expr* expr_;
    PoolString* text_;
};

class Decl : public Stmt
{
  public:
    Decl(AstKind kind, const token_pos_t& pos, sp::Atom* name)
      : Stmt(kind, pos),
        name_(name)
    {}

    sp::Atom* name() const {
        return name_;
    }

  protected:
    sp::Atom* DecorateInnerName(sp::Atom* parent_name, sp::Atom* field_name);

  protected:
    sp::Atom* name_;
};

class BinaryExpr;

class VarDecl : public Decl
{
  public:
    VarDecl(const token_pos_t& pos, sp::Atom* name, const typeinfo_t& type, int vclass,
            bool is_public, bool is_static, bool is_stock, Expr* initializer);

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::VarDecl; }

    // Bind only the typeinfo.
    bool BindType(SemaContext& sc);

    BinaryExpr* init() const { return init_; }
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
    bool autozero() const { return autozero_; }
    void set_no_autozero() { autozero_ = false; }
    symbol* sym() const { return sym_; }

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
              Expr* expr)
      : VarDecl(pos, name, type, vclass, false, false, false, nullptr),
        expr_(expr)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

  private:
    Expr* expr_;
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
      : Decl(AstKind::EnumDecl, pos, name),
        vclass_(vclass),
        label_(label),
        increment_(increment),
        multiplier_(multiplier)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::EnumDecl; }

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
      : pos(pos), name(name), type(typeinfo), field(nullptr)
    {}

    token_pos_t pos;
    sp::Atom* name;
    typeinfo_t type;
    structarg_t* field;
};

// "Pawn Struct", or p-struct, a hack to effect a replacement for register_plugin()
// when SourceMod was first being prototyped. Theoretically these could be retooled
// as proper structs.
class PstructDecl : public Decl
{
  public:
    PstructDecl(const token_pos_t& pos, sp::Atom* name)
      : Decl(AstKind::PstructDecl, pos, name),
        ps_(nullptr)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::PstructDecl; }

    PoolList<StructField>& fields() {
        return fields_;
    }

  protected:
    pstruct_t* ps_;
    PoolList<StructField> fields_;
};

struct TypedefInfo : public PoolObject {
    token_pos_t pos;
    TypenameInfo ret_type;
    PoolList<declinfo_t*> args;

    functag_t* Bind(SemaContext& sc);
};

class TypedefDecl : public Decl
{
  public:
    explicit TypedefDecl(const token_pos_t& pos, sp::Atom* name, TypedefInfo* type)
      : Decl(AstKind::TypedefDecl, pos, name),
        type_(type)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::TypedefDecl; }

  private:
    TypedefInfo* type_;
    funcenum_t* fe_ = nullptr;
};

// Unsafe typeset - only supports function types. This is a transition hack for SP2.
class TypesetDecl : public Decl
{
  public:
    explicit TypesetDecl(const token_pos_t& pos, sp::Atom* name)
      : Decl(AstKind::TypesetDecl, pos, name)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::TypesetDecl; }

    PoolList<TypedefInfo*>& types() {
        return types_;
    }

  private:
    PoolList<TypedefInfo*> types_;
    funcenum_t* fe_ = nullptr;
};

class UsingDecl : public Decl
{
  public:
    explicit UsingDecl(const token_pos_t& pos)
      : Decl(AstKind::UsingDecl, pos, nullptr)
    {}

    bool EnterNames(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::UsingDecl; }
};

class Expr : public ParseNode
{
  public:
    explicit Expr(AstKind kind, const token_pos_t& pos)
      : ParseNode(kind, pos),
        lvalue_(false),
        can_alloc_heap_(false)
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

    // Mark the node's value as consumed.
    virtual void MarkUsed(SemaContext&) {}

    value& val() { return val_; }
    const value& val() const { return val_; }
    bool lvalue() const { return lvalue_; }
    void set_lvalue(bool lvalue) { lvalue_ = lvalue; }
    bool can_alloc_heap() const { return can_alloc_heap_; }
    void set_can_alloc_heap(bool b) { can_alloc_heap_ = b; }

    void MarkAndProcessUses(SemaContext& sc) {
        MarkUsed(sc);
        ProcessUses(sc);
    }

  protected:
    value val_ = {};
    bool lvalue_ : 1;
    bool can_alloc_heap_ : 1;
};

class IsDefinedExpr final : public Expr
{
  public:
    IsDefinedExpr(const token_pos_t& pos, sp::Atom* name)
      : Expr(AstKind::IsDefinedExpr, pos),
        name_(name)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::IsDefinedExpr; }

    cell value() const { return value_; }

  private:
    cell value_ = 0;
    sp::Atom* name_;
};

class UnaryExpr final : public Expr
{
  public:
    UnaryExpr(const token_pos_t& pos, int token, Expr* expr)
      : Expr(AstKind::UnaryExpr, pos),
        token_(token),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        return expr_->Bind(sc);
    }
    bool HasSideEffects() override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::UnaryExpr; }

    int token() const { return token_; }
    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }
    bool userop() const { return userop_; }
    void set_userop() { userop_ = true; }

  private:
    int token_;
    Expr* expr_;
    bool userop_ = false;
};

class BinaryExprBase : public Expr
{
  public:
    BinaryExprBase(AstKind kind, const token_pos_t& pos, int token, Expr* left, Expr* right);

    bool Bind(SemaContext& sc) override;
    bool HasSideEffects() override;
    void ProcessUses(SemaContext& sc) override;

    int token() const { return token_; }
    Expr* left() const { return left_; }
    Expr* set_left(Expr* left) { return left_ = left; }
    Expr* right() const { return right_; }
    Expr* set_right(Expr* right) { return right_ = right; }

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
    bool FoldToConstant() override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::BinaryExpr; }

    int oper() const { return oper_tok_; }
    UserOperation& userop() { return userop_; }
    UserOperation& assignop() { return assignop_; }
    void set_initializer() { initializer_ = true; }
    cell array_copy_length() const { return array_copy_length_; }
    void set_array_copy_length(cell len) { array_copy_length_ = len; }
    bool initializer() const { return initializer_; }

  private:
    bool ValidateAssignmentLHS();
    bool ValidateAssignmentRHS(SemaContext& sc);

  private:
    UserOperation userop_;
    UserOperation assignop_;
    cell array_copy_length_ = 0;
    bool initializer_ = false;
    int oper_tok_ = 0;
};

class LogicalExpr final : public BinaryExprBase
{
  public:
    LogicalExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
      : BinaryExprBase(AstKind::LogicalExpr, pos, token, left, right)
    {}

    void FlattenLogical(int token, std::vector<Expr*>* out) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::LogicalExpr; }
};

struct CompareOp
{
    CompareOp(const token_pos_t& pos, int token, Expr* expr);

    token_pos_t pos;
    int token;
    Expr* expr;
    int oper_tok;
    UserOperation userop = {};
};

class ChainedCompareExpr final : public Expr
{
  public:
    explicit ChainedCompareExpr(const token_pos_t& pos, Expr* first)
      : Expr(AstKind::ChainedCompareExpr, pos),
        first_(first)
    {}

    bool Bind(SemaContext& sc) override;
    bool HasSideEffects() override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ChainedCompareExpr; }

    Expr* first() const { return first_; }
    Expr* set_first(Expr* first) { return first_ = first; }
    PoolList<CompareOp>& ops() { return ops_; }

  private:
    Expr* first_;
    PoolList<CompareOp> ops_;
};

class TernaryExpr final : public Expr
{
  public:
    TernaryExpr(const token_pos_t& pos, Expr* first, Expr* second, Expr* third)
      : Expr(AstKind::TernaryExpr, pos),
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
    bool FoldToConstant() override;
    bool HasSideEffects() override {
        return first_->HasSideEffects() || second_->HasSideEffects() || third_->HasSideEffects();
    }
    void ProcessUses(SemaContext& sc) override;
    void ProcessDiscardUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::TernaryExpr; }

    Expr* first() const { return first_; }
    Expr* set_first(Expr* first) { return first_ = first; }
    Expr* second() const { return second_; }
    Expr* set_second(Expr* second) { return second_ = second; }
    Expr* third() const { return third_; }
    Expr* set_third(Expr* third) { return third_ = third; }

  private:
    Expr* first_;
    Expr* second_;
    Expr* third_;
};

class IncDecExpr : public Expr
{
  public:
    IncDecExpr(const token_pos_t& pos, int token, Expr* expr, bool prefix)
      : Expr(AstKind::IncDecExpr, pos),
        token_(token),
        expr_(expr),
        prefix_(prefix)
    {}

    bool Bind(SemaContext& sc) override {
        return expr_->Bind(sc);
    }
    bool HasSideEffects() override {
        return true;
    }
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::IncDecExpr; }

    int token() const { return token_; }
    Expr* expr() const { return expr_; }
    UserOperation& userop() { return userop_; }
    bool prefix() const { return prefix_; }

  protected:
    int token_;
    Expr* expr_;
    UserOperation userop_;
    bool prefix_;
};

class PreIncExpr final : public IncDecExpr
{
  public:
    PreIncExpr(const token_pos_t& pos, int token, Expr* expr)
      : IncDecExpr(pos, token, expr, true)
    {}
};

class PostIncExpr final : public IncDecExpr
{
  public:
    PostIncExpr(const token_pos_t& pos, int token, Expr* expr)
      : IncDecExpr(pos, token, expr, false)
    {}
};

class CastExpr final : public Expr
{
  public:
    CastExpr(const token_pos_t& pos, int token, const TypenameInfo& type, Expr* expr)
      : Expr(AstKind::CastExpr, pos),
        token_(token),
        type_(type),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override;
    bool HasSideEffects() override {
        return expr_->HasSideEffects();
    }
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::CastExpr; }

    Expr* expr() const { return expr_; }
    const auto& type() const { return type_; }

  private:
    int token_;
    TypenameInfo type_;
    Expr* expr_;
};

class SizeofExpr final : public Expr
{
  public:
    SizeofExpr(const token_pos_t& pos, sp::Atom* ident, sp::Atom* field, int suffix_token, int array_levels)
      : Expr(AstKind::SizeofExpr, pos),
        ident_(ident),
        field_(field),
        suffix_token_(suffix_token),
        array_levels_(array_levels)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::SizeofExpr; }

    sp::Atom* ident() const { return ident_; }
    sp::Atom* field() const { return field_; }
    int suffix_token() const { return suffix_token_; }
    int array_levels() const { return array_levels_; }
    symbol* sym() const { return sym_; }

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
      : Expr(AstKind::SymbolExpr, pos),
        name_(name),
        sym_(nullptr)
    {
    }

    bool Bind(SemaContext& sc) override;
    bool BindLval(SemaContext& sc) override;
    void MarkUsed(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::SymbolExpr; }

    symbol* sym() const { return sym_; }

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
      : Expr(AstKind::CallExpr, pos),
        token_(token),
        target_(target)
    {}

    bool Bind(SemaContext& sc) override;
    void MarkUsed(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    bool HasSideEffects() override {
        return true;
    }

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::CallExpr; }

    PoolList<ParsedArg>& args() { return args_; }
    PoolList<ComputedArg>& argv() { return argv_; }
    const PoolList<ComputedArg>& argv() const { return argv_; }
    Expr* target() const { return target_; }
    int token() const { return token_; }
    Expr* implicit_this() const { return implicit_this_; }
    void set_implicit_this(Expr* expr) { implicit_this_ = expr; }
    symbol* sym() const { return sym_; }
    void set_sym(symbol* sym) { sym_ = sym; }

  private:
    bool ProcessArg(SemaContext& sc, arginfo* arg, Expr* param, unsigned int pos);

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
    explicit EmitOnlyExpr(AstKind kind, const token_pos_t& pos)
      : Expr(kind, pos)
    {}

    bool Bind(SemaContext& sc) override {
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
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::CallUserOpExpr; }

    const UserOperation& userop() const { return userop_; }
    Expr* expr() const { return expr_; }

  private:
    UserOperation userop_;
    Expr* expr_;
};

class DefaultArgExpr final : public EmitOnlyExpr
{
  public:
    DefaultArgExpr(const token_pos_t& pos, arginfo* arg);

    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::DefaultArgExpr; }

    arginfo* arg() { return arg_; }

  private:
    arginfo* arg_;
};

class FieldAccessExpr final : public Expr
{
  public:
    FieldAccessExpr(const token_pos_t& pos, int tok, Expr* base, sp::Atom* name)
      : Expr(AstKind::FieldAccessExpr, pos),
        token_(tok),
        base_(base),
        name_(name)
    {}

    bool Bind(SemaContext& sc) override {
        return base_->Bind(sc);
    }
    bool HasSideEffects() override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::FieldAccessExpr; }

    int token() const { return token_; }
    Expr* base() const { return base_; }
    Expr* set_base(Expr* base) { return base_ = base; }
    sp::Atom* name() const { return name_; }
    symbol* field() const { return field_; }
    void set_field(symbol* field) { field_ = field; }

    methodmap_method_t* method() const { return method_; }
    void set_method(methodmap_method_t* method) { method_ = method; }

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
      : Expr(AstKind::IndexExpr, pos),
        base_(base),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        bool ok = base_->Bind(sc);
        ok &= expr_->Bind(sc);
        return ok;
    }
    void ProcessUses(SemaContext& sc) override;
    bool HasSideEffects() override {
        return base_->HasSideEffects() || expr_->HasSideEffects();
    }

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::IndexExpr; }

    Expr* base() const { return base_; }
    Expr* set_base(Expr* base) { return base_ = base; }
    Expr* index() const { return expr_; }
    Expr* set_index(Expr* index) { return expr_ = index; }

  private:
    Expr* base_;
    Expr* expr_;
};

class RvalueExpr final : public EmitOnlyExpr
{
  public:
    explicit RvalueExpr(Expr* expr);

    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::RvalueExpr; }

    Expr* expr() const { return expr_; }

  private:
    Expr* expr_;
};

class CommaExpr final : public Expr
{
  public:
    explicit CommaExpr(const token_pos_t& pos)
      : Expr(AstKind::CommaExpr, pos)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void ProcessDiscardUses(SemaContext& sc) override;
    bool HasSideEffects() override {
        return has_side_effects_;
    }

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::CommaExpr; }

    PoolList<Expr*>& exprs() { return exprs_; }
    void set_has_side_effects() { has_side_effects_ = true; }

  private:
    PoolList<Expr*> exprs_;
    bool has_side_effects_ = false;
};

class ThisExpr final : public Expr
{
  public:
    explicit ThisExpr(const token_pos_t& pos)
      : Expr(AstKind::ThisExpr, pos),
        sym_(nullptr)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ThisExpr; }

    symbol* sym() const { return sym_; }

  private:
    symbol* sym_;
};

class NullExpr final : public Expr
{
  public:
    explicit NullExpr(const token_pos_t& pos)
      : Expr(AstKind::NullExpr, pos)
    {}

    void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::NullExpr; }
};

class TaggedValueExpr : public Expr
{
  public:
    TaggedValueExpr(const token_pos_t& pos, int tag, cell value)
      : Expr(AstKind::TaggedValueExpr, pos),
        tag_(tag),
        value_(value)
    {}

    void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::TaggedValueExpr; }

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
      : Expr(AstKind::StringExpr, pos),
        text_(new PoolString(str, len))
    {}

    void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::StringExpr; }

    PoolString* text() const {
        return text_;
    }

  private:
    PoolString* text_;
};

class NewArrayExpr final : public Expr
{
  public:
    explicit NewArrayExpr(const token_pos_t& pos, const TypenameInfo& ur)
      : Expr(AstKind::NewArrayExpr, pos),
        type_(ur)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::NewArrayExpr; }

    int tag() { return type_.tag(); }
    TypenameInfo& type() { return type_; }
    PoolList<Expr*>& exprs() { return exprs_; }
    const TypenameInfo& type() const { return type_; }
    bool autozero() const { return autozero_; }
    void set_no_autozero() { autozero_ = false; }
    bool analyzed() const { return analyzed_.isValid(); }
    bool analysis_result() const { return analyzed_.get(); }
    void set_analysis_result(bool value) { analyzed_.init(value); }

  private:
    TypenameInfo type_;
    PoolList<Expr*> exprs_;
    bool autozero_ = true;
    ke::Maybe<bool> analyzed_;
};

class ArrayExpr final : public Expr
{
  public:
    explicit ArrayExpr(const token_pos_t& pos)
      : Expr(AstKind::ArrayExpr, pos)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ArrayExpr; }

    PoolList<Expr*>& exprs() { return exprs_; }
    bool ellipses() const { return ellipses_; }
    void set_ellipses() { ellipses_ = true; }
    bool synthesized_for_compat() const { return synthesized_for_compat_; }
    void set_synthesized_for_compat() { synthesized_for_compat_ = true; }

  private:
    bool ellipses_ = false;
    bool synthesized_for_compat_ = false;
    PoolList<Expr*> exprs_;
};

struct StructInitField {
    StructInitField(sp::Atom* name, Expr* value, const token_pos_t& pos)
        : name(name), value(value), pos(pos)
    {}
    sp::Atom* name;
    Expr* value;
    token_pos_t pos;
};

class StructExpr final : public Expr
{
  public:
    explicit StructExpr(const token_pos_t& pos)
      : Expr(AstKind::StructExpr, pos)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext&) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::StructExpr; }

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
      : Stmt(AstKind::IfStmt, pos),
        cond_(cond),
        on_true_(on_true),
        on_false_(on_false)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::IfStmt; }

    Expr* cond() const { return cond_; }
    Expr* set_cond(Expr* cond) { return cond_ = cond; }
    Stmt* on_true() const { return on_true_; }
    Stmt* on_false() const { return on_false_; }

  private:
    Expr* cond_;
    Stmt* on_true_;
    Stmt* on_false_;
};

class ExprStmt : public Stmt
{
  public:
    ExprStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(AstKind::ExprStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }

    void ProcessUses(SemaContext& sc) override {
        expr_->ProcessDiscardUses(sc);
    }

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ExprStmt; }

    Expr* expr() const { return expr_; }

  private:
    Expr* expr_;
};

class ReturnStmt : public Stmt
{
  public:
    explicit ReturnStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(AstKind::ReturnStmt, pos),
        expr_(expr)
    {
        set_flow_type(Flow_Return);
    }

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ReturnStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }
    typeinfo_t& array() { return array_; }
    const typeinfo_t& array() const { return array_; }

  private:
    bool CheckArrayReturn(SemaContext& sc);

  private:
    Expr* expr_;
    typeinfo_t array_;
};

class AssertStmt : public Stmt
{
  public:
    explicit AssertStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(AstKind::AssertStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }

    void ProcessUses(SemaContext& sc) override {
        expr_->MarkAndProcessUses(sc);
    }

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::AssertStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

  private:
    Expr* expr_;
};

class DeleteStmt : public Stmt
{
  public:
    explicit DeleteStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(AstKind::DeleteStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }

    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::DeleteStmt; }

    Expr* expr() const { return expr_; }
    methodmap_t* map() const { return map_; }
    void set_map(methodmap_t* map) { map_ = map; }

  private:
    Expr* expr_;
    methodmap_t* map_;
};

class ExitStmt : public Stmt
{
  public:
    explicit ExitStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(AstKind::ExitStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ExitStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

  private:
    Expr* expr_;
};

class DoWhileStmt : public Stmt
{
  public:
    explicit DoWhileStmt(const token_pos_t& pos, int token, Expr* cond, Stmt* body)
      : Stmt(AstKind::DoWhileStmt, pos),
        token_(token),
        cond_(cond),
        body_(body)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::DoWhileStmt; }

    int token() const { return token_; }
    Expr* cond() const { return cond_; }
    Expr* set_cond(Expr* expr) { return cond_ = expr; }
    Stmt* body() const { return body_; }
    bool always_taken() const { return always_taken_; }
    void set_always_taken(bool val) { always_taken_ = val; }
    bool never_taken() const { return never_taken_; }
    void set_never_taken(bool val) { never_taken_ = val; }

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
      : Stmt(AstKind::ForStmt, pos),
        scope_(nullptr),
        init_(init),
        cond_(cond),
        advance_(advance),
        body_(body)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::ForStmt; }

    SymbolScope* scope() const { return scope_; }
    Stmt* init() const { return init_; }
    Expr* cond() const { return cond_; }
    Expr* set_cond(Expr* cond) { return cond_ = cond; }
    Expr* advance() const { return advance_; }
    Stmt* body() const { return body_; }
    bool always_taken() const { return always_taken_; }
    void set_always_taken(bool val) { always_taken_ = val; }
    bool never_taken() const { return never_taken_; }
    void set_never_taken(bool val) { never_taken_ = val; }
    bool has_continue() const { return has_continue_; }
    void set_has_continue(bool val) { has_continue_ = val; }

  private:
    SymbolScope* scope_;
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
    typedef std::pair<PoolArray<Expr*>, Stmt*> Case;

    explicit SwitchStmt(const token_pos_t& pos, Expr* expr, std::vector<Case>&& cases,
                        Stmt* default_case)
      : Stmt(AstKind::SwitchStmt, pos),
        expr_(expr),
        default_case_(default_case),
        cases_(std::move(cases))
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::SwitchStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }
    Stmt* default_case() const { return default_case_; }
    const PoolArray<Case>& cases() const { return cases_; }

  private:
    Expr* expr_;
    Stmt* default_case_;

    PoolArray<Case> cases_;
};

class PragmaUnusedStmt : public Stmt
{
  public:
    PragmaUnusedStmt(const token_pos_t& pos)
      : Stmt(AstKind::PragmaUnusedStmt, pos)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::PragmaUnusedStmt; }

    PoolList<sp::Atom*>& names() { return names_; }
    PoolList<symbol*>& symbols() { return symbols_; }

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
    void ProcessUses(SemaContext& sc);

    const token_pos_t& end_pos() const { return end_pos_; }
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

    void set_is_forward() { is_forward_ = true; }
    bool is_forward() const { return is_forward_; }

    void set_is_public() { is_public_ = true; }
    bool is_public() const { return is_public_; }

    void set_is_stock() { is_stock_ = true; }
    bool is_stock() const { return is_stock_; }

    void set_is_static() { is_static_ = true; }
    bool is_static() const { return is_static_; }

    PoolList<FunctionArg>& args() { return args_; }
    const token_pos_t& pos() const { return pos_; }

    declinfo_t& decl() { return decl_; }
    const declinfo_t& decl() const { return decl_; }

    symbol* sym() const { return sym_; }
    void set_sym(symbol* sym) { sym_ = sym; }

    const typeinfo_t& type() const { return decl_.type; }
    typeinfo_t& mutable_type() { return decl_.type; }

    bool is_analyzing() const { return is_analyzing_; }
    void set_is_analyzing(bool val) { is_analyzing_ = val; }
    bool is_analyzed() const { return analyzed_.isValid(); }
    bool analysis_status() const { return analyzed_.get(); }
    void set_analyzed(bool val) { analyzed_.init(val); }

    SymbolScope* scope() const { return scope_; }

    bool maybe_returns_array() const { return maybe_returns_array_; }
    void set_maybe_returns_array() { maybe_returns_array_ = true; }

    void CheckReturnUsage();

  private:
    bool BindArgs(SemaContext& sc);

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
    SymbolScope* scope_ = nullptr;
    ke::Maybe<int> this_tag_;
    sp::Atom* alias_ = nullptr;
    ke::Maybe<bool> analyzed_;
    bool is_analyzing_ = false;
    bool maybe_returns_array_ = false;
};

class FunctionDecl : public Decl
{
  public:
    explicit FunctionDecl(const token_pos_t& pos, sp::Atom* atom, FunctionInfo* info)
      : Decl(AstKind::FunctionDecl, pos, atom),
        info_(info)
    {}
    explicit FunctionDecl(const token_pos_t& pos, FunctionInfo* info)
      : Decl(AstKind::FunctionDecl, pos, info->name()),
        info_(info)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::FunctionDecl; }

    void set_deprecate(const std::string& deprecate) { deprecate_ = new PoolString(deprecate); }

    FunctionInfo* info() const { return info_; }
    symbol* sym() const { return info_->sym(); }

  private:
    bool CanRedefine(symbol* sym);

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
      : Decl(AstKind::EnumStructDecl, pos, name)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::EnumStructDecl; }

    PoolList<FunctionDecl*>& methods() { return methods_; }
    PoolList<EnumStructField>& fields() { return fields_; }

  private:
    PoolList<FunctionDecl*> methods_;
    PoolList<EnumStructField> fields_;
    symbol* root_ = nullptr;
};

struct MethodmapProperty : public PoolObject {
    token_pos_t pos;
    typeinfo_t type;
    sp::Atom* name = nullptr;
    FunctionInfo* getter = nullptr;
    FunctionInfo* setter = nullptr;
    methodmap_method_t* entry = nullptr;
};

struct MethodmapMethod : public PoolObject {
    bool is_static = false;
    FunctionDecl* decl = nullptr;
    methodmap_method_t* entry = nullptr;
};

class MethodmapDecl : public Decl
{
  public:
    explicit MethodmapDecl(const token_pos_t& pos, sp::Atom* name, bool nullable, sp::Atom* extends)
      : Decl(AstKind::MethodmapDecl, pos, name),
        nullable_(nullable),
        extends_(extends)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(ParseNode* node) { return node->kind() == AstKind::MethodmapDecl; }

    PoolList<MethodmapProperty*>& properties() { return properties_; }
    PoolList<MethodmapMethod*>& methods() { return methods_; }

  private:
    bool BindGetter(SemaContext& sc, MethodmapProperty* prop);
    bool BindSetter(SemaContext& sc, MethodmapProperty* prop);

  private:
    bool nullable_;
    sp::Atom* extends_;
    PoolList<MethodmapProperty*> properties_;
    PoolList<MethodmapMethod*> methods_;

    methodmap_t* map_ = nullptr;
    symbol* sym_ = nullptr;
};
