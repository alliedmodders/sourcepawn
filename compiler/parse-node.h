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

#include <optional>

#include "ast-types.h"
#include "expressions.h"
#include "lexer.h"
#include "pool-allocator.h"
#include "sc.h"
#include "shared/string-pool.h"
#include "symbols.h"

namespace sp {
namespace cc {

class FunctionDecl;

struct UserOperation
{
    UserOperation() {}

    FunctionDecl* sym = nullptr;
    int oper = 0;
    int paramspassed;
    bool savepri;
    bool savealt;
    bool swapparams;
};

typedef void (*OpFunc)();

class Expr;
class LayoutFieldDecl;
class MethodmapDecl;
class MethodmapMethodDecl;
class SemaContext;
class SymbolScope;
struct StructInitField;
struct structarg_t;

class ParseNode : public PoolObject
{
  public:
    explicit ParseNode(const token_pos_t& pos)
      : pos_(pos),
        tree_has_heap_allocs_(false)
    {}

    virtual bool Bind(SemaContext& sc) {
        return true;
    }
    virtual bool BindLval(SemaContext& sc) {
        return Bind(sc);
    }

    const token_pos_t& pos() const {
        return pos_;
    }

    bool tree_has_heap_allocs() const { return tree_has_heap_allocs_; }
    void set_tree_has_heap_allocs(bool b) { tree_has_heap_allocs_ = b; }

  protected:
    void error(const token_pos_t& pos, int number);

  private:
    // Hide this symbol. Calls to error(pos... will get more accurate as we
    // make adjustments.
    void error(int number) = delete;

  protected:
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
    explicit Stmt(StmtKind kind, const token_pos_t& pos)
      : ParseNode(pos),
        kind_(kind),
        flow_type_(Flow_None)
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

    bool IsTerminal() const;

    StmtKind kind() const { return kind_; }
    bool is(StmtKind k) const { return kind() == k; }

    template <class T> T* as() {
        if (T::is_a(this))
            return reinterpret_cast<T*>(this);
        return nullptr;
    }
    template <class T> T* to() {
        assert(T::is_a(this));
        return reinterpret_cast<T*>(this);
    }

  private:
    StmtKind kind_ : 8;
    FlowType flow_type_ : 4;
};

class ChangeScopeNode : public Stmt
{
  public:
    explicit ChangeScopeNode(const token_pos_t& pos, SymbolScope* scope, const std::string& file)
      : Stmt(StmtKind::ChangeScopeNode, pos),
        scope_(scope),
        file_(new PoolString(file))
    {}

    virtual bool EnterNames(SemaContext& sc) override;
    virtual bool Bind(SemaContext& sc) override;
    virtual void ProcessUses(SemaContext&) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ChangeScopeNode; }

    SymbolScope* scope() const { return scope_; }
    PoolString* file() const { return file_; }

  private:
    SymbolScope* scope_;
    PoolString* file_;
};

class StmtList : public Stmt
{
  public:
    explicit StmtList(StmtKind kind, const token_pos_t& pos, const std::vector<Stmt*>& stmts)
      : Stmt(kind, pos),
        stmts_(stmts)
    {}
    explicit StmtList(const token_pos_t& pos, const std::vector<Stmt*>& stmts)
      : Stmt(StmtKind::StmtList, pos),
        stmts_(stmts)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    Stmt* GetLast() override {
        return stmts_.empty() ? this : stmts_.back();
    }

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::StmtList; }

    PoolArray<Stmt*>& stmts() {
        return stmts_;
    }

  protected:
    PoolArray<Stmt*> stmts_;
};

class ParseTree : public PoolObject
{
  public:
    explicit ParseTree(StmtList* stmts)
      : stmts_(stmts)
    {}

    bool ResolveNames(SemaContext& sc);

    StmtList* stmts() { return stmts_; }

  private:
    StmtList* stmts_;
};

class BlockStmt : public StmtList
{
  public:
    explicit BlockStmt(const token_pos_t& pos, const std::vector<Stmt*>& stmts)
      : StmtList(StmtKind::BlockStmt, pos, stmts),
        scope_(nullptr)
    {}

    static BlockStmt* WrapStmt(Stmt* stmt);

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::BlockStmt; }

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
      : Stmt(StmtKind::BreakStmt, pos)
    {
        set_flow_type(Flow_Break);
    }

    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::BreakStmt; }
};

class ContinueStmt : public Stmt
{
  public:
    explicit ContinueStmt(const token_pos_t& pos)
      : Stmt(StmtKind::ContinueStmt, pos)
    {
        set_flow_type(Flow_Continue);
    }

    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ContinueStmt; }
};

class StaticAssertStmt : public Stmt
{
  public:
    explicit StaticAssertStmt(const token_pos_t& pos, Expr* expr, Atom* text)
      : Stmt(StmtKind::StaticAssertStmt, pos),
        expr_(expr),
        text_(text)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::StaticAssertStmt; }

    Expr* expr() const { return expr_; }
    Atom* text() const { return text_; }

  private:
    Expr* expr_;
    Atom* text_;
};

class Decl : public Stmt
{
  public:
    Decl(StmtKind kind, const token_pos_t& pos, Atom* name)
      : Stmt(kind, pos),
        name_(name)
    {}

    cell ConstVal();

    IdentifierKind ident();
    IdentifierKind ident_impl();
    char vclass();
    bool is_const();
    virtual Type* type() const;

    Atom* name() const { return name_; }

  protected:
    Atom* DecorateInnerName(Atom* parent_name, Atom* field_name);

  protected:
    Atom* name_;

  public:
    // :TODO: remove.
    Decl* next = nullptr;
};

class BinaryExpr;

class VarDeclBase : public Decl
{
  public:
    VarDeclBase(StmtKind kind, const token_pos_t& pos, Atom* name, const typeinfo_t& type,
                int vclass, bool is_public, bool is_static, bool is_stock, Expr* initializer);

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    // Bind only the typeinfo.
    bool BindType(SemaContext& sc);

    void BindAddress(cell addr);

    static bool is_a(Stmt* node) {
        return node->kind() == StmtKind::VarDecl ||
               node->kind() == StmtKind::ArgDecl ||
               node->kind() == StmtKind::ConstDecl;
    }

    BinaryExpr* init() const { return init_; }
    Expr* init_rhs() const;
    int vclass() const { return vclass_; }
    const typeinfo_t& type_info() const { return type_; }
    typeinfo_t* mutable_type_info() { return &type_; }
    void set_init(Expr* expr);
    bool autozero() const { return autozero_; }
    void set_no_autozero() { autozero_ = false; }
    bool is_public() const { return is_public_; }
    bool is_stock() const { return is_stock_; }
    bool is_read() const { return is_read_; }
    void set_is_read() { is_read_ = true; }
    bool is_written() const { return is_written_; }
    void set_is_written() { is_written_ = true; }
    bool implicit_dynamic_array() const { return implicit_dynamic_array_; }
    void set_implicit_dynamic_array() { implicit_dynamic_array_ = true; }
    Label* label() { return &addr_; }
    cell addr() const { return addr_.offset(); }
    Type* type() const override { return type_.type; }

    bool is_used() const { return is_read_ || is_written_; }

  protected:
    typeinfo_t type_;
    BinaryExpr* init_ = nullptr;
    uint8_t vclass_ : 4; // This will be implied by scope, when we get there.
    bool is_public_ : 1;
    bool is_static_ : 1;
    bool is_stock_ : 1;
    bool autozero_ : 1;
    bool is_read_ : 1;
    bool is_written_ : 1;
    bool implicit_dynamic_array_ : 1;
    Label addr_;
};

class VarDecl : public VarDeclBase
{
  public:
    VarDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
            bool is_public, bool is_static, bool is_stock, Expr* initializer)
      : VarDeclBase(StmtKind::VarDecl, pos, name, type, vclass, is_public, is_static, is_stock,
                    initializer)
    {}
    VarDecl(StmtKind kind, const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
            bool is_public, bool is_static, bool is_stock, Expr* initializer)
      : VarDeclBase(kind, pos, name, type, vclass, is_public, is_static, is_stock,
                    initializer)
    {}

    static bool is_a(Stmt* node) {
        return node->kind() == StmtKind::VarDecl || node->kind() == StmtKind::ConstDecl;
    }
};

class ArgDecl : public VarDeclBase
{
  public:
    ArgDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
            bool is_public, bool is_static, bool is_stock, Expr* initializer)
      : VarDeclBase(StmtKind::ArgDecl, pos, name, type, vclass, is_public, is_static, is_stock,
                    initializer)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ArgDecl; }

    DefaultArg* default_value() const { return default_value_; }
    void set_default_value(DefaultArg* arg) { default_value_ = arg; }

  private:
    DefaultArg* default_value_ = nullptr;
};

class ConstDecl : public VarDecl
{
  public:
    ConstDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
              Expr* expr)
      : VarDecl(StmtKind::ConstDecl, pos, name, type, vclass, false, false, false, nullptr),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override;
    bool EnterNames(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ConstDecl; }

    cell const_val() const { return value_; }

  private:
    Expr* expr_;
    cell value_;
};

class EnumFieldDecl : public Decl
{
  public:
    EnumFieldDecl(const token_pos_t& pos, Atom* name, Expr* value)
      : Decl(StmtKind::EnumFieldDecl, pos, name),
        value_(value)
    {}

    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::EnumFieldDecl; }

    Expr* value() const { return value_; }
    Type* type() const override { return type_; }
    void set_type(Type* type) { type_ = type; }

    cell const_val() const { return const_val_; }
    void set_const_val(cell const_val) { const_val_ = const_val; }

  private:
    Type* type_ = nullptr;
    Expr* value_;
    cell const_val_ = 0;
};

class EnumDecl : public Decl
{
  public:
    explicit EnumDecl(const token_pos_t& pos, int vclass, Atom* label, Atom* name,
                      const std::vector<EnumFieldDecl*>& fields, int increment, int multiplier)
      : Decl(StmtKind::EnumDecl, pos, name),
        vclass_(vclass),
        label_(label),
        fields_(fields),
        increment_(increment),
        multiplier_(multiplier)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::EnumDecl; }

    PoolArray<EnumFieldDecl*>& fields() { return fields_; }
    int increment() const { return increment_; }
    int multiplier() const { return multiplier_; }
    int array_size() const { return array_size_; }
    Type* type() const override { return type_; }

    MethodmapDecl* mm() const { return mm_; }
    void set_mm(MethodmapDecl* mm) { mm_ = mm; }

  private:
    int vclass_;
    Atom* label_;
    PoolArray<EnumFieldDecl*> fields_;
    int increment_;
    int multiplier_;
    int array_size_ = 0;
    Type* type_ = nullptr;
    MethodmapDecl* mm_ = nullptr;
};

// "Pawn Struct", or p-struct, a hack to effect a replacement for register_plugin()
// when SourceMod was first being prototyped. Theoretically these could be retooled
// as proper structs.
class PstructDecl : public Decl
{
  public:
    PstructDecl(const token_pos_t& pos, Atom* name, const std::vector<LayoutFieldDecl*>& fields)
      : Decl(StmtKind::PstructDecl, pos, name),
        fields_(fields)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::PstructDecl; }

    LayoutFieldDecl* FindField(Atom* name);

    PoolArray<LayoutFieldDecl*>& fields() { return fields_; }

  protected:
    PoolArray<LayoutFieldDecl*> fields_;
};

struct TypedefInfo : public PoolObject {
    TypedefInfo(const token_pos_t& pos, const TypenameInfo& ret_type,
                const std::vector<declinfo_t*>& args)
     : pos(pos),
       ret_type(ret_type),
       args(args)
    {}
    token_pos_t pos;
    TypenameInfo ret_type;
    PoolArray<declinfo_t*> args;

    FunctionType* Bind(SemaContext& sc);
};

class TypedefDecl : public Decl
{
  public:
    explicit TypedefDecl(const token_pos_t& pos, Atom* name, TypedefInfo* type)
      : Decl(StmtKind::TypedefDecl, pos, name),
        type_(type)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::TypedefDecl; }

  private:
    TypedefInfo* type_;
    funcenum_t* fe_ = nullptr;
};

// Unsafe typeset - only supports function types. This is a transition hack for SP2.
class TypesetDecl : public Decl
{
  public:
    explicit TypesetDecl(const token_pos_t& pos, Atom* name,
                         const std::vector<TypedefInfo*>& types)
      : Decl(StmtKind::TypesetDecl, pos, name),
        types_(types)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::TypesetDecl; }

    PoolArray<TypedefInfo*>& types() {
        return types_;
    }

  private:
    PoolArray<TypedefInfo*> types_;
    funcenum_t* fe_ = nullptr;
};

class Expr : public ParseNode
{
  public:
    explicit Expr(ExprKind kind, const token_pos_t& pos)
      : ParseNode(pos),
        kind_(kind),
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
    bool EvalConst(cell* value, Type** type);

    // Return whether or not the expression is idempotent (eg has side effects).
    bool HasSideEffects();

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

    ExprKind kind() const { return kind_; }
    bool is(ExprKind k) const { return kind() == k; }

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
    value val_ = {};
    ExprKind kind_ : 8;
    bool lvalue_ : 1;
    bool can_alloc_heap_ : 1;
};

class UnaryExpr final : public Expr
{
  public:
    UnaryExpr(const token_pos_t& pos, int token, Expr* expr)
      : Expr(ExprKind::UnaryExpr, pos),
        token_(token),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        return expr_->Bind(sc);
    }
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::UnaryExpr; }

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
    BinaryExprBase(ExprKind kind, const token_pos_t& pos, int token, Expr* left, Expr* right);

    bool Bind(SemaContext& sc) override;
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

    bool FoldToConstant() override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::BinaryExpr; }

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
      : BinaryExprBase(ExprKind::LogicalExpr, pos, token, left, right)
    {}

    void FlattenLogical(int token, std::vector<Expr*>* out) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::LogicalExpr; }
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
    explicit ChainedCompareExpr(const token_pos_t& pos, Expr* first,
                                const std::vector<CompareOp>& ops)
      : Expr(ExprKind::ChainedCompareExpr, pos),
        first_(first),
        ops_(ops)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::ChainedCompareExpr; }

    Expr* first() const { return first_; }
    Expr* set_first(Expr* first) { return first_ = first; }
    PoolArray<CompareOp>& ops() { return ops_; }

  private:
    Expr* first_;
    PoolArray<CompareOp> ops_;
};

class TernaryExpr final : public Expr
{
  public:
    TernaryExpr(const token_pos_t& pos, Expr* first, Expr* second, Expr* third)
      : Expr(ExprKind::TernaryExpr, pos),
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
    void ProcessUses(SemaContext& sc) override;
    void ProcessDiscardUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::TernaryExpr; }

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
      : Expr(ExprKind::IncDecExpr, pos),
        token_(token),
        expr_(expr),
        prefix_(prefix)
    {}

    bool Bind(SemaContext& sc) override {
        return expr_->Bind(sc);
    }
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::IncDecExpr; }

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
      : Expr(ExprKind::CastExpr, pos),
        token_(token),
        type_(type),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::CastExpr; }

    Expr* expr() const { return expr_; }
    const auto& type_info() const { return type_; }
    Type* type() const { return type_.type(); }

  private:
    int token_;
    TypenameInfo type_;
    Expr* expr_;
};

class SizeofExpr final : public Expr
{
  public:
    SizeofExpr(const token_pos_t& pos, Expr* child)
      : Expr(ExprKind::SizeofExpr, pos),
        child_(child)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::SizeofExpr; }

    Expr* child() const { return child_; }

  private:
    Expr* child_;
};

class SymbolExpr final : public Expr
{
  public:
    SymbolExpr(const token_pos_t& pos, Atom* name)
      : Expr(ExprKind::SymbolExpr, pos),
        name_(name),
        decl_(nullptr)
    {
    }

    bool Bind(SemaContext& sc) override;
    bool BindLval(SemaContext& sc) override;
    void MarkUsed(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::SymbolExpr; }

    Decl* decl() const { return decl_; }

  private:
    bool DoBind(SemaContext& sc, bool is_lval);

  private:
    Atom* name_;
    Decl* decl_;
};

class NamedArgExpr : public Expr
{
  public:
    NamedArgExpr(const token_pos_t& pos, Atom* name, Expr* expr)
      : Expr(ExprKind::NamedArgExpr, pos),
        name(name),
        expr(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr->Bind(sc); }
    void ProcessUses(SemaContext& sc) override { expr->ProcessUses(sc); }

    static bool is_a(Expr* node) { return node->kind() == ExprKind::NamedArgExpr; }

    Atom* name;
    Expr* expr;
};

class CallExpr final : public Expr
{
  public:
    CallExpr(const token_pos_t& pos, int token, Expr* target, const std::vector<Expr*>& args)
      : Expr(ExprKind::CallExpr, pos),
        token_(token),
        target_(target),
        args_(args)
    {}

    bool Bind(SemaContext& sc) override;
    void MarkUsed(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::CallExpr; }

    PoolArray<Expr*>& args() { return args_; }
    Expr* target() const { return target_; }
    int token() const { return token_; }
    Expr* implicit_this() const { return implicit_this_; }
    void set_implicit_this(Expr* expr) { implicit_this_ = expr; }
    FunctionDecl* fun() const { return fun_; }
    void set_fun(FunctionDecl* fun) { fun_ = fun; }

  private:
    bool ProcessArg(SemaContext& sc, VarDecl* arg, Expr* param, unsigned int pos);

    int token_;
    Expr* target_;
    PoolArray<Expr*> args_;
    FunctionDecl* fun_ = nullptr;
    Expr* implicit_this_ = nullptr;
};

class EmitOnlyExpr : public Expr
{
  public:
    explicit EmitOnlyExpr(ExprKind kind, const token_pos_t& pos)
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

    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::CallUserOpExpr; }

    const UserOperation& userop() const { return userop_; }
    Expr* expr() const { return expr_; }

  private:
    UserOperation userop_;
    Expr* expr_;
};

class DefaultArgExpr final : public Expr
{
  public:
    DefaultArgExpr(const token_pos_t& pos, ArgDecl* arg);

    bool Bind(SemaContext& sc) override { return true; }
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::DefaultArgExpr; }

    ArgDecl* arg() { return arg_; }
    void set_arg(ArgDecl* arg) { arg_ = arg; }

  private:
    ArgDecl* arg_;
};

class FieldAccessExpr final : public Expr
{
  public:
    FieldAccessExpr(const token_pos_t& pos, int tok, Expr* base, Atom* name)
      : Expr(ExprKind::FieldAccessExpr, pos),
        token_(tok),
        base_(base),
        name_(name)
    {}

    bool Bind(SemaContext& sc) override {
        return base_->Bind(sc);
    }
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::FieldAccessExpr; }

    int token() const { return token_; }
    Expr* base() const { return base_; }
    Expr* set_base(Expr* base) { return base_ = base; }
    Atom* name() const { return name_; }
    Decl* resolved() const { return resolved_; }
    void set_resolved(Decl* resolved) { resolved_ = resolved; }

  private:
    int token_;
    Expr* base_;
    Atom* name_;
    Decl* resolved_;
};

class IndexExpr final : public Expr
{
  public:
    IndexExpr(const token_pos_t& pos, Expr* base, Expr* expr)
      : Expr(ExprKind::IndexExpr, pos),
        base_(base),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override {
        bool ok = base_->Bind(sc);
        if (expr_)
            ok &= expr_->Bind(sc);
        return ok;
    }
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::IndexExpr; }

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

    static bool is_a(Expr* node) { return node->kind() == ExprKind::RvalueExpr; }

    Expr* expr() const { return expr_; }

  private:
    Expr* expr_;
};

class CommaExpr final : public Expr
{
  public:
    CommaExpr(const token_pos_t& pos, const std::vector<Expr*>& exprs)
      : Expr(ExprKind::CommaExpr, pos),
        exprs_(exprs)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;
    void ProcessDiscardUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::CommaExpr; }

    PoolArray<Expr*>& exprs() { return exprs_; }

  private:
    PoolArray<Expr*> exprs_;
};

class ThisExpr final : public Expr
{
  public:
    explicit ThisExpr(const token_pos_t& pos)
      : Expr(ExprKind::ThisExpr, pos)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext&) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::ThisExpr; }

    VarDeclBase* decl() const { return decl_; }

  private:
    VarDeclBase* decl_ = nullptr;
};

class NullExpr final : public Expr
{
  public:
    explicit NullExpr(const token_pos_t& pos)
      : Expr(ExprKind::NullExpr, pos)
    {}

    void ProcessUses(SemaContext&) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::NullExpr; }
};

class TaggedValueExpr : public Expr
{
  public:
    TaggedValueExpr(const token_pos_t& pos, Type* type, cell value)
      : Expr(ExprKind::TaggedValueExpr, pos),
        type_(type),
        value_(value)
    {}

    void ProcessUses(SemaContext&) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::TaggedValueExpr; }

    Type* type() const { return type_; }
    cell value() const { return value_; }

  protected:
    Type* type_;
    cell value_;
};

class NumberExpr final : public TaggedValueExpr
{
  public:
    NumberExpr(const token_pos_t& pos, Type* type, cell value)
      : TaggedValueExpr(pos, type, value)
    {}
};

class FloatExpr final : public TaggedValueExpr
{
  public:
    FloatExpr(CompileContext& cc, const token_pos_t& pos, cell value);
};

class StringExpr final : public Expr
{
  public:
    StringExpr(const token_pos_t& pos, Atom* atom)
      : Expr(ExprKind::StringExpr, pos),
        text_(atom)
    {}

    void ProcessUses(SemaContext&) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::StringExpr; }

    Atom* text() const {
        return text_;
    }

  private:
    Atom* text_;
};

class NewArrayExpr final : public Expr
{
  public:
    NewArrayExpr(const token_pos_t& pos, const TypenameInfo& ur, const std::vector<Expr*>& exprs)
      : Expr(ExprKind::NewArrayExpr, pos),
        type_(ur),
        exprs_(exprs)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::NewArrayExpr; }

    Type* type() const { return type_.type(); }
    TypenameInfo& type_info() { return type_; }
    PoolArray<Expr*>& exprs() { return exprs_; }
    const TypenameInfo& type_info() const { return type_; }
    bool autozero() const { return autozero_; }
    void set_no_autozero() { autozero_ = false; }
    bool analyzed() const { return analyzed_.isValid(); }
    bool analysis_result() const { return analyzed_.get(); }
    void set_analysis_result(bool value) { analyzed_.init(value); }

  private:
    TypenameInfo type_;
    PoolArray<Expr*> exprs_;
    bool autozero_ = true;
    ke::Maybe<bool> analyzed_;
};

class ArrayExpr final : public Expr
{
  public:
    ArrayExpr(const token_pos_t& pos, const std::vector<Expr*>& exprs, bool ellipses)
      : Expr(ExprKind::ArrayExpr, pos),
        ellipses_(ellipses),
        exprs_(exprs)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext&) override {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::ArrayExpr; }

    PoolArray<Expr*>& exprs() { return exprs_; }
    bool ellipses() const { return ellipses_; }
    void set_ellipses() { ellipses_ = true; }
    bool synthesized_for_compat() const { return synthesized_for_compat_; }
    void set_synthesized_for_compat() { synthesized_for_compat_ = true; }

  private:
    bool ellipses_ = false;
    bool synthesized_for_compat_ = false;
    PoolArray<Expr*> exprs_;
};

struct StructInitFieldExpr : public Expr {
    StructInitFieldExpr(Atom* name, Expr* value, const token_pos_t& pos)
      : Expr(ExprKind::StructInitFieldExpr, pos),
        name(name), value(value)
    {}

    void ProcessUses(SemaContext& sc) override { value->ProcessUses(sc); }

    static bool is_a(Expr* node) { return node->kind() == ExprKind::StructInitFieldExpr; }

    Atom* name;
    Expr* value;
};

class StructExpr final : public Expr
{
  public:
    explicit StructExpr(const token_pos_t& pos)
      : Expr(ExprKind::StructExpr, pos)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {
        for (const auto& field : fields_)
            field->ProcessUses(sc);
    }

    static bool is_a(Expr* node) { return node->kind() == ExprKind::StructExpr; }

    PoolList<StructInitFieldExpr*>& fields() {
        return fields_;
    }

  private:
    PoolList<StructInitFieldExpr*> fields_;
};

class IfStmt : public Stmt
{
  public:
    explicit IfStmt(const token_pos_t& pos, Expr* cond, Stmt* on_true, Stmt* on_false)
      : Stmt(StmtKind::IfStmt, pos),
        cond_(cond),
        on_true_(on_true),
        on_false_(on_false)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::IfStmt; }

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
      : Stmt(StmtKind::ExprStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }

    void ProcessUses(SemaContext& sc) override {
        expr_->ProcessDiscardUses(sc);
    }

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ExprStmt; }

    Expr* expr() const { return expr_; }

  private:
    Expr* expr_;
};

class ReturnStmt : public Stmt
{
  public:
    explicit ReturnStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(StmtKind::ReturnStmt, pos),
        expr_(expr)
    {
        set_flow_type(Flow_Return);
    }

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ReturnStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

  private:
    bool CheckArrayReturn(SemaContext& sc);

  private:
    Expr* expr_;
};

class AssertStmt : public Stmt
{
  public:
    explicit AssertStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(StmtKind::AssertStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }

    void ProcessUses(SemaContext& sc) override {
        expr_->MarkAndProcessUses(sc);
    }

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::AssertStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

  private:
    Expr* expr_;
};

class DeleteStmt : public Stmt
{
  public:
    explicit DeleteStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(StmtKind::DeleteStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override { return expr_->Bind(sc); }

    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::DeleteStmt; }

    Expr* expr() const { return expr_; }
    MethodmapDecl* map() const { return map_; }
    void set_map(MethodmapDecl* map) { map_ = map; }

  private:
    Expr* expr_;
    MethodmapDecl* map_;
};

class ExitStmt : public Stmt
{
  public:
    explicit ExitStmt(const token_pos_t& pos, Expr* expr)
      : Stmt(StmtKind::ExitStmt, pos),
        expr_(expr)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ExitStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

  private:
    Expr* expr_;
};

class DoWhileStmt : public Stmt
{
  public:
    explicit DoWhileStmt(const token_pos_t& pos, int token, Expr* cond, Stmt* body)
      : Stmt(StmtKind::DoWhileStmt, pos),
        token_(token),
        cond_(cond),
        body_(body)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::DoWhileStmt; }

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
      : Stmt(StmtKind::ForStmt, pos),
        scope_(nullptr),
        init_(init),
        cond_(cond),
        advance_(advance),
        body_(body)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ForStmt; }

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
      : Stmt(StmtKind::SwitchStmt, pos),
        expr_(expr),
        default_case_(default_case),
        cases_(std::move(cases))
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::SwitchStmt; }

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
    PragmaUnusedStmt(const token_pos_t& pos, const std::vector<Atom*>& names)
      : Stmt(StmtKind::PragmaUnusedStmt, pos),
        names_(names)
    {}

    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::PragmaUnusedStmt; }

    PoolArray<Atom*>& names() { return names_; }
    PoolArray<VarDeclBase*>& symbols() { return symbols_; }

  private:
    PoolArray<Atom*> names_;
    PoolArray<VarDeclBase*> symbols_;
};

class FunctionDecl : public Decl
{
  public:
    FunctionDecl(const token_pos_t& pos, const declinfo_t& decl)
      : FunctionDecl(StmtKind::FunctionDecl, pos, decl)
    {}
    FunctionDecl(StmtKind kind, const token_pos_t& pos, const declinfo_t& decl);

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    void AddReferenceTo(FunctionDecl* other);

    static bool is_a(Stmt* node) {
        return node->kind() == StmtKind::FunctionDecl ||
               node->kind() == StmtKind::MemberFunctionDecl ||
               node->kind() == StmtKind::MethodmapMethodDecl;
    }

    bool IsVariadic() const;
    int FindNamedArg(Atom* name) const;
    bool MustReturnValue() const;

    const token_pos_t& end_pos() const { return end_pos_; }
    void set_end_pos(const token_pos_t& end_pos) { end_pos_ = end_pos; }

    Type* this_type() const { return this_type_; }
    void set_this_type(Type* type) {
        assert(type);
        this_type_ = type;
    }

    Stmt* body() const { return body_; }
    void set_body(Stmt* body) { body_ = body; }

    TokenCache* tokens() const { return tokens_; }
    void set_tokens(TokenCache* tokens) { tokens_ = tokens; }

    void set_name(Atom* name) { name_ = name; }

    // The undecorated name.
    Atom* decl_name() const { return decl_.name; }

    // Return the prototype version of this function, or |this| if there is
    // only one definition.
    FunctionDecl* prototype();

    // Return the implementation version of this function, or |this| if there is
    // only one definition. If no version has a body, this returns nullptr.
    FunctionDecl* impl();

    // Returns impl ? impl : prototype.
    FunctionDecl* canonical();

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

    PoolArray<ArgDecl*>& args() { return args_; }
    const token_pos_t& pos() const { return pos_; }

    declinfo_t& decl() { return decl_; }
    const declinfo_t& decl() const { return decl_; }

    Type* type() const override { return return_type(); }
    Type* return_type() const { return decl_.type.type; }

    // Only to be called when updating the type for return arrays.
    // This should be removed when arrays are fully dynamic, or if type
    // resolution becomes fully recursive.
    void update_return_type(Type* type) { decl_.type.type = type; }

    const typeinfo_t& type_info() const { return decl_.type; }
    typeinfo_t& mutable_type_info() { return decl_.type; }

    bool is_analyzing() const { return is_analyzing_; }
    void set_is_analyzing(bool val) { is_analyzing_ = val; }
    bool is_analyzed() const { return analyzed_; }
    bool analysis_status() const { return analyze_result_; }
    void set_analyzed(bool val) {
        analyzed_ = true;
        analyze_result_ = val;
    }
    bool retvalue_used() const { return retvalue_used_; }
    void set_retvalue_used() { retvalue_used_ = true; }
    bool is_callback() const { return is_callback_; }
    void set_is_callback() { is_callback_ = true; }
    bool returns_value() const { return returns_value_; }
    void set_returns_value(bool value) { returns_value_ = value; }
    bool always_returns() const { return always_returns_; }
    void set_always_returns(bool value) { always_returns_ = value; }
    bool is_live() const { return is_live_; }
    void set_is_live() { is_live_ = true; }
    bool maybe_used() const { return maybe_used_; }
    void set_maybe_used() { maybe_used_ = true; }

    void set_deprecate(const std::string& deprecate) { deprecate_ = new PoolString(deprecate); }
    const char* deprecate() const {
        return deprecate_ ? deprecate_->chars() : nullptr;
    }

    SymbolScope* scope() const { return scope_; }

    void CheckReturnUsage();
    bool IsVariadic();

    struct ReturnArrayInfo : public PoolObject {
        cell_t hidden_address = 0;
        cell_t iv_size = 0;
        cell_t dat_addr = 0;
        cell_t zeroes = 0;
    };
    ReturnArrayInfo* return_array() const { return return_array_; }
    void set_return_array(ReturnArrayInfo* base) { return_array_ =  base; }

    const PoolForwardList<FunctionDecl*>* refers_to() const {
        return refers_to_;
    }

    struct CGInfo : public PoolObject {
        tr::vector<tr::string>* dbgstrs = nullptr;
        Label label;     // modern replacement for addr
        Label funcid;
        int max_local_stack = 0;
        int max_callee_stack = 0;
        uint32_t pcode_end = 0;
    };
    CGInfo* cg();

  protected:
    bool BindArgs(SemaContext& sc);
    FunctionDecl* CanRedefine(Decl* other);
    Atom* NameForOperator();

  protected:
    token_pos_t end_pos_;
    declinfo_t decl_;
    Stmt* body_ = nullptr;
    PoolArray<ArgDecl*> args_;
    SymbolScope* scope_ = nullptr;
    Type* this_type_ = nullptr;
    PoolString* deprecate_ = nullptr;
    TokenCache* tokens_ = nullptr;
    FunctionDecl* proto_or_impl_ = nullptr;
    ReturnArrayInfo* return_array_ = nullptr;

    // Other symbols that this symbol refers to.
    PoolForwardList<FunctionDecl*>* refers_to_ = nullptr;

    // Set during codegen.
    CGInfo* cg_ = nullptr;

    bool analyzed_ SP_BITFIELD(1);
    bool analyze_result_ SP_BITFIELD(1);
    bool is_public_ SP_BITFIELD(1);
    bool is_static_ SP_BITFIELD(1);
    bool is_stock_ SP_BITFIELD(1);
    bool is_forward_ SP_BITFIELD(1);
    bool is_native_ SP_BITFIELD(1);
    bool is_analyzing_ SP_BITFIELD(1);
    bool explicit_return_type_ SP_BITFIELD(1);
    bool retvalue_used_ SP_BITFIELD(1);
    bool is_callback_ SP_BITFIELD(1);
    bool returns_value_ SP_BITFIELD(1);  // whether any path returns a value
    bool always_returns_ SP_BITFIELD(1); // whether all paths have an explicit return statement
    bool is_live_ SP_BITFIELD(1);        // must have code generated/linkage
    bool maybe_used_ SP_BITFIELD(1);     // not necessarily live, but do not warn if unused.
    bool checked_one_signature SP_BITFIELD(1);
    bool compared_prototype_args SP_BITFIELD(1);
};

class LayoutDecl : public Decl
{
  public:
    explicit LayoutDecl(StmtKind kind, const token_pos_t& pos, Atom* name)
      : Decl(kind, pos, name)
    {}

    static bool is_a(Stmt* node) {
        return node->kind() == StmtKind::MethodmapDecl ||
               node->kind() == StmtKind::EnumStructDecl;
    }
};

class MemberFunctionDecl : public FunctionDecl
{
  public:
    MemberFunctionDecl(const token_pos_t& pos, LayoutDecl* parent, const declinfo_t& decl)
      : FunctionDecl(StmtKind::MemberFunctionDecl, pos, decl),
        parent_(parent)
    {}
    MemberFunctionDecl(StmtKind kind, const token_pos_t& pos, LayoutDecl* parent,
                       const declinfo_t& decl)
      : FunctionDecl(kind, pos, decl),
        parent_(parent)
    {}

    static bool is_a(Stmt* node) {
        return node->kind() == StmtKind::MemberFunctionDecl ||
               node->kind() == StmtKind::MethodmapMethodDecl;
    }

    LayoutDecl* parent() const { return parent_; }

  private:
    LayoutDecl* parent_;
};

class LayoutFieldDecl : public Decl
{
  public:
    LayoutFieldDecl(const token_pos_t& pos, const declinfo_t& decl)
      : Decl(StmtKind::LayoutFieldDecl, pos, decl.name),
        type_(decl.type)
    {}

    void ProcessUses(SemaContext& sc) override {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::LayoutFieldDecl; }

    const typeinfo_t& type_info() const { return type_; }
    typeinfo_t& mutable_type_info() { return type_; }
    Type* type() const override { return type_info().type; }

    cell_t offset() const { return offset_; }
    void set_offset(cell_t offset) { offset_ = offset; }

  private:
    typeinfo_t type_;
    cell_t offset_;
};

class EnumStructDecl : public LayoutDecl
{
  public:
    EnumStructDecl(const token_pos_t& pos, Atom* name)
      : LayoutDecl(StmtKind::EnumStructDecl, pos, name)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::EnumStructDecl; }

    PoolArray<FunctionDecl*>& methods() { return methods_; }
    PoolArray<LayoutFieldDecl*>& fields() { return fields_; }

    cell_t array_size() const { return array_size_; }
    Type* type() const override { return type_; }

  private:
    PoolArray<FunctionDecl*> methods_;
    PoolArray<LayoutFieldDecl*> fields_;
    Type* type_ = nullptr;
    cell_t array_size_ = 0;
};

class MethodmapPropertyDecl : public Decl {
  public:
    MethodmapPropertyDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type,
                          MemberFunctionDecl* getter, MemberFunctionDecl* setter)
      : Decl(StmtKind::MethodmapPropertyDecl, pos, name),
        type_(type),
        getter_(getter),
        setter_(setter)
    {}

    void ProcessUses(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::MethodmapPropertyDecl; }

    Type* property_type() const;

    const typeinfo_t& type_info() const { return type_; }
    typeinfo_t& mutable_type_info() { return type_; }
    Type* type() const override { return type_.type; }
    MemberFunctionDecl* getter() const { return getter_; }
    MemberFunctionDecl* setter() const { return setter_; }
    LayoutDecl* parent() const {
        return getter_ ? getter_->parent() : setter_->parent();
    }

  private:
    typeinfo_t type_;
    MemberFunctionDecl* getter_;
    MemberFunctionDecl* setter_;
};

class MethodmapDecl : public LayoutDecl
{
  public:
    explicit MethodmapDecl(const token_pos_t& pos, Atom* name, bool nullable, Atom* extends)
      : LayoutDecl(StmtKind::MethodmapDecl, pos, name),
        nullable_(nullable),
        is_bound_(false),
        extends_(extends)
    {}

    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;
    void ProcessUses(SemaContext& sc) override;

    static MethodmapDecl* LookupMethodmap(Decl* decl);

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::MethodmapDecl; }

    Decl* FindMember(Atom* name) const;

    PoolArray<MethodmapMethodDecl*>& methods() { return methods_; }
    PoolArray<MethodmapPropertyDecl*>& properties() { return properties_; }
    MethodmapDecl* parent() const { return parent_; }
    bool nullable() const { return nullable_; }
    bool is_bound() const { return is_bound_; }
    Type* type() const override { return type_; }
    MethodmapMethodDecl* ctor() const { return ctor_; }
    MethodmapMethodDecl* dtor() const { return dtor_; }

  private:
    bool BindGetter(SemaContext& sc, MethodmapPropertyDecl* prop);
    bool BindSetter(SemaContext& sc, MethodmapPropertyDecl* prop);

  private:
    bool nullable_ : 1;
    bool is_bound_ : 1;
    Atom* extends_;
    PoolArray<MethodmapPropertyDecl*> properties_;
    PoolArray<MethodmapMethodDecl*> methods_;
    MethodmapDecl* parent_ = nullptr;
    MethodmapMethodDecl* ctor_ = nullptr;
    MethodmapMethodDecl* dtor_ = nullptr;
    Type* type_ = nullptr;
};

class MethodmapMethodDecl : public MemberFunctionDecl {
  public:
    MethodmapMethodDecl(const token_pos_t& pos, const declinfo_t& decl, MethodmapDecl* parent,
                        bool is_ctor, bool is_dtor)
      : MemberFunctionDecl(StmtKind::MethodmapMethodDecl, pos, parent, decl),
        is_ctor_(is_ctor),
        is_dtor_(is_dtor)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::MethodmapMethodDecl; }

    bool is_ctor() const { return is_ctor_; }
    bool is_dtor() const { return is_dtor_; }

  private:
    bool is_ctor_ : 1;
    bool is_dtor_ : 1;
};

} // namespace cc
} // namespace sp
