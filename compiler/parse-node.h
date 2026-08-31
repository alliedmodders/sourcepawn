// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
//
#pragma once

#include <amtl/am-maybe.h>
#include <amtl/am-string.h>
#include <amtl/am-vector.h>

#include <bit>
#include <optional>
#include <tuple>
#include <variant>
#include <vector>

#include "ast-types.h"
#include "coercion-rules.h"
#include "lexer.h"
#include "sc.h"
#include "utils/pool-allocator.h"
#include "utils/string-pool.h"
#include "symbols.h"

namespace sp {
namespace cc {

class Expr;
class FunctionDecl;
class LayoutMemberDecl;
class LayoutFieldDecl;
class MemberFunctionDecl;
class MethodmapDecl;
class PropertyDecl;
class UpvarDecl;
class SemaContext;
class SymbolScope;
class VarDeclBase;
struct StructInitField;

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

    const token_pos_t& pos() const {
        return pos_;
    }

  protected:
    void error(const token_pos_t& pos, int number);

  private:
    // Hide this symbol. Calls to error(pos... will get more accurate as we
    // make adjustments.
    void error(int number) = delete;

  protected:
    token_pos_t pos_;
};

enum FlowType {
    Flow_None,
    Flow_Continue,
    Flow_Break,
    Flow_Return,
};

static inline bool IsTerminalFlow(FlowType type) {
    return type == Flow_Return || type == Flow_Break;
}

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

    // Return the last statement in a linear statement chain.
    virtual Stmt* GetLast() { return this; }

    FlowType flow_type() const { return flow_type_; }
    void set_flow_type(FlowType type) { flow_type_ = type; }

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

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

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

    PoolArray<FunctionDecl*>& global_ctors() { return global_ctors_; }

  private:
    StmtList* stmts_;
    PoolArray<FunctionDecl*> global_ctors_;
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

class GlobalInitStmt final : public Stmt
{
  public:
    explicit GlobalInitStmt(const token_pos_t& pos, const std::vector<VarDeclBase*>& vars)
      : Stmt(StmtKind::GlobalInitStmt, pos),
        vars_(vars)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::GlobalInitStmt; }

    bool Bind(SemaContext& sc) override { return true; }

    PoolArray<VarDeclBase*>& vars() { return vars_; }

  private:
    PoolArray<VarDeclBase*> vars_;
};

class BreakStmt : public Stmt
{
  public:
    explicit BreakStmt(const token_pos_t& pos)
      : Stmt(StmtKind::BreakStmt, pos)
    {
        set_flow_type(Flow_Break);
    }

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

    ExprVal ConstVal();

    char vclass();
    bool is_const();
    QualType type();

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

enum VarDeclFlags {
    VARDECL_DEFAULT = 0x0,
    VARDECL_PUBLIC = 0x1,
    VARDECL_STATIC = 0x2,
    VARDECL_STOCK  = 0x4,
    VARDECL_SHARED = 0x8,
};

inline VarDeclFlags operator|(VarDeclFlags a, VarDeclFlags b) {
    return VarDeclFlags(static_cast<int>(a) | static_cast<int>(b));
}
inline VarDeclFlags& operator|=(VarDeclFlags& a, VarDeclFlags b) {
    a = a | b;
    return a;
}

class VarDeclBase : public Decl
{
  public:
    VarDeclBase(StmtKind kind, const token_pos_t& pos, Atom* name, const typeinfo_t& type,
                int vclass, VarDeclFlags flags, Expr* initializer);

    bool Bind(SemaContext& sc) override;
    bool EnterNames(SemaContext& sc) override;

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
    QualType type() const { return type_.qualified(); }

    bool is_used() const { return is_read_ || is_written_; }

    bool is_emitted() const { return is_emitted_; }
    void set_is_emitted() { is_emitted_ = true; }
    bool is_shared() const { return is_shared_; }
    void set_is_shared() { is_shared_ = true; }
    bool is_captured() const { return is_captures_; }
    void set_is_captured() { is_captures_ = true; }

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
    bool is_shared_ : 1;
    bool is_captures_ : 1;
    bool already_bound_ : 1;
    bool is_emitted_ : 1;
    Label addr_;
};

class VarDecl : public VarDeclBase
{
  public:
    VarDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
            VarDeclFlags flags, Expr* initializer)
      : VarDeclBase(StmtKind::VarDecl, pos, name, type, vclass, flags, initializer)
    {}
    VarDecl(StmtKind kind, const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
            VarDeclFlags flags, Expr* initializer)
      : VarDeclBase(kind, pos, name, type, vclass, flags, initializer)
    {}

    static bool is_a(Stmt* node) {
        return node->kind() == StmtKind::VarDecl || node->kind() == StmtKind::ConstDecl;
    }
};

class ArgDecl : public VarDeclBase
{
  public:
    ArgDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
            VarDeclFlags flags, Expr* initializer)
      : VarDeclBase(StmtKind::ArgDecl, pos, name, type, vclass, flags, initializer)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ArgDecl; }
};

class ConstDecl : public VarDecl
{
  public:
    ConstDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type, int vclass,
              Expr* expr)
       : VarDecl(StmtKind::ConstDecl, pos, name, type, vclass, VARDECL_DEFAULT, nullptr),
        expr_(expr),
        already_bound_(false)
    {}

    bool Bind(SemaContext& sc) override;
    bool EnterNames(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ConstDecl; }

    const ExprVal& value() const { return value_; }

  private:
    Expr* expr_;
    ExprVal value_;
    bool already_bound_ : 1;
};

class EnumFieldDecl : public Decl
{
  public:
    EnumFieldDecl(const token_pos_t& pos, Atom* name, Expr* value)
      : Decl(StmtKind::EnumFieldDecl, pos, name),
        value_(value)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::EnumFieldDecl; }

    Expr* value() const { return value_; }
    QualType type() const { return QualType(type_); }
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

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::EnumDecl; }

    PoolArray<EnumFieldDecl*>& fields() { return fields_; }
    int increment() const { return increment_; }
    int multiplier() const { return multiplier_; }
    int array_size() const { return array_size_; }
    QualType type() const { return QualType(type_); }

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
    PstructDecl(const token_pos_t& pos, Atom* name, const std::vector<LayoutFieldDecl*>& fields);

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::PstructDecl; }

    LayoutFieldDecl* FindField(Atom* name);

    PoolArray<LayoutFieldDecl*>& fields() { return fields_; }

    QualType type() const { return QualType(type_); }
    void set_type(Type* type) { type_ = type; }

  protected:
    PoolArray<LayoutFieldDecl*> fields_;
    Type* type_ = nullptr;
};

struct TypedefInfo : public PoolObject {
    TypedefInfo(const token_pos_t& pos, const TypenameInfo& ret_type,
                const std::vector<declinfo_t*>& args, FunctionType::Convention conv)
     : pos(pos),
       ret_type(ret_type),
       args(args),
       conv(conv)
    {}
    token_pos_t pos;
    TypenameInfo ret_type;
    PoolArray<declinfo_t*> args;
    FunctionType::Convention conv;

    FunctionType* Bind(SemaContext& sc);
};

class TypedefDecl : public Decl
{
  public:
    TypedefDecl(const token_pos_t& pos, Atom* name, TypedefInfo* type)
      : Decl(StmtKind::TypedefDecl, pos, name),
        type_(type)
    {}
    TypedefDecl(const token_pos_t& pos, Atom* name, typeinfo_t* ti)
      : Decl(StmtKind::TypedefDecl, pos, name),
        ti_(ti)
    {}

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::TypedefDecl; }

    TypedefInfo* typedef_info() const { return type_; }
    typeinfo_t* ti() const { return ti_; }

  private:
    TypedefInfo* type_ = nullptr;
    typeinfo_t* ti_ = nullptr;
    Type* placeholder_ = nullptr;
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

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

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
        kind_(kind)
    {}

    // Flatten a series of binary expressions into a single list.
    void FlattenLogical(int token, std::vector<Expr*>* out);

    // Fold the expression into a constant. The expression must have been
    // bound and analyzed. False indicates the expression is non-constant.
    //
    // If an expression folds constants during analysis, it can return false
    // here. ExprToConst handles both cases.
    bool FoldToConstant();

    // Evaluate as a constant. Returns false if non-const. This is a wrapper
    // around FoldToConstant().
    bool EvalConst(cell* value, Type** type);

    // Return whether or not the expression is idempotent (eg has side effects).
    bool HasSideEffects();

    // Return whether or not this Expr handles EMIT_DISCARD_RESULT.
    bool HandlesDiscardResult();

    ExprVal& val() { return val_; }
    const ExprVal& val() const { return val_; }

    // Returns whether this is an l-value (eg can appear on the left-hand
    // side of an assignment).
    inline bool lvalue() const;

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

    template <class T> static T* As(Expr* e) {
        return e ? e->as<T>() : nullptr;
    }

  protected:
    ExprVal val_ = {};
    ExprKind kind_ : 8;
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

    static bool is_a(Expr* node) { return node->kind() == ExprKind::UnaryExpr; }

    int token() const { return token_; }
    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

  private:
    int token_;
    Expr* expr_;
};

class BinaryExprBase : public Expr
{
  public:
    BinaryExprBase(ExprKind kind, const token_pos_t& pos, int token, Expr* left, Expr* right);

    bool Bind(SemaContext& sc) override;

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

    bool FoldToConstant();

    static bool is_a(Expr* node) { return node->kind() == ExprKind::BinaryExpr; }

    void set_initializer() { initializer_ = true; }
    bool initializer() const { return initializer_; }

  private:
    bool ValidateAssignmentLHS();
    bool ValidateAssignmentRHS(SemaContext& sc);

  private:
    bool initializer_ = false;
};

class LogicalExpr final : public BinaryExprBase
{
  public:
    LogicalExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
      : BinaryExprBase(ExprKind::LogicalExpr, pos, token, left, right)
    {}

    void FlattenLogical(int token, std::vector<Expr*>* out);

    static bool is_a(Expr* node) { return node->kind() == ExprKind::LogicalExpr; }
};

struct CompareOp
{
    CompareOp(const token_pos_t& pos, int token, Expr* expr);

    token_pos_t pos;
    int token;
    Expr* expr;
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
    bool FoldToConstant();

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
        return expr_->BindLval(sc);
    }

    static bool is_a(Expr* node) { return node->kind() == ExprKind::IncDecExpr; }

    int token() const { return token_; }
    Expr* expr() const { return expr_; }
    bool prefix() const { return prefix_; }

  protected:
    int token_;
    Expr* expr_;
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
    bool FoldToConstant();

    static bool is_a(Expr* node) { return node->kind() == ExprKind::CastExpr; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }
    const auto& type_info() const { return type_; }
    Type* type() const { return type_.type(); }
    int token() const { return token_; }

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

    static bool is_a(Expr* node) { return node->kind() == ExprKind::SymbolExpr; }

    Decl* decl() const { return decl_; }
    void set_decl(Decl* decl) { decl_ = decl; }
    Atom* name() const { return name_; }

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

    static bool is_a(Expr* node) { return node->kind() == ExprKind::NamedArgExpr; }

    Atom* name;
    Expr* expr;
};

using CallTarget = std::variant<std::monostate, FunctionDecl*, FunctionType*, Expr*>;

class CallExpr final : public Expr
{
  public:
    CallExpr(const token_pos_t& pos, int token, Expr* target, const std::vector<Expr*>& args)
      : Expr(ExprKind::CallExpr, pos),
        token_(token),
        target_(target),
        args_(args)
    {}
    CallExpr(const token_pos_t& pos, int token, FunctionDecl* target, const std::vector<Expr*>& args)
      : Expr(ExprKind::CallExpr, pos),
        token_(token),
        target_(nullptr),
        args_(args),
        resolved_target_(target)
    {}

    bool Bind(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::CallExpr; }

    PoolArray<Expr*>& args() { return args_; }
    Expr* target() const { return target_; }
    void set_target(Expr* target) { target_ = target; }
    int token() const { return token_; }

    FunctionDecl* fun() const {
        if (auto p = std::get_if<FunctionDecl*>(&resolved_target_))
            return *p;
        return nullptr;
    }
    void set_callee(FunctionDecl* fun) { resolved_target_ = fun; }
    void set_callee(FunctionType* ft) { resolved_target_ = ft; }
    const CallTarget& callee() const { return resolved_target_; }
    FunctionType* callee_type();

    Expr* implicit_this() const {
        if (auto p = std::get_if<Expr*>(&implicit_this_))
            return *p;
        return nullptr;
    }
    void set_implicit_this(Expr* expr) { implicit_this_ = expr; }

    Type* ctor_type() const {
        if (auto p = std::get_if<Type*>(&implicit_this_))
            return *p;
        return nullptr;
    }
    void set_ctor_type(Type* type) { implicit_this_ = type; }

  private:
    bool ProcessArg(SemaContext& sc, VarDecl* arg, Expr* param, unsigned int pos);

    int token_;
    Expr* target_;
    PoolArray<Expr*> args_;
    CallTarget resolved_target_;
    std::variant<std::monostate, Expr*, Type*> implicit_this_;
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

class DefaultArgExpr final : public Expr
{
  public:
    DefaultArgExpr(const token_pos_t& pos, ArgDecl* arg);

    bool Bind(SemaContext& sc) override { return true; }

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
    explicit RvalueExpr(Expr* lval);

    static bool is_a(Expr* node) { return node->kind() == ExprKind::RvalueExpr; }

    Expr* lval() const { return lval_; }

  private:
    Expr* lval_;
};

class SliceExpr final : public EmitOnlyExpr
{
  public:
    explicit SliceExpr(Expr* expr, Expr* index, Type* type);

    static bool is_a(Expr* node) { return node->kind() == ExprKind::SliceExpr; }

    Expr* expr() const { return expr_; }
    Expr* index() const { return index_; }

  private:
    Expr* expr_;
    Expr* index_;
};

class SimpleCastExpr final : public EmitOnlyExpr
{
  public:
    SimpleCastExpr(Expr* from, Type* to);

    static bool is_a(Expr* node) { return node->kind() == ExprKind::SimpleCastExpr; }
    bool FoldToConstant();

    Expr* from() const { return from_; }
    Type* to() const { return to_; }

  private:
    Expr* from_;
    Type* to_;
};

class CommaExpr final : public Expr
{
  public:
    CommaExpr(const token_pos_t& pos, const std::vector<Expr*>& exprs)
      : Expr(ExprKind::CommaExpr, pos),
        exprs_(exprs)
    {}

    bool Bind(SemaContext& sc) override;

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

    static bool is_a(Expr* node) { return node->kind() == ExprKind::NullExpr; }
};

class NumberExpr : public Expr
{
  public:
    NumberExpr(const token_pos_t& pos, Type* type, cell value)
      : Expr(ExprKind::NumberExpr, pos)
    {
        val_.set_constval(type, value);
    }
    NumberExpr(const token_pos_t& pos, Type* type, int64_t value)
      : Expr(ExprKind::NumberExpr, pos)
    {
        val_.set_const_int64(type, value);
    }
    NumberExpr(const token_pos_t& pos, Type* type, double value)
      : Expr(ExprKind::NumberExpr, pos)
    {
        val_.set_const_double(type, value);
    }

    static bool is_a(Expr* node) { return node->kind() == ExprKind::NumberExpr; }
    Type* type() const { return val_.type(); }
};

class StringExpr final : public Expr
{
  public:
    StringExpr(const token_pos_t& pos, Atom* atom)
      : Expr(ExprKind::StringExpr, pos),
        text_(atom)
    {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::StringExpr; }

    Atom* text() const {
        return text_;
    }
    void set_text(Atom* text) { text_ = text; }

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

class StructInitFieldExpr final : public Expr {
  public:
    StructInitFieldExpr(Atom* name, Expr* value, const token_pos_t& pos)
      : Expr(ExprKind::StructInitFieldExpr, pos),
        name(name), value(value)
    {}

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

    static bool is_a(Expr* node) { return node->kind() == ExprKind::StructExpr; }

    PoolList<StructInitFieldExpr*>& fields() {
        return fields_;
    }

  private:
    PoolList<StructInitFieldExpr*> fields_;
};

class SpreadArgsExpr final : public Expr {
  public:
    explicit SpreadArgsExpr(const token_pos_t& pos)
      : Expr(ExprKind::SpreadArgsExpr, pos)
    {}

    static bool is_a(Expr* node) { return node->kind() == ExprKind::SpreadArgsExpr; }
};

class FunctionExpr final : public Expr
{
  public:
    FunctionExpr(const token_pos_t& pos, FunctionDecl* decl)
      : Expr(ExprKind::FunctionExpr, pos),
        decl_(decl)
    {}

    bool Bind(SemaContext& sc) override;

    static bool is_a(Expr* node) { return node->kind() == ExprKind::FunctionExpr; }

    FunctionDecl* decl() const { return decl_; }

  private:
    FunctionDecl* decl_;
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

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ExprStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

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

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ReturnStmt; }

    Expr* expr() const { return expr_; }
    Expr* set_expr(Expr* expr) { return expr_ = expr; }

  private:
    bool CheckArrayReturn(SemaContext& sc);

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

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::DeleteStmt; }

    Expr* expr() const { return expr_; }
    MethodmapDecl* map() const { return map_; }
    void set_map(MethodmapDecl* map) { map_ = map; }

  private:
    Expr* expr_;
    MethodmapDecl* map_;
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

    void AddReferenceTo(FunctionDecl* other);

    static bool is_a(Stmt* node) {
        return node->kind() == StmtKind::FunctionDecl ||
               node->kind() == StmtKind::MemberFunctionDecl;
    }

    bool IsVariadic() const;
    uint32_t FormalArgc() const {
        return args_.size() - (IsVariadic() ? 1 : 0);
    }
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

    PoolList<Stmt*>& prebody() { return prebody_; }
    const PoolList<Stmt*>& prebody() const { return prebody_; }

    TokenCache* tokens() const { return tokens_; }
    void set_tokens(TokenCache* tokens) { tokens_ = tokens; }

    void set_name(Atom* name) { name_ = name; }
    int next_lambda_id() { return lambda_count_++; }

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

    void set_is_builtin() { is_builtin_ = true; }
    bool is_builtin() const { return is_builtin_; }

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

    QualType type() const { return QualType(signature_); }
    Type* return_type() const { return signature_->return_type().unqualified(); }
    FunctionType* signature() const { return signature_; }

    FunctionType* function_type() const { return signature_; }
    void set_function_type(FunctionType* ft) { signature_ = ft; }

    // Only to be called when updating the type for return arrays.
    // This should be removed when arrays are fully dynamic, or if type
    // resolution becomes fully recursive.
    void update_return_type(Type* type);

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
    bool is_live() const { return is_live_; }
    void set_is_live() { is_live_ = true; }
    bool is_global_ctor() const { return is_global_ctor_; }
    void set_is_global_ctor() { is_global_ctor_ = true; }
    bool maybe_used() const { return maybe_used_; }
    void set_maybe_used() { maybe_used_ = true; }

    void AddSharedVar(VarDeclBase* var);
    UpvarDecl* AddUpvar(const token_pos_t& pos, FunctionDecl* owner, VarDeclBase* var);
    LayoutFieldDecl* GetSharedVarField(VarDeclBase* var);
    size_t NumUpvars() const { return upvars_.size(); }
    UpvarDecl* GetUpvar(size_t index) const { return upvars_[index]; }
    UpvarDecl* FindUpvarDecl(VarDeclBase* var) const;

    // Adds shared object handles as copy-capture upvars, so codegen treats them uniformly.
    void AddUpvarsForSharedObjects();

    bool CheckUpvarTypes();

    FunctionDecl* outer() const { return outer_; }

    void set_deprecate(const std::string& deprecate) { deprecate_ = new PoolString(deprecate); }
    const char* deprecate() const {
        return deprecate_ ? deprecate_->chars() : nullptr;
    }

    SymbolScope* scope() const { return scope_; }

    void CheckReturnUsage();
    bool IsVariadic();
    bool GenerateSharedClass(SemaContext& sc);
    void UpdateSharedClassFieldTypes();

    struct ReturnArrayInfo : public PoolObject {
        cell_t iv_size = 0;
        cell_t dat_addr = 0;
        cell_t zeroes = 0;
    };
    ReturnArrayInfo* return_array() const { return return_array_; }
    void set_return_array(ReturnArrayInfo* base) { return_array_ =  base; }

    int32_t num_int64_slots() const { return num_int64_slots_; }
    void set_num_int64_slots(int32_t num_int64_slots) { num_int64_slots_ = num_int64_slots; }
    int32_t num_int32_slots() const { return num_int32_slots_; }
    void set_num_int32_slots(int32_t num_int32_slots) { num_int32_slots_ = num_int32_slots; }

    const PoolForwardList<FunctionDecl*>* refers_to() const {
        return refers_to_;
    }

    struct CGInfo : public PoolObject {
        Label method_id;
        bool in_queue = false;
    };
    CGInfo* cg();

    // Generated shared class and hidden local for captured shared vars.
    ClassDecl* shared_class() const { return shared_class_; }
    VarDeclBase* shared_object() const { return shared_object_; }

  protected:
    bool BindArgs(SemaContext& sc);
    FunctionDecl* CanRedefine(Decl* other);

  protected:
    token_pos_t end_pos_;
    declinfo_t decl_;
    PoolList<Stmt*> prebody_;
    Stmt* body_ = nullptr;
    PoolArray<ArgDecl*> args_;
    SymbolScope* scope_ = nullptr;
    Type* this_type_ = nullptr;
    FunctionType* signature_ = nullptr;
    PoolString* deprecate_ = nullptr;
    TokenCache* tokens_ = nullptr;
    FunctionDecl* proto_or_impl_ = nullptr;
    ReturnArrayInfo* return_array_ = nullptr;

    // Other symbols that this symbol refers to.
    PoolForwardList<FunctionDecl*>* refers_to_ = nullptr;

    // Enclosing function (immediate parent in the nesting chain).
    FunctionDecl* outer_ = nullptr;

    // Variables this function has copy-captured from an outer function.
    PoolList<UpvarDecl*> upvars_;
    PoolMap<VarDeclBase*, UpvarDecl*> upvar_decls_;

    // Local variables that were captured by reference in inner functions.
    PoolMap<VarDeclBase*, LayoutFieldDecl*> shared_vars_;
    PoolList<VarDeclBase*> shared_var_list_;

    // Generated shared class and hidden local for captured shared vars.
    ClassDecl* shared_class_ = nullptr;
    VarDeclBase* shared_object_ = nullptr;

    // Set during codegen.
    CGInfo* cg_ = nullptr;

    int32_t num_int64_slots_ = 0;
    int32_t num_int32_slots_ = 0;

    bool analyzed_ SP_BITFIELD(1);
    bool analyze_result_ SP_BITFIELD(1);
    bool is_public_ SP_BITFIELD(1);
    bool is_static_ SP_BITFIELD(1);
    bool is_stock_ SP_BITFIELD(1);
    bool is_forward_ SP_BITFIELD(1);
    bool is_native_ SP_BITFIELD(1);
    bool is_builtin_ SP_BITFIELD(1);
    bool is_analyzing_ SP_BITFIELD(1);
    bool explicit_return_type_ SP_BITFIELD(1);
    bool retvalue_used_ SP_BITFIELD(1);
    bool is_callback_ SP_BITFIELD(1);
    bool returns_value_ SP_BITFIELD(1);  // whether any path returns a value
    int lambda_count_ = 0;
    bool is_live_ SP_BITFIELD(1);        // must have code generated/linkage
    bool is_global_ctor_ SP_BITFIELD(1); // global constructor (.init)
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
               node->kind() == StmtKind::EnumStructDecl ||
               node->kind() == StmtKind::ClassDecl;
    }

    Decl* FindMember(Atom* name);

    PoolArray<PropertyDecl*>& properties() { return properties_; }
    const PoolArray<PropertyDecl*>& properties() const { return properties_; }

    PoolArray<MemberFunctionDecl*>& methods() { return methods_; }
    const PoolArray<MemberFunctionDecl*>& methods() const { return methods_; }

    PoolArray<LayoutFieldDecl*>& fields() { return fields_; }
    const PoolArray<LayoutFieldDecl*>& fields() const { return fields_; }

  protected:
    bool BindGetter(SemaContext& sc, PropertyDecl* prop, Type* type);
    bool BindSetter(SemaContext& sc, PropertyDecl* prop, Type* type);

    PoolArray<PropertyDecl*> properties_;
    PoolArray<MemberFunctionDecl*> methods_;
    PoolArray<LayoutFieldDecl*> fields_;
};

class LayoutMemberDecl : public Decl
{
  public:
    LayoutMemberDecl(StmtKind kind, const token_pos_t& pos, Atom* name)
      : Decl(kind, pos, name),
        is_private_(false)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::LayoutFieldDecl; }

    bool is_private() const { return is_private_; }
    void set_is_private() { is_private_ = true; }

  private:
    bool is_private_ : 1;
};

class MemberFunctionDecl : public FunctionDecl
{
  public:
    MemberFunctionDecl(const token_pos_t& pos, LayoutDecl* parent, const declinfo_t& decl,
                       bool is_ctor = false, bool is_dtor = false)
      : FunctionDecl(StmtKind::MemberFunctionDecl, pos, decl),
        parent_(parent),
        is_ctor_(is_ctor),
        is_dtor_(is_dtor),
        is_private_(false)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::MemberFunctionDecl; }

    LayoutDecl* parent() const { return parent_; }

    bool is_ctor() const { return is_ctor_; }
    bool is_dtor() const { return is_dtor_; }

    bool is_private() const { return is_private_; }
    void set_is_private() { is_private_ = true; }

  private:
    LayoutDecl* parent_;
    bool is_ctor_ : 1;
    bool is_dtor_ : 1;
    bool is_private_ : 1;
};

class LayoutFieldDecl : public LayoutMemberDecl
{
  public:
    LayoutFieldDecl(const token_pos_t& pos, const declinfo_t& decl, Decl* parent = nullptr)
      : LayoutMemberDecl(StmtKind::LayoutFieldDecl, pos, decl.name),
        type_(decl.type),
        parent_(parent)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::LayoutFieldDecl; }

    const typeinfo_t& type_info() const { return type_; }
    typeinfo_t& mutable_type_info() { return type_; }
    QualType type() const { return type_info().qualified(); }

    cell_t offset() const { return offset_; }
    void set_offset(cell_t offset) { offset_ = offset; }

    Decl* parent() const { return parent_; }
    void set_parent(Decl* parent) { parent_ = parent; }

  private:
    typeinfo_t type_;
    cell_t offset_;
    Decl* parent_ = nullptr;
};

class EnumStructDecl : public LayoutDecl
{
  public:
    EnumStructDecl(const token_pos_t& pos, Atom* name)
      : LayoutDecl(StmtKind::EnumStructDecl, pos, name)
    {}

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::EnumStructDecl; }

    QualType type() const { return QualType(type_); }

  private:
    Type* type_ = nullptr;
};

class ClassDecl : public LayoutDecl
{
  public:
    ClassDecl(const token_pos_t& pos, Atom* name)
      : LayoutDecl(StmtKind::ClassDecl, pos, name)
    {}

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::ClassDecl; }

    QualType type() const { return QualType(type_); }
    MemberFunctionDecl* ctor() const { return ctor_; }

    Decl* FindMember(Atom* name);

  private:
    Type* type_ = nullptr;
    MemberFunctionDecl* ctor_ = nullptr;
};

class PropertyDecl : public Decl {
  public:
    PropertyDecl(const token_pos_t& pos, Atom* name, const typeinfo_t& type,
                 MemberFunctionDecl* getter, MemberFunctionDecl* setter)
      : Decl(StmtKind::PropertyDecl, pos, name),
        type_(type),
        getter_(getter),
        setter_(setter)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::PropertyDecl; }

    Type* property_type() const;

    const typeinfo_t& type_info() const { return type_; }
    typeinfo_t& mutable_type_info() { return type_; }
    QualType type() const { return type_.qualified(); }
    MemberFunctionDecl* getter() const { return getter_; }
    MemberFunctionDecl* setter() const { return setter_; }
    LayoutDecl* parent() const {
        assert(getter_ || setter_);
        return getter_ ? getter_->parent() : setter_->parent();
    }

  private:
    typeinfo_t type_;
    MemberFunctionDecl* getter_;
    MemberFunctionDecl* setter_;
};

class UpvarDecl : public Decl
{
  public:
    UpvarDecl(const token_pos_t& pos, VarDeclBase* var, FunctionDecl* enclosure)
      : Decl(StmtKind::UpvarDecl, pos, var->name()),
        var_(var),
        enclosure_(enclosure)
    {}

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::UpvarDecl; }

    VarDeclBase* var() const { return var_; }
    FunctionDecl* enclosure() const { return enclosure_; }
    QualType type() const { return var_->type(); }

    uint16_t upvar_index() const { return upvar_index_; }
    void set_upvar_index(uint16_t index) { upvar_index_ = index; }

    uint16_t shared_obj_upvar_index() const { return shared_obj_upvar_index_; }
    void set_shared_obj_upvar_index(uint16_t index) { shared_obj_upvar_index_ = index; }

  private:
    VarDeclBase* var_;
    FunctionDecl* enclosure_;
    uint16_t upvar_index_ = 0;
    uint16_t shared_obj_upvar_index_ = 0;
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

    bool EnterTypes(SemaContext& sc);
    bool EnterNames(SemaContext& sc) override;
    bool Bind(SemaContext& sc) override;

    static MethodmapDecl* LookupMethodmap(Decl* decl);

    static bool is_a(Stmt* node) { return node->kind() == StmtKind::MethodmapDecl; }

    Decl* FindMember(Atom* name);

    MethodmapDecl* parent() const { return parent_; }
    bool nullable() const { return nullable_; }
    bool is_bound() const { return is_bound_; }
    QualType type() const { return QualType(type_); }
    MemberFunctionDecl* ctor() const { return ctor_; }
    MemberFunctionDecl* dtor() const { return dtor_; }
    Atom* extends() const { return extends_; }

  private:
    bool nullable_ : 1;
    bool is_bound_ : 1;
    Atom* extends_;
    MethodmapDecl* parent_ = nullptr;
    MemberFunctionDecl* ctor_ = nullptr;
    MemberFunctionDecl* dtor_ = nullptr;
    Type* type_ = nullptr;
};

inline bool Expr::lvalue() const {
    switch (val_.ident) {
        case iVARIABLE:
        case iACCESSOR:
        case iARRAYELEM:
        case iFIELD:
        case iADDRESS:
        case iUPVAR:
            if (kind() == ExprKind::RvalueExpr)
                return false;
            return true;
        default:
            return false;
    }
}

} // namespace cc
} // namespace sp
