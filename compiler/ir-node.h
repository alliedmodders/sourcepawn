// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2021-2026 AlliedModders LLC
//
#pragma once

#include <vector>

#include "ast-types.h"
#include "parse-node.h"
#include "pool-objects.h"
#include "constant-fold.h"
#include "value.h"

namespace sp {
namespace cc {
namespace ir {

inline bool IsLvalue(IrKind kind) {
    switch (kind) {
        case IrKind::Variable:
        case IrKind::Upvar:
        case IrKind::Index:
        case IrKind::FieldRef:
        case IrKind::Accessor:
        case IrKind::LvalueCast:
            return true;
        default:
            return false;
    }
}

class Value : public PoolObject
{
  public:
    explicit Value(IrKind kind, Expr* parent)
      : parent_(parent),
        kind_(kind)
    {}

    Value(IrKind kind, Expr* parent, const ExprVal& val)
      : parent_(parent),
        kind_(kind),
        val_(val)
    {}

    Expr* pn() const { return parent_; }
    IrKind kind() const { return kind_; }

    const ExprVal& val() const { return val_; }

    const token_pos_t& pos() const { return parent_->pos(); }

    bool lvalue() const {
        return IsLvalue(kind_);
    }

    bool is(IrKind k) const { return kind() == k; }

    template <class T> T* as() {
        if (T::is_a(this))
            return reinterpret_cast<T*>(this);
        return nullptr;
    }
    template <class T> T* to() {
        assert(T::is_a(this));
        return reinterpret_cast<T*>(this);
    }
    template <class T> static inline T* As(Value* node) {
        return node ? node->as<T>() : nullptr;
    }

  protected:
    Expr* parent_;
    IrKind kind_;
    ExprVal val_ = {};
};

class Lvalue : public Value
{
  public:
    static bool is_a(Value* node) { return IsLvalue(node->kind()); }

  protected:
    using Value::Value;
};

class Constant final : public Value
{
  public:
    Constant(Expr* parent, const ConstVal& value)
      : Value(IrKind::Constant, parent),
        value_(value)
    {
        val_.set_type(value.type);
    }

    const ConstVal& value() const { return value_; }

    bool is_intptr() const { return val().type()->isIntPtr(); }
    bool is_float() const { return val().type()->isFloat(); }
    bool is_double() const { return val().type()->isDouble(); }
    bool is_int64() const { return val().type()->isInt64(); }

    cell get_cell() const { return value_.get_cell(); }
    cell get_i32() const { return value_.get_i32(); }
    cell get_intptr() const { return value_.get_intptr(); }
    float get_float() const { return value_.get_float(); }
    double get_double() const { return value_.get_double(); }
    int64_t get_int64() const { return value_.get_int64(); }

    static bool is_a(Value* node) { return node->kind() == IrKind::Constant; }

  private:
    ConstVal value_;
};

class Rvalue final : public Value
{
  public:
    explicit Rvalue(Lvalue* operand);

    Lvalue* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Rvalue; }

  private:
    Lvalue* expr_;
};

class Typename final : public Value
{
  public:
    Typename(Expr* parent, Decl* decl)
      : Value(IrKind::Typename, parent),
        decl_(decl)
    {
        val_.set_type(decl->type());
    }

    Decl* decl() const { return decl_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Typename; }

  private:
    Decl* decl_;
};

class FunctionRef final : public Value
{
  public:
    FunctionRef(Expr* parent, FunctionDecl* fun)
      : Value(IrKind::FunctionRef, parent),
        fun_(fun)
    {
        val_.set_expr(fun->type());
    }

    FunctionDecl* decl() const { return fun_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::FunctionRef; }

  private:
    FunctionDecl* fun_;
};

class Variable final : public Lvalue
{
  public:
    Variable(Expr* parent, VarDeclBase* decl)
      : Lvalue(IrKind::Variable, parent),
        decl_(decl)
    {
        val_.set_type(decl->type());
    }

    VarDeclBase* decl() const { return decl_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Variable; }

  private:
    VarDeclBase* decl_;
};

class Upvar final : public Lvalue
{
  public:
    Upvar(Expr* parent, UpvarDecl* decl, QualType type)
      : Lvalue(IrKind::Upvar, parent),
        decl_(decl)
    {
        val_.set_type(type);
    }

    UpvarDecl* decl() const { return decl_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Upvar; }

  private:
    UpvarDecl* decl_;
};

class String final : public Value
{
  public:
    String(Expr* parent, const ExprVal& val = {})
      : Value(IrKind::String, parent, val)
    {}

    StringExpr* parent() const { return pn()->to<StringExpr>(); }

    static bool is_a(Value* node) { return node->kind() == IrKind::String; }
};

class Unary final : public Value
{
  public:
    Unary(Expr* parent, int token, Value* expr, const ExprVal& val)
      : Value(IrKind::Unary, parent, val),
        token_(token),
        expr_(expr)
    {}

    int token() const { return token_; }
    Value* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Unary; }

  private:
    int token_;
    Value* expr_;
};

class Index final : public Lvalue
{
  public:
    Index(Expr* parent, Value* base, Value* index, const ExprVal& val)
      : Lvalue(IrKind::Index, parent, val),
        base_(base),
        index_(index)
    {}

    Index(Expr* parent, Value* base, Value* index, Type* elem_type)
      : Lvalue(IrKind::Index, parent),
        base_(base),
        index_(index)
    {
        val_.set_type(QualType(elem_type));
    }

    Value* base() const { return base_; }
    Value* index() const { return index_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Index; }

  private:
    Value* base_;
    Value* index_;
};

class StaticFieldRef final : public Value
{
  public:
    StaticFieldRef(Expr* parent, Value* base, LayoutFieldDecl* field, Type* int_type)
      : Value(IrKind::StaticFieldRef, parent),
        base_(base),
        field_(field)
    {
        val_.set_expr(int_type);
    }

    Value* base() const { return base_; }
    LayoutFieldDecl* field() const { return field_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::StaticFieldRef; }

  private:
    Value* base_;
    LayoutFieldDecl* field_;
};

class FieldRef final : public Lvalue
{
  public:
    FieldRef(Expr* parent, int token, Value* base, LayoutFieldDecl* field)
      : Lvalue(IrKind::FieldRef, parent),
        token_(token),
        base_(base),
        field_(field)
    {
        val_.set_type(field->type());
    }

    int token() const { return token_; }
    Value* base() const { return base_; }
    LayoutFieldDecl* field() const { return field_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::FieldRef; }

  private:
    int token_;
    Value* base_;
    LayoutFieldDecl* field_;
};

// Helper to bind a member function call target.
class MethodRef final : public Value
{
  public:
    MethodRef(Expr* parent, int token, Value* base, MemberFunctionDecl* method)
      : Value(IrKind::MethodRef, parent),
        token_(token),
        base_(base),
        method_(method)
    {}

    int token() const { return token_; }
    Value* base() const { return base_; }
    MemberFunctionDecl* decl() const { return method_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::MethodRef; }

  private:
    int token_;
    Value* base_;
    MemberFunctionDecl* method_;
};

class Accessor final : public Lvalue
{
  public:
    Accessor(Expr* parent, int token, Value* base, PropertyDecl* prop)
      : Lvalue(IrKind::Accessor, parent),
        token_(token),
        base_(base),
        prop_(prop)
    {
        val_.set_type(prop->property_type());
    }

    int token() const { return token_; }
    Value* base() const { return base_; }
    PropertyDecl* accessor() const { return prop_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Accessor; }

  private:
    int token_;
    Value* base_;
    PropertyDecl* prop_;
};

class Cast final : public Value
{
  public:
    Cast(Expr* parent, Value* expr, const ExprVal& val)
      : Value(IrKind::Cast, parent, val),
        expr_(expr)
    {}

    Value* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Cast; }

  private:
    Value* expr_;
};

class LvalueCast final : public Lvalue
{
  public:
    LvalueCast(Expr* parent, Value* expr, const ExprVal& val)
      : Lvalue(IrKind::LvalueCast, parent, val),
        expr_(expr)
    {}

    Value* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::LvalueCast; }

  private:
    Value* expr_;
};

class SimpleCast final : public Value
{
  public:
    SimpleCast(Expr* parent, Value* from, Type* to)
      : Value(IrKind::SimpleCast, parent, ExpressionVal(to)),
        from_(from),
        to_(to)
    {}

    Value* from() const { return from_; }
    Type* to() const { return to_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::SimpleCast; }

  private:
    Value* from_;
    Type* to_;
};

class Sizeof final : public Value
{
  public:
    Sizeof(Expr* parent, Value* child, const ExprVal& val)
      : Value(IrKind::Sizeof, parent, val),
        child_(child)
    {}

    Value* child() const { return child_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Sizeof; }

  private:
    Value* child_;
};

class IncDec final : public Value
{
  public:
    IncDec(Expr* parent, int token, bool prefix, Lvalue* expr, const ExprVal& val)
      : Value(IrKind::IncDec, parent, val),
        token_(token),
        prefix_(prefix),
        expr_(expr)
    {}

    int token() const { return token_; }
    bool prefix() const { return prefix_; }
    Lvalue* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::IncDec; }

  private:
    int token_;
    bool prefix_;
    Lvalue* expr_;
};

class Binary final : public Value
{
  public:
    Binary(Expr* parent, int token, Value* left, Value* right, const ExprVal& val)
      : Value(IrKind::Binary, parent, val),
        token_(token),
        left_(left),
        right_(right)
    {}

    int token() const { return token_; }
    Value* left() const { return left_; }
    Value* right() const { return right_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Binary; }

  private:
    int token_;
    Value* left_;
    Value* right_;
};

class Logical final : public Value
{
  public:
    Logical(Expr* parent, int token, Value* left, Value* right, const ExprVal& val)
      : Value(IrKind::Logical, parent, val),
        token_(token),
        left_(left),
        right_(right)
    {}

    int token() const { return token_; }
    Value* left() const { return left_; }
    Value* right() const { return right_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Logical; }

  private:
    int token_;
    Value* left_;
    Value* right_;
};

class Ternary final : public Value
{
  public:
    Ternary(Expr* parent, Value* first, Value* second, Value* third,
            const ExprVal& val)
      : Value(IrKind::Ternary, parent, val),
        first_(first),
        second_(second),
        third_(third)
    {}

    Value* first() const { return first_; }
    Value* second() const { return second_; }
    Value* third() const { return third_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Ternary; }

  private:
    Value* first_;
    Value* second_;
    Value* third_;
};

class Comma final : public Value
{
  public:
    Comma(Expr* parent, std::vector<Value*> exprs)
      : Value(IrKind::Comma, parent, ExpressionVal(exprs.back()->val().qualified())),
        exprs_(std::move(exprs))
    {}

    const PoolArray<Value*>& exprs() const { return exprs_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Comma; }

  private:
    PoolArray<Value*> exprs_;
};

class ChainedCompare final : public Value
{
  public:
    struct Op
    {
        int token;
        Value* expr;
    };

    ChainedCompare(Expr* parent, Value* first, std::vector<Op> ops, const ExprVal& val)
      : Value(IrKind::ChainedCompare, parent, val),
        first_(first),
        ops_(std::move(ops))
    {}

    Value* first() const { return first_; }
    const PoolArray<Op>& ops() const { return ops_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::ChainedCompare; }

  private:
    Value* first_;
    PoolArray<Op> ops_;
};

class Call final : public Value
{
  public:
    Call(Expr* parent, Value* target, const std::vector<Value*>& args, const ExprVal& val)
      : Value(IrKind::Call, parent, val),
        target_(target),
        args_(args)
    {}

    Value* target() const { return target_; }
    const PoolArray<Value*>& args() const { return args_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Call; }

  private:
    Value* target_;
    PoolArray<Value*> args_;
};

class DefaultArg final : public Value
{
  public:
    explicit DefaultArg(Expr* parent)
      : Value(IrKind::DefaultArg, parent)
    {}

    DefaultArgExpr* parent() const { return pn()->to<DefaultArgExpr>(); }

    static bool is_a(Value* node) { return node->kind() == IrKind::DefaultArg; }
};

class NamedArg final : public Value
{
  public:
    NamedArg(Expr* parent, Value* expr, const ExprVal& val)
      : Value(IrKind::NamedArg, parent, val),
        expr_(expr)
    {}

    Value* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::NamedArg; }

  private:
    Value* expr_;
};

class SpreadArgs final : public Value
{
  public:
    explicit SpreadArgs(Expr* parent)
      : Value(IrKind::SpreadArgs, parent)
    {}

    static bool is_a(Value* node) { return node->kind() == IrKind::SpreadArgs; }
};

class Function final : public Value
{
  public:
    Function(Expr* parent, const ExprVal& val)
      : Value(IrKind::Function, parent, val)
    {}

    FunctionExpr* parent() const { return pn()->to<FunctionExpr>(); }

    static bool is_a(Value* node) { return node->kind() == IrKind::Function; }
};

class Array final : public Value
{
  public:
    Array(Expr* parent, const std::vector<Value*>& elements, bool ellipses, const ExprVal& val = {})
      : Value(IrKind::Array, parent, val),
        elements_(elements),
        ellipses_(ellipses)
    {}

    const PoolArray<Value*>& elements() const { return elements_; }
    bool ellipses() const { return ellipses_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Array; }

  private:
    PoolArray<Value*> elements_;
    bool ellipses_;
};

class Slice final : public Value
{
  public:
    Slice(Value* base, Value* index, Type* type)
      : Value(IrKind::Slice, base->pn(), ExpressionVal(type)),
        base_(base),
        index_(index)
    {}

    Value* base() const { return base_; }
    Value* index() const { return index_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Slice; }

  private:
    Value* base_;
    Value* index_;
};

class NewArray final : public Value
{
  public:
    NewArray(Expr* parent, const std::vector<Value*>& dims, const ExprVal& val = {})
      : Value(IrKind::NewArray, parent, val),
        dims_(dims)
    {}

    NewArrayExpr* parent() const { return pn()->to<NewArrayExpr>(); }

    const PoolArray<Value*>& dims() const { return dims_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::NewArray; }

  private:
    PoolArray<Value*> dims_;
};

class Struct final : public Value
{
  public:
    Struct(Expr* parent, std::vector<Value*> fields)
      : Value(IrKind::Struct, parent),
        fields_(std::move(fields))
    {}

    const PoolArray<Value*>& fields() const { return fields_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Struct; }

  private:
    PoolArray<Value*> fields_;
};

class StructInitField final : public Value
{
  public:
    StructInitField(Expr* parent, Value* value, const ExprVal& val)
      : Value(IrKind::StructInitField, parent, val),
        value_(value)
    {}

    Value* value() const { return value_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::StructInitField; }

  private:
    Value* value_;
};

} // namespace ir
} // namespace cc
} // namespace sp
