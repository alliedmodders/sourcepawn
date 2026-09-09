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
#include "value.h"

namespace sp {
namespace cc {
namespace ir {

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

    ExprVal& val() { return val_; }
    const ExprVal& val() const { return val_; }

    const token_pos_t& pos() const { return parent_->pos(); }

    bool lvalue() const {
        return val().is_lvalue() && kind_ != IrKind::Rvalue;
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

  protected:
    Expr* parent_;
    IrKind kind_;
    ExprVal val_ = {};
};

class Number final : public Value
{
  public:
    Number(Expr* parent, const ExprVal& val)
      : Value(IrKind::Number, parent, val)
    {}

    static bool is_a(Value* node) { return node->kind() == IrKind::Number; }
};

class Rvalue final : public Value
{
  public:
    explicit Rvalue(Value* operand);

    Value* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Rvalue; }

  private:
    Value* expr_;
};

class Symbol final : public Value
{
  public:
    explicit Symbol(Expr* parent)
      : Value(IrKind::Symbol, parent)
    {}

    static bool is_a(Value* node) { return node->kind() == IrKind::Symbol; }
};

class String final : public Value
{
  public:
    explicit String(Expr* parent)
      : Value(IrKind::String, parent)
    {}

    StringExpr* parent() const { return pn()->to<StringExpr>(); }

    static bool is_a(Value* node) { return node->kind() == IrKind::String; }
};

class This final : public Value
{
  public:
    explicit This(Expr* parent)
      : Value(IrKind::This, parent)
    {}

    static bool is_a(Value* node) { return node->kind() == IrKind::This; }
};

class Null final : public Value
{
  public:
    explicit Null(Expr* parent)
      : Value(IrKind::Null, parent)
    {}

    static bool is_a(Value* node) { return node->kind() == IrKind::Null; }
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

class Index final : public Value
{
  public:
    Index(Expr* parent, Value* base, Value* index)
      : Value(IrKind::Index, parent),
        base_(base),
        index_(index)
    {}

    Value* base() const { return base_; }
    Value* index() const { return index_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::Index; }

  private:
    Value* base_;
    Value* index_;
};

class FieldAccess final : public Value
{
  public:
    FieldAccess(Expr* parent, int token, Value* base, Decl* resolved)
      : Value(IrKind::FieldAccess, parent),
        token_(token),
        base_(base),
        resolved_(resolved)
    {}

    void set_base(Value* base) { base_ = base; }
    void set_resolved(Decl* decl) { resolved_ = decl; }

    int token() const { return token_; }
    Value* base() const { return base_; }
    Decl* resolved() const { return resolved_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::FieldAccess; }

  private:
    int token_;
    Value* base_;
    Decl* resolved_;
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

class SimpleCast final : public Value
{
  public:
    SimpleCast(Expr* parent, Value* from, Type* to)
      : Value(IrKind::SimpleCast, parent),
        from_(from),
        to_(to)
    {
        val().set_expr(to);
    }

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
    Sizeof(Expr* parent, Value* child)
      : Value(IrKind::Sizeof, parent),
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
    IncDec(Expr* parent, int token, bool prefix, Value* expr)
      : Value(IrKind::IncDec, parent),
        token_(token),
        prefix_(prefix),
        expr_(expr)
    {}

    int token() const { return token_; }
    bool prefix() const { return prefix_; }
    Value* expr() const { return expr_; }

    static bool is_a(Value* node) { return node->kind() == IrKind::IncDec; }

  private:
    int token_;
    bool prefix_;
    Value* expr_;
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
    Logical(Expr* parent, int token, Value* left, Value* right)
      : Value(IrKind::Logical, parent),
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
      : Value(IrKind::Comma, parent),
        exprs_(std::move(exprs))
    {
        val().set_expr(exprs_.back()->val().qualified());
    }

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

    ChainedCompare(Expr* parent, Value* first, std::vector<Op> ops)
      : Value(IrKind::ChainedCompare, parent),
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
    Array(Expr* parent, const std::vector<Value*>& elements, bool ellipses)
      : Value(IrKind::Array, parent),
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
      : Value(IrKind::Slice, base->pn()),
        base_(base),
        index_(index)
    {
        val().ident = iEXPRESSION;
        val().set_type(type);
    }

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
    NewArray(Expr* parent, const std::vector<Value*>& dims, const ExprVal& val)
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
