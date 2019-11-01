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
#include "shared/string-pool.h"

struct UserOperation
{
    UserOperation() {}

    symbol* sym = nullptr;
    void (*oper)() = nullptr;
    int paramspassed;
    bool savepri;
    bool savealt;
    int swapparams;
};

typedef void (*OpFunc)();

class BinaryExpr;
class DefaultArgExpr;
class SymbolExpr;

class ParseNode : public PoolObject
{
  public:
    virtual bool Bind() {
        return true;
    }
    virtual bool Analyze() = 0;
    virtual void Emit() = 0;
    virtual bool HasSideEffects() {
        return false;
    }
};

class Expr : public ParseNode
{
  public:
    explicit Expr(const token_pos_t& pos)
      : pos_(pos)
    {}

    // Flatten a series of binary expressions into a single list.
    virtual void FlattenLogical(int token, ke::Vector<Expr*>* out);

    virtual void EmitTest(bool jump_on_true, int taken, int fallthrough);
    virtual symbol* BindCallTarget(int token, Expr** implicit_this) {
        return nullptr;
    }
    virtual symbol* BindNewTarget() {
        return nullptr;
    }

    // Process any child nodes whose value is consumed.
    virtual void ProcessUses() = 0;

    // Mark the node's value as consumed.
    virtual void MarkUsed() {}

    void Emit() override;

    const value& val() const {
        return val_;
    }
    bool lvalue() const {
        return lvalue_;
    }
    const token_pos_t& pos() const {
        return pos_;
    }

    void MarkAndProcessUses() {
        MarkUsed();
        ProcessUses();
    }

    // Casts.
    virtual BinaryExpr* AsBinaryExpr() {
        return nullptr;
    }
    virtual DefaultArgExpr* AsDefaultArgExpr() {
        return nullptr;
    }
    virtual SymbolExpr* AsSymbolExpr() {
        return nullptr;
    }

  private:
    // Hide this symbol. Calls to error(pos... will get more accurate as we
    // make adjustments.
    void error(int number, ...) = delete;

  protected:
    virtual void DoEmit() = 0;

    void error(const token_pos_t& pos, int number, ...);

  protected:
    token_pos_t pos_;
    value val_ = {};
    bool lvalue_ = 0;
};

class ErrorExpr final : public Expr
{
  public:
    ErrorExpr() : Expr(token_pos_t{})
    {}

    // Errors never bind.
    bool Bind() override {
        return false;
    }
    bool Analyze() override {
        return false;
    }
    void DoEmit() override {}
    void ProcessUses() override {}
};

class IsDefinedExpr final : public Expr
{
  public:
    IsDefinedExpr(const token_pos_t& pos, sp::Atom* name)
      : Expr(pos),
        name_(name)
    {}

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}

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

    bool Bind() override {
        return expr_->Bind();
    }
    bool Analyze() override;
    void DoEmit() override;
    bool HasSideEffects() override;
    void ProcessUses() override;

  private:
    int token_;
    Expr* expr_;
    bool userop_ = false;
};

class BinaryExprBase : public Expr
{
  public:
    BinaryExprBase(const token_pos_t& pos, int token, Expr* left, Expr* right);

    bool Bind() override {
        bool ok = left_->Bind();
        ok &= right_->Bind();
        return ok;
    }
    bool HasSideEffects() override;
    void ProcessUses() override;

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
    bool Analyze() override;
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

    static void EmitInner(OpFunc oper, const UserOperation& userop, Expr* left, Expr* right);

  private:
    bool ValidateAssignmentLHS();
    bool ValidateAssignmentRHS();

  private:
    UserOperation userop_;
    UserOperation assignop_;
    OpFunc oper_ = nullptr;
    cell array_copy_length_ = 0;
};

class LogicalExpr final : public BinaryExprBase
{
  public:
    LogicalExpr(const token_pos_t& pos, int token, Expr* left, Expr* right)
      : BinaryExprBase(pos, token, left, right)
    {}

    bool Analyze() override;
    void DoEmit() override;
    void EmitTest(bool jump_on_true, int taken, int fallthrough) override;
    void FlattenLogical(int token, ke::Vector<Expr*>* out) override;
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

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    bool HasSideEffects() override;
    void ProcessUses() override;

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

    bool Bind() override {
        bool ok = first_->Bind();
        ok &= second_->Bind();
        ok &= third_->Bind();
        return ok;
    }
    bool Analyze() override;
    void DoEmit() override;
    bool HasSideEffects() override {
        return first_->HasSideEffects() || second_->HasSideEffects() || third_->HasSideEffects();
    }
    void ProcessUses() override;

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

    bool Bind() override {
        return expr_->Bind();
    }
    bool Analyze() override;
    bool HasSideEffects() override {
        return true;
    }
    void ProcessUses() override;

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

    bool Bind() override {
        return expr_->Bind();
    }
    bool Analyze() override;
    void DoEmit() override;
    bool HasSideEffects() override {
        return expr_->HasSideEffects();
    }
    void ProcessUses() override;

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

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}

  private:
    // :TODO: switch more things to atoms.
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

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}
    symbol* BindCallTarget(int token, Expr** implicit_this) override;
    symbol* BindNewTarget() override;
    SymbolExpr* AsSymbolExpr() override {
        return this;
    }

    bool AnalyzeWithOptions(bool allow_types);

  private:
    sp::Atom* name_;
    symbol* sym_;
};

struct ParsedArg {
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

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    void MarkUsed() override;
    void ProcessUses() override;
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

    bool Bind() override {
        assert(false);
        return true;
    }
    bool Analyze() override {
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
    void ProcessUses() override;

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
    void ProcessUses() override {}

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

    bool Bind() override {
        return base_->Bind();
    }
    symbol* BindCallTarget(int token, Expr** implicit_this) override;
    bool Analyze() override;
    bool HasSideEffects() override;
    void DoEmit() override;
    void ProcessUses() override;

  private:
    bool AnalyzeStaticAccess();
    bool AnalyzeEnumStructAccess(Type* type, symbol* root);

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

    bool Bind() override {
        bool ok = base_->Bind();
        ok &= expr_->Bind();
        return ok;
    }
    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override;
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
    void ProcessUses() override;

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

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override;
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

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}

  private:
    symbol* sym_;
};

class NullExpr final : public Expr
{
  public:
    explicit NullExpr(const token_pos_t& pos)
      : Expr(pos)
    {}

    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}
};

class NumberExpr final : public Expr
{
  public:
    explicit NumberExpr(const token_pos_t& pos, cell value)
      : Expr(pos),
        value_(value)
    {}

    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}

  private:
    cell value_;
};

class FloatExpr final : public Expr
{
  public:
    explicit FloatExpr(const token_pos_t& pos, cell value)
      : Expr(pos),
        value_(value)
    {}

    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}

  private:
    cell value_;
};

class StringExpr final : public Expr
{
  public:
    StringExpr(const token_pos_t& pos, cell lit_addr, cell length)
      : Expr(pos),
        lit_addr_(lit_addr),
        length_(length)
    {}

    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}

  private:
    cell lit_addr_;
    cell length_;
};

class ArrayExpr final : public Expr
{
  public:
    ArrayExpr(const token_pos_t& pos)
      : Expr(pos)
    {}

    bool Bind() override;
    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}

    PoolList<Expr*>& exprs() {
        return exprs_;
    }

  private:
    cell_t addr_ = 0;
    PoolList<Expr*> exprs_;
};
