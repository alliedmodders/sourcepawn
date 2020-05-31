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
class BinaryExpr;
class DefaultArgExpr;
class StringExpr;
class StructExpr;
class SymbolExpr;
class TaggedValueExpr;
struct StructInitField;

class ParseNode : public PoolObject
{
  public:
    explicit ParseNode(const token_pos_t& pos)
      : pos_(pos)
    {}

    virtual bool Bind() {
        return true;
    }
    virtual bool Analyze() = 0;
    virtual void Emit() = 0;
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

class Stmt : public ParseNode
{
  public:
    explicit Stmt(const token_pos_t& pos)
      : ParseNode(pos)
    {}

    // Helper to immediately bind, analyze, and emit. This will be removed once
    // the two-phase process is eliminated.
    void Process();
};

class StmtList : public Stmt
{
  public:
    explicit StmtList(const token_pos_t& pos)
      : Stmt(pos)
    {}

    bool Bind() override;
    bool Analyze() override;
    void Emit() override;

    PoolList<Stmt*>& stmts() {
        return stmts_;
    }

  private:
    PoolList<Stmt*> stmts_;
};

class Decl : public Stmt
{
  public:
    Decl(const token_pos_t& pos, sp::Atom* name)
      : Stmt(pos),
        name_(name)
    {}

    bool Analyze() override;
    void Emit() override;

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

    bool Bind() override {
        return false;
    }
};

class VarDecl : public Decl
{
  public:
    VarDecl(const token_pos_t& pos, sp::Atom* name, const typeinfo_t& type, int vclass,
            bool is_public, bool is_static, bool is_stock, Expr* initializer)
      : Decl(pos, name),
        type_(type),
        vclass_(vclass),
        init_(initializer),
        is_public_(is_public),
        is_static_(is_static),
        is_stock_(is_stock)
    {}

    bool Bind() override;
    bool Analyze() override;
    void Emit() override;

  private:
    bool AnalyzePstruct();
    bool AnalyzePstructArg(const pstruct_t* ps, const StructInitField& field,
                           std::vector<bool>* visited);
    void EmitPstruct();

  protected:
    typeinfo_t type_;
    int vclass_; // This will be implied by scope, when we get there.
    Expr* init_;
    bool is_public_ : 1;
    bool is_static_ : 1;
    bool is_stock_ : 1;
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

    bool Bind() override;
    bool Analyze() override;

  private:
    int expr_tag_;
    int value_;
};

struct EnumField {
    EnumField(const token_pos_t& pos, sp::Atom* name, cell value)
      : pos(pos), name(name), value(value)
    {}
    token_pos_t pos;
    sp::Atom* name;
    cell value;
};

class EnumDecl : public Decl
{
  public:
    explicit EnumDecl(const token_pos_t& pos, int vclass, sp::Atom* label, sp::Atom* name)
      : Decl(pos, name),
        vclass_(vclass),
        label_(label)
    {}

    bool Bind() override;
    PoolList<EnumField>& fields() {
        return fields_;
    }

  private:
    int vclass_;
    sp::Atom* label_;
    PoolList<EnumField> fields_;
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

    bool Bind() override;

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

    bool Bind() override;

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

    bool Bind() override;
};

class UsingDecl : public Decl
{
  public:
    explicit UsingDecl(const token_pos_t& pos)
      : Decl(pos, nullptr)
    {}

    bool Bind() override;
};

class Expr : public ParseNode
{
  public:
    explicit Expr(const token_pos_t& pos)
      : ParseNode(pos)
    {}

    // Flatten a series of binary expressions into a single list.
    virtual void FlattenLogical(int token, std::vector<Expr*>* out);

    virtual void EmitTest(bool jump_on_true, int target);
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

    void MarkAndProcessUses() {
        MarkUsed();
        ProcessUses();
    }

    // Casts.
    virtual BinaryExpr* AsBinaryExpr() { return nullptr; }
    virtual DefaultArgExpr* AsDefaultArgExpr() {  return nullptr; }
    virtual SymbolExpr* AsSymbolExpr() { return nullptr; }
    virtual StructExpr* AsStructExpr() { return nullptr; }
    virtual StringExpr* AsStringExpr() { return nullptr; }
    virtual TaggedValueExpr* AsTaggedValueExpr() { return nullptr; }

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

    bool AnalyzeWithOptions(bool from_call);

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
    void EmitTest(bool jump_on_true, int target) override;
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


class TaggedValueExpr : public Expr
{
  public:
    TaggedValueExpr(const token_pos_t& pos, int tag, cell value)
      : Expr(pos),
        tag_(tag),
        value_(value)
    {}

    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}
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

    bool Analyze() override;
    void DoEmit() override;
    void ProcessUses() override {}
    StringExpr* AsStringExpr() override {
        return this;
    }

    PoolString* text() const {
        return text_;
    }

  private:
    PoolString* text_;
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

    bool Analyze() override {
        return true;
    }
    void ProcessUses() override {
        assert(false);
    }
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
