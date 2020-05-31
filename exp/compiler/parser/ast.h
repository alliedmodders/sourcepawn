// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 David Anderson
// 
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.

#ifndef _include_sourcepawn_ast_h_
#define _include_sourcepawn_ast_h_

#include "shared/string-pool.h"
#include "pool-allocator.h"
#include "label.h"
#include "tokens.h"
#include "types.h"
#include "symbols.h"
#include "tokens.h"
#include "type-specifier.h"
#include "value-attrs.h"
#include <amtl/am-vector.h>
#include <limits.h>

namespace sp {

using namespace ke;

namespace sema {
class Expr;
} // namespace sema

class Scope;
class FunctionScope;
class LayoutScope;
class CompileContext;

#define ASTKINDS(_)       \
  _(VarDecl)              \
  _(ForStatement)         \
  _(ReturnStatement)      \
  _(IntegerLiteral)       \
  _(FloatLiteral)         \
  _(StringLiteral)        \
  _(CharLiteral)          \
  _(BinaryExpression)     \
  _(BlockStatement)       \
  _(Assignment)           \
  _(NameProxy)            \
  _(ExpressionStatement)  \
  _(FunctionStatement)    \
  _(CallNewExpr)          \
  _(NewArrayExpr)         \
  _(CallExpression)       \
  _(FieldExpression)      \
  _(IfStatement)          \
  _(IndexExpression)      \
  _(EnumConstant)         \
  _(EnumStatement)        \
  _(WhileStatement)       \
  _(BreakStatement)       \
  _(ContinueStatement)    \
  _(IncDecExpression)     \
  _(UnaryExpression)      \
  _(ViewAsExpression)       \
  _(SizeofExpression)     \
  _(TernaryExpression)    \
  _(TokenLiteral)         \
  _(SwitchStatement)      \
  _(ArrayLiteral)         \
  _(TypedefDecl)          \
  _(StructInitializer)    \
  _(TypesetDecl)          \
  _(RecordDecl)           \
  _(MethodmapDecl)        \
  _(MethodDecl)           \
  _(PropertyDecl)         \
  _(FieldDecl)            \
  _(ThisExpression)       \
  _(DeleteStatement)      \
  _(FoldedExpr)

class FileContext;

namespace ast {

// Forward declarations.
class AstVisitor;

// Forward declarations of ast nodes.
#define _(name) class name;
ASTKINDS(_)
#undef _

enum class AstKind
{
#  define _(name) k##name,
  ASTKINDS(_)
#  undef _
  AstKind_Invalid
};

// Interface for AST nodes.
class AstNode : public PoolObject
{
  SourceLocation location_;

 public:
  AstNode(const SourceLocation& location)
    : location_(location)
  {
  }
  
  virtual AstKind kind() const = 0;
  virtual const char* kindName() const = 0;

#define _(name)   bool is##name() { return kind() == AstKind::k##name; }            \
          name* to##name() { assert(is##name()); return (name*)this; }             \
          name* as##name() { if (!is##name()) return nullptr; return to##name(); }
  ASTKINDS(_)
#undef _

  virtual void accept(AstVisitor* visitor) = 0;

  const SourceLocation& loc() const {
    return location_;
  }
};

#define DECLARE_NODE(type)            \
  AstKind kind() const {              \
    return AstKind::k##type;          \
  }                                   \
  void accept(AstVisitor* visitor) {  \
    visitor->visit##type(this);       \
  }                                   \
  const char* kindName() const {      \
    return #type;                     \
  }

class AstVisitor
{
 public:
#define _(name) virtual void visit##name(name* node) = 0;
  ASTKINDS(_)
#undef _
};

// PartialAstVisitor only cares about certain nodes and ignores the rest.
class PartialAstVisitor : public AstVisitor
{
 public:
#define _(name) virtual void visit##name(name* node) { }
  ASTKINDS(_)
#undef _
};

// StrictAstVisitor will assert if it's missing any methods, but it does not
// implement all methods.
class StrictAstVisitor : public AstVisitor
{
 public:
#define _(name) virtual void visit##name(name* node) { assert(false); }
  ASTKINDS(_)
#undef _
};

class Statement : public AstNode
{
 public:
  Statement(const SourceLocation& pos)
    : AstNode(pos)
  {
  }
};

class Expression : public AstNode
{
 public:
  Expression(const SourceLocation& pos)
   : AstNode(pos)
  {
  }
};

typedef PoolList<Statement*> StatementList;
typedef PoolList<Expression*> ExpressionList;
typedef PoolList<VarDecl*> ParameterList;

class FunctionSignature : public PoolObject
{
 public:
  FunctionSignature()
  {}

  FunctionSignature(const TypeExpr& returnType, ParameterList* parameters)
   : returnType_(returnType),
     parameters_(parameters),
     destructor_(false),
     native_(false),
     resolved_(false)
  {
  }

  TypeExpr& returnType() {
    return returnType_;
  }
  const TypeExpr& returnType() const {
    return returnType_;
  }
  ParameterList* parameters() const {
    return parameters_;
  }
  bool destructor() const {
    return destructor_;
  }
  void setDestructor() {
    destructor_ = true;
  }
  bool native() const {
    return native_;
  }
  void setNative() {
    native_ = true;
  }
  void setResolved() {
    resolved_ = true;
  }
  bool isResolved() const {
    return resolved_;
  }

 private:
  TypeExpr returnType_;
  ParameterList* parameters_;
  bool destructor_ : 1;
  bool native_ : 1;
  bool resolved_ : 1;
};

class VarDecl : public Statement
{
 public:
  VarDecl(const NameToken& name, TokenKind classifier, Expression* initialization)
   : Statement(name.start),
     name_(name.atom),
     initialization_(initialization),
     sema_init_(nullptr),
     sym_(nullptr),
     next_(nullptr),
     classifier_(classifier),
     must_zero_init_(true)
  {
  }

  DECLARE_NODE(VarDecl);

  Expression* initialization() const {
    return initialization_;
  }
  sema::Expr* sema_init() const {
    return sema_init_;
  }
  void set_sema_init(sema::Expr* init) {
    sema_init_ = init;
  }
  Atom* name() const {
    return name_;
  }
  TypeExpr& te() {
    return te_;
  }
  const TypeExpr& te() const {
    return te_;
  }
  void setSymbol(VariableSymbol* sym) {
    assert(!sym_);
    sym_ = sym;
  }
  VariableSymbol* sym() const {
    return sym_;
  }
  TokenKind classifier() const {
    return classifier_;
  }
  bool must_zero_init() const {
    return must_zero_init_;
  }
  void set_must_zero_init(bool value) {
    must_zero_init_ = value;
  }

  // This is only valid after type resolution.
  Type* type() const {
    return te().resolved();
  }

  // This is used for old-style dynamic arrays, where we infer an initializer
  // during type resolution.
  void set_initializer(ast::Expression* expr) {
    assert(!initialization_);
    initialization_ = expr;
  }

  // When parsed as a list of declarations, for example:
  //   int x, y, z
  //
  // The declarations are chained together in a linked list.
  void setNext(VarDecl* next) {
    assert(!next_);
    next_ = next;
  }
  VarDecl* next() const {
    return next_;
  }

 private:
  Atom* name_;
  Expression* initialization_;
  sema::Expr* sema_init_;
  TypeExpr te_;
  VariableSymbol* sym_;
  VarDecl* next_;
  TokenKind classifier_;
  bool must_zero_init_;
};

class NameProxy : public Expression
{
  Atom* name_;
  Symbol* binding_;

 public:
  NameProxy(const SourceLocation& pos, Atom* name)
    : Expression(pos),
      name_(name),
      binding_(nullptr)
  {
  }

  DECLARE_NODE(NameProxy);
  
  Atom* name() const {
    return name_;
  }
  Symbol* sym() const {
    return binding_;
  }
  void bind(Symbol* sym) {
    binding_ = sym;
  }
};

class TokenLiteral : public Expression
{
  TokenKind token_;

 public:
  TokenLiteral(const SourceLocation& pos, TokenKind token)
    : Expression(pos),
    token_(token)
  {
  }

  DECLARE_NODE(TokenLiteral);
  TokenKind token() const {
    return token_;
  }
};

class CharLiteral : public Expression
{
 public:
  CharLiteral(const SourceLocation& pos, int32_t value)
    : Expression(pos),
      value_(value)
  {
  }

  DECLARE_NODE(CharLiteral);

  int32_t value() const {
    return value_;
  }

 private:
  int32_t value_;
};

class IntegerLiteral : public Expression
{
 public:
  IntegerLiteral(const SourceLocation& pos, int64_t value)
    : Expression(pos),
      value_(value)
  {
  }

  DECLARE_NODE(IntegerLiteral);

  int64_t value() const {
    return value_;
  }

 private:
  int64_t value_;
};

class FloatLiteral : public Expression
{
  double value_;

 public:
  FloatLiteral(const SourceLocation& pos, double value)
    : Expression(pos),
      value_(value)
  {
  }

  DECLARE_NODE(FloatLiteral);

  double value() const {
    return value_;
  }
};

class StringLiteral : public Expression
{
 public:
  StringLiteral(const SourceLocation& pos, Atom* literal)
    : Expression(pos),
      literal_(literal)
  {
  }

  DECLARE_NODE(StringLiteral);

  Atom* literal() const {
    return literal_;
  }
  int32_t arrayLength() const {
    assert(literal()->length() + 1 < INT_MAX);
    return int32_t(literal()->length() + 1);
  }

 private:
  Atom* literal_;
};

class NameAndValue : public PoolObject
{
 public:
  NameAndValue(const NameToken& name, Expression* expr)
   : name_(name),
     expr_(expr)
  {
  }

  Atom* name() const {
    return name_.atom;
  }
  Expression* expr() const {
    return expr_;
  }
  const SourceLocation& loc() const {
    return name_.start;
  }

 private:
  NameToken name_;
  Expression* expr_;
};

typedef PoolList<NameAndValue*> NameAndValueList;

class StructInitializer : public Expression
{
 public:
  StructInitializer(const SourceLocation& loc, NameAndValueList* pairs)
   : Expression(loc),
     pairs_(pairs)
  {
  }

  DECLARE_NODE(StructInitializer);

  NameAndValueList* pairs() const {
    return pairs_;
  }

 private:
  NameAndValueList* pairs_;
};

class ArrayLiteral : public Expression
{
 public:
  ArrayLiteral(const SourceLocation& pos, ExpressionList* expressions, bool repeatLastElement)
    : Expression(pos),
      expressions_(expressions),
      repeatLastElement_(repeatLastElement)
  {
  }

  DECLARE_NODE(ArrayLiteral);

  int32_t arrayLength() const {
    assert(expressions()->size() < INT_MAX);
    return int32_t(expressions()->size());
  }
  ExpressionList* expressions() const {
    return expressions_;
  }
  bool repeatLastElement() const {
    return repeatLastElement_;
  }

 private:
  ExpressionList* expressions_;
  bool repeatLastElement_;
};

class SizeofExpression : public Expression
{
 public:
  SizeofExpression(const SourceLocation& pos, NameProxy* proxy, size_t level)
   : Expression(pos),
     proxy_(proxy),
     level_(level)
  {}

  DECLARE_NODE(SizeofExpression);

  NameProxy* proxy() const {
    return proxy_;
  }
  size_t level() const {
    return level_;
  }

 private:
  NameProxy* proxy_;
  size_t level_;
};

class ViewAsExpression : public Expression
{
 public:
  ViewAsExpression(const SourceLocation& pos, const TypeExpr& te, Expression* expr)
   : Expression(pos),
     te_(te),
     expr_(expr)
  {}

  DECLARE_NODE(ViewAsExpression);

  TypeExpr& te() {
    return te_;
  }
  const TypeExpr& te() const {
    return te_;
  }
  Expression* expr() const {
    return expr_;
  }

 private:
  TypeExpr te_;
  Expression* expr_;
};

class UnaryExpression : public Expression
{
 public:
  UnaryExpression(const SourceLocation& pos, TokenKind token, Expression* expr)
   : Expression(pos),
     expression_(expr),
     token_(token),
     tag_(nullptr)
  {
  }

  UnaryExpression(const SourceLocation& pos, TokenKind token, Expression* expr,
          NameProxy* tag)
    : Expression(pos),
      expression_(expr),
      token_(token),
      tag_(tag)
  {
  }

  DECLARE_NODE(UnaryExpression);

  Expression* expression() const {
    return expression_;
  }
  TokenKind token() const {
    return token_;
  }
  NameProxy* tag() const {
    return tag_;
  }

 private:
  Expression* expression_;
  TokenKind token_;
  NameProxy* tag_;
};

class ThisExpression : public Expression
{
 public:
  ThisExpression(const SourceLocation& pos)
   : Expression(pos)
  {}

  DECLARE_NODE(ThisExpression);
};

class BinaryExpression : public Expression
{
  Expression* left_;
  Expression* right_;
  TokenKind token_;

 public:
  BinaryExpression(const SourceLocation& pos, TokenKind token, Expression* left, Expression* right)
   : Expression(pos),
     left_(left),
     right_(right),
     token_(token)
  {
  }

  DECLARE_NODE(BinaryExpression);

  Expression* left() const {
    return left_;
  }
  Expression* right() const {
    return right_;
  }
  TokenKind token() const {
    return token_;
  }
};

class TernaryExpression : public Expression
{
 public:
  TernaryExpression(const SourceLocation& pos, Expression* condition, Expression* left, Expression* right)
    : Expression(pos),
      condition_(condition),
      left_(left),
      right_(right)
  {
  }

  DECLARE_NODE(TernaryExpression);

  Expression* condition() const {
    return condition_;
  }
  Expression* left() const {
    return left_;
  }
  Expression* right() const {
    return right_;
  }

 private:
  Expression* condition_;
  Expression* left_;
  Expression* right_;
};

class FieldExpression : public Expression
{
 public:
  FieldExpression(const SourceLocation& pos, Expression* base, const NameToken& field)
   : Expression(pos),
     base_(base),
     field_(field)
  {} 

  DECLARE_NODE(FieldExpression);

  Expression* base() const {
    return base_;
  }
  Atom* field() const {
    return field_.atom;
  }

 private:
  Expression* base_;
  NameToken field_;
};

class IndexExpression : public Expression
{
 public:
  IndexExpression(const SourceLocation& pos, Expression* left, Expression* right)
   : Expression(pos),
     left_(left),
     right_(right)
  {
  }

  DECLARE_NODE(IndexExpression);

  Expression* left() const {
    return left_;
  }
  Expression* right() const {
    return right_;
  }

 private:
  Expression* left_;
  Expression* right_;
};

class CallNewExpr : public Expression
{
 public:
  CallNewExpr(const SourceLocation& pos, const TypeExpr& te, ExpressionList* arguments)
   : Expression(pos),
     te_(te),
     arguments_(arguments)
  {
  }

  DECLARE_NODE(CallNewExpr);

  TypeExpr& te() {
    return te_;
  }
  const TypeExpr& te() const {
    return te_;
  }
  ExpressionList* arguments() const {
    return arguments_;
  }

 private:
  TypeExpr te_;
  ExpressionList* arguments_;
};

class NewArrayExpr : public Expression
{
 public:
  NewArrayExpr(const SourceLocation& pos, const TypeExpr& te, ExpressionList* dims)
   : Expression(pos),
     te_(te),
     dims_(dims)
  {
  }

  DECLARE_NODE(NewArrayExpr);

  TypeExpr& te() {
    return te_;
  }
  const TypeExpr& te() const {
    return te_;
  }
  ExpressionList* dims() const {
    return dims_;
  }

 private:
  TypeExpr te_;
  ExpressionList* dims_;
};

class CallExpression : public Expression
{
 public:
  CallExpression(const SourceLocation& pos, Expression* callee, ExpressionList* arguments)
    : Expression(pos),
      callee_(callee),
      arguments_(arguments)
  {
  }

  DECLARE_NODE(CallExpression);

  Expression* callee() const {
    return callee_;
  }
  ExpressionList* arguments() const {
    return arguments_;
  }

  void setCallee(Expression* expr) {
    callee_ = expr;
  }

 private:
  Expression* callee_;
  ExpressionList* arguments_;
};

class ForStatement : public Statement
{
  Statement* initialization_;
  Expression* condition_;
  Statement* update_;
  Statement* body_;
  Scope* scope_;

 public:
  ForStatement(const SourceLocation& pos, Statement* initialization,
               Expression* condition, Statement* update, Statement* body,
               Scope* scope)
   : Statement(pos),
     initialization_(initialization),
     condition_(condition),
     update_(update),
     body_(body),
     scope_(scope),
     sema_cond_(nullptr)
  {
  }

  DECLARE_NODE(ForStatement);

  Statement* initialization() const {
    return initialization_;
  }
  Expression* condition() const {
    return condition_;
  }
  Statement* update() const {
    return update_;
  }
  Statement* body() const {
    return body_;
  }
  Scope* scope() const {
    return scope_;
  }
  void setScope(Scope* scope) {
    assert(!scope_);
    scope_ = scope;
  }

  sema::Expr* sema_cond() const {
    return sema_cond_;
  }
  void set_sema_cond(sema::Expr* expr) {
    sema_cond_ = expr;
  }

 private:
  sema::Expr* sema_cond_;
};

// FoldedExprs note that an expression has been constant-folded during semantic
// analysis, but the original tree is still available if needed.
class FoldedExpr : public Expression
{
 public:
  FoldedExpr(const SourceRange& range, Expression* original, const BoxedValue& value)
   : Expression(range.start),
     original_(original),
     value_(value)
  {}

  DECLARE_NODE(FoldedExpr);

  Expression* original() const {
    return original_;
  }
  const BoxedValue& value() const {
    return value_;
  }

 private:
  Expression* original_;
  BoxedValue value_;
};

class WhileStatement : public Statement
{
  TokenKind token_;
  Expression* condition_;
  Statement* body_;

 public:
  WhileStatement(const SourceLocation& pos, TokenKind kind, Expression* condition, Statement* body)
    : Statement(pos),
      token_(kind),
      condition_(condition),
      body_(body),
      sema_cond_(nullptr)
  {
  }
  
  DECLARE_NODE(WhileStatement);

  TokenKind token() const {
    return token_;
  }
  Expression* condition() const {
    return condition_;
  }
  Statement* body() const {
    return body_;
  }

  sema::Expr* sema_cond() const {
    return sema_cond_;
  }
  void set_sema_cond(sema::Expr* expr) {
    sema_cond_ = expr;
  }

 private:
  sema::Expr* sema_cond_;
};

class ReturnStatement : public Statement
{
 public:
  ReturnStatement(const SourceLocation& pos, Expression* expr)
    : Statement(pos),
      expr_(expr),
      sema_expr_(nullptr)
  {
  }

  DECLARE_NODE(ReturnStatement);

  Expression* expr() const {
    return expr_;
  }
  sema::Expr* sema_expr() const {
    return sema_expr_;
  }
  void set_sema_expr(sema::Expr* expr) {
    assert(!sema_expr_);
    sema_expr_ = expr;
  }

 private:
  Expression* expr_;
  sema::Expr* sema_expr_;
};

class BreakStatement : public Statement
{
 public:
  BreakStatement(const SourceLocation& pos)
    : Statement(pos)
  {
  }

  DECLARE_NODE(BreakStatement);
};

class ContinueStatement : public Statement
{
 public:
  ContinueStatement(const SourceLocation& pos)
    : Statement(pos)
  {
  }

  DECLARE_NODE(ContinueStatement);
};

class Assignment : public Expression
{
  TokenKind token_;
  Expression* lvalue_;
  Expression* expression_;

 public:
  Assignment(const SourceLocation& pos, TokenKind token, Expression* left, Expression* right)
    : Expression(pos),
      token_(token),
      lvalue_(left),
      expression_(right)
  {
  }

  DECLARE_NODE(Assignment);

  TokenKind token() const {
    return token_;
  }
  Expression* lvalue() const {
    return lvalue_;
  }
  Expression* expression() const {
    return expression_;
  }
};

class ExpressionStatement : public Statement
{
  Expression* expression_;

 public:
  ExpressionStatement(Expression* expression)
   : Statement(expression->loc()),
     expression_(expression),
     sema_expr_(nullptr)
  {
  }

  DECLARE_NODE(ExpressionStatement);

  Expression* expr() const {
    return expression_;
  }

  sema::Expr* sema_expr() const {
    return sema_expr_;
  }
  void set_sema_expr(sema::Expr* expr) {
    sema_expr_ = expr;
  }

 private:
  sema::Expr* sema_expr_;
};

// Block statements have a "kind" denoting information about the block.
//   TOK_LBRACE - A normal block started by a {.
//   TOK_FUNCTION - A function body.
class BlockStatement : public Statement
{
  StatementList* statements_;
  Scope* scope_;
  TokenKind type_;

 public:
  BlockStatement(const SourceLocation& pos,
                 StatementList* statements,
                 TokenKind kind,
                 Scope* scope)
    : Statement(pos),
      statements_(statements),
      scope_(scope),
      type_(kind)
  {
  }

  DECLARE_NODE(BlockStatement);

  StatementList* statements() const {
    return statements_;
  }
  TokenKind type() const {
    return type_;
  }
};

class FunctionNode : public PoolObject
{
 public:
  FunctionNode(TokenKind kind)
   : kind_(kind),
     body_(nullptr),
     signature_(nullptr),
     funScope_(nullptr),
     signature_type_(nullptr),
     guaranteed_return_(false)
  {
  }

  TokenKind token() const {
    return kind_;
  }

  void setBody(BlockStatement* body) {
    body_ = body;
  }
  BlockStatement* body() const {
    return body_;
  }

  void setSignature(FunctionSignature* signature) {
    signature_ = signature;
  }
  FunctionSignature* signature() {
    return signature_;
  }

  void setArgScope(Scope* funScope) {
    assert(!funScope_);
    funScope_ = funScope;
  }
  Scope* funScope() const {
    return funScope_;
  }

  // Set if we're shadowing another symbol.
  void setShadowed(FunctionSymbol* shadowed) {
    shadowed_ = shadowed;
  }
  FunctionSymbol* shadowed() const {
    return shadowed_;
  }

  Type* signature_type() const {
    return signature_type_;
  }
  void set_signature_type(Type* type) {
    signature_type_ = type;
  }

  void set_guaranteed_return() {
    guaranteed_return_ = true;
  }
  bool guaranteed_return() const {
    return guaranteed_return_;
  }

  Label* address() {
    return &address_;
  }

 private:
  TokenKind kind_;
  BlockStatement* body_;
  FunctionSignature* signature_;
  Scope* funScope_;
  FunctionSymbol* shadowed_;
  Label address_;
  Type* signature_type_;
  bool guaranteed_return_;
};

class FunctionStatement :
  public Statement,
  public FunctionNode
{
 public:
  FunctionStatement(const NameToken& name, TokenKind kind, SymAttrs flags)
   : Statement(name.start),
     FunctionNode(kind),
     name_(name),
     sym_(nullptr),
     type_(nullptr),
     flags_(flags)
  {
  }

  DECLARE_NODE(FunctionStatement);

  Atom* name() const {
    return name_.atom;
  }
  void setSymbol(FunctionSymbol* sym) {
    assert(!sym_);
    sym_ = sym;
  }
  FunctionSymbol* sym() const {
    return sym_;
  }
  Flags<SymAttrs> flags() const {
    return flags_;
  }

  // These are used in the function -> value decay operation. Since most
  // functions are not used as values, this is an optional cache.
  FunctionType* type() const {
    return type_;
  }
  void setType(FunctionType* type) {
    type_ = type;
  }

  const char* decoration() const {
    if (token() == TOK_FORWARD)
      return "forward";
    if (token() == TOK_NATIVE)
      return "native";
    if (token() == TOK_PUBLIC)
      return "public";
    if (token() == TOK_STATIC) {
      if (flags_ & SymAttrs::Stock)
        return "static stock";
      return "static";
    }
    if (flags_ & SymAttrs::Stock)
      return "stock";
    return "function";
  }

 private:
  NameToken name_;
  FunctionSymbol* sym_;
  FunctionType* type_;
  Flags<SymAttrs> flags_;
};

struct IfClause
{
  IfClause(Expression* cond, Statement* body)
   : cond(cond),
     body(body),
     sema_cond(nullptr)
  {}

  Expression* cond;
  Statement* body;
  sema::Expr* sema_cond;
};

class IfStatement : public Statement
{
 public:
  IfStatement(const SourceLocation& pos, PoolList<IfClause>* clauses, Statement* fallthrough)
   : Statement(pos),
     clauses_(clauses),
     fallthrough_(fallthrough)
  {
  }

  DECLARE_NODE(IfStatement);

  PoolList<IfClause>* clauses() const {
    return clauses_;
  }
  Statement* fallthrough() const {
    return fallthrough_;
  }

 private:
  PoolList<IfClause>* clauses_;
  Statement* fallthrough_;
};

// There is one EnumConstant per entry in an EnumStatement. We need this to be
// separate from EnumStatement so we can properly put it in the symbol table.
class EnumConstant : public Statement
{
 public:
  EnumConstant(const SourceLocation& loc, EnumStatement* parent, Atom* name, Expression* expr)
   : Statement(loc),
     parent_(parent),
     name_(name),
     expr_(expr),
     sym_(nullptr)
  {}

  DECLARE_NODE(EnumConstant);

  EnumStatement* parent() const {
    return parent_;
  }
  Atom* name() const {
    return name_;
  }
  Expression* expression() const {
    return expr_;
  }

  void setSymbol(ConstantSymbol* sym) {
    assert(!sym_);
    sym_ = sym;
  }
  ConstantSymbol* sym() const {
    return sym_;
  }

 private:
  EnumStatement* parent_;
  Atom* name_;
  Expression* expr_;
  ConstantSymbol* sym_;
};

typedef PoolList<EnumConstant*> EnumConstantList;

class EnumStatement : public Statement
{
 public:
  EnumStatement(const SourceLocation& pos, Atom* name)
   : Statement(pos),
     name_(name),
     sym_(nullptr),
     entries_(nullptr),
     resolved_(false),
     resolving_(false)
  {
  }

  DECLARE_NODE(EnumStatement);

  Atom* name() const {
    return name_;
  }

  void setEntries(EnumConstantList* entries) {
    assert(!entries_);
    entries_ = entries;
  }
  EnumConstantList* entries() const {
    return entries_;
  }

  void setSymbol(TypeSymbol* sym) {
    assert(!sym_);
    sym_ = sym;
  }
  TypeSymbol* sym() const {
    return sym_;
  }

  bool isResolved() const {
    return resolved_;
  }
  bool isResolving() const {
    return resolving_;
  }
  void setResolving() {
    assert(!isResolving() && !isResolved());
    resolving_ = true;
  }
  void setResolved() {
    assert(isResolving());
    resolving_ = false;
    resolved_ = true;
  }

  void setMethodmap(MethodmapDecl* methodmap) {
    methodmap_ = methodmap;
  }
  MethodmapDecl* methodmap() const {
    return methodmap_;
  }

 private:
  Atom* name_;
  TypeSymbol* sym_;
  EnumConstantList* entries_;
  MethodmapDecl* methodmap_;
  bool resolved_ : 1;
  bool resolving_ : 1;
};

class IncDecExpression : public Expression
{
  TokenKind token_;
  Expression* expression_;
  bool postfix_;

 public:
  IncDecExpression(const SourceLocation& pos, TokenKind token, Expression* expression, bool postfix)
    : Expression(pos),
    token_(token),
    expression_(expression),
    postfix_(postfix)
  {
  }

  DECLARE_NODE(IncDecExpression);

  TokenKind token() const {
    return token_;
  }
  Expression* expression() const {
    return expression_;
  }
  bool postfix() const {
    return postfix_;
  }
};

class Case : public PoolObject
{
 public:
  Case(Expression* expression, ExpressionList* others, Statement* statement)
   : expression_(expression),
     others_(others),
     statement_(statement),
     values_(nullptr)
  {
  }

  Expression* expression() const {
    return expression_;
  }
  PoolList<Expression*>* others() const {
    return others_;
  }
  Statement* statement() const {
    return statement_;
  }

  void setValues(FixedPoolList<int32_t>* values) {
    values_ = values;
  }
  const FixedPoolList<int32_t>* values() const {
    return values_;
  }

 private:
  Expression* expression_;
  PoolList<Expression*>* others_;
  Statement* statement_;

  // Filled in later by semantic analysis.
  FixedPoolList<int32_t>* values_;
};

class SwitchStatement : public Statement
{
 public:
  SwitchStatement(const SourceLocation& pos, Expression* expression, PoolList<Case*>* cases,
                  Statement* def)
   : Statement(pos),
     expression_(expression),
     cases_(cases),
     default_(def),
     sema_expr_(nullptr)
  {
  }

  DECLARE_NODE(SwitchStatement);

  Expression* expression() const {
    return expression_;
  }
  PoolList<Case*>* cases() const {
    return cases_;
  }
  Statement* defaultCase() const {
    return default_;
  }

  sema::Expr* sema_expr() const {
    return sema_expr_;
  }
  void set_sema_expr(sema::Expr* expr) {
    sema_expr_ = expr;
  }

 private:
  Expression* expression_;
  PoolList<Case*>* cases_;
  Statement* default_;
  sema::Expr* sema_expr_;
};

class LayoutDecl : public Statement
{
 public:
  LayoutDecl(const SourceLocation& loc)
   : Statement(loc)
  {}
};

class FieldDecl : public LayoutDecl
{
 public:
  FieldDecl(const SourceLocation& loc, const NameToken& name, const TypeExpr& te)
   : LayoutDecl(loc),
     name_(name),
     te_(te),
     sym_(nullptr)
  {}

  DECLARE_NODE(FieldDecl);

  Atom* name() const {
    return name_.atom;
  }
  TypeExpr& te() {
    return te_;
  }
  const TypeExpr& te() const {
    return te_;
  }

  void setSymbol(FieldSymbol* sym) {
    assert(!sym_);
    sym_ = sym;
  }
  FieldSymbol* sym() const {
    return sym_;
  }

 private:
  NameToken name_;
  TypeExpr te_;
  FieldSymbol* sym_;
  Type* type_;
};

class MethodDecl : public LayoutDecl
{
 public:
  MethodDecl(const SourceLocation& loc, const NameToken& name, bool isStatic)
   : LayoutDecl(loc),
     name_(name),
     method_(nullptr),
     sym_(nullptr),
     static_(isStatic)
  {}

  DECLARE_NODE(MethodDecl);

  Atom* name() const {
    return name_.atom;
  }
  FunctionNode* method() {
    return method_;
  }
  void setMethod(FunctionNode* node) {
    method_ = node;
  }

  bool isStatic() const {
    return static_;
  }

  void setSymbol(MethodSymbol* sym) {
    assert(!sym_);
    sym_ = sym;
  }
  MethodSymbol* sym() const {
    return sym_;
  }

 private:
  NameToken name_;
  FunctionNode* method_;
  MethodSymbol* sym_;
  bool static_;
};

class PropertyDecl : public LayoutDecl
{
 public:
  PropertyDecl(const SourceLocation& loc, const NameToken& name, const TypeExpr& spec)
   : LayoutDecl(loc),
     name_(name),
     te_(spec),
     getter_(nullptr),
     setter_(nullptr),
     sym_(nullptr)
  {}

  DECLARE_NODE(PropertyDecl);

  Atom* name() const {
    return name_.atom;
  }
  TypeExpr& te() {
    return te_;
  }
  const TypeExpr& te() const {
    return te_;
  }
  FunctionNode* getter() {
    return getter_;
  }
  FunctionNode* setter() {
    return setter_;
  }

  void setGetter(FunctionNode* get) {
    getter_ = get;
  }
  void setSetter(FunctionNode* set) {
    setter_ = set;
  }

  void setSymbol(PropertySymbol* sym) {
    assert(!sym_);
    sym_ = sym;
  }
  PropertySymbol* sym() const {
    return sym_;
  }

 private:
  NameToken name_;
  TypeExpr te_;
  FunctionNode* getter_;
  FunctionNode* setter_;
  PropertySymbol* sym_;
};

class TypesetDecl : public Statement
{
 public:
  TypesetDecl(const SourceLocation& loc, const NameToken& name)
   : Statement(loc),
     name_(name),
     types_(nullptr),
     sym_(nullptr),
     can_eagerly_resolve_(true),
     is_resolved_(false)
  {
  }

  DECLARE_NODE(TypesetDecl);

  struct Entry {
    SourceLocation loc;
    TypeExpr te;

    Entry()
    {}
    Entry(const SourceLocation& loc, const TypeExpr& te)
     : loc(loc), te(te)
    {}
  };
  typedef FixedPoolList<Entry> Entries;

  Atom* name() const {
    return name_.atom;
  }

  void setTypes(Entries* types) {
    types_ = types;
  }
  Entries* types() const {
    return types_;
  }

  void setSymbol(TypeSymbol* sym) {
    sym_ = sym;
  }
  TypeSymbol* sym() const {
    return sym_;
  }

  void setNeedsFullTypeResolution() {
    can_eagerly_resolve_ = false;
  }
  bool needsFullTypeResolution() const {
    return !can_eagerly_resolve_;
  }
  void setResolved() {
    is_resolved_ = true;
  }
  bool isResolved() const {
    return is_resolved_;
  }

 private:
  NameToken name_;
  Entries* types_;
  TypeSymbol* sym_;
  bool can_eagerly_resolve_ : 1;
  bool is_resolved_ : 1;
};

typedef PoolList<LayoutDecl*> LayoutDecls;

class RecordDecl : public Statement
{
 public:
  RecordDecl(const SourceLocation& loc, TokenKind token, const NameToken& name)
   : Statement(loc),
     name_(name),
     token_(token),
     body_(nullptr),
     sym_(nullptr),
     scope_(nullptr)
  {
  }

  DECLARE_NODE(RecordDecl);

  Atom* name() const {
    return name_.atom;
  }
  TokenKind token() const {
    return token_;
  }

  void setBody(LayoutDecls* body) {
    body_ = body;
  }
  LayoutDecls* body() const {
    return body_;
  }

  void setSymbol(TypeSymbol* sym) {
    sym_ = sym;
  }
  TypeSymbol* sym() const {
    return sym_;
  }

  void setScope(LayoutScope* scope) {
    assert(!scope_);
    scope_ = scope;
  }
  LayoutScope* scope() const {
    return scope_;
  }

 private:
  NameToken name_;
  TokenKind token_;
  LayoutDecls* body_;
  TypeSymbol* sym_;
  LayoutScope* scope_;
};

class MethodmapDecl : public Statement
{
 public:
  MethodmapDecl(const SourceLocation& loc, const NameToken& name, NameProxy* parent)
   : Statement(loc),
     name_(name),
     parent_(parent),
     body_(nullptr),
     sym_(nullptr),
     scope_(nullptr),
     extends_(nullptr),
     nullable_(false)
  {
  }

  DECLARE_NODE(MethodmapDecl);

  Atom* name() const {
    return name_.atom;
  }
  NameProxy* parent() const {
    return parent_;
  }
  bool nullable() const {
    return nullable_;
  }
  void setNullable() {
    nullable_ = true;
  }

  void setBody(LayoutDecls* body) {
    body_ = body;
  }
  LayoutDecls* body() const {
    return body_;
  }

  EnumType* extends() const {
    return extends_;
  }
  void setExtends(EnumType* extends) {
    extends_ = extends;
  }

  void setSymbol(TypeSymbol* sym) {
    sym_ = sym;
  }
  TypeSymbol* sym() const {
    return sym_;
  }

  void setScope(LayoutScope* scope) {
    assert(!scope_);
    scope_ = scope;
  }
  LayoutScope* scope() const {
    return scope_;
  }

 private:
  NameToken name_;
  NameProxy* parent_;
  LayoutDecls* body_;
  TypeSymbol* sym_;
  LayoutScope* scope_;
  EnumType* extends_;
  bool nullable_;
};

class DeleteStatement : public Statement
{
 public:
  DeleteStatement(const SourceLocation& pos, Expression* expr)
   : Statement(pos),
     expr_(expr)
  {}

  DECLARE_NODE(DeleteStatement);

  Expression* expression() const {
    return expr_;
  }

 private:
  Expression* expr_;
};

class TypedefDecl : public Statement
{
 public:
  TypedefDecl(const SourceLocation& pos, Atom* name, const TypeExpr& spec)
   : Statement(pos),
     name_(name),
     te_(spec),
     sym_(nullptr)
  {
  }

  DECLARE_NODE(TypedefDecl);

  Atom* name() const {
    return name_;
  }
  TypeExpr& te() {
    return te_;
  }
  const TypeExpr& te() const {
    return te_;
  }

  TypeSymbol* sym() const {
    return sym_;
  }
  void setSymbol(TypeSymbol* sym) {
    sym_ = sym;
  }

 private:
  Atom* name_;
  TypeExpr te_;
  TypeSymbol* sym_;
};

typedef PoolList<NameProxy*> NameProxyList;

#undef DECLARE_NODE

class ParseTree : public PoolObject
{
  StatementList* statements_;

 public:
  ParseTree(StatementList* statements)
    : statements_(statements),
      uses_handle_intrinsics_(false)
  {
  }

  void dump(FILE* fp);
  void toJson(CompileContext& cc, FILE* fp);

  StatementList* statements() const {
    return statements_;
  }

  bool uses_handle_intrinsics() const {
    return uses_handle_intrinsics_;
  }
  void set_uses_handle_intrinsics() {
    uses_handle_intrinsics_ = true;
  }

 private:
  bool uses_handle_intrinsics_;
};

// For new AstVisitors, copy-paste.
//  void visitVarDecl(VarDecl* node) override;
//  void visitForStatement(ForStatement* node) override;
//  void visitReturnStatement(ReturnStatement* node) override;
//  void visitIntegerLiteral(IntegerLiteral* node) override;
//  void visitFloatLiteral(FloatLiteral* node) override;
//  void visitStringLiteral(StringLiteral* node) override;
//  void visitCharLiteral(CharLiteral* node) override;
//  void visitBinaryExpression(BinaryExpression* node) override;
//  void visitBlockStatement(BlockStatement* node) override;
//  void visitAssignment(Assignment* node) override;
//  void visitNameProxy(NameProxy* node) override;
//  void visitExpressionStatement(ExpressionStatement* node) override;
//  void visitFunctionStatement(FunctionStatement* node) override;
//  void visitCallExpression(CallExpression* node) override;
//  void visitFieldExpression(FieldExpression* node) override;
//  void visitIfStatement(IfStatement* node) override;
//  void visitIndexExpression(IndexExpression* node) override;
//  void visitEnumConstant(EnumConstant* node) override;
//  void visitEnumStatement(EnumStatement* node) override;
//  void visitWhileStatement(WhileStatement* node) override;
//  void visitBreakStatement(BreakStatement* node) override;
//  void visitContinueStatement(ContinueStatement* node) override;
//  void visitIncDecExpression(IncDecExpression* node) override;
//  void visitUnaryExpression(UnaryExpression* node) override;
//  void visitViewAsExpression(ViewAsExpression* node) override;
//  void visitSizeofExpression(SizeofExpression* node) override;
//  void visitTernaryExpression(TernaryExpression* node) override;
//  void visitTokenLiteral(TokenLiteral* node) override;
//  void visitSwitchStatement(SwitchStatement* node) override;
//  void visitArrayLiteral(ArrayLiteral* node) override;
//  void visitTypedefDecl(TypedefDecl* node) override;
//  void visitStructInitializer(StructInitializer* node) override;
//  void visitRecordDecl(RecordDecl* node) override;
//  void visitMethodmapDecl(MethodmapDecl* node) override;
//  void visitMethodDecl(MethodDecl* node) override;
//  void visitPropertyDecl(PropertyDecl* node) override;
//  void visitFieldDecl(FieldDecl* node) override;
//  void visitThisExpression(ThisExpression* node) override;
//  void visitDeleteStatement(DeleteStatement* node) override;

} // namespace ast
} // namespace sp

#endif // _include_sourcepawn_ast_h_
