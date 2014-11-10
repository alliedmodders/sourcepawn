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

#include "pool-allocator.h"
#include <am-vector.h>
#include "tokens.h"
#include "types.h"
#include "symbols.h"
#include "string-pool.h"
#include "tokens.h"
#include <limits.h>

namespace ke {

class Scope;
class FunctionScope;

#define ASTKINDS(_)       \
  _(VariableDeclaration)  \
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
  _(CallExpression)       \
  _(FieldExpression)      \
  _(IfStatement)          \
  _(IndexExpression)      \
  _(EnumStatement)        \
  _(WhileStatement)       \
  _(BreakStatement)       \
  _(ContinueStatement)    \
  _(IncDecExpression)     \
  _(UnaryExpression)      \
  _(SizeofExpression)     \
  _(TernaryExpression)    \
  _(TokenLiteral)         \
  _(SwitchStatement)      \
  _(ArrayLiteral)         \
  _(TypedefStatement)     \
  _(StructInitializer)    \
  _(LayoutStatement)      \
  _(ThisExpression)

// Forward declarations.
#define _(name) class name;
ASTKINDS(_)
#undef _

class AstVisitor;
class FileContext;

// Interface for AST nodes.
class AstNode : public PoolObject
{
  SourceLocation location_;

 public:
  enum Kind {
#     define _(name) k##name,
    ASTKINDS(_)
#     undef _
    AstKind_Invalid
  };

  AstNode(const SourceLocation &location)
    : location_(location)
  {
  }
  
  virtual Kind kind() const = 0;

#define _(name)   bool is##name() { return kind() == k##name; }           \
          name *to##name() { assert(is##name()); return (name *)this; }   \
          name *as##name() { if (!is##name()) return nullptr; return to##name(); }
  ASTKINDS(_)
#undef _

  virtual void accept(AstVisitor *visitor) = 0;

  const SourceLocation &loc() const {
    return location_;
  }
};

#define DECLARE_NODE(type)            \
  Kind kind() const {             \
    return k##type;             \
  }                       \
  void accept(AstVisitor *visitor) {      \
    visitor->visit##type(this);       \
  }

class AstVisitor
{
 public:
#define _(name) virtual void visit##name(name *node) { assert(false); }
  ASTKINDS(_)
#undef _
};

class Statement : public AstNode
{
 public:
  Statement(const SourceLocation &pos)
    : AstNode(pos)
  {
  }
};

class Expression : public AstNode
{
 public:
  Expression(const SourceLocation &pos)
    : AstNode(pos)
  {
  }
};

typedef PoolList<Statement *> StatementList;
typedef PoolList<Expression *> ExpressionList;

typedef PoolList<VariableDeclaration *> ParameterList;

class FunctionSignature;

class TypeSpecifier
{
 public:
  enum Attrs {
    Const      = 0x01,
    Variadic   = 0x02,
    ByRef      = 0x04,
    SizedArray = 0x08,
    NewDecl    = 0x10,
    PostDims   = 0x20
  };

 public:
  TypeSpecifier()
   : attrs_(0),
     resolver_(TOK_NONE),
     type_(nullptr)
  {
    dims_ = nullptr;
    assert(rank_ == 0);
  }

  bool needsBinding() const {
    switch (resolver()) {
    case TOK_NAME:
    case TOK_LABEL:
    case TOK_FUNCTION:
      return true;
    default:
      return false;
    }
  }

  void setConst(const SourceLocation &loc) {
    attrs_ |= Const;
    const_loc_ = loc;
  }
  void setVariadic(const SourceLocation &loc) {
    attrs_ |= Variadic;
    variadic_loc_ = loc;
  }
  void setByRef(const SourceLocation &loc) {
    attrs_ |= ByRef;
    sigil_loc_ = loc;
  }
  bool isConst() const {
    return !!(attrs_ & Const);
  }
  bool isByRef() const {
    return !!(attrs_ & ByRef);
  }
  bool isVariadic() const {
    return !!(attrs_ & Variadic);
  }
  const SourceLocation &variadicLoc() const {
    assert(isVariadic());
    return variadic_loc_;
  }
  const SourceLocation &byRefLoc() const {
    assert(isByRef());
    return sigil_loc_;
  }

  void setBuiltinType(TokenKind kind) {
    resolver_ = kind;
  }
  void setNamedType(TokenKind kind, NameProxy *proxy) {
    resolver_ = kind;
    proxy_ = proxy;
  }
  void setFunctionType(FunctionSignature *signature) {
    resolver_ = TOK_FUNCTION;
    signature_ = signature;
  }

  // As a small space-saving optimization, we overlay dims with rank. In many
  // cases the array will have no sized ranks and we'll save allocating a
  // list of nulls - or, in the vast majority of cases - we'll have no arrays
  // at all and we save an extra word on the very common TypeSpecifier.
  void setRank(const SourceLocation &sigil, uint32_t aRank) {
    assert(!rank());
    rank_ = aRank;
    sigil_loc_ = sigil;
  }
  void setDimensionSizes(const SourceLocation &sigil, ExpressionList *dims) {
    assert(!rank());
    dims_ = dims;
    attrs_ |= SizedArray;
    sigil_loc_ = sigil;
  }
  bool isArray() const {
    return rank() > 0;
  }
  const SourceLocation &arrayLoc() const {
    assert(isArray());
    return sigil_loc_;
  }

  ExpressionList *dims() const {
    if (attrs_ & SizedArray)
      return dims_;
    return nullptr;
  }
  Expression *sizeOfRank(uint32_t r) {
    assert(r < rank());
    if (attrs_ & SizedArray)
      return dims_->at(r);
    return nullptr;
  }
  uint32_t rank() const {
    if (attrs_ & SizedArray)
      return dims_->length();
    return rank_;
  }

  TokenKind resolver() const {
    return resolver_;
  }
  NameProxy *proxy() const {
    assert(resolver() == TOK_NAME || resolver() == TOK_LABEL);
    return proxy_;
  }
  FunctionSignature *signature() const {
    assert(resolver() == TOK_FUNCTION);
    return signature_;
  }

  bool isNewDecl() const {
    return !!(attrs_ & NewDecl);
  }
  void setNewDecl() {
    attrs_ |= NewDecl;
  }
  bool hasPostDims() const {
    return !!(attrs_ & PostDims);
  }
  void setHasPostDims() {
    attrs_ |= PostDims;
  }
  bool isFixedArray() const {
    return isNewDecl() && hasPostDims();
  }

  bool isOldDecl() const {
    return !isNewDecl();
  }

  void unsetHasPostDims() {
    attrs_ &= ~PostDims;
  }

  // For reparse_decl().
  void resetWithAttrs(uint32_t attrMask) {
    uint32_t saveAttrs = attrs_ & attrMask;
    *this = TypeSpecifier();
    attrs_ = saveAttrs;
  }
  void resetArray() {
    rank_ = 0;
    dims_ = nullptr;
    attrs_ &= ~(SizedArray|PostDims);
  }

  void setResolved(Type *type) {
    type_ = type;
  }
  Type *resolved() const {
    return type_;
  }

 private:
  SourceLocation const_loc_;
  SourceLocation variadic_loc_;
  SourceLocation sigil_loc_;
  uint32_t attrs_;
  union {
    uint32_t rank_;
    ExpressionList *dims_;
  };
  TokenKind resolver_;
  union {
    NameProxy *proxy_;
    FunctionSignature *signature_;
  };
  Type *type_;
};

class FunctionSignature : public PoolObject
{
 public:
  FunctionSignature(const TypeSpecifier &returnType, ParameterList *parameters)
    : returnType_(returnType),
      parameters_(parameters),
      destructor_(false),
      native_(false)
  {
  }

  TypeSpecifier *returnType() {
    return &returnType_;
  }
  ParameterList *parameters() const {
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

 private:
  TypeSpecifier returnType_;
  ParameterList *parameters_;
  bool destructor_;
  bool native_;
};

class VariableDeclaration : public Statement
{
 public:
  VariableDeclaration(const NameToken &name,
                      const TypeSpecifier &spec,
                      Expression *initialization)
   : Statement(name.start),
     name_(name.atom),
     initialization_(initialization),
     spec_(spec),
     sym_(nullptr),
     next_(nullptr)
  {
  }

  DECLARE_NODE(VariableDeclaration);

  Expression *initialization() const {
    return initialization_;
  }
  Atom *name() const {
    return name_;
  }
  TypeSpecifier *spec() {
    return &spec_;
  }
  void setSymbol(VariableSymbol *sym) {
    assert(!sym_);
    sym_ = sym;
  }
  VariableSymbol *sym() const {
    return sym_;
  }

  // When parsed as a list of declarations, for example:
  //   int x, y, z
  //
  // The declarations are chained together in a linked list.
  void setNext(VariableDeclaration *next) {
    assert(!next_);
    next_ = next;
  }
  VariableDeclaration *next() const {
    return next_;
  }

 private:
  Atom *name_;
  Expression *initialization_;
  TypeSpecifier spec_;
  VariableSymbol *sym_;
  VariableDeclaration *next_;
};

class NameProxy : public Expression
{
  Atom *name_;
  Symbol *binding_;

 public:
  NameProxy(const SourceLocation &pos, Atom *name)
    : Expression(pos),
      name_(name),
      binding_(nullptr)
  {
  }

  DECLARE_NODE(NameProxy);
  
  Atom *name() const {
    return name_;
  }
  Symbol *sym() const {
    return binding_;
  }
  void bind(Symbol *sym) {
    binding_ = sym;
  }
};

class TokenLiteral : public Expression
{
  TokenKind token_;

 public:
  TokenLiteral(const SourceLocation &pos, TokenKind token)
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
  int32_t value_;

 public:
  CharLiteral(const SourceLocation &pos, int32_t value)
    : Expression(pos),
      value_(value)
  {
  }

  DECLARE_NODE(CharLiteral);

  int32_t value() const {
    return value_;
  }
};

class IntegerLiteral : public Expression
{
  int64_t value_;

 public:
  IntegerLiteral(const SourceLocation &pos, int64_t value)
    : Expression(pos),
      value_(value)
  {
  }

  DECLARE_NODE(IntegerLiteral);

  int64_t value() const {
    return value_;
  }
};

class FloatLiteral : public Expression
{
  double value_;

 public:
  FloatLiteral(const SourceLocation &pos, double value)
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
  StringLiteral(const SourceLocation &pos, Atom *literal)
    : Expression(pos),
      literal_(literal)
  {
  }

  DECLARE_NODE(StringLiteral);

  const Atom *literal() const {
    return literal_;
  }
  int32_t arrayLength() const {
    assert(literal()->length() + 1 < INT_MAX);
    return int32_t(literal()->length() + 1);
  }

 private:
  Atom *literal_;
};

class NameAndValue : public PoolObject
{
 public:
  NameAndValue(const Token &name, Expression *expr)
   : name_(name),
     expr_(expr)
  {
  }

  Atom *name() const {
    return name_.atom();
  }
  Expression *expr() const {
    return expr_;
  }

 private:
  Token name_;
  Expression *expr_;
};

typedef PoolList<NameAndValue *> NameAndValueList;

class StructInitializer : public Expression
{
 public:
  StructInitializer(const SourceLocation &loc, NameAndValueList *pairs)
   : Expression(loc),
     pairs_(pairs)
  {
  }

  DECLARE_NODE(StructInitializer);

  NameAndValueList *pairs() const {
    return pairs_;
  }

 private:
  NameAndValueList *pairs_;
};

class ArrayLiteral : public Expression
{
 public:
  ArrayLiteral(const SourceLocation &pos, TokenKind token, ExpressionList *expressions, bool repeatLastElement)
    : Expression(pos),
      token_(token),
      expressions_(expressions),
      repeatLastElement_(repeatLastElement)
  {
  }

  DECLARE_NODE(ArrayLiteral);

  TokenKind token() const {
    return token_;
  }
  int32_t arrayLength() const {
    assert(expressions()->length() < INT_MAX);
    return int32_t(expressions()->length());
  }
  bool isFixedArrayLiteral() const {
    return token() == TOK_LBRACE;
  }
  ExpressionList *expressions() const {
    return expressions_;
  }
  bool repeatLastElement() const {
    return repeatLastElement_;
  }

 private:
  TokenKind token_;
  ExpressionList *expressions_;
  bool repeatLastElement_;
};

class SizeofExpression : public Expression
{
 public:
  SizeofExpression(const SourceLocation &pos, NameProxy *proxy, size_t level)
   : Expression(pos),
     proxy_(proxy),
     level_(level)
  {}

  DECLARE_NODE(SizeofExpression);

  NameProxy *proxy() const {
    return proxy_;
  }
  size_t level() const {
    return level_;
  }

 private:
  NameProxy *proxy_;
  size_t level_;
};

class UnaryExpression : public Expression
{
 public:
  UnaryExpression(const SourceLocation &pos, TokenKind token, Expression *expr)
   : Expression(pos),
     expression_(expr),
     token_(token),
     tag_(nullptr)
  {
  }

  UnaryExpression(const SourceLocation &pos, TokenKind token, Expression *expr,
          NameProxy *tag)
    : Expression(pos),
      expression_(expr),
      token_(token),
      tag_(tag)
  {
  }

  DECLARE_NODE(UnaryExpression);

  Expression *expression() const {
    return expression_;
  }
  TokenKind token() const {
    return token_;
  }
  NameProxy *tag() const {
    return tag_;
  }

 private:
  Expression *expression_;
  TokenKind token_;
  NameProxy *tag_;
};

class ThisExpression : public Expression
{
 public:
  ThisExpression(const SourceLocation &pos)
   : Expression(pos)
  {}

  DECLARE_NODE(ThisExpression);
};

class BinaryExpression : public Expression
{
  Expression *left_;
  Expression *right_;
  TokenKind token_;

 public:
  BinaryExpression(const SourceLocation &pos, TokenKind token, Expression *left, Expression *right)
   : Expression(pos),
     left_(left),
     right_(right),
     token_(token)
  {
  }

  DECLARE_NODE(BinaryExpression);

  Expression *left() const {
    return left_;
  }
  Expression *right() const {
    return right_;
  }
  TokenKind token() const {
    return token_;
  }
};

class TernaryExpression : public Expression
{
  Expression *condition_;
  Expression *left_;
  Expression *right_;

 public:
  TernaryExpression(const SourceLocation &pos, Expression *condition, Expression *left, Expression *right)
    : Expression(pos),
      condition_(condition),
      left_(left),
      right_(right)
  {
  }

  DECLARE_NODE(TernaryExpression);

  Expression *condition() const {
    return condition_;
  }
  Expression *left() const {
    return left_;
  }
  Expression *right() const {
    return right_;
  }
};

class FieldExpression : public Expression
{
 public:
  FieldExpression(const SourceLocation &pos, Expression *base, const NameToken &field)
   : Expression(pos),
     base_(base),
     field_(field)
  {} 

  DECLARE_NODE(FieldExpression);

  Expression *base() const {
    return base_;
  }
  Atom *field() const {
    return field_.atom;
  }

 private:
  Expression *base_;
  NameToken field_;
};

class IndexExpression : public Expression
{
 public:
  IndexExpression(const SourceLocation &pos, Expression *left, Expression *right)
   : Expression(pos),
     left_(left),
     right_(right)
  {
  }

  DECLARE_NODE(IndexExpression);

  Expression *left() const {
    return left_;
  }
  Expression *right() const {
    return right_;
  }

 private:
  Expression *left_;
  Expression *right_;
};

class CallExpression : public Expression
{
 public:
  CallExpression(const SourceLocation &pos, Expression *callee, ExpressionList *arguments)
    : Expression(pos),
      callee_(callee),
      arguments_(arguments)
  {
  }

  DECLARE_NODE(CallExpression);

  Expression *callee() const {
    return callee_;
  }
  ExpressionList *arguments() const {
    return arguments_;
  }

 private:
  Expression *callee_;
  ExpressionList *arguments_;
};

class ForStatement : public Statement
{
  Statement *initialization_;
  Expression *condition_;
  Statement *update_;
  Statement *body_;
  Scope *scope_;

 public:
  ForStatement(const SourceLocation &pos, Statement *initialization,
               Expression *condition, Statement *update, Statement *body)
    : Statement(pos),
      initialization_(initialization),
      condition_(condition),
      update_(update),
      body_(body),
      scope_(nullptr)
  {
  }

  DECLARE_NODE(ForStatement);

  Statement *initialization() const {
    return initialization_;
  }
  Expression *condition() const {
    return condition_;
  }
  Statement *update() const {
    return update_;
  }
  Statement *body() const {
    return body_;
  }
  Scope *scope() const {
    return scope_;
  }
  void setScope(Scope *scope) {
    scope_ = scope;
  }
};

class WhileStatement : public Statement
{
  TokenKind token_;
  Expression *condition_;
  Statement *body_;

 public:
  WhileStatement(const SourceLocation &pos, TokenKind kind, Expression *condition, Statement *body)
    : Statement(pos),
      token_(kind),
      condition_(condition),
      body_(body)
  {
  }
  
  DECLARE_NODE(WhileStatement);

  TokenKind token() const {
    return token_;
  }
  Expression *condition() const {
    return condition_;
  }
  Statement *body() const {
    return body_;
  }
};

class ReturnStatement : public Statement
{
  Expression *expression_;

 public:
  ReturnStatement(const SourceLocation &pos, Expression *expression)
    : Statement(pos),
      expression_(expression)
  {
  }

  DECLARE_NODE(ReturnStatement);

  Expression *expression() const {
    return expression_;
  }
};

class BreakStatement : public Statement
{
 public:
  BreakStatement(const SourceLocation &pos)
    : Statement(pos)
  {
  }

  DECLARE_NODE(BreakStatement);
};

class ContinueStatement : public Statement
{
 public:
  ContinueStatement(const SourceLocation &pos)
    : Statement(pos)
  {
  }

  DECLARE_NODE(ContinueStatement);
};

class Assignment : public Expression
{
  TokenKind token_;
  Expression *lvalue_;
  Expression *expression_;

 public:
  Assignment(const SourceLocation &pos, TokenKind token, Expression *left, Expression *right)
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
  Expression *lvalue() const {
    return lvalue_;
  }
  Expression *expression() const {
    return expression_;
  }
};

class ExpressionStatement : public Statement
{
  Expression *expression_;

 public:
  ExpressionStatement(Expression *expression)
   : Statement(expression->loc()),
     expression_(expression)
  {
  }

  DECLARE_NODE(ExpressionStatement);

  Expression *expression() const {
    return expression_;
  }
};

// Block statements have a "kind" denoting information about the block.
//   TOK_LBRACE - A normal block started by a {.
//   TOK_FUNCTION - A function body.
class BlockStatement : public Statement
{
  StatementList *statements_;
  Scope *scope_;
  TokenKind type_;

 public:
  BlockStatement(const SourceLocation &pos, StatementList *statements, TokenKind kind)
    : Statement(pos),
      statements_(statements),
      scope_(nullptr),
      type_(kind)
  {
  }

  DECLARE_NODE(BlockStatement);

  StatementList *statements() const {
    return statements_;
  }
  Scope *scope() const {
    return scope_;
  }
  void setScope(Scope *scope) {
    scope_ = scope;
  }
  TokenKind type() const {
    return type_;
  }
};

class MethodBody : public BlockStatement
{
 public:
  MethodBody(const SourceLocation &pos, StatementList *statements, bool returnedValue)
   : BlockStatement(pos, statements, TOK_FUNCTION)
  {
  }

  bool returnedValue() const {
    return returnedValue_;
  }

 private:
  bool returnedValue_;
};

class FunctionNode : public PoolObject
{
 public:
  FunctionNode(TokenKind kind, MethodBody *body, const FunctionSignature &signature)
   : kind_(kind),
     body_(body),
     signature_(signature),
     sym_(nullptr),
     funScope_(nullptr),
     varScope_(nullptr)
  {
  }

  MethodBody *body() const {
    return body_;
  }
  FunctionSignature *signature() {
    return &signature_;
  }
  TokenKind token() const {
    return kind_;
  }
  void setSymbol(FunctionSymbol *sym) {
    assert(!sym_);
    sym_ = sym;
  }
  FunctionSymbol *sym() const {
    return sym_;
  }
  void setScopes(FunctionScope *funScope, Scope *varScope) {
    funScope_ = funScope;
    varScope_ = varScope;
  }
  FunctionScope *funScope() const {
    return funScope_;
  }
  Scope *varScope() const {
    return varScope_;
  }

  // Set if we're shadowing another symbol.
  void setShadowed(FunctionSymbol *shadowed) {
    shadowed_ = shadowed;
  }
  FunctionSymbol *shadowed() const {
    return shadowed_;
  }

 private:
  TokenKind kind_;
  MethodBody *body_;
  FunctionSignature signature_;
  FunctionSymbol *sym_;
  FunctionScope *funScope_;
  Scope *varScope_;
  FunctionSymbol *shadowed_;
};

class FunctionStatement :
  public Statement,
  public FunctionNode
{
 public:
  FunctionStatement(const NameToken &name, TokenKind kind, MethodBody *body, const FunctionSignature &signature)
   : Statement(name.start),
     FunctionNode(kind, body, signature),
     name_(name)
  {
  }

  DECLARE_NODE(FunctionStatement);

  Atom *name() const {
    return name_.atom;
  }

 private:
  NameToken name_;
};

class IfStatement : public Statement
{
  Expression *condition_;
  Statement *ifTrue_;
  Statement *ifFalse_;

 public:
  IfStatement(const SourceLocation &pos, Expression *condition, Statement *ifTrue)
    : Statement(pos),
      condition_(condition),
      ifTrue_(ifTrue),
      ifFalse_(nullptr)
  {
  }

  DECLARE_NODE(IfStatement);

  Expression *condition() const {
    return condition_;
  }
  Statement *ifTrue() const {
    return ifTrue_;
  }
  Statement *ifFalse() const {
    return ifFalse_;
  }
  void setIfFalse(Statement *ifFalse) {
    ifFalse_ = ifFalse;
  }
};

class EnumStatement : public Statement
{
  Atom *name_;
  TypeSymbol *sym_;

 public:
  struct Entry {
    NameProxy *proxy;
    Expression *expr;
    ConstantSymbol *sym;

    Entry(NameProxy *proxy, Expression *expr)
      : proxy(proxy),
        expr(expr),
        sym(nullptr)
    {
    }
  };

  typedef PoolList<Entry> EntryList;

 public:
  EnumStatement(const SourceLocation &pos, Atom *name, EntryList *entries)
    : Statement(pos),
      name_(name),
      sym_(nullptr),
      entries_(entries)
  {
  }

  DECLARE_NODE(EnumStatement);

  Atom *name() const {
    return name_;
  }
  EntryList *entries() const {
    return entries_;
  }
  void setSymbol(TypeSymbol *sym) {
    assert(!sym_);
    sym_ = sym;
  }
  TypeSymbol *sym() const {
    return sym_;
  }
  TokenKind visibility() const {
    return visibility_;
  }

 private:
  EntryList *entries_;
  TokenKind visibility_;
};

class IncDecExpression : public Expression
{
  TokenKind token_;
  Expression *expression_;
  bool postfix_;

 public:
  IncDecExpression(const SourceLocation &pos, TokenKind token, Expression *expression, bool postfix)
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
  Expression *expression() const {
    return expression_;
  }
  bool postfix() const {
    return postfix_;
  }
};

class Case : public PoolObject
{
 public:
  Case(Expression *expression, ExpressionList *others, Statement *statement)
    : expression_(expression),
    others_(others),
    statement_(statement)
  {
  }

  Expression *expression() const {
    return expression_;
  }
  PoolList<Expression *> *others() const {
    return others_;
  }
  Statement *statement() const {
    return statement_;
  }

 private:
  Expression *expression_;
  PoolList <Expression *> *others_;
  Statement *statement_;
};

struct CaseValue
{
  BoxedPrimitive box;
  size_t statement;

  CaseValue(size_t statement)
    : statement(statement)
  {
  }
};

typedef PoolList<CaseValue> CaseValueList;

class SwitchStatement : public Statement
{
 public:
  SwitchStatement(const SourceLocation &pos, Expression *expression, PoolList<Case *> *cases,
                  Statement *def)
   : Statement(pos),
     expression_(expression),
     cases_(cases),
     default_(def),
     table_(nullptr)
  {
  }

  DECLARE_NODE(SwitchStatement);

  Expression *expression() const {
    return expression_;
  }
  PoolList<Case *> *cases() const {
    return cases_;
  }
  Statement *defaultCase() const {
    return default_;
  }
  void setCaseValueList(CaseValueList *list) {
    table_ = list;
  }
  CaseValueList *caseValueList() const {
    return table_;
  }

 private:
  Expression *expression_;
  PoolList<Case *> *cases_;
  Statement *default_;
  CaseValueList *table_;
};

class FunctionOrAlias
{
 public:
  FunctionOrAlias()
   : fun_(nullptr),
     alias_(nullptr)
  {
  }
  FunctionOrAlias(FunctionNode *fun)
   : fun_(fun),
     alias_(nullptr)
  { }
  FunctionOrAlias(NameProxy *alias)
   : fun_(nullptr),
     alias_(alias)
  {
  }

  bool isEmpty() const {
    return !alias_ && !fun_;
  }

  bool isAlias() const {
    return !!alias_;
  }
  NameProxy *alias() const {
    assert(isAlias());
    return alias_;
  }

  bool isFunction() const {
    return !!fun_;
  }
  FunctionNode *fun() const {
    return fun_;
  }

 private:
  FunctionNode *fun_;
  NameProxy *alias_;
};

class LayoutEntry : public PoolObject
{
 public:
  enum Type {
    Field,
    Accessor,
    Method
  };

  LayoutEntry(const NameToken &name, const TypeSpecifier &spec)
   : type_(Field),
     name_(name),
     spec_(spec)
  {
  }
  LayoutEntry(const NameToken &name, const FunctionOrAlias &method)
    : type_(Method),
      name_(name)
  {
    *method_.address() = method;
  }
  LayoutEntry(const NameToken &name, const TypeSpecifier &spec,
              const FunctionOrAlias &getter, const FunctionOrAlias &setter)
   : type_(Accessor),
     name_(name),
     spec_(spec),
     setter_(setter)
  {
    *getter_.address() = getter;
  }

  Type type() const {
    return type_;
  }
  Atom *name() const {
    return name_.atom;
  }
  TypeSpecifier *spec() {
    assert(type() == Field || type() == Accessor);
    return &spec_;
  }
  const FunctionOrAlias &method() const {
    assert(type() == Method);
    return *method_.address();
  }
  const FunctionOrAlias &getter() const {
    assert(type() == Accessor);
    return *getter_.address();
  }
  const FunctionOrAlias &setter() const {
    assert(type() == Accessor);
    return setter_;
  }

 private:
  Type type_;
  NameToken name_;
  TypeSpecifier spec_;
  union {
    NameProxy *alias_;
    StorageBuffer<FunctionOrAlias> method_;
    StorageBuffer<FunctionOrAlias> getter_;
  };
  FunctionOrAlias setter_;
};

typedef PoolList<LayoutEntry *> LayoutList;

class LayoutStatement : public Statement
{
 public:
  LayoutStatement(const SourceLocation &loc, TokenKind spec, const NameToken &name, NameProxy *parent, LayoutList *body)
   : Statement(loc),
     name_(name),
     spec_(spec),
     parent_(parent),
     body_(body),
     sym_(nullptr),
     nullable_(spec == TOK_CLASS)
  {
  }

  DECLARE_NODE(LayoutStatement);

  Atom *name() const {
    return name_.atom;
  }
  TokenKind spec() const {
    return spec_;
  }
  LayoutList *body() const {
    return body_;
  }
  bool nullable() const {
    return nullable_;
  }
  void setNullable() {
    nullable_ = true;
  }

  void setSymbol(Symbol *sym) {
    sym_ = sym;
  }
  Symbol *sym() const {
    return sym_;
  }

 private:
  NameToken name_;
  TokenKind spec_;
  NameProxy *parent_;
  LayoutList *body_;
  Symbol *sym_;
  bool nullable_;
};

class TypedefStatement : public Statement
{
 public:
  TypedefStatement(const SourceLocation &pos, Atom *name, const TypeSpecifier &spec)
   : Statement(pos),
     name_(name),
     spec_(spec),
     sym_(nullptr)
  {
  }

  DECLARE_NODE(TypedefStatement);

  Atom *name() const {
    return name_;
  }
  TypeSpecifier *spec() {
    return &spec_;
  }

  TypeSymbol *sym() const {
    return sym_;
  }
  void setSymbol(TypeSymbol *sym) {
    sym_ = sym;
  }

 private:
  Atom *name_;
  TypeSpecifier spec_;
  TypeSymbol *sym_;
};

typedef PoolList<Atom *> NameList;
typedef PoolList<NameProxy *> NameProxyList;

#undef DECLARE_NODE

class ParseTree : public PoolObject
{
  StatementList *statements_;

 public:
  ParseTree(StatementList *statements)
    : statements_(statements)
  {
  }

  void dump(FILE *fp);

  StatementList *statements() const {
    return statements_;
  }
};

}

#endif // _include_sourcepawn_ast_h_
