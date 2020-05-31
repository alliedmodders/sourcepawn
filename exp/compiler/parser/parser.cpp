// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012 David Anderson
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
#include <string.h>
#include "shared/string-pool.h"
#include "parser.h"
#include "preprocessor.h"
#include "sema/name-resolver.h"

using namespace ke;

namespace sp {

using namespace ast;

Parser::Parser(CompileContext& cc, Preprocessor& pp, NameResolver& resolver)
: cc_(cc),
  pool_(cc_.pool()),
  scanner_(pp),
  options_(cc_.options()),
  delegate_(resolver),
  allowDeclarations_(true),
  uses_handle_intrinsics_(false)
{
  atom_Float_ = cc_.add("Float");
  atom_String_ = cc_.add("String");
  atom_underbar_ = cc_.add("_");
}

bool
Parser::peek(TokenKind kind)
{
  return scanner_.peek() == kind;
}

bool
Parser::match(TokenKind kind)
{
  return scanner_.match(kind);
}

bool
Parser::expect(TokenKind kind)
{
  return scanner_.expect(kind);
}

Atom*
Parser::maybeName()
{
  if (!match(TOK_NAME))
    return nullptr;
  return scanner_.current_name();
}

Atom*
Parser::expectName()
{
  if (!expect(TOK_NAME))
    return nullptr;
  return scanner_.current_name();
}

// Requires ; or \n.
bool
Parser::requireTerminator()
{
  TokenKind tok = scanner_.peekTokenSameLine();
  if (tok == TOK_SEMICOLON) {
    scanner_.next();
    return true;
  }

  // We always require that the semicolon be on the same line.
  if (options_.RequireSemicolons)
    cc_.report(scanner_.end(), rmsg::expected_semicolon);

  if (tok == TOK_EOL)
    return true;

  cc_.report(scanner_.end(), rmsg::expected_newline_or_semicolon);
  return false;
}

// Requires \n on the same line.
bool
Parser::requireNewline()
{
  if (scanner_.peekTokenSameLine() == TOK_EOL)
    return true;
  cc_.report(scanner_.end(), rmsg::expected_newline);
  return false;
}

// Requires ; or \n on the same line.
bool
Parser::requireNewlineOrSemi()
{
  if (scanner_.peekTokenSameLine() == TOK_SEMICOLON)
    scanner_.next();
  if (scanner_.peekTokenSameLine() == TOK_EOL)
    return true;
  cc_.report(scanner_.end(), rmsg::expected_newline);
  return false;
}

class AutoEnterScope
{
 public:
  AutoEnterScope(NameResolver& delegate, Scope::Kind kind, Scope** ptr)
   : delegate_(delegate),
     kind_(kind),
     ptr_(ptr)
  {
    delegate_.OnEnterScope(kind);
  }
  ~AutoEnterScope() {
    *ptr_ = delegate_.OnLeaveScope();
  }

 private:
  NameResolver& delegate_;
  Scope::Kind kind_;
  Scope** ptr_;
};

NameProxy*
Parser::nameref(const Token* tok)
{
  if (!tok)
    tok = scanner_.current();

  NameProxy* proxy = new (pool_) NameProxy(tok->start.loc, tok->atom());

  delegate_.OnNameProxy(proxy);
  return proxy;
}

NameProxy*
Parser::tagref(const Token* tok)
{
  if (!tok)
    tok = scanner_.current();

  NameProxy* proxy = new (pool_) NameProxy(tok->start.loc, tok->atom());

  delegate_.OnTagProxy(proxy);
  return proxy;
}

void
Parser::parse_new_typename(TypeSpecifier* spec, const Token* tok)
{
  spec->setBaseLoc(tok->start.loc);

  if (IsNewTypeToken(tok->kind)) {
    spec->setBuiltinType(tok->kind);
    return;
  }

  if (tok->kind == TOK_LABEL) {
    NameProxy* proxy = nameref();
    spec->setNamedType(TOK_LABEL, proxy);
    cc_.report(scanner_.begin(), rmsg::label_in_newdecl);
    return;
  }

  if (tok->kind != TOK_NAME) {
    cc_.report(scanner_.begin(), rmsg::expected_typeexpr);
    return;
  }

  NameProxy* proxy = nameref();
  spec->setNamedType(TOK_NAME, proxy);

  if (proxy->name() == atom_Float_)
    cc_.report(scanner_.begin(), rmsg::type_is_deprecated) << "Float" << "float";
  else if (proxy->name() == atom_String_)
    cc_.report(scanner_.begin(), rmsg::type_is_deprecated) << "String" << "char";
  else if (proxy->name() == atom_underbar_)
    cc_.report(scanner_.begin(), rmsg::type_is_deprecated) << "_" << "int";
}

void
Parser::parse_function_type(TypeSpecifier* spec, uint32_t flags)
{
  // Current token is TOK_FUNCTION.
  spec->setBaseLoc(scanner_.begin());

  TypeSpecifier returnType;
  parse_new_type_expr(&returnType, 0);

  // :TODO: only allow new-style arguments.
  ParameterList* params;
  bool canResolveEagerly = true;
  {
    delegate_.OnEnterScope(Scope::Argument);
    if ((params = arguments(&canResolveEagerly)) == nullptr)
      params = new (pool_) ParameterList();
    delegate_.OnLeaveOrphanScope();
  }

  FunctionSignature* sig =
    delegate_.HandleFunctionSignature(TOK_PUBLIC, returnType, params, canResolveEagerly);
  spec->setFunctionType(sig);
}

void
Parser::parse_new_type_expr(TypeSpecifier* spec, uint32_t flags)
{
  if (match(TOK_CONST)) {
    if (spec->isConst())
      cc_.report(scanner_.begin(), rmsg::const_specified_twice);
    spec->setConst(scanner_.begin());
  }

  bool lparen = false;
  bool function = false;
  if (match(TOK_LPAREN)) {
    if (match(TOK_FUNCTION)) {
      function = true;
      lparen = true;
    } else {
      scanner_.undo();
    }
  } else {
    function = match(TOK_FUNCTION);
  }

  if (function)
    parse_function_type(spec, flags);
  else
    parse_new_typename(spec, scanner_.nextToken());

  if (lparen)
    match(TOK_RPAREN);

  // If we didn't already fill out array dimensions, do so now.
  if (!spec->isArray() && match(TOK_LBRACKET)) {
    SourceLocation begin = scanner_.begin();
    uint32_t rank = 0;
    do {
      rank++;
      if (!match(TOK_RBRACKET))
        cc_.report(scanner_.begin(), rmsg::fixed_array_in_prefix);
    } while (match(TOK_LBRACKET));
    spec->setRank(begin, rank);
  }

  if (flags & DeclFlags::Argument) {
    if (match(TOK_AMPERSAND))
      spec->setByRef(scanner_.begin());
  }

  spec->setNewDecl();
}

bool
Parser::parse_new_decl(Declaration* decl, uint32_t flags)
{
  parse_new_type_expr(&decl->spec, flags);

  if (flags & DeclFlags::NamedMask) {
    // special case for |typexpr ...|
    if ((flags & DeclFlags::Argument) && match(TOK_ELLIPSES)) {
      decl->spec.setVariadic(scanner_.begin());
      return true;
    }

    bool named = false;
    if (flags & DeclFlags::MaybeNamed) {
      named = match(TOK_NAME);
    } else {
      if (!expect(TOK_NAME))
        return false;
      named = true;
    }

    if (named) {
      decl->name = scanner_.current();
      if (match(TOK_LBRACKET))
        parse_old_array_dims(decl, flags);
    }
  }

  return true;
}

void
Parser::parse_old_array_dims(Declaration* decl, uint32_t flags)
{
  TypeSpecifier* spec = &decl->spec;

  SourceLocation loc = scanner_.begin();

  uint32_t rank = 0;
  ExpressionList* dims = nullptr;
  do {
    rank++;

    // Check if the size is unspecified.
    if (match(TOK_RBRACKET)) {
      if (dims)
        dims->push_back(nullptr);
      continue;
    }

    if (!dims) {
      // Allocate a list to store dimension sizes, populating our previous
      // indeterminate dimensions with null.
      dims = new (pool_) ExpressionList();
      for (uint32_t i = 0; i < rank - 1; i++)
        dims->push_back(nullptr);
    }

    Expression* expr = expression();
    if (!expr)
      break;
    dims->push_back(expr);

    if (!expect(TOK_RBRACKET))
      break;
  } while (match(TOK_LBRACKET));

  if (spec->isArray()) {
    cc_.report(loc, rmsg::double_array_dims);
    return;
  }

  if (dims)
    spec->setDimensionSizes(loc, dims);
  else
    spec->setRank(loc, rank);

  spec->setHasPostDims();
}

bool
Parser::parse_old_decl(Declaration* decl, uint32_t flags)
{
  TypeSpecifier* spec = &decl->spec;

  if (match(TOK_CONST)) {
    if (spec->isConst())
      cc_.report(scanner_.begin(), rmsg::const_specified_twice);
    spec->setConst(scanner_.begin());
  }

  if (flags & DeclFlags::Argument) {
    if (match(TOK_AMPERSAND))
      spec->setByRef(scanner_.begin());
  }

  if (match(TOK_LABEL)) {
    spec->setBaseLoc(scanner_.begin());

    Atom* atom = scanner_.current_name();
    if (Type* type = cc_.types()->typeForLabelAtom(atom))
      spec->setResolvedBaseType(type);
    else
      spec->setNamedType(TOK_LABEL, tagref());
  } else {
    spec->setBuiltinType(TOK_IMPLICIT_INT);
  }

  // Look for varargs and end early.
  if ((flags & DeclFlags::Argument) && match(TOK_ELLIPSES)) {
    // If we just saw "...", make sure we set a base location even though we
    // never got one.
    if (spec->resolver() == TOK_IMPLICIT_INT)
      spec->setBaseLoc(scanner_.begin());
    spec->setVariadic(scanner_.begin());
    return true;
  }

  if (flags & DeclFlags::NamedMask) {
    // If this is label-less, check for something like "new int x".
    if (!peek(TOK_NAME) && (flags & (DeclFlags::Variable|DeclFlags::Argument))) {
      TokenKind kind = scanner_.next();
      if (IsNewTypeToken(kind))
        cc_.report(scanner_.begin(), rmsg::newdecl_with_new);
      else
        scanner_.undo();
    }
    if (!expect(TOK_NAME))
      return false;

    decl->name = scanner_.current();
    spec->setBaseLoc(decl->name.start);

    if (match(TOK_LBRACKET))
      parse_old_array_dims(decl, flags);
  } else {
    assert(spec->resolver() != TOK_IMPLICIT_INT);
  }

  // :TODO: require newdecls
  return true;
}

bool
Parser::reparse_decl(Declaration* decl, uint32_t flags)
{
  if (!decl->spec.isNewDecl()) {
    decl->spec.resetWithAttrs(TypeSpecifier::Const);
    return parse_old_decl(decl, flags);
  }

  // Newer decls are more complex to reparse.
  if (!expect(TOK_NAME))
    return false;
  decl->name = scanner_.current();

  if (decl->spec.hasPostDims()) {
    // We have something like:
    //   int x[], y...
    //
    // Reset the fact that we saw an array.
    decl->spec.resetArray();
    if (match(TOK_LBRACKET))
      parse_old_array_dims(decl, flags);
  } else {
    // Note: in spcomp2, we just have to make sure we're not doubling up on
    // dimension lists. In spcomp1, we had to reset the dimension sizes as
    // well because parsing initializers would change them.
    if (match(TOK_LBRACKET)) {
      if (decl->spec.isArray())
        cc_.report(scanner_.begin(), rmsg::double_array_dims);
    }

    assert(!decl->spec.dims());
  }

  return true;
}

// The infamous parse_decl() from spcomp1.
bool
Parser::parse_decl(Declaration* decl, uint32_t flags)
{
  // Match early varargs as old decls.
  if ((flags & DeclFlags::Argument) && peek(TOK_ELLIPSES))
    return parse_old_decl(decl, flags);

  // Match const first - it's a common prefix for old and new decls.
  if (match(TOK_CONST))
    decl->spec.setConst(scanner_.begin());

  // Sometimes we know ahead of time whether the declaration will be old, for
  // example, if preceded by "new" or "decl".
  if (flags & DeclFlags::Old)
    return parse_old_decl(decl, flags);

  // If parsing an argument, there are two simple checks for whether this is a
  // new or old-style declaration.
  if ((flags & DeclFlags::Argument) && (peek(TOK_AMPERSAND) || peek(TOK_LBRACE)))
    return parse_old_decl(decl, flags);

  // Another dead giveaway is a label. Operators would work here too, but they
  // are not supported in spcomp2.
  if (peek(TOK_LABEL))
    return parse_old_decl(decl, flags);

  // Otherwise, eat a symbol and try to see what's afterit.
  if (match(TOK_NAME)) {
    if (peek(TOK_NAME) || peek(TOK_AMPERSAND) || peek(TOK_ELLIPSES)) {
      // This is a new-style declaration. Given the name back to the lexer.
      scanner_.undo();
      return parse_new_decl(decl, flags);
    }

    // Make sure to save the name token locally first.
    Token name = *scanner_.current();
    if ((flags & DeclFlags::NamedMask) && match(TOK_LBRACKET)) {
      // Set the base loc early in case we end up not seeing a newdecl.
      decl->spec.setBaseLoc(name.start.loc);

      // Oh no - we have to parse array dims before we ca n tell what kind of
      // declarator this is. It could be either:
      //   "x[] y" (new-style), or
      //   "y[],"  (old-style)
      parse_old_array_dims(decl, flags);

      if (peek(TOK_NAME) || peek(TOK_AMPERSAND)) {
        // This must be a newdecl, "x[] y" or "x[] &y", the latter of which
        // is illegal, but we flow it through the right path anyway.
        decl->spec.unsetHasPostDims();
        scanner_.injectBack(name);
        return parse_new_decl(decl, flags);
      }

      // We got something like "x[]". Just finish off the declaration.
      decl->name = name;
      decl->spec.setBuiltinType(TOK_INT);
      return true;
    }

    // Give the symbol back to the lexer; this is an old decl.
    scanner_.undo();
    return parse_old_decl(decl, flags);
  }

  // All else has failed. Probably got a type keyword. Try new-style.
  return parse_new_decl(decl, flags);
}

Expression*
Parser::primitive()
{
  switch (scanner_.next()) {
    case TOK_FLOAT_LITERAL:
    {
      const Token* tok = scanner_.current();
      return new (pool_) FloatLiteral(scanner_.begin(), tok->doubleValue());
    }

    case TOK_HEX_LITERAL:
    {
      const Token* tok = scanner_.current();
      return new (pool_) IntegerLiteral(scanner_.begin(), tok->int64Value());
    }

    case TOK_INTEGER_LITERAL:
    {
      const Token* tok = scanner_.current();
      return new (pool_) IntegerLiteral(scanner_.begin(), tok->int64Value());
    }

    case TOK_M_LINE:
    {
      SourceLocation pos = scanner_.begin();
      unsigned line = cc_.source().getLine(pos);
      return new (pool_) IntegerLiteral(pos, line);
    }

    case TOK_TRUE:
    case TOK_FALSE:
    case TOK_NULL:
    case TOK_INVALID_FUNCTION:
      return new (pool_) TokenLiteral(scanner_.begin(), scanner_.current()->kind);

    case TOK_STRING_LITERAL:
    {
      Atom* literal = scanner_.current_name();
      return new (pool_) StringLiteral(scanner_.begin(), literal);
    }

    case TOK_CHAR_LITERAL:
    {
      const Token* tok = scanner_.current();
      return new (pool_) CharLiteral(scanner_.begin(), tok->charValue());
    }

    case TOK_THIS:
      return new (pool_) ThisExpression(scanner_.begin());

    case TOK_LBRACE:
      // :TODO: only allow this from initializers
      return parseCompoundLiteral();

    default:
    {
      TokenKind kind = scanner_.current()->kind;
      if (kind != TOK_UNKNOWN) {
        cc_.report(scanner_.begin(), rmsg::expected_expression)
          << TokenNames[kind];
      }
      return nullptr;
    }
  }
}

Expression*
Parser::parseStructInitializer(const SourceLocation& pos)
{
  NameAndValueList* pairs = new (pool_) NameAndValueList();

  while (!match(TOK_RBRACE)) {
    if (!expect(TOK_NAME))
      return nullptr;
    NameToken name = *scanner_.current();

    if (!match(TOK_ASSIGN))
      return nullptr;

    Expression* expr = expression();
    if (!expr)
      return nullptr;

    // Eat an optional comma.
    match(TOK_COMMA);

    pairs->push_back(new (pool_) NameAndValue(name, expr));
  }

  return new (pool_) StructInitializer(pos, pairs);
}

Expression*
Parser::parseCompoundLiteral()
{
  SourceLocation pos = scanner_.begin();

  // If the syntax is <literal> "=" we treat this as a struct initializer.
  if (match(TOK_NAME)) {
    bool assigns = peek(TOK_ASSIGN);

    // Push the name back.
    scanner_.undo();

    if (assigns)
      return parseStructInitializer(pos);
  }

  // Otherwise, we need to build a list.
  ExpressionList* list = new (pool_) ExpressionList();
  bool repeatLastElement = false;
  while (!peek(TOK_RBRACE)) {
    if (match(TOK_ELLIPSES)) {
      repeatLastElement = true;
      break;
    }

    Expression* item = expression();
    if (!item)
      return nullptr;
    list->push_back(item);

    if (!match(TOK_COMMA))
      break;
  }
  expect(TOK_RBRACE);

  return new (pool_) ArrayLiteral(pos, list, repeatLastElement);
}

Expression*
Parser::prefix()
{
  switch (scanner_.next()) {
    case TOK_LPAREN:
    {
      Expression* expr = expression();
      if (!expr)
        return nullptr;
      if (!expect(TOK_RPAREN))
        return nullptr;
      return expr;
    }

    case TOK_NEW:
    {
      SourceLocation loc = scanner_.begin();

      TypeSpecifier spec;
      parse_new_typename(&spec, scanner_.nextToken());

      if (peek(TOK_LBRACKET)) {
        // :TODO: this is not the final grammar. Match C++?
        ExpressionList* dims = dimensions();
        if (!dims)
          return nullptr;
        return delegate_.HandleNewArrayExpr(loc, spec, dims);
      }

      if (!expect(TOK_LPAREN))
        return nullptr;
      ExpressionList* args = callArgs();
      if (!args)
        return nullptr;

      return delegate_.HandleCallNewExpr(loc, spec, args);
    }

    // Same as the label version, in unary().
    case TOK_VIEW_AS:
    {
      SourceLocation pos = scanner_.begin();
      if (!expect(TOK_LT))
        return nullptr;

      TypeSpecifier spec;
      parse_new_type_expr(&spec, 0);

      if (!expect(TOK_GT))
        return nullptr;
      if (!expect(TOK_LPAREN))
        return nullptr;
      Expression* expr = expression();
      if (!expr)
        return nullptr;
      if (!expect(TOK_RPAREN))
        return nullptr;

      return delegate_.HandleViewAs(pos, spec, expr);
    }

    case TOK_NAME:
      return nameref();

    default:
    {
      const Token* tok = scanner_.current();
      if (IsNewTypeToken(tok->kind)) {
        // Treat the type as a name, even though it's a keyword.
        return nameref(tok);
      }

      scanner_.undo();
      return primitive();
    }
  }
}

// Caller must lex '('.
ExpressionList*
Parser::callArgs()
{
  ExpressionList* arguments = new (pool_) ExpressionList();
  
  if (!match(TOK_RPAREN)) {
    while (true) {
      Expression* expr = expression();
      if (!expr)
        return nullptr;

      arguments->push_back(expr);

      if (!match(TOK_COMMA))
        break;
    }

    if (!expect(TOK_RPAREN))
      return nullptr;
  }

  return arguments;
}

Expression*
Parser::dotfield(Expression* base)
{
  expect(TOK_DOT);

  SourceLocation pos = scanner_.begin();

  if (!expect(TOK_NAME))
    return nullptr;
  NameToken name = *scanner_.current();

  return new (pool_) FieldExpression(pos, base, name);
}

Expression*
Parser::index(Expression* left)
{
  expect(TOK_LBRACKET);
  
  SourceLocation pos = scanner_.begin();
  Expression* expr = expression();
  if (!expr)
    return nullptr;

  if (!expect(TOK_RBRACKET))
    return nullptr;

  return new (pool_) IndexExpression(pos, left, expr);
}

Expression*
Parser::primary()
{
  Expression* expr = prefix();
  if (!expr)
    return nullptr;

  for (;;) {
    switch (scanner_.peek()) {
      case TOK_LPAREN:
      {
        expect(TOK_LPAREN);

        ExpressionList* args = callArgs();
        if (!args)
          return nullptr;

        return new (pool_) CallExpression(expr->loc(), expr, args);
      }

      case TOK_DOT:
        if ((expr = dotfield(expr)) == nullptr)
          return nullptr;
        break;

      case TOK_LBRACKET:
        if ((expr = index(expr)) == nullptr)
          return nullptr;
        break;

      default:
        return expr;
    }
  }
}

Expression*
Parser::unary()
{
  TokenKind token = scanner_.peek();

  switch (token) {
    case TOK_INCREMENT:
    case TOK_DECREMENT:
    {
      scanner_.next();
      SourceLocation pos = scanner_.begin();
      Expression* expr = unary();
      if (!expr)
        return nullptr;
      return new (pool_) IncDecExpression(pos, token, expr, false);
    }

    case TOK_MINUS:
    case TOK_NOT:
    case TOK_TILDE:
    {
      scanner_.next();
      SourceLocation pos = scanner_.begin();
      Expression* expr = unary();
      if (!expr)
        return nullptr;
      if (token == TOK_MINUS)
        token = TOK_NEGATE;
      return new (pool_) UnaryExpression(pos, token, expr);
    }

    case TOK_SIZEOF:
    {
      scanner_.next();
      return parseSizeof();
    }

    case TOK_LABEL:
    {
      scanner_.next();
      SourceLocation pos = scanner_.begin();

      TypeSpecifier spec;
      spec.setBaseLoc(scanner_.begin());

      Atom* atom = scanner_.current_name();
      if (Type* type = cc_.types()->typeForLabelAtom(atom))
        spec.setResolvedBaseType(type);
      else
        spec.setNamedType(TOK_LABEL, tagref());

      Expression* expr = unary();
      if (!expr)
        return nullptr;
      return delegate_.HandleViewAs(pos, spec, expr);
    }

    default:
      if (IsNewTypeToken(token)) {
        scanner_.next();
        
        if (peek(TOK_COLON)) {
          cc_.report(scanner_.begin(), rmsg::need_view_as_operator)
            << (cc_.note(scanner_.begin(), rmsg::view_as_example) <<
                scanner_.current_name());
          return nullptr;
        }

        // This won't parse, but give the token back anyway.
        scanner_.undo();
      }
      break;
  }

  Expression* expr = primary();

  token = scanner_.peek();
  if (token == TOK_INCREMENT || token == TOK_DECREMENT) {
    scanner_.next();
    return new (pool_) IncDecExpression(scanner_.begin(), token, expr, true);
  }

  return expr;
}

Expression*
Parser::parseSizeof()
{
  SourceLocation loc = scanner_.begin();

  size_t nparens = 0;
  while (match(TOK_LPAREN))
    nparens++;

  if (!expectName())
    return nullptr;
  NameProxy* proxy = nameref();

  size_t level = 0;
  while (match(TOK_LBRACKET)) {
    if (!expect(TOK_RBRACKET))
      return nullptr;
    level++;
  }

  for (size_t i = 0; i < nparens; i++) {
    if (!expect(TOK_RPAREN))
      return nullptr;
  }

  return new (pool_) SizeofExpression(loc, proxy, level);
}

Expression*
Parser::multiplication()
{
  Expression* left = unary();
  do {
    if (!match(TOK_SLASH) && !match(TOK_STAR) && !match(TOK_PERCENT))
      break;
    SourceLocation pos = scanner_.begin();
    TokenKind kind = scanner_.current()->kind;
    Expression* right = unary();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, kind, left, right);
  } while (left);
  return left;
}

Expression*
Parser::addition()
{
  Expression* left = multiplication();
  while (left) {
    if (!match(TOK_PLUS) && !match(TOK_MINUS))
      break;
    SourceLocation pos = scanner_.begin();
    TokenKind kind = scanner_.current()->kind;
    Expression* right = multiplication();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, kind, left, right);
  }
  return left;
}

Expression*
Parser::shift()
{
  Expression* left = addition();
  while (left) {
    if (!match(TOK_SHL) && !match(TOK_SHR) && !match(TOK_USHR))
      break;
    SourceLocation pos = scanner_.begin();
    TokenKind kind = scanner_.current()->kind;
    Expression* right = addition();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, kind, left, right);
  }
  return left;
}

Expression*
Parser::bitand_()
{
  Expression* left = shift();
  while (left) {
    if (!match(TOK_BITAND))
      break;
    SourceLocation pos = scanner_.begin();
    Expression* right = shift();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, TOK_BITAND, left, right);
  }
  return left;
}

Expression*
Parser::bitxor()
{
  Expression* left = bitand_();
  while (left) {
    if (!match(TOK_BITXOR))
      break;
    SourceLocation pos = scanner_.begin();
    Expression* right = shift();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, TOK_BITXOR, left, right);
  }
  return left;
}

Expression*
Parser::bitor_()
{
  Expression* left = bitxor();
  while (left) {
    if (!match(TOK_BITOR))
      break;
    SourceLocation pos = scanner_.begin();
    Expression* right = bitxor();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, TOK_BITOR, left, right);
  }
  return left;
}

Expression*
Parser::relational()
{
  Expression* left = bitor_();
  size_t count = 0;
  while (left) {
    TokenKind kind = scanner_.peek();
    if (kind < TOK_LT || kind > TOK_GE)
      break;
    scanner_.next();
    SourceLocation pos = scanner_.begin();
    Expression* right = shift();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, kind, left, right);
    if (++count > 1) {
      cc_.report(pos, rmsg::no_chained_relops);
      return nullptr;
    }
  }
  return left;
}

Expression*
Parser::equals()
{
  Expression* left = relational();
  while (left) {
    if (!match(TOK_EQUALS) && !match(TOK_NOTEQUALS))
      break;
    TokenKind kind = scanner_.current()->kind;
    SourceLocation pos = scanner_.begin();
    Expression* right = relational();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, kind, left, right);
  }
  return left;
}

Expression*
Parser::and_()
{
  Expression* left = equals();
  while (left) {
    if (!match(TOK_AND))
      break;
    SourceLocation pos = scanner_.begin();
    Expression* right = equals();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, TOK_AND, left, right);
  }
  return left;
}

Expression*
Parser::or_()
{
  Expression* left = and_();
  while (left) {
    if (!match(TOK_OR))
      break;
    SourceLocation pos = scanner_.begin();
    Expression* right = and_();
    if (!right)
      return nullptr;
    left = new (pool_) BinaryExpression(pos, TOK_OR, left, right);
  }
  return left;
}

Expression*
Parser::ternary()
{
  Expression* cond = or_();
  if (!cond)
    return nullptr;
  
  if (!match(TOK_QMARK))
    return cond;

  Expression* left;
  SourceLocation pos = scanner_.begin();
  if (match(TOK_LABEL)) {
    // A ? B:C would normally get evaluated as NAME QMARK LABEL NAME, but we
    // special case it here so that the LABEL becomes NAME and the colon gets
    // stolen.
    left = nameref();
  } else {
    if ((left = expression()) == nullptr)
      return nullptr;
    if (!expect(TOK_COLON))
      return nullptr;
  }

  Expression* right = expression();
  if (!right)
    return nullptr;

  return new (pool_) TernaryExpression(pos, cond, left, right);
}

Expression*
Parser::assignment()
{
  Expression* left = ternary();
  while (left) {
    TokenKind token = scanner_.peek();
    if (token != TOK_ASSIGN && (token < TOK_ASSIGN_ADD || token > TOK_ASSIGN_SHL))
      break;
    scanner_.next();
    SourceLocation pos = scanner_.begin();
    Expression* expr = assignment();
    if (!expr)
      return nullptr;

    left = new (pool_) Assignment(pos, token, left, expr);
  }
  return left;
}

Expression*
Parser::expression()
{
  return assignment();
}

Statement*
Parser::while_()
{
  // while ::= "while" "(" expr ")" statement
  SourceLocation pos = scanner_.begin();

  if (!expect(TOK_LPAREN))
    return nullptr;

  Expression* condition = expression();
  if (!condition)
    return nullptr;

  if (!expect(TOK_RPAREN))
    return nullptr;

  Statement* body = statementOrBlock();
  if (!body)
    return nullptr;

  requireNewline();

  return new (pool_) WhileStatement(pos, TOK_WHILE, condition, body);
}

Statement*
Parser::do_()
{
  // do ::= "do" block "while" "(" expr ")"
  SourceLocation pos = scanner_.begin();

  Statement* body = block();
  if (!body)
    return nullptr;

  if (!expect(TOK_WHILE))
    return nullptr;

  if (!expect(TOK_LPAREN))
    return nullptr;
  Expression* condition = expression();
  if (!condition)
    return nullptr;
  if (!expect(TOK_RPAREN))
    return nullptr;

  requireTerminator();

  return new (pool_) WhileStatement(pos, TOK_DO, condition, body);
}

FunctionNode*
Parser::parseFunctionBase(const TypeExpr& returnType, TokenKind kind)
{
  FunctionNode* node = new (pool_) FunctionNode(kind);

  // If our return type and arguments are all eagerly resolvable, we can mark
  // the signature as resolved.
  Scope* argScope = nullptr;
  {
    AutoEnterScope argEnv(delegate_, Scope::Argument, &argScope);

    bool canEarlyResolve = true;
    ParameterList* params = arguments(&canEarlyResolve);
    if (!params) {
      params = new (pool_) ParameterList();
      scanner_.skipUntil(TOK_LBRACE, SkipFlags::StopAtLine | SkipFlags::StopBeforeMatch);
    }

    delegate_.HandleFunctionSignature(kind, node, returnType, params, canEarlyResolve);

    // Hrm... should be complting the decl here instead? Right now it seems
    // fine not to, but in the future it could lead to weird ordering in the
    // type resolver's queue.
    BlockStatement* body = nullptr;
    if (kind != TOK_NATIVE) {
      if ((body = methodBody()) == nullptr)
        return nullptr;
    }
    node->setBody(body);
  }
  node->setArgScope(argScope);

  if (!node->body())
    requireNewlineOrSemi();

  return node;
}

PropertyDecl*
Parser::parseAccessor()
{
  SourceLocation begin = scanner_.begin();

  TypeSpecifier spec;
  parse_new_type_expr(&spec, 0);

  if (!expect(TOK_NAME))
    return nullptr;
  NameToken name = *scanner_.current();

  if (!expect(TOK_LBRACE))
    return nullptr;

  PropertyDecl* decl = delegate_.EnterPropertyDecl(begin, name, spec);

  while (!match(TOK_RBRACE)) {
    expect(TOK_PUBLIC);

    TokenKind kind = TOK_NONE;
    if (match(TOK_NATIVE))
      kind = TOK_NATIVE;

    Atom* name = expectName();
    if (!name) {
      scanner_.skipUntil(TOK_RBRACE, SkipFlags::StartAtCurrent);
      break;
    }

    bool is_getter = false;
    bool is_setter = false;
    if (strcmp(name->chars(), "get") == 0)
      is_getter = true;
    else if (strcmp(name->chars(), "set") == 0)
      is_setter = true;
    else
      cc_.report(scanner_.begin(), rmsg::invalid_accessor_name);

    FunctionNode* node = parseFunctionBase(decl->te(), kind);
    if (!node)
      break;

    if (is_getter)
      decl->setGetter(node);
    else if (is_setter)
      decl->setSetter(node);
  }

  delegate_.LeavePropertyDecl(decl);
  return decl;
}

MethodDecl*
Parser::parseMethod(Atom* layoutName)
{
  SourceLocation begin = scanner_.begin();

  bool isStatic = match(TOK_STATIC);

  TokenKind kind = TOK_NONE;
  if (match(TOK_NATIVE))
    kind = TOK_NATIVE;

  NameToken name;
  TypeSpecifier spec;
  if (match(TOK_NAME)) {
    // See if we have a constructor.
    name = *scanner_.current();
    if (name.atom == layoutName && peek(TOK_LPAREN)) {
      // This is a constructor.
      spec.setNamedType(TOK_NAME, nameref(scanner_.current()));
      spec.setBaseLoc(scanner_.begin());
    } else {
      // Give the name token back.
      scanner_.undo();
      name = NameToken();
    }
  }

  if (!name.atom) {
    parse_new_type_expr(&spec, 0);

    if (peek(TOK_LPAREN))
      cc_.report(scanner_.begin(), rmsg::expected_name_and_type);
    else if (expect(TOK_NAME))
      name = scanner_.current();
  }

  if (!name.atom) {
    // Parse the body (or try to), then leave, if we never got a name.
    TypeExpr te(new (pool_) TypeSpecifier(spec));
    parseFunctionBase(te, kind);
    return nullptr;
  }

  TypeExpr te;
  MethodDecl* decl = delegate_.EnterMethodDecl(begin, name, &spec, &te, isStatic);

  FunctionNode* node = parseFunctionBase(te, kind);
  if (!node) {
    delegate_.LeaveMethodDecl(decl);
    return nullptr;
  }
  decl->setMethod(node);

  delegate_.LeaveMethodDecl(decl);
  return decl;
}

Statement*
Parser::methodmap(TokenKind kind)
{
  SourceLocation begin = scanner_.begin();

  if (!expect(TOK_NAME))
    return nullptr;
  NameToken name = *scanner_.current();

  bool nullable = match(TOK_NULLABLE);

  NameProxy* extends = nullptr;
  if (match(TOK_LT) && expect(TOK_NAME))
    extends = nameref();

  if (!expect(TOK_LBRACE))
    return nullptr;

  MethodmapDecl* methodmap = new (pool_) MethodmapDecl(begin, name, extends);
  if (nullable)
    methodmap->setNullable();

  delegate_.OnEnterMethodmap(methodmap);

  LayoutDecls* decls = new (pool_) LayoutDecls();
  while (!match(TOK_RBRACE)) {
    LayoutDecl* decl = nullptr;
    if (match(TOK_PROPERTY)) {
      decl = parseAccessor();
    } else {
      expect(TOK_PUBLIC);
      decl = parseMethod(name.atom);
    }

    if (!decl) {
      if (match(TOK_EOF))
        break;
    } else {
      decls->push_back(decl);
    }
  }
  methodmap->setBody(decls);

  // Clean up.
  delegate_.OnLeaveMethodmap(methodmap);

  requireNewlineOrSemi();
  return methodmap;
}

Statement*
Parser::switch_()
{
  // switch ::= "switch" "(" expr ")" "{" case* defaultcase? "}"
  // case ::= "case" casevals ":" statement
  // defaultcase ::= "default" ":" statement
  //
  SourceLocation pos = scanner_.begin();

  if (!expect(TOK_LPAREN))
    return nullptr;
  Expression* expr = expression();
  if (!expr)
    return nullptr;
  if (!expect(TOK_RPAREN))
    return nullptr;

  if (!expect(TOK_LBRACE))
    return nullptr;

  SourceLocation defaultPos;
  PoolList<Case*>* cases = new (pool_) PoolList<Case*>();

  Statement* defaultCase = nullptr;

  while (!peek(TOK_RBRACE)) {
    Expression* expr = nullptr;
    ExpressionList* others = nullptr;

    bool need_colon = true;
    if (match(TOK_DEFAULT)) {
      if (defaultCase) {
        cc_.report(scanner_.begin(), rmsg::duplicate_default_case);
        return nullptr;
      }

      defaultPos = scanner_.begin();
    } else {
      if (!expect(TOK_CASE))
        return nullptr;

      // A limitation in the grammar is that |case <NAME>:| will be detected as
      // a label. SP1 has a disable/enable tag option that gets set here and
      // unset after an open-paren. We could do that, but in general it's risky
      // since our token buffer could already have buffered a label. Instead,
      // we special-case a label token.
      if (match(TOK_LABEL)) {
        // We assume this is an identifier and not a tag. If there's anything
        // else after (like a:b) the user will get parse errors.
        expr = nameref();
        need_colon = false;
      } else {
        if ((expr = expression()) == nullptr)
          return nullptr;

        if (peek(TOK_COMMA)) {
          others = new (pool_) ExpressionList();
          while (need_colon && match(TOK_COMMA)) {
            if (match(TOK_LABEL)) {
              NameProxy* proxy = nameref();
              others->push_back(proxy);
              need_colon = false;
              break;
            }

            Expression* other = expression();
            if (!other)
              return nullptr;
            others->push_back(other);
          }
        }
      }
    }

    if (need_colon && !expect(TOK_COLON))
      return nullptr;

    Statement* stmt = statementOrBlock();
    if (!stmt)
      return nullptr;

    requireNewline();

    if (!peek(TOK_CASE) && !peek(TOK_DEFAULT) && !peek(TOK_RBRACE)) {
      cc_.report(scanner_.begin(), rmsg::expected_case);
      return nullptr;
    }

    if (expr) {
      Case* caze = new (pool_) Case(expr, others, stmt);
      cases->push_back(caze);
    } else {
      defaultCase = stmt;
    }
  }

  if (!expect(TOK_RBRACE))
    return nullptr;

  requireNewline();

  return new (pool_) SwitchStatement(pos, expr, cases, defaultCase);
}

Statement*
Parser::for_()
{
  // for ::= "for" "(" forinit? ";" forcond? ";" forstep ")" statement
  // forint ::= "new" vardecl |
  //      exprstmt
  // forcond ::= expr
  // forstep ::= exprstmt
  //
  SourceLocation pos = scanner_.begin();
  if (!expect(TOK_LPAREN))
    return nullptr;

  Scope* scope = nullptr;
  Statement* decl = nullptr;
  Expression* condition = nullptr;
  Statement* update = nullptr;
  Statement* body = nullptr;
  {
    AutoEnterScope env(delegate_, Scope::Block, &scope);

    if (!match(TOK_SEMICOLON)) {
      bool is_decl = false;
      if (match(TOK_NEW))
        is_decl = true;
      else if (IsNewTypeToken(scanner_.peek()))
        is_decl = true;

      if (is_decl) {
        if ((decl = localVarDecl(TOK_NEW, DeclFlags::Inline)) == nullptr)
          return nullptr;
      } else {
        if ((decl = expressionStatement()) == nullptr)
          return nullptr;
      }
      if (!expect(TOK_SEMICOLON))
        return nullptr;
    }

    if (!match(TOK_SEMICOLON)) {
      if ((condition = expression()) == nullptr)
        return nullptr;
      if (!expect(TOK_SEMICOLON))
        return nullptr;
    }

    if (!match(TOK_RPAREN)) {
      if ((update = expressionStatement()) == nullptr)
        return nullptr;
      if (!expect(TOK_RPAREN))
        return nullptr;
    }

    if ((body = statementOrBlock()) == nullptr)
      return nullptr;
  }

  requireNewline();

  return new (pool_) ForStatement(pos, decl, condition, update, body, scope);
}

ExpressionList*
Parser::dimensions()
{
  // dimensions ::= ("[" expr? "]")*

  ExpressionList* postDimensions = new (pool_) ExpressionList();
  while (match(TOK_LBRACKET)) {
    Expression* dim = nullptr;
    if (!match(TOK_RBRACKET)) {
      if ((dim = expression()) == nullptr)
        return nullptr;
      if (!expect(TOK_RBRACKET))
        return nullptr;
    }
    postDimensions->push_back(dim);
  }
  return postDimensions;
}

Statement*
Parser::variable(TokenKind tok, Declaration* decl, SymAttrs flags, uint32_t decl_flags)
{
  Expression* init = nullptr;
  if (match(TOK_ASSIGN))
    init = expression();

  VarDecl* first = delegate_.HandleVarDecl(decl->name, tok, flags, decl->spec, init);

  VarDecl* prev = first;
  while (scanner_.peekTokenSameLine() == TOK_COMMA) {
    scanner_.next();

    // Parse the next declaration re-using any sticky information from the
    // first decl.
    if (!reparse_decl(decl, DeclFlags::Variable))
      break;

    Expression* init = nullptr;
    if (match(TOK_ASSIGN))
      init = expression();

    VarDecl* var = delegate_.HandleVarDecl(decl->name, tok, flags, decl->spec, init);

    prev->setNext(var);
    prev = var;
  }

  if (!(decl_flags & DeclFlags::Inline))
    requireTerminator();

  return first;
}

// Wrapper around variable() for locals.
Statement*
Parser::localVarDecl(TokenKind kind, uint32_t flags)
{
  // :TODO: reject CONST on arrays.
  Declaration decl;

  if (!allowDeclarations_)
    cc_.report(scanner_.begin(), rmsg::variable_without_block);

  flags |= DeclFlags::Variable;
  if (!parse_decl(&decl, flags))
    return nullptr;

  SymAttrs attrs = SymAttrs::None;
  if (kind == TOK_DECL) {
    attrs |= SymAttrs::Uninitialized;
    kind = TOK_NEW;
  }

  return variable(kind, &decl, attrs, flags);
}

Statement*
Parser::delete_()
{
  // delete ::= "delete" expr
  SourceLocation pos = scanner_.begin();

  Expression* expr = expression();
  if (!expr)
    return nullptr;

  requireTerminator();
  return new (pool_) DeleteStatement(pos, expr);
}

Statement*
Parser::return_()
{
  // return ::= "return" term |
  //      "return" "expr"
  SourceLocation pos = scanner_.begin();

  Expression* expr = nullptr;
  TokenKind next = scanner_.peekTokenSameLine();
  if (next != TOK_EOL && next != TOK_EOF && next != TOK_SEMICOLON) {
    if ((expr = expression()) == nullptr) {
      scanner_.skipUntil(TOK_SEMICOLON, SkipFlags::StopAtLine);
      return nullptr;
    }
  }

  requireTerminator();

  ReturnStatement* stmt = new (pool_) ReturnStatement(pos, expr);
  delegate_.OnReturnStmt(stmt);
  return stmt;
}

Statement*
Parser::expressionStatement()
{
  // exprstmt ::= expr
  Expression* left = assignment();
  if (!left) {
    scanner_.skipUntil(TOK_SEMICOLON, SkipFlags::StopAtLine);
    return nullptr;
  }

  return new (pool_) ExpressionStatement(left);
}

// Parses statements, expecting the "{" to have already been parsed.
StatementList*
Parser::statements()
{
  StatementList* list = new (pool_) StatementList();
  while (!match(TOK_RBRACE)) {
    // Call statement() directly, so we don't set allowDeclaratiosn to false.
    if (Statement* stmt = statement())
      list->push_back(stmt);

    // Note: we keep looping, since statement() will always consume a token.
  }
  return list;
}

Statement*
Parser::block()
{
  // block ::= "{" statement* "}"
  if (!expect(TOK_LBRACE))
    return nullptr;

  SourceLocation pos = scanner_.begin();

  Scope* scope = nullptr;
  StatementList* list = nullptr;
  {
    AutoEnterScope env(delegate_, Scope::Block, &scope);

    SaveAndSet<bool> save(&allowDeclarations_, true);
    if ((list = statements()) == nullptr)
      return nullptr;
  }

  return new (pool_) BlockStatement(pos, list, TOK_LBRACE, scope);
}

Statement*
Parser::if_()
{
  // if ::= "if" "(" expr ")" statement elseif* else?
  // elseif ::= "elseif" "(" expr ")" statement
  // else ::= "else" statement
  SourceLocation pos = scanner_.begin();
  if (!expect(TOK_LPAREN))
    return nullptr;

  Expression* cond = expression();
  if (!cond)
    return nullptr;

  if (!expect(TOK_RPAREN))
    return nullptr;

  Statement* ifTrue = statementOrBlock();
  if (!ifTrue)
    return nullptr;

  PoolList<IfClause>* clauses = new (pool_) PoolList<IfClause>();
  clauses->push_back(IfClause(cond, ifTrue));

  Statement* fallthrough = nullptr;

  while (match(TOK_ELSE)) {
    if (!match(TOK_IF)) {
      if (!(fallthrough = statementOrBlock()))
        return nullptr;
      break;
    }

    if (!expect(TOK_LPAREN))
      return nullptr;

    Expression* otherCond = expression();
    if (!otherCond)
      return nullptr;

    if (!expect(TOK_RPAREN))
      return nullptr;

    Statement* otherIfTrue = statementOrBlock();
    if (!otherIfTrue)
      return nullptr;

    clauses->push_back(IfClause(otherCond, otherIfTrue));
  }

  requireNewline();
  return new (pool_) IfStatement(pos, clauses, fallthrough);
}

Statement*
Parser::statement()
{
  // statement ::= stmt term
  // stmt ::= do | for | if | while | struct | enum |
  //      localvars | return | switch | break | continue

  Statement* stmt = nullptr;

  // Shortcut out early for block, since it wants to expect(TOK_LBRACE).
  if (peek(TOK_LBRACE))
    return block();

  TokenKind kind = scanner_.next();

  // We don't have enough lookahead to differentiate some declarations from
  // expressions, so we cheat a bit here and just do some pattern matching:
  //
  //   "name[]" probably starts a declaration, as does "name name".
  if (kind == TOK_NAME) {
    bool is_decl = false;

    if (match(TOK_LBRACKET)) {
      if (peek(TOK_RBRACKET))
        is_decl = true;
      scanner_.undo();
    } else if (peek(TOK_NAME)) {
      is_decl = true;
    }

    if (is_decl) {
      scanner_.undo();
      return localVarDecl(TOK_NEW);
    }
  }

  // Other declarations don't need any special sniffing.
  if (IsNewTypeToken(kind) ||
      kind == TOK_STATIC ||
      kind == TOK_CONST)
  {
    if (IsNewTypeToken(kind) || kind == TOK_CONST) {
      scanner_.undo();
      kind = TOK_NEW;
    }
    return localVarDecl(kind);
  }

  // We special case these, since otherwise we could mess up parsing this:
  //    new x;
  //    x = 5;
  //
  // As: "x x = 5".
  if (kind == TOK_DECL || kind == TOK_NEW)
    return localVarDecl(kind, DeclFlags::Old);

  // Statements which must be followed by a semicolon will break out of the
  // switch. If they may end in BlockStatements, they'll immediately return.
  switch (kind) {
    case TOK_FOR:
      return for_();

    case TOK_WHILE:
      return while_();

    case TOK_BREAK:
      stmt = new (pool_) BreakStatement(scanner_.begin());
      break;

    case TOK_CONTINUE:
      stmt = new (pool_) ContinueStatement(scanner_.begin());
      break;

    case TOK_DO:
      return do_();

    case TOK_RETURN:
      return return_();

    case TOK_ENUM:
      return enum_();

    case TOK_SWITCH:
      return switch_();

    case TOK_IF:
      return if_();

    case TOK_DELETE:
      return delete_();

    default:
      break;
  }

  if (!stmt) {
    scanner_.undo();
    if ((stmt = expressionStatement()) == nullptr)
      return nullptr;
  }

  requireTerminator();
  return stmt;
}

Statement*
Parser::statementOrBlock()
{
  SaveAndSet<bool> save(&allowDeclarations_, false);
  return statement();
}

Statement*
Parser::enum_()
{
  // enum ::= "enum" name? { enum_members? }
  // enum_members ::= enum_member ","? |
  //          enum_member "," enum_members
  // enum_member ::= ident ("=" constexpr)?
  SourceLocation pos = scanner_.begin();

  Atom* name = nullptr;
  if (match(TOK_NAME) || match(TOK_LABEL))
    name = scanner_.current_name();

  EnumConstantList* entries = new (pool_) EnumConstantList();

  if (!expect(TOK_LBRACE))
    return nullptr;

  EnumStatement* stmt = new (pool_) EnumStatement(pos, name);
  delegate_.OnEnumDecl(stmt);

  do {
    if (scanner_.peek() == TOK_RBRACE)
      break;

    if (match(TOK_LABEL))
      cc_.report(scanner_.begin(), rmsg::no_enum_structs);

    Atom* name = expectName();
    SourceLocation loc = scanner_.begin();

    if (peek(TOK_LBRACKET)) {
      if (dimensions())
        cc_.report(scanner_.begin(), rmsg::no_enum_structs);
    }

    Expression* expr = nullptr;
    if (match(TOK_ASSIGN)) {
      if ((expr = expression()) == nullptr)
        return nullptr;
    }

    EnumConstant* cs = new (pool_) EnumConstant(loc, stmt, name, expr);
    delegate_.OnEnumValueDecl(cs);
    entries->push_back(cs);
  } while (match(TOK_COMMA));
  if (!expect(TOK_RBRACE))
    return nullptr;

  stmt->setEntries(entries);

  requireTerminator();
  return stmt;
}

ParameterList*
Parser::arguments(bool* canEarlyResolve)
{
  ParameterList* params = new (pool_) ParameterList;

  // Note: though our callers 
  if (!expect(TOK_LPAREN)) {
    scanner_.skipUntil(TOK_RPAREN, SkipFlags::StopAtLine);
    return nullptr;
  }

  if (match(TOK_RPAREN))
    return params;

  bool variadic = false;
  do {
    Declaration decl;
    if (!parse_decl(&decl, DeclFlags::Argument)) {
      scanner_.skipUntil(TOK_RPAREN, SkipFlags::StopBeforeMatch);
      break;
    }

    Expression* init = nullptr;
    if (match(TOK_ASSIGN))
      init = expression();

    if (decl.spec.isVariadic()) {
      if (variadic)
        cc_.report(decl.spec.variadicLoc(), rmsg::multiple_varargs);
      variadic = true;

      // We don't have a name for this variable, but we want its name token to
      // have a valid pos since that's what the AstNode uses as its base.
      decl.name.start = decl.spec.baseLoc();
    } else if (variadic) {
      cc_.report(decl.spec.baseLoc(), rmsg::varargs_must_be_last);
    }

    VarDecl* node = delegate_.HandleVarDecl(decl.name, TOK_NEW, SymAttrs::None, decl.spec, init);

    // Mark whether we eagerly resolved a type for this node.
    *canEarlyResolve &= !!node->sym()->type();

    params->push_back(node);
  } while (match(TOK_COMMA));

  if (!expect(TOK_RPAREN)) {
    scanner_.skipUntil(TOK_RPAREN,
                       SkipFlags::StopAtLine|SkipFlags::StopBeforeMatch,
                       TOK_LBRACE);
  }
  return params;
}

BlockStatement*
Parser::methodBody()
{
  // Enter a new scope to parse the method body.
  bool lbrace = match(TOK_LBRACE);
  SourceLocation pos = scanner_.begin();

  Scope* scope = nullptr;
  StatementList* list = nullptr;
  if (lbrace) {
    SaveAndSet<bool> saveDeclState(&allowDeclarations_, true);
    AutoEnterScope varEnv(delegate_, Scope::Block, &scope);

    if ((list = statements()) == nullptr)
      return nullptr;
  } else {
    cc_.report(pos, rmsg::no_single_line_functions);

    Statement* stmt = statement();
    if (!stmt)
      return nullptr;
    list = new (pool_) StatementList();
    list->push_back(stmt);
  }

  requireNewline();
  return new (pool_) BlockStatement(pos, list, TOK_FUNCTION, scope);
}

Statement*
Parser::function(TokenKind kind, Declaration& decl, SymAttrs flags)
{
  FunctionStatement* stmt = new (pool_) FunctionStatement(decl.name, kind, flags);

  delegate_.OnEnterFunctionDecl(stmt);

  Scope* argScope = nullptr;
  {
    // Enter a new scope for arguments. We have to do this even for functions
    // that don't have bodies, unfortunately, since they could contain default
    // arguments that bind to named constants or the magic sizeof(arg) expr.
    AutoEnterScope argEnv(delegate_, Scope::Argument, &argScope);

    bool canEagerResolve = true;
    ParameterList* params = arguments(&canEagerResolve);
    if (!params) {
      params = new (pool_) ParameterList();
      scanner_.skipUntil(TOK_LBRACE, SkipFlags::StopAtLine | SkipFlags::StopBeforeMatch);
    }

    delegate_.HandleFunctionSignature(kind, stmt, decl.spec, params, canEagerResolve);

    BlockStatement* body = nullptr;
    if (kind != TOK_FORWARD && kind != TOK_NATIVE) {
      if ((body = methodBody()) == nullptr) {
        delegate_.OnLeaveFunctionDecl(stmt);
        return nullptr;
      }
    }
    stmt->setBody(body);
  }
  stmt->setArgScope(argScope);

  if (stmt->body())
    requireNewline();
  else
    requireTerminator();

  delegate_.OnLeaveFunctionDecl(stmt);
  return stmt;
}

Statement*
Parser::global(TokenKind kind)
{
  Declaration decl;

  if (kind == TOK_NATIVE || kind == TOK_FORWARD) {
    if (!parse_decl(&decl, DeclFlags::MaybeFunction))
      return nullptr;
    return function(kind, decl, SymAttrs::None);
  }

  SymAttrs flags = SymAttrs::None;
  if (kind == TOK_STOCK)
    flags |= SymAttrs::Stock;
  if (kind == TOK_STATIC && match(TOK_STOCK))
    flags |= SymAttrs::Stock;

  uint32_t decl_flags = DeclFlags::MaybeFunction | DeclFlags::Variable;
  if (kind == TOK_NEW)
    decl_flags |= DeclFlags::Old;

  if (!parse_decl(&decl, decl_flags))
    return nullptr;

  bool is_var_decl = (kind == TOK_NEW) ||
                      decl.spec.hasPostDims() ||
                      !peek(TOK_LPAREN);

  TokenKind spec;
  if (kind == TOK_PUBLIC || kind == TOK_STATIC)
    spec = kind;
  else if (is_var_decl)
    spec = TOK_NEW;
  else
    spec = TOK_FUNCTION;

  if (is_var_decl) {
    if (kind == TOK_NEW && decl.spec.isNewDecl())
      cc_.report(scanner_.begin(), rmsg::newdecl_with_new);
    return variable(spec, &decl, flags, 0);
  }
  return function(spec, decl, flags);
}

Statement*
Parser::typeset_()
{
  SourceLocation loc = scanner_.begin();

  if (!expect(TOK_NAME))
    return nullptr;
  NameToken name = *scanner_.current();

  if (!expect(TOK_LBRACE))
    return nullptr;

  TypesetDecl* decl = delegate_.EnterTypeset(loc, name);

  std::vector<TypesetDecl::Entry> types;
  while (!match(TOK_RBRACE)) {
    TypeSpecifier spec;
    parse_new_type_expr(&spec, 0);

    delegate_.EnterTypeIntoTypeset(decl, types, spec);

    requireNewlineOrSemi();
  }
  delegate_.FinishTypeset(decl, types);

  requireNewlineOrSemi();
  return decl;
}

Statement*
Parser::struct_(TokenKind kind)
{
  SourceLocation loc = scanner_.begin();

  if (!expect(TOK_NAME))
    return nullptr;
  NameToken name = *scanner_.current();

  if (!expect(TOK_LBRACE))
    return nullptr;

  uint32_t flags = DeclFlags::Field;
  if (kind == TOK_UNION)
    flags |= DeclFlags::MaybeNamed;

  RecordDecl* decl = new (pool_) RecordDecl(loc, kind, name);
  delegate_.OnEnterRecordDecl(decl);

  LayoutDecls* decls = new (pool_) LayoutDecls();
  while (!match(TOK_RBRACE)) {
    Declaration decl;

    // Structs need a |public| keyword right now.
    expect(TOK_PUBLIC);

    if (!parse_new_decl(&decl, flags))
      break;

    SourceLocation begin = decl.spec.startLoc();
    FieldDecl* field = delegate_.HandleFieldDecl(begin, decl.name, decl.spec);
    decls->push_back(field);

    requireNewlineOrSemi();
  }
  decl->setBody(decls);

  delegate_.OnLeaveRecordDecl(decl);

  requireNewlineOrSemi();
  return decl;
}

Statement*
Parser::typedef_()
{
  SourceLocation begin = scanner_.begin();

  Atom* name = expectName();
  if (!name)
    return nullptr;

  expect(TOK_ASSIGN);

  TypeSpecifier spec;
  parse_new_type_expr(&spec, 0);

  requireNewlineOrSemi();

  return delegate_.HandleTypedefDecl(begin, name, spec);
}

bool
Parser::using_()
{
  if (!expect(TOK_INTRINSICS))
    return false;
  if (!expect(TOK_DOT))
    return false;

  Atom* name = expectName();
  if (!name)
    return false;

  if (strcmp(name->chars(), "Handle") == 0) {
    uses_handle_intrinsics_ = true;
    return true;
  }

  cc_.report(scanner_.begin(), rmsg::intrinsic_not_found) <<
    name;
  return false;
}

ParseTree*
Parser::parse()
{
  StatementList* list = new (pool_) StatementList();

  delegate_.OnEnterParser();

  for (;;) {
    Statement* statement = nullptr;

    TokenKind kind = scanner_.next();
    switch (kind) {
      case TOK_ERROR:
      case TOK_UNKNOWN:
        goto err_out;

      case TOK_EOF:
        break;

      case TOK_NAME:
      case TOK_CHAR:
      case TOK_INT:
      case TOK_VOID:
      case TOK_OBJECT:
      case TOK_FLOAT:
      case TOK_LABEL:
        scanner_.undo();
        // Fallthrough.
      case TOK_NEW:
      case TOK_CONST:
      case TOK_STATIC:
      case TOK_PUBLIC:
      case TOK_STOCK:
      case TOK_NATIVE:
      case TOK_FORWARD:
      {
        statement = global(kind);
        break;
      }

      case TOK_METHODMAP:
        statement = methodmap(TOK_METHODMAP);
        break;

      case TOK_ENUM:
        statement = enum_();
        break;

      case TOK_STRUCT:
        statement = struct_(kind);
        break;

      case TOK_TYPESET:
        statement = typeset_();
        break;

      case TOK_TYPEDEF:
        statement = typedef_();
        break;

      case TOK_FUNCTAG:
        cc_.report(scanner_.begin(), rmsg::functags_not_supported);
        scanner_.eatRestOfLine();
        break;

      case TOK_USING:
        if (using_())
          requireNewlineOrSemi();
        else
          scanner_.eatRestOfLine();
        break;

      default:
        if (kind != TOK_UNKNOWN) {
          cc_.report(scanner_.begin(), rmsg::expected_global_decl)
            << TokenNames[kind];
        }
        goto err_out;
    }

    if (!statement) {
      if (scanner_.current()->kind == TOK_EOF)
        break;
    } else {
      list->push_back(statement);
    }
  }

  delegate_.OnLeaveParser();

 err_out:
  ParseTree* pt = new (pool_) ParseTree(list);
  if (uses_handle_intrinsics_)
    pt->set_uses_handle_intrinsics();
  return pt;
}

} // namespace sp
