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
#include "preprocessor.h"

using namespace ke;
using namespace sp;

bool
Preprocessor::eval(int* out)
{
  // We must be in a file, processing the text of a directive. This ensures
  // that any newline or EOF we get becomes an EOL token, and the lexing stack
  // never consumes the current lexer.
  //
  // If evaluation fails, we will have to manually discard lexers until we
  // reach the original lexer.
  RefPtr<Lexer> start = lexer_;
  assert(start->processingDirective());

  // Switch to the preprocessing token buffer. Since we're lexing a directive
  // from within the Preprocessor, we cannot mix the token streams.
  SaveAndSet<TokenRing*> switchTokenRings(&tokens_, &pp_tokens_);

  // We must disable macro expansion, since we could predictively expand an
  // identifier token when we don't want to do that for the "defined"
  // operator. We instead expand macros via helper functions.
  SaveAndSet<bool> disableMacroExpansion(&allow_macro_expansion_, false);

  bool ok = eval_inner(out);

  // We must ensure that any leftover tokens from expansion are consumed,
  // so we arrive back on the start lexer.
  while (next() != TOK_EOL) {
    if (!macro_lexer_) {
      // The current token should be from the original lexer. Give it back.
      undo();
      break;
    }
    if (ok) {
      ok = false;
      cc_.report(current()->start.loc, rmsg::pp_extra_characters);
    }
  }
  assert(lexer_ == start);

  return ok;
}

bool
Preprocessor::eval_inner(int* out)
{
  int e;
  if (!eval_binary(12, &e))
    return false;

  if (!match(TOK_QMARK)) {
    *out = e;
    return true;
  }

  int left, right;
  if (!eval_inner(&left))
    return false;
  if (!expect(TOK_COLON))
    return false;
  if (!eval_inner(&right))
    return false;

  *out = e ? left : right;
  return true;
}

struct Op
{
  TokenKind tok;
  int order;
  int prec;
  int arity;
};

static Op Operators[] = {
#define _(name, token, order, prec, arity)  \
  { TOK_##token, order, prec, arity },
  OPERATOR_MAP(_)
#undef _
  { TOK_NONE, -1, -1, -1 }
};

static inline const Op*
FindOperator(TokenKind tok)
{
  switch (tok) {
#define _(name, token, order, prec, arity)  \
    case TOK_##token:                       \
      return &Operators[Operator_##name];   \
      break;
    OPERATOR_MAP(_)
#undef _
    default:
      return NULL;
  }
}

int
BinaryEval(TokenKind kind, int left, int right)
{
  switch (kind) {
    case TOK_STAR:
      return left * right;
    case TOK_SLASH:
      return left / right;
    case TOK_PERCENT:
      return left % right;
    case TOK_PLUS:
      return left + right;
    case TOK_MINUS:
      return left - right;
    case TOK_SHR:
      return left >> right;
    case TOK_USHR:
      return unsigned(left) >> unsigned(right);
    case TOK_SHL:
      return left << right;
    case TOK_AMPERSAND:
      return left & right;
    case TOK_BITXOR:
      return left ^ right;
    case TOK_BITOR:
      return left | right;
    case TOK_LT:
      return left < right;
    case TOK_LE:
      return left <= right;
    case TOK_GT:
      return left > right;
    case TOK_GE:
      return left >= right;
    case TOK_EQUALS:
      return left == right;
    case TOK_NOTEQUALS:
      return left != right;
    case TOK_AND:
      return left && right;
    case TOK_OR:
      return left || right;
    default:
      assert(false);
      return 0;
  }
}

TokenKind
Preprocessor::eval_next()
{
  while (next() == TOK_NAME) {
    // Macro expansion is disabled, so we manually expand the macro. If we
    // can't, it's not a valid identifier.
    const Token* tok = current();
    SaveAndSet<bool> enableMacroExpansion(&allow_macro_expansion_, true);
    if (!enterMacro(tok->start.loc, tok->atom())) {
      cc_.report(tok->start.loc, rmsg::macro_not_found)
        << tok->atom();
      return TOK_NAME;
    }
  }
  return current()->kind;
}

TokenKind
Preprocessor::eval_peek()
{
  // Boil away any TOK_NAMES.
  if (peek() == TOK_NAME) {
    eval_next();
    undo();
  }
  return peek();
}

bool
Preprocessor::eval_match(TokenKind kind)
{
  // Boil away any TOK_NAMES.
  if (peek() == TOK_NAME) {
    eval_next();
    undo();
  }
  return match(kind);
}

bool
Preprocessor::eval_expect(TokenKind kind)
{
  // Boil away any TOK_NAMES.
  if (peek() == TOK_NAME) {
    eval_next();
    undo();
  }
  return expect(kind);
}

bool
Preprocessor::eval_unary(int* val)
{
  TokenKind kind = eval_next();

  switch (kind) {
    case TOK_MINUS:
      if (!eval_inner(val))
        return false;
      *val = -*val;
      return true;

    case TOK_NOT:
      if (!eval_inner(val))
        return false;
      *val = !*val;
      return true;

    case TOK_TILDE:
      if (!eval_inner(val))
        return false;
      *val = ~*val;
      return true;

    case TOK_LPAREN:
      if (!eval_inner(val))
        return false;
      if (!eval_expect(TOK_RPAREN))
        return false;
      return true;

    // If we see a TOK_DEFINED in eval_next(), we won't decompose it, so we
    // should be guaranteed we don't decompose the TOK_NAME after it
    // prematurely. We are stricter here in that we require (), we don't allow
    // something like:
    //   #define LP (
    //   #define RP )
    //   #if defined LP egg RP
    //
    // Since the LP would be ambiguous. SP1 allowed the RP, but for consistency
    // we allow neither.
    case TOK_DEFINED:
    {
      size_t nparens = 0;
      while (match(TOK_LPAREN))
        nparens++;
      if (!expect(TOK_NAME))
        return false;
      for (size_t i = 0; i < nparens; i++) {
        if (!expect(TOK_RPAREN))
          return false;
      }

      Atom* atom = current()->atom();
      AtomMap<Macro*>::Result r = macros_.find(atom);
      *val = r.found() ? 1 : 0;
      return true;
    }

    case TOK_INTEGER_LITERAL:
    {
      const Token* tok = current();
      if (tok->int64Value() > INT_MAX || tok->int64Value() < INT_MIN) {
        cc_.report(tok->start.loc, rmsg::int_literal_overflow);
        return false;
      }
      *val = tok->int32Value();
      return true;
    }

    case TOK_NAME:
      // Error already reported.
      return false;

    default:
    {
      const Token* tok = current();
      cc_.report(tok->start.loc, rmsg::unexpected_directive_token)
        << TokenNames[tok->kind];
      return false;
    }
  }
}

bool
Preprocessor::eval_binary(int prec, int* val)
{
  assert(prec <= 12);
  assert(prec >= 2);

  int left;
  bool r = (prec == 2)
           ? eval_unary(&left)
           : eval_binary(prec - 1, &left);
  if (!r)
    return false;

  while (true) {
    TokenKind kind = eval_peek();
    const Op* op = FindOperator(kind);
    if (!op || op->prec != prec)
      break;

    SourceLocation loc;
    {
      Token* tok = tokens_->pop();
      assert(tok->kind == kind);

      loc = tok->start.loc;
    }

    assert(op->arity == 2);
    int right;
    bool r = (prec >= 2)
             ? eval_unary(&right)
             : eval_binary(prec - 1, &right);
    if (!r)
      return false;
    if ((kind == TOK_SLASH || kind == TOK_PERCENT) && !right) {
      cc_.report(loc, rmsg::divide_by_zero);
      return false;
    }
    left = BinaryEval(kind, left, right);
  }

  *val = left;
  return true;
}
