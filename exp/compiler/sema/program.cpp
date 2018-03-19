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
#include "program.h"
#include "expressions.h"
#include "parser/ast.h"

namespace sp {

class SemaPrinter : public ast::StrictAstVisitor
{
 public:
  explicit SemaPrinter(FILE* fp)
   : fp_(fp),
     level_(0)
  {}

  void print(sema::Program* program) {
    for (ast::FunctionStatement* stmt : program->functions)
      printFunction(stmt);
  }

 private:
  void printFunction(ast::FunctionStatement* node) {
    prefix();
    fprintf(fp_, "- FunctionStatement (%s)\n", node->name()->chars());
    indent();
    {
      prefix();
      dump(node->signature());
      if (node->body())
        printBlock(node->body());
    }
    unindent();
  }

  void printBlock(ast::BlockStatement* body) {
    prefix();
    fprintf(fp_, "- BlockStatement\n");
    indent();
    {
      ast::StatementList* statements = body->statements();
      for (size_t i = 0; i < statements->length(); i++) {
        ast::Statement* stmt = statements->at(i);
        stmt->accept(this);
      }
    }
    unindent();
  }

  void visitReturnStatement(ast::ReturnStatement* stmt) {
    prefix();
    fprintf(fp_, "- ReturnStatement\n");
    indent();
    {
      sema::Expr* expr = stmt->sema_expr();
      if (!expr)
        fprintf(fp_, "(void)\n");
      else
        printExpr(expr);
    }
    unindent();
  }

  void printExpr(sema::Expr* expr) {
    switch (expr->kind()) {
      case sema::ExprKind::ConstValue:
        printConstValue(expr->toConstValueExpr());
        break;
      default:
        assert(false);
    }
  }

  void printConstValue(sema::ConstValueExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      const BoxedValue& value = expr->value();
      prefix();
      dump(value);
      fprintf(fp_, "\n");
    }
    unindent();
  }

  void dump(const BoxedValue& value) {
    switch (value.kind()) {
      case BoxedValue::Kind::Bool:
        fprintf(fp_, "%s", value.toBool() ? "true" : "false");
        break;
      case BoxedValue::Kind::Integer:
      {
        const IntValue& iv = value.toInteger();
        if (iv.isSigned())
          fprintf(fp_, "%" KE_FMT_I64, iv.asSigned());
        else
          fprintf(fp_, "%" KE_FMT_U64, iv.asUnsigned());
        break;
      }
      default:
        assert(false);
    }
  }

  void enter(sema::Expr* expr, Type* type) {
    prefix();
    AString str = BuildTypeName(type, nullptr);
    fprintf(fp_, "- %s (%s)\n", expr->prettyName(), str.chars());
  }

  void dump(const ast::TypeExpr& te, Atom *name) {
    AString str = BuildTypeName(te.resolved(), name);
    fprintf(fp_, "%s", str.chars());
  }

  void dump(ast::FunctionSignature *sig) {
    dump(sig->returnType(), nullptr);
    if (!sig->parameters()->length()) {
      fprintf(fp_, " ()\n");
      return;
    }
    fprintf(fp_, " (\n");
    indent();
    for (size_t i = 0; i < sig->parameters()->length(); i++) {
      prefix();
      ast::VarDecl *param = sig->parameters()->at(i);
      dump(param->te(), param->name());
      fprintf(fp_, "\n");
    }
    unindent();
    prefix();
    fprintf(fp_, ")");
  }

 private:
  void prefix() {
    for (size_t i = 0; i < level_; i++)
      fprintf(fp_, "  ");
  }
  void indent() {
    level_++;
  }
  void unindent() {
    level_--;
  }

 private:
  FILE* fp_;
  size_t level_;
}; 

void
sema::Program::dump(FILE* fp)
{
  SemaPrinter printer(fp);
  printer.print(this);
}

} // namespace sp
