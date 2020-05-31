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

static const char*
GetCastOpName(sema::CastOp op)
{
  switch (op) {
#define _(name) case sema::CastOp::name: return #name;
    CAST_OP_MAP(_)
#undef _
    default:
      return "unknown";
  }
}

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
        visitBlockStatement(node->body());
    }
    unindent();
  }

  void visitBlockStatement(ast::BlockStatement* body) override {
    prefix();
    fprintf(fp_, "- BlockStatement\n");
    indent();
    {
      ast::StatementList* statements = body->statements();
      for (size_t i = 0; i < statements->size(); i++) {
        ast::Statement* stmt = statements->at(i);
        stmt->accept(this);
      }
    }
    unindent();
  }

  void visitVarDecl(ast::VarDecl* stmt) override {
    for (; stmt; stmt = stmt->next()) {
      prefix();
      fprintf(fp_, "- VarDecl: %s\n", BuildTypeName(stmt->sym()->type(), stmt->sym()->name()).c_str());
      indent();
      {
        if (sema::Expr* expr = stmt->sema_init())
          printExpr(expr);
      }
      unindent();
    }
  }

  void visitReturnStatement(ast::ReturnStatement* stmt) override {
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

  void visitWhileStatement(ast::WhileStatement* stmt) override {
    prefix();
    fprintf(fp_, "- WhileStatement\n");
    indent();
    {
      prefix();
      fprintf(fp_, "%s\n", TokenNames[stmt->token()]);
      printExpr(stmt->sema_cond());
      stmt->body()->accept(this);
    }
    unindent();
  }

  void visitForStatement(ast::ForStatement* stmt) override {
    prefix();
    fprintf(fp_, "- ForStatement\n");
    indent();
    {
      if (ast::Statement* init = stmt->initialization()) {
        init->accept(this);
      } else {
        prefix();
        fprintf(fp_, "(no init)\n");
      }
      if (sema::Expr* expr = stmt->sema_cond()) {
        printExpr(expr);
      } else {
        prefix();
        fprintf(fp_, "(no cond)\n");
      }
      if (ast::Statement* update = stmt->update()) {
        update->accept(this);
      } else {
        prefix();
        fprintf(fp_, "(no update)\n");
      }
      stmt->body()->accept(this);
    }
    unindent();
  }

  void visitBreakStatement(ast::BreakStatement* stmt) override {
    prefix();
    fprintf(fp_, "- BreakStatement\n");
  }

  void visitContinueStatement(ast::ContinueStatement* stmt) override {
    prefix();
    fprintf(fp_, "- ContinueStatement\n");
  }

  void visitIfStatement(ast::IfStatement* node) override {
    prefix();
    fprintf(fp_, "- IfStatement\n");
    indent();
    {
      for (size_t i = 0; i < node->clauses()->size(); i++) {
        const ast::IfClause& clause = node->clauses()->at(i);
        printExpr(clause.sema_cond);
        clause.body->accept(this);
      }
      if (ast::Statement* stmt = node->fallthrough())
        stmt->accept(this);
    }
    unindent();
  }

  void visitSwitchStatement(ast::SwitchStatement* node) override {
    prefix();
    fprintf(fp_, "- SwitchStatement\n");
    indent();
    {
      for (size_t i = 0; i < node->cases()->size(); i++) {
        ast::Case* entry = node->cases()->at(i);

        prefix();
        fprintf(fp_, "case: ");
        for (size_t j = 0; j < entry->values()->size(); j++) {
          fprintf(fp_, "%d", entry->values()->at(j));
          if (j != entry->values()->size() - 1)
            fprintf(fp_, ",");
        }
        fprintf(fp_, "\n");

        indent();
        {
          entry->statement()->accept(this);
        }
        unindent();
      }

      if (ast::Statement* stmt = node->defaultCase()) {
        prefix();
        fprintf(fp_, "default:\n");
        indent();
        {
          stmt->accept(this);
        }
        unindent();
      }
    }
    unindent();
  }

  void visitExpressionStatement(ast::ExpressionStatement* stmt) override {
    prefix();
    fprintf(fp_, "- ExpressionStatement\n");
    indent();
    {
      printExpr(stmt->sema_expr());
    }
    unindent();
  }

  void printExpr(sema::Expr* expr) {
    switch (expr->kind()) {
      case sema::ExprKind::ConstValue:
        printConstValue(expr->toConstValueExpr());
        break;
      case sema::ExprKind::Binary:
        printBinary(expr->toBinaryExpr());
        break;
      case sema::ExprKind::Unary:
        printUnary(expr->toUnaryExpr());
        break;
      case sema::ExprKind::Call:
        printCall(expr->toCallExpr());
        break;
      case sema::ExprKind::NamedFunction:
        printNamedFunction(expr->toNamedFunctionExpr());
        break;
      case sema::ExprKind::Var:
        printVar(expr->toVarExpr());
        break;
      case sema::ExprKind::IncDec:
        printIncDec(expr->toIncDecExpr());
        break;
      case sema::ExprKind::ImplicitCast:
        printImplicitCast(expr->toImplicitCastExpr());
        break;
      case sema::ExprKind::String:
        printString(expr->toStringExpr());
        break;
      case sema::ExprKind::Index:
        printIndex(expr->toIndexExpr());
        break;
      case sema::ExprKind::Load:
        printLoad(expr->toLoadExpr());
        break;
      case sema::ExprKind::Store:
        printStore(expr->toStoreExpr());
        break;
      case sema::ExprKind::ArrayInit:
        printArrayInit(expr->toArrayInitExpr());
        break;
      case sema::ExprKind::NewArray:
        printNewArray(expr->toNewArrayExpr());
        break;
      case sema::ExprKind::Ternary:
        printTernary(expr->toTernaryExpr());
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

  void printCall(sema::CallExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      printExpr(expr->callee());
      for (size_t i = 0; i < expr->args()->size(); i++)
        printExpr(expr->args()->at(i));
    }
    unindent();
  }

  void printNamedFunction(sema::NamedFunctionExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      prefix();
      fprintf(fp_, "%s\n", expr->sym()->name()->chars());
    }
    unindent();
  }

  void printTernary(sema::TernaryExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      printExpr(expr->choose());
      printExpr(expr->left());
      printExpr(expr->right());
    }
    unindent();
  }

  void printBinary(sema::BinaryExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      prefix();
      fprintf(fp_, "\"%s\"\n", TokenNames[expr->token()]);
      printExpr(expr->left());
      printExpr(expr->right());
    }
    unindent();
  }

  void printUnary(sema::UnaryExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      prefix();
      fprintf(fp_, "\"%s\"\n", TokenNames[expr->token()]);
      printExpr(expr->expr());
    }
    unindent();
  }

  void printLoad(sema::LoadExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      printExpr(expr->lvalue());
    }
    unindent();
  }

  void printIndex(sema::IndexExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      printExpr(expr->base());
      printExpr(expr->index());
    }
    unindent();
  }

  void printImplicitCast(sema::ImplicitCastExpr* expr) {
    enter(expr, expr->type(), GetCastOpName(expr->op()));
    indent();
    {
      printExpr(expr->expr());
    }
    unindent();
  }

  void printString(sema::StringExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      // Note: this is gross because we don't print \n or \t.
      prefix();
      fprintf(fp_, "\"");
      const char* ptr = expr->literal()->chars();
      while (*ptr) {
        if (*ptr == '\n') {
          fprintf(fp_, "\\n");
        } else if (*ptr == '\t') {
          fprintf(fp_, "\\t");
        } else {
          fprintf(fp_, "%c", *ptr);
        }
        ptr++;
      }
      fprintf(fp_, "\"\n");
    }
    unindent();
  }

  void printVar(sema::VarExpr* expr) {
    prefix();
    std::string str = BuildTypeName(expr->type(), nullptr);
    fprintf(fp_, "- %s %s (%s)\n",
      expr->prettyName(),
      expr->sym()->name()->chars(),
      str.c_str());
  }

  void printIncDec(sema::IncDecExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      prefix();
      fprintf(fp_, "%s (%s)\n",
        TokenNames[expr->token()],
        (expr->postfix() ? "postfix" : "prefix"));
      printExpr(expr->expr());
    }
    unindent();
  }

  void printStore(sema::StoreExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      printExpr(expr->left());
      printExpr(expr->right());
    }
    unindent();
  }

  void printArrayInit(sema::ArrayInitExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      for (size_t i = 0; i < expr->exprs()->size(); i++) {
        sema::Expr* e = expr->exprs()->at(i);
        printExpr(e);
      }
      if (expr->repeat_last_element()) {
        prefix();
        fprintf(fp_, "(repeat)\n");
      }
    }
    unindent();
  }

  void printNewArray(sema::NewArrayExpr* expr) {
    enter(expr, expr->type());
    indent();
    {
      for (size_t i = 0; i < expr->exprs()->size(); i++) {
        sema::Expr* e = expr->exprs()->at(i);
        printExpr(e);
      }
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

  void enter(sema::Expr* expr, Type* type, const char* extra = nullptr) {
    prefix();
    std::string str = BuildTypeName(type, nullptr);
    if (extra)
      fprintf(fp_, "- %s (%s) %s\n", expr->prettyName(), str.c_str(), extra);
    else
      fprintf(fp_, "- %s (%s)\n", expr->prettyName(), str.c_str());
  }

  void dump(const ast::TypeExpr& te, Atom* name) {
    std::string str = BuildTypeName(te.resolved(), name);
    fprintf(fp_, "%s", str.c_str());
  }

  void dump(ast::FunctionSignature* sig) {
    dump(sig->returnType(), nullptr);
    if (!sig->parameters()->size()) {
      fprintf(fp_, " ()\n");
      return;
    }
    fprintf(fp_, " (\n");
    indent();
    for (size_t i = 0; i < sig->parameters()->size(); i++) {
      prefix();
      ast::VarDecl* param = sig->parameters()->at(i);
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
