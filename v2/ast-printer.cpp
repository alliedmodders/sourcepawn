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
#include "ast.h"

using namespace ke;

class AstPrinter : public AstVisitor
{
  FILE *fp_;
  size_t level_;

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

  public:
  AstPrinter(FILE *fp)
    : fp_(fp),
    level_(0)
  {
  }

  void dump(const TypeSpecifier &spec, Atom *name) {
    if (spec.isConst())
      fprintf(fp_, "const ");
    if (spec.resolver() == TOK_NAME) {
      fprintf(fp_, "%s", spec.proxy()->name()->chars());
    } else if (spec.resolver() == TOK_LABEL) {
      fprintf(fp_, "%s:", spec.proxy()->name()->chars());
    } else if (IsNewTypeToken(spec.resolver())) {
      fprintf(fp_, "%s", TokenNames[spec.resolver()]);
    } else if (spec.resolver() == TOK_IMPLICIT_INT) {
      fprintf(fp_, "implicit-int");
    } else if (spec.resolver() == TOK_FUNCTION) {
      fprintf(fp_, "function ");
      dump(spec.signature());
    }

    if (spec.resolver() != TOK_LABEL && !spec.dims() && spec.rank()) {
      for (uint32_t i = 0; i < spec.rank(); i++)
        fprintf(fp_, "[]");
    }

    if (name)
      fprintf(fp_, " %s", name->chars());

    if (spec.resolver() == TOK_LABEL || spec.dims()) {
      for (uint32_t i = 0; i < spec.rank(); i++)
        fprintf(fp_, "[]");
    }
  }

  void dump(FunctionSignature *sig) {
    dump(*sig->returnType(), nullptr);
    if (!sig->parameters()->length()) {
      fprintf(fp_, " ()\n");
      return;
    }
    fprintf(fp_, " (\n");
    indent();
    for (size_t i = 0; i < sig->parameters()->length(); i++) {
      prefix();
      VariableDeclaration *param = sig->parameters()->at(i);
      dump(*param->spec(), param->name());
      fprintf(fp_, "\n");
    }
    unindent();
    prefix();
    fprintf(fp_, ")");
  }
  void dump(Atom *name, const FunctionOrAlias &method, const char *prefix) {
    if (prefix)
      fprintf(fp_, "%s method ", prefix);
    else
      fprintf(fp_, "method ");
    if (method.isAlias()) {
      fprintf(fp_, "%s = %s", name->chars(), method.alias()->name()->chars());
    } else {
      FunctionNode *node = method.fun();
      fprintf(fp_, "%s ", name->chars());
      dump(node->signature());
    }
  }
  void visitNameProxy(NameProxy *name) {
    prefix();
    fprintf(fp_, "[ NameProxy (%s)\n", name->name()->chars());
  }
  void visitCallExpression(CallExpression *node) {
    prefix();
    fprintf(fp_, "[ CallExpression\n");
    indent();
    node->callee()->accept(this);
    for (size_t i = 0; i < node->arguments()->length(); i++)
      node->arguments()->at(i)->accept(this);
    unindent();
  }
  void visitFunctionStatement(FunctionStatement *node) {
    prefix();
    fprintf(fp_, "[ FunctionStatement (%s)\n", node->name()->chars());
    indent();
    {
      prefix();
      dump(node->signature());
      fprintf(fp_, "\n");
      if (node->body())
        node->body()->accept(this);
    }
    unindent();
  }
  void visitExpressionStatement(ExpressionStatement *node) {
    prefix();
    fprintf(fp_, "[ ExpressionStatement\n");
    indent();
    node->expression()->accept(this);
    unindent();
  }
  void visitAssignment(Assignment *node) {
    prefix();
    fprintf(fp_, "[ Assignment\n");
    indent();
    node->lvalue()->accept(this);
    node->expression()->accept(this);
    unindent();
  }
  void visitTernaryExpression(TernaryExpression *node) {
    prefix();
    fprintf(fp_, "[ TernaryExpression\n");
    indent();
    node->condition()->accept(this);
    node->left()->accept(this);
    node->right()->accept(this);
    unindent();
  }
  void visitBinaryExpression(BinaryExpression *node) {
    prefix();
    fprintf(fp_, "[ BinaryExpression (%s)\n", TokenNames[node->token()]);
    indent();
    node->left()->accept(this);
    node->right()->accept(this);
    unindent();
  }
  void visitUnaryExpression(UnaryExpression *node) {
    prefix();
    fprintf(fp_, "[ UnaryExpression (%s)\n", TokenNames[node->token()]);
    indent();
    node->expression()->accept(this);
    unindent();
  }
  void visitReturnStatement(ReturnStatement *node) {
    prefix();
    fprintf(fp_, "[ ReturnStatement\n");
    indent();
    if (node->expression())
      node->expression()->accept(this);
    unindent();
  }
  void visitForStatement(ForStatement *node) {
    prefix();
    fprintf(fp_, "[ ForStatement\n");
    indent();
    if (node->initialization())
      node->initialization()->accept(this);
    if (node->condition())
      node->condition()->accept(this);
    if (node->update())
      node->update()->accept(this);
    node->body()->accept(this);
    unindent();
  }
  void visitBlockStatement(BlockStatement *node) {
    prefix();
    fprintf(fp_, "[ BlockStatement\n");
    indent();
    for (size_t i = 0; i < node->statements()->length(); i++)
      node->statements()->at(i)->accept(this);
    unindent();
  }
  void visitVariableDeclaration(VariableDeclaration *node) {
    prefix();
    fprintf(fp_, "[ VariableDeclaration (%s)\n", node->name()->chars());
    indent();
    if (node->initialization())
      node->initialization()->accept(this);
    unindent();
    if (node->next())
      node->next()->accept(this);
  }
  void visitCharLiteral(CharLiteral *node) {
    prefix();
    fprintf(fp_, "[ CharLiteral (%c)\n", (char)node->value());
  }
  void visitIntegerLiteral(IntegerLiteral *node) {
    prefix();
    fprintf(fp_, "[ IntegerLiteral (%" KE_FMT_I64 ")\n", node->value());
  }
  void visitTokenLiteral(TokenLiteral *node) {
    prefix();
    fprintf(fp_, "[ TokenLiteral (%s)\n", TokenNames[node->token()]);
  }
  void visitFloatLiteral(FloatLiteral *node) {
    prefix();
    fprintf(fp_, "[ FloatLiteral (%f)\n", node->value());
  }
  void visitIfStatement(IfStatement *node) {
    prefix();
    fprintf(fp_, "[ IfStatement\n");
    indent();
    node->ifTrue()->accept(this);
    if (node->ifFalse())
      node->ifFalse()->accept(this);
    unindent();
  }
  void visitFieldExpression(FieldExpression *node) {
    prefix();
    fprintf(fp_, "[ FieldExpression (%s)\n", node->field()->chars());
    indent();
    node->base()->accept(this);
    unindent();
  }
  void visitIndexExpression(IndexExpression *node) {
    prefix();
    fprintf(fp_, "[ IndexExpression\n");
    indent();
    node->left()->accept(this);
    node->right()->accept(this);
    unindent();
  }
  void visitEnumStatement(EnumStatement *node) override {
    prefix();
    fprintf(fp_, "[ EnumStatement (%s)\n", node->name() ? node->name()->chars() : "<anonymous>");
    indent();
    for (size_t i = 0; i < node->entries()->length(); i++)
      node->entries()->at(i)->accept(this);
    unindent();
  }
  void visitEnumConstant(EnumConstant *node) {
    prefix();
    fprintf(fp_, "[ EnumConstant (%s)\n", node->name()->chars());
    if (node->expression()) {
      indent();
      node->expression()->accept(this);
      unindent();
    }
  }
  void visitSizeofExpression(SizeofExpression *node) {
    prefix();
    fprintf(fp_, "[ SizeofExpression (level=%d)\n", (int)node->level());
    indent();
    node->proxy()->accept(this);
    unindent();
  }
  void visitWhileStatement(WhileStatement *node) {
    prefix();
    fprintf(fp_, "[ WhileStatement (%s)\n",
        (node->token() == TOK_DO) ? "do" : "while");
    indent();
    if (node->token() == TOK_DO) {
      node->condition()->accept(this);
      node->body()->accept(this);
    } else {
      node->condition()->accept(this);
      node->body()->accept(this);
    }
    unindent();
  }
  void visitBreakStatement(BreakStatement *node) {
    prefix();
    fprintf(fp_, "[ BreakStatement\n");
  }
  void visitContinueStatement(ContinueStatement *node) {
    prefix();
    fprintf(fp_, "[ ContinueStatement\n");
  }
  void visitStringLiteral(StringLiteral *node) {
    prefix();
    fprintf(fp_, "[ StringLiteral\n");
  }
  void visitIncDecExpression(IncDecExpression *node) {
    prefix();
    fprintf(fp_, "[ IncDecExpression (postfix=%d)\n", node->postfix());
    indent();
    node->expression()->accept(this);
    unindent();
  }
  void visitThisExpression(ThisExpression *node) {
    prefix();
    fprintf(fp_, "[ ThisExpression\n");
  }
  void visitSwitchStatement(SwitchStatement *node) {
    prefix();
    fprintf(fp_, "[ SwitchStatement\n");
    indent();
    node->expression()->accept(this);
    for (size_t i = 0; i < node->cases()->length(); i++) {
      Case *c = node->cases()->at(i);
      c->expression()->accept(this);
      if (c->others()) {
        for (size_t j = 0; j < c->others()->length(); j++)
          c->others()->at(j)->accept(this);
      }
      indent();
      c->statement()->accept(this);
      unindent();
    }
    if (node->defaultCase())
      node->defaultCase()->accept(this);
    unindent();
  }
  void visitArrayLiteral(ArrayLiteral *node) {
    prefix();
    fprintf(fp_, "[ ArrayLiteral\n");
    indent();
    for (size_t i = 0; i < node->expressions()->length(); i++) {
      Expression *expr = node->expressions()->at(i);
      indent();
      expr->accept(this);
      unindent();
    }
    if (node->repeatLastElement()) {
      indent();
      prefix();
      fprintf(fp_, "...\n");
      unindent();
    }
  }
  void visitStructInitializer(StructInitializer *node) {
    prefix();
    fprintf(fp_, "[ StructInitializer\n");
    indent();
    for (size_t i = 0; i < node->pairs()->length(); i++) {
      NameAndValue *pair = node->pairs()->at(i);
      prefix();
      fprintf(fp_, "%s = \n", pair->name()->chars());
      indent();
      pair->expr()->accept(this);
      unindent();
    }
    unindent();
  }
  void visitTypedefStatement(TypedefStatement *node) {
    prefix();
    fprintf(fp_, "[ TypedefStatement\n");
    indent();
    prefix();
    dump(*node->spec(), node->name());
    fprintf(fp_, "\n");
    unindent();
  }

  void dumpLayout(LayoutDecls *body) {
    for (size_t i = 0; i < body->length(); i++)
      body->at(i)->accept(this);
  }

  void visitFieldDecl(FieldDecl *decl) override {
    prefix();
    fprintf(fp_, "[ FieldDecl ");
    dump(*decl->spec(), decl->name());
    fprintf(fp_, "\n");
  }
  void visitMethodDecl(MethodDecl *decl) override {
    prefix();
    fprintf(fp_, "[ MethodDecl ");
    dump(decl->name(), *decl->method(), nullptr);
    fprintf(fp_, "\n");
  }
  void visitPropertyDecl(PropertyDecl *decl) override {
    prefix();
    fprintf(fp_, "[ PropertyDecl ");
    indent();
    if (!decl->getter()->isEmpty()) {
      prefix();
      dump(decl->name(), *decl->getter(), "getter");
      fprintf(fp_, "\n");
    }
    if (!decl->setter()->isEmpty()) {
      prefix();
      dump(decl->name(), *decl->setter(), "setter");
      fprintf(fp_, "\n");
    }
    unindent();
  }

  void visitRecordDecl(RecordDecl *node) override {
    prefix();
    fprintf(fp_, "[ LayoutStatement %s %s\n",
      TokenNames[node->token()],
      node->name()->chars()
    );
    indent();
    dumpLayout(node->body());
    unindent();
  }
  void visitMethodmapDecl(MethodmapDecl *node) {
    prefix();
    fprintf(fp_, "[ MethodmapDecl %s", node->name()->chars());
    if (node->parent())
      fprintf(fp_, " (extends %s)", node->parent()->name()->chars());
    if (node->nullable())
      fprintf(fp_, " (nullable)");
    fprintf(fp_, "\n");
    indent();
    dumpLayout(node->body());
    unindent();
  }
};

void
ParseTree::dump(FILE *fp)
{
  AstPrinter printer(fp);

  for (size_t i = 0; i < statements_->length(); i++)
    statements_->at(i)->accept(&printer);
}
