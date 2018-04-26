// vim: set ts=2 sw=2 tw=99 et:
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
#include <am-string.h>
#include "ast.h"
#include "compile-context.h"
#include "json-tools.h"
#include "source-manager.h"

using namespace ke;
using namespace sp;

class JsonBuilder : public AstVisitor
{
 public:
  JsonBuilder(CompileContext& cc)
   : cc_(cc),
     pool_(cc.pool())
  {
    atom_type_ = cc_.add("type");
    atom_file_ = cc_.add("file");
    atom_offset_ = cc_.add("offset");
    atom_loc_ = cc_.add("loc");
    atom_origin_ = cc_.add("origin");
    atom_expanded_ = cc_.add("expanded");
    atom_expression_ = cc_.add("expression");
    atom_left_ = cc_.add("left");
    atom_right_ = cc_.add("right");
    atom_token_ = cc_.add("token");
    atom_name_ = cc_.add("name");
    atom_value_ = cc_.add("value");
    atom_condition_ = cc_.add("condition");
    atom_body_ = cc_.add("body");
    atom_initializer_ = cc_.add("initializer");
    atom_iftrue_ = cc_.add("iftrue");
    atom_ifelse_ = cc_.add("ifelse");
    atom_expressions_ = cc_.add("expressions");
    atom_statements_ = cc_.add("statements");
    atom_callee_ = cc_.add("callee");
    atom_arguments_ = cc_.add("arguments");
    atom_entries_ = cc_.add("entries");
    atom_parent_ = cc_.add("parent");
    atom_typespec_ = cc_.add("typespec");
    atom_resolver_ = cc_.add("resolver");
    atom_returntype_ = cc_.add("returntype");
    atom_dims_ = cc_.add("dims");
    atom_postdims_ = cc_.add("postdims");
    file_list_ = new (pool_) JsonList();
  }

  JsonObject* toJson(ParseTree* tree) {
    JsonObject* obj = new (pool_) JsonObject();

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < tree->statements()->length(); i++)
      list->add(toJson(tree->statements()->at(i)));
    obj->add(atom_body_, list);
    return obj;
  }

  // Harder cases.
  void visitPropertyDecl(PropertyDecl* node) override {
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));
    rval_->add(atom_typespec_, toJson(node->te()));
    if (!node->getter()->isEmpty())
      rval_->add(cc_.add("getter"), toJson(new (pool_) JsonObject(), node->getter()));
    if (!node->setter()->isEmpty())
      rval_->add(cc_.add("setter"), toJson(new (pool_) JsonObject(), node->setter()));
  }
  void visitVarDecl(VarDecl* node) override {
    if (node->name())
      rval_->add(atom_name_, new (pool_) JsonString(node->name()));
    rval_->add(atom_typespec_, toJson(node->te()));
  }
  void visitTypedefDecl(TypedefDecl* node) override {
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));
    rval_->add(atom_typespec_, toJson(node->te()));
  }
  void visitFunctionStatement(FunctionStatement* node) override {
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));
    rval_ = toJson(rval_, node);
  }
  void visitMethodDecl(MethodDecl* node) override {
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));
    rval_ = toJson(rval_, node->method());
  }
  void visitSwitchStatement(SwitchStatement* node) override {
    rval_->add(atom_expression_, toJson(node->expression()));

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->cases()->length(); i++) {
      Case* caze = node->cases()->at(i);
      JsonObject* obj = new (pool_) JsonObject();

      JsonList* exprs = new (pool_) JsonList();
      exprs->add(toJson(caze->expression()));
      if (caze->others()) {
        for (size_t i = 0; i < caze->others()->length(); i++)
          exprs->add(toJson(caze->others()->at(i)));
      }
      obj->add(atom_expressions_, exprs);

      obj->add(atom_body_, toJson(caze->statement()));
    }
    rval_->add(cc_.add("cases"), list);
    if (node->defaultCase())
      rval_->add(cc_.add("default"), toJson(node->defaultCase()));
  }
  void visitTokenLiteral(TokenLiteral* node) override {
    switch (node->token()) {
      case TOK_TRUE:
        rval_->add(atom_value_, new (pool_) JsonBool(true));
        break;
      case TOK_FALSE:
        rval_->add(atom_value_, new (pool_) JsonBool(false));
        break;
      case TOK_NULL:
      case TOK_INVALID_FUNCTION:
        rval_->add(atom_value_, new (pool_) JsonNull());
        break;
      default:
        assert(false);
        break;
    }
  }
  void visitArrayLiteral(ArrayLiteral* node) override {
    Atom* token = cc_.add(TokenNames[node->token()]);
    rval_->add(atom_token_, new (pool_) JsonString(token));

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->expressions()->length(); i++) {
      Expression* expr = node->expressions()->at(i);
      list->add(toJson(expr));
    }
    rval_->add(atom_expressions_, list);

    if (node->repeatLastElement())
      rval_->add(cc_.add("repeatLastElement"), new (pool_) JsonBool(true));
  }
  void visitBlockStatement(BlockStatement* node) override {
    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->statements()->length(); i++) {
      Statement* stmt = node->statements()->at(i);
      list->add(toJson(stmt));
    }
    rval_->add(atom_statements_, list);
  }
  void visitCallExpr(CallExpr* node) override {
    rval_->add(atom_callee_, toJson(node->callee()));

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->arguments()->length(); i++) {
      Expression* expr = node->arguments()->at(i);
      list->add(toJson(expr));
    }
    rval_->add(atom_arguments_, list);
  }
  void visitEnumStatement(EnumStatement* node) override {
    if (node->name())
      rval_->add(atom_name_, new (pool_) JsonString(node->name()));

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->entries()->length(); i++) {
      EnumConstant* cs = node->entries()->at(i);
      list->add(toJson(cs));
    }
    rval_->add(atom_entries_, list);
  }
  void visitStructInitializer(StructInitializer* node) override {
    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->pairs()->length(); i++) {
      NameAndValue* nv = node->pairs()->at(i);
      JsonObject* obj = new (pool_) JsonObject();
      obj->add(atom_name_, new (pool_) JsonString(nv->name()));
      obj->add(atom_expression_, toJson(nv->expr()));
      list->add(obj);
    }
    rval_->add(cc_.add("values"), list);
  }
  void visitMethodmapDecl(MethodmapDecl* node) override {
    if (node->parent())
      rval_->add(atom_parent_, new (pool_) JsonString(node->parent()->name()));
    else
      rval_->add(atom_parent_, new (pool_) JsonNull());
    rval_->add(cc_.add("nullable"), new (pool_) JsonBool(node->nullable()));
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->body()->length(); i++) {
      LayoutDecl* decl = node->body()->at(i);
      list->add(toJson(decl));
    }
    rval_->add(atom_body_, list);
  }
  void visitRecordDecl(RecordDecl* node) override {
    Atom* token = cc_.add(TokenNames[node->token()]);
    rval_->add(atom_token_, new (pool_) JsonString(token));
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < node->body()->length(); i++) {
      LayoutDecl* decl = node->body()->at(i);
      list->add(toJson(decl));
    }
    rval_->add(atom_body_, list);
  }

  // Simple cases.
  void visitFieldDecl(FieldDecl* node) override {
    if (node->name())
      rval_->add(atom_name_, new (pool_) JsonString(node->name()));
    rval_->add(atom_typespec_, toJson(node->te()));
  }
  void visitEnumConstant(EnumConstant* node) override {
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));
    if (node->expression())
      rval_->add(atom_expression_, toJson(node->expression()));
  }
  void visitIfStatement(IfStatement* node) override {
    rval_->add(atom_condition_, toJson(node->condition()));
    rval_->add(atom_iftrue_, toJson(node->ifTrue()));
    if (node->ifFalse())
      rval_->add(atom_ifelse_, toJson(node->ifFalse()));
  }
  void visitForStatement(ForStatement* node) override {
    if (node->initialization())
      rval_->add(atom_initializer_, toJson(node->initialization()));
    if (node->condition())
      rval_->add(atom_condition_, toJson(node->condition()));
    if (node->update())
      rval_->add(cc_.add("update"), toJson(node->update()));
    if (node->body())
      rval_->add(atom_body_, toJson(node->body()));
  }
  void visitWhileStatement(WhileStatement* node) override {
    Atom* token = cc_.add(TokenNames[node->token()]);
    rval_->add(atom_token_, new (pool_) JsonString(token));
    rval_->add(atom_condition_, toJson(node->condition()));
    rval_->add(atom_body_, toJson(node->body()));
  }
  void visitFloatLiteral(FloatLiteral* node) override {
    char value[64];
    ke::SafeSprintf(value, sizeof(value), "%f", node->value());
    rval_->add(atom_value_, new (pool_) JsonString(cc_.add(value)));
  }
  void visitCharLiteral(CharLiteral* node) override {
    char value[2] = {(char)node->value(), '\0'};
    rval_->add(atom_value_, new (pool_) JsonString(cc_.add(value)));
  }
  void visitStringLiteral(StringLiteral* node) override {
    rval_->add(atom_value_, new (pool_) JsonString(node->literal()));
  }
  void visitIntegerLiteral(IntegerLiteral* node) override {
    char value[64];
    ke::SafeSprintf(value, sizeof(value), "%" KE_FMT_I64, node->value());
    rval_->add(atom_value_, new (pool_) JsonString(cc_.add(value)));
  }
  void visitTernaryExpression(TernaryExpression* node) override {
    rval_->add(atom_condition_, toJson(node->condition()));
    rval_->add(atom_left_, toJson(node->left()));
    rval_->add(atom_right_, toJson(node->right()));
  }
  void visitIncDecExpression(IncDecExpression* node) override {
    rval_->add(atom_expression_, toJson(node->expression()));
  }
  void visitSizeofExpression(SizeofExpression* node) override {
    rval_->add(atom_name_, new (pool_) JsonString(node->proxy()->name()));
    rval_->add(cc_.add("depth"), new (pool_) JsonInt(node->level()));
  }
  void visitFieldExpression(FieldExpression* node) override {
    rval_->add(atom_expression_, toJson(node->base()));
    rval_->add(atom_name_, new (pool_) JsonString(node->field()));
  }
  void visitIndexExpression(IndexExpression* node) override {
    rval_->add(atom_left_, toJson(node->left()));
    rval_->add(atom_right_, toJson(node->right()));
  }
  void visitNameProxy(NameProxy* node) override {
    rval_->add(atom_name_, new (pool_) JsonString(node->name()));
  }
  void visitReturnStatement(ReturnStatement* node) override {
    if (node->expression())
      rval_->add(atom_expression_, toJson(node->expression()));
  }
  void visitAssignment(Assignment* node) override {
    Atom* token = cc_.add(TokenNames[node->token()]);
    rval_->add(atom_token_, new (pool_) JsonString(token));
    rval_->add(atom_left_, toJson(node->lvalue()));
    rval_->add(atom_right_, toJson(node->expression()));
  }
  void visitBinaryExpression(BinaryExpression* node) override {
    Atom* token = cc_.add(TokenNames[node->token()]);
    rval_->add(atom_token_, new (pool_) JsonString(token));
    rval_->add(atom_left_, toJson(node->left()));
    rval_->add(atom_right_, toJson(node->right()));
  }
  void visitDeleteStatement(DeleteStatement* node) override {
    rval_->add(atom_expression_, toJson(node->expression()));
  }
  void visitUnsafeCastExpr(UnsafeCastExpr* node) override {
    rval_->add(atom_type_, toJson(node->te()));
    rval_->add(atom_expression_, toJson(node->expr()));
  }
  void visitUnaryExpression(UnaryExpression* node) override {
    rval_->add(atom_expression_, toJson(node->expression()));
  }
  void visitExpressionStatement(ExpressionStatement* node) override {
    rval_->add(atom_expression_, toJson(node->expr()));
  }

  // No-op cases.
  void visitThisExpression(ThisExpression* node) override {
  }
  void visitBreakStatement(BreakStatement* node) override {
  }
  void visitContinueStatement(ContinueStatement* node) override {
  }

 private:
  JsonObject* toJson(AstNode* node) {
    SaveAndSet<JsonObject*> save(&rval_, new (pool_) JsonObject());
    JsonString* nodeName = new (pool_) JsonString(cc_.add(node->kindName()));
    rval_->add(atom_type_, nodeName);
    rval_->add(atom_loc_, toJson(node->loc()));
    node->accept(this);
    return ReturnAndVoid(rval_);
  }

  JsonObject* toJson(JsonObject* obj, FunctionOrAlias* method) {
    if (method->isAlias())
      obj->add(atom_value_, toJson(method->alias()));
    else
      obj = toJson(obj, method->fun());
    return obj;
  }

  JsonObject* toJson(JsonObject* obj, FunctionNode* node) {
    Atom* token = cc_.add(TokenNames[node->token()]);
    obj->add(atom_token_, new (pool_) JsonString(token));
    obj->add(atom_typespec_, toJson(new (pool_) JsonObject(), node->signature()));
    if (node->body())
      obj->add(atom_body_, toJson(node->body()));
    return obj;
  }

  JsonObject* toJson(const TypeExpr& te) {
    if (te.spec())
      return toJson(te.spec());

    // :TODO:
    return new (pool_) JsonObject();
  }

  JsonObject* toJson(TypeSpecifier* spec) {
    JsonObject* obj = new (pool_) JsonObject();
    if (spec->isVariadic())
      obj->add(cc_.add("variadic"), toJson(spec->variadicLoc()));
    if (spec->isByRef())
      obj->add(cc_.add("byref"), toJson(spec->byRefLoc()));
    if (spec->isConst())
      obj->add(cc_.add("const"), toJson(spec->constLoc()));
    obj->add(cc_.add("newdecl"), new (pool_) JsonBool(spec->isNewDecl()));

    JsonObject* resolver = new (pool_) JsonObject();
    resolver->add(atom_token_, new (pool_) JsonString(cc_.add(TokenNames[spec->resolver()])));
    switch (spec->resolver()) {
      case TOK_NAME:
      case TOK_LABEL:
        resolver->add(atom_name_, toJson(spec->proxy()));
        break;
      case TOK_FUNCTION:
        resolver = toJson(resolver, spec->signature());
        break;
      case TOK_DEFINED:
        resolver->add(
          atom_name_,
          new (pool_) JsonString(BuildTypeName(spec->getResolvedBase(), nullptr).chars()));
      default:
        break;
    }
    obj->add(atom_resolver_, resolver);

    if (!spec->rank())
      return obj;

    JsonList* dims = new (pool_) JsonList();
    for (size_t i = 0; i < spec->rank(); i++) {
      if (spec->sizeOfRank(i))
        dims->add(toJson(spec->sizeOfRank(i)));
      else
        dims->add(new (pool_) JsonNull());
    }
    if (spec->hasPostDims())
      obj->add(atom_postdims_, dims);
    else
      obj->add(atom_dims_, dims);

    return obj;
  }

  JsonObject* toJson(JsonObject* obj, FunctionSignature* sig) {
    obj->add(atom_returntype_, toJson(sig->returnType()));

    JsonList* list = new (pool_) JsonList();
    for (size_t i = 0; i < sig->parameters()->length(); i++)
      list->add(toJson(sig->parameters()->at(i)));
    obj->add(atom_arguments_, list);
    return obj;
  }

  JsonValue* toJson(const SourceLocation& loc) {
    TokenHistory history;
    cc_.source().getTokenHistory(loc, &history);

    JsonList* expanded = nullptr;
    if (!history.macros.length()) {
      if (!history.files.length())
        return new (pool_) JsonObject();
    } else {
      expanded = new (pool_) JsonList();
      for (size_t i = 0; i < history.macros.length(); i++) {
        FullSourceRef ref = cc_.source().getOrigin(history.macros[i]);
        expanded->add(toJson(ref));
      }
    }

    JsonObject* obj = new (pool_) JsonObject();
    if (expanded)
      obj->add(atom_expanded_, expanded);
    obj->add(atom_origin_, toJson(history.files[0]));
    return obj;
  }

  JsonValue* toJson(SourceFile* file) {
    Atom* path = cc_.add(file->path());
    AtomMap<JsonInt*>::Insert p = files_.findForAdd(path);
    if (p.found())
      return p->value;

    JsonInt* fileno = new (pool_) JsonInt((int)file_list_->length());
    file_list_->add(new (pool_) JsonString(path));
    files_.add(p, path, fileno);

    return fileno;
  }

  JsonValue* toJson(const FullSourceRef& ref) {
    if (!ref.file)
      return new (pool_) JsonNull();

    JsonObject* obj = new (pool_) JsonObject();
    obj->add(atom_file_, toJson(ref.file));
    obj->add(atom_offset_, new (pool_) JsonInt(ref.offset));
    return obj;
  }

 private:
  CompileContext& cc_;
  PoolAllocator& pool_;
  Atom* atom_type_;
  Atom* atom_file_;
  Atom* atom_offset_;
  Atom* atom_loc_;
  Atom* atom_origin_;
  Atom* atom_expanded_;
  Atom* atom_expression_;
  Atom* atom_left_;
  Atom* atom_right_;
  Atom* atom_token_;
  Atom* atom_name_;
  Atom* atom_value_;
  Atom* atom_condition_;
  Atom* atom_body_;
  Atom* atom_initializer_;
  Atom* atom_iftrue_;
  Atom* atom_ifelse_;
  Atom* atom_expressions_;
  Atom* atom_statements_;
  Atom* atom_callee_;
  Atom* atom_arguments_;
  Atom* atom_entries_;
  Atom* atom_parent_;
  Atom* atom_typespec_;
  Atom* atom_resolver_;
  Atom* atom_returntype_;
  Atom* atom_dims_;
  Atom* atom_postdims_;
  AtomMap<JsonInt*> files_; 
  JsonList* file_list_;

  JsonObject* rval_;
};

void
ParseTree::toJson(CompileContext& cc, FILE* fp)
{
  JsonBuilder builder(cc);
  JsonObject* obj = builder.toJson(this);
  JsonRenderer renderer(fp);
  renderer.Render(obj);
  fprintf(fp, "\n");
}
