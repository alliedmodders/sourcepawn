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
#ifndef _include_spcomp_name_resolver_h_
#define _include_spcomp_name_resolver_h_

#include "scopes.h"
#include "parser.h"

namespace sp {

using namespace ke;

class NameResolver : public Parser::Delegate
{
 public:
  NameResolver(CompileContext &cc);

  void OnEnterParser() override;
  void OnLeaveParser() override;
  void OnEnterScope(Scope::Kind kind) override;
  Scope *OnLeaveScope() override;
  void OnLeaveOrphanScope() override;
  void OnNameProxy(NameProxy *proxy) override;
  void OnTagProxy(NameProxy *proxy) override;
  void OnEnumDecl(EnumStatement *node) override;
  void OnEnumValueDecl(EnumConstant *cs) override;
  void OnVarDecl(VariableDeclaration *var) override;
  void OnEnterMethodmap(MethodmapDecl *methodmap) override;
  void OnLeaveMethodmap(MethodmapDecl *methodmap) override;
  void OnEnterRecordDecl(RecordDecl *decl) override;
  void OnLeaveRecordDecl(RecordDecl *decl) override;
  void OnMethodDecl(MethodDecl *decl) override;
  void OnPropertyDecl(PropertyDecl *decl) override;
  void OnFieldDecl(FieldDecl *decl) override;
  void OnEnterFunctionDecl(FunctionStatement *stmt) override;
  void OnLeaveFunctionDecl(FunctionStatement *node) override;
  void OnReturnStmt(ReturnStatement *stmt) override;
  void OnTypedefDecl(TypedefStatement *stmt) override;

  // In the future we should let this feedback into the next phase, so we can
  // avoid re-walking the entire AST to resolve types.
  TypeExpr HandleTypeExpr(const TypeSpecifier &spec) override;

 private:
  void declareSystemTypes(Scope *scope);
  void declareSystemType(Scope *scope, const char *name, PrimitiveType prim);
  void declareSystemType(Scope *scope, const char *name, Type *type);
  Scope *getOrCreateScope();
  Symbol *lookup(Atom *name);
  bool registerSymbol(Symbol *sym);
  void registerFunction(FunctionSymbol *sym);
  void reportRedeclaration(Symbol *sym, Symbol *other);
  void defineMethodmap(MethodmapDecl *methodmap);
  void resolveUnknownTags();
  void resolveUnboundNames();

 private:
  // Rather than create a Scope for every block we encounter, we place a
  // marker on the stack that can create scopes lazily. These markers are
  // called "symbol environments".
  //
  // As we leave symbol environments, we may have created a scope, and thus, it
  // has to be linked into the scope hierarchy. This poses a problem, as we
  // don't necessarily have all intermediate scopes yet. Instead, we propagate
  // child scopes and link them once reified.
  class SymbolEnv
  {
   public:
    SymbolEnv()
    {}
    SymbolEnv(Scope *scope)
     : scope_(scope),
       kind_(scope->kind())
    {}
    SymbolEnv(Scope::Kind kind)
     : scope_(nullptr),
       kind_(kind)
    {}
    SymbolEnv(SymbolEnv &&other)
     : scope_(other.scope_),
       kind_(other.kind_),
       children_(Move(other.children_))
    {
    }

    Scope *scope() const {
      return scope_;
    }
    Scope::Kind kind() const {
      return kind_;
    }
    Vector<Scope *> &children() {
      return children_;
    }

    void setScope(Scope *scope) {
      assert(!scope_);
      assert(scope->kind() == kind_);
      scope_ = scope;
    }

    void addChild(Scope *child) {
      if (scope_) {
        // We've got a scope already, so just add the child.
        child->setParent(scope_);
      } else {
        // Wait until we leave the environment to decide what to do.
        children_.append(child);
      }
    }

   private:
    SymbolEnv(const SymbolEnv &other) KE_DELETE;
    SymbolEnv &operator =(const SymbolEnv &other) KE_DELETE;
    
   private:
    Scope *scope_;
    Scope::Kind kind_;
    Vector<Scope *> children_;
  };

 private:
  CompileContext &cc_;
  PoolAllocator &pool_;
  GlobalScope *globals_;
  Vector<SymbolEnv> env_;

  LayoutScope *layout_scope_;
  Vector<LayoutScope *> saved_layout_scopes_;

  bool encountered_return_value_;

  Atom *atom_String_;
  Atom *atom_Float_;
  Atom *atom_any_;
  Atom *atom_Function_;
  Atom *atom_bool_;

  AtomMap<NameProxy *> user_tags_;
  Vector<NameProxy *> unresolved_names_;
};

}

#endif // _include_spcomp_name_resolver_h_
