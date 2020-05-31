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

#include <utility>

#include "scopes.h"
#include "type-resolver.h"

namespace sp {

using namespace ke;

class NameResolver
{
 public:
  NameResolver(CompileContext& cc);

  void OnEnterParser();
  void OnLeaveParser();
  void OnEnterScope(Scope::Kind kind);
  Scope* OnLeaveScope();
  void OnLeaveOrphanScope();
  void OnNameProxy(NameProxy* proxy);
  void OnTagProxy(NameProxy* proxy);
  void OnEnumDecl(EnumStatement* node);
  void OnEnumValueDecl(EnumConstant* cs);
  VarDecl* HandleVarDecl(NameToken name, TokenKind kind, SymAttrs flags, TypeSpecifier& spec, Expression* init);
  void OnEnterMethodmap(MethodmapDecl* methodmap);
  void OnLeaveMethodmap(MethodmapDecl* methodmap);
  void OnEnterRecordDecl(RecordDecl* decl);
  void OnLeaveRecordDecl(RecordDecl* decl);
  void OnEnterFunctionDecl(FunctionStatement* stmt);
  void OnLeaveFunctionDecl(FunctionStatement* node);
  void OnReturnStmt(ReturnStatement* stmt);
  FieldDecl* HandleFieldDecl(const SourceLocation& pos,
                             const NameToken& name,
                             TypeSpecifier& spec);
  MethodDecl* EnterMethodDecl(const SourceLocation& begin,
                              const NameToken& nameToken,
                              TypeSpecifier* spec,
                              TypeExpr* te,
                              bool isStatic);
  void LeaveMethodDecl(MethodDecl* decl);
  PropertyDecl* EnterPropertyDecl(const SourceLocation& begin,
                                  const NameToken& nameToken,
                                  TypeSpecifier& spec);
  void LeavePropertyDecl(PropertyDecl* decl);
  TypedefDecl* HandleTypedefDecl(const SourceLocation& begin,
                                 Atom* name,
                                 TypeSpecifier& spec);
  ViewAsExpression* HandleViewAs(const SourceLocation& pos,
                                 TypeSpecifier& spec,
                                 Expression* expr);
  CallNewExpr* HandleCallNewExpr(const SourceLocation& pos,
                                 TypeSpecifier& spec,
                                 ExpressionList* args);
  NewArrayExpr* HandleNewArrayExpr(const SourceLocation& pos,
                                   TypeSpecifier& spec,
                                   ExpressionList* args);
  TypesetDecl* EnterTypeset(const SourceLocation& loc, const NameToken& name);
  void EnterTypeIntoTypeset(TypesetDecl* decl, std::vector<TypesetDecl::Entry>& types, TypeSpecifier& spec);
  void FinishTypeset(TypesetDecl* decl, const std::vector<TypesetDecl::Entry>& types);

  void HandleFunctionSignature(
    TokenKind kind,
    FunctionNode* node,
    TypeSpecifier& spec,
    ParameterList* params,
    bool canResolveEagerly);
  void HandleFunctionSignature(
    TokenKind kind,
    FunctionNode* node,
    const TypeExpr& te,
    ParameterList* params,
    bool canResolveEagerly);

  FunctionSignature* HandleFunctionSignature(
    TokenKind kind,
    TypeSpecifier& spec,
    ParameterList* params,
    bool canResolveEagerly);
  FunctionSignature* HandleFunctionSignature(
    TokenKind kind,
    const TypeExpr& te,
    ParameterList* params,
    bool canResolveEagerly);

 private:
  void declareSystemTypes(Scope* scope);
  void declareSystemType(Scope* scope, const char* name, PrimitiveType prim);
  void declareSystemType(Scope* scope, const char* name, Type* type);
  Scope* getOrCreateScope();
  Symbol* lookup(Atom* name);
  bool registerSymbol(Symbol* sym);
  void registerFunction(FunctionSymbol* sym);
  void reportRedeclaration(Symbol* sym, Symbol* other);
  bool canDefineMethodmap(MethodmapDecl* methodmap);
  void resolveUnknownTags();
  void resolveUnboundNames();
  TypeExpr delay(const TypeSpecifier& spec);
  Type* resolveBase(TypeSpecifier& spec);
  TypeExpr resolve(TypeSpecifier& spec, TypeSpecHelper* helper = nullptr);
  TypeExpr resolveReturnType(TypeSpecifier& spec);

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
    SymbolEnv(Scope* scope)
     : scope_(scope),
       kind_(scope->kind())
    {}
    SymbolEnv(Scope::Kind kind)
     : scope_(nullptr),
       kind_(kind)
    {}
    SymbolEnv(SymbolEnv&& other)
     : scope_(other.scope_),
       kind_(other.kind_),
       children_(std::move(other.children_))
    {
    }

    Scope* scope() const {
      return scope_;
    }
    Scope::Kind kind() const {
      return kind_;
    }
    std::vector<Scope*>& children() {
      return children_;
    }

    void setScope(Scope* scope) {
      assert(!scope_);
      assert(scope->kind() == kind_);
      scope_ = scope;
    }

    void addChild(Scope* child) {
      if (scope_) {
        // We've got a scope already, so just add the child.
        child->setParent(scope_);
      } else {
        // Wait until we leave the environment to decide what to do.
        children_.push_back(child);
      }
    }

    SymbolEnv& operator =(SymbolEnv&& other) {
      scope_ = other.scope_;
      kind_ = other.kind_;
      children_ = std::move(other.children_);
      return *this;
    }

   private:
    SymbolEnv(const SymbolEnv& other) = delete;
    SymbolEnv& operator =(const SymbolEnv& other) = delete;
    
   private:
    Scope* scope_;
    Scope::Kind kind_;
    std::vector<Scope*> children_;
  };

 private:
  CompileContext& cc_;
  PoolAllocator& pool_;
  TypeResolver tr_;
  GlobalScope* globals_;
  std::vector<SymbolEnv> env_;

  LayoutScope* layout_scope_;
  std::vector<LayoutScope*> saved_layout_scopes_;

  bool encountered_return_value_;

  Atom* atom_String_;
  Atom* atom_Float_;
  Atom* atom_any_;
  Atom* atom_Function_;
  Atom* atom_bool_;

  AtomMap<NameProxy*> user_tags_;
  std::vector<NameProxy*> unresolved_names_;
};

}

#endif // _include_spcomp_name_resolver_h_
