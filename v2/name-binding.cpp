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
#include "compile-context.h"
#include "compile-phases.h"
#include "ast.h"
#include "scopes.h"
#include "symbols.h"
#include "constant-evaluator.h"
#include "types.h"
#include "source-manager.h"
#include <am-hashset.h>

using namespace ke;

// The name resolution phase is responsible for creating the symbol and scope
// hierarchy, as well as performing name binding. After this phase, on success,
// all names guaranteed to be bound.
class NameResolver : public AstVisitor
{
 private:
  // Rather than create a Scope for every block we encounter, we place a
  // marker on the stack that can create scopes lazily. These markers are
  // called "symbol environments".
  //
  // As we leave symbol environments, we may have created a scope, and thus, it
  // has to be linked into the scope hierarchy. This poses a problem, as we
  // don't necessarily have all intermediate scopes yet. Instead, we propagate
  // child scopes and link them once reified.
  class SymbolEnv : public StackLinked<SymbolEnv>
  {
   public:
    // Constructor for eagerly created scopes.
    SymbolEnv(SymbolEnv **prevp, Scope *scope)
     : StackLinked(prevp),
       scope_(scope),
       kind_(scope->kind())
    {
    }

    // Constructor for lazily created scopes.
    SymbolEnv(SymbolEnv **prevp, Scope::Kind kind)
     : StackLinked(prevp),
       scope_(nullptr),
       kind_(kind)
    {
    }

    ~SymbolEnv() {
      if (scope_) {
        // Fix up children.
        for (size_t i = 0; i < children_.length(); i++)
          children_[i]->setParent(scope_);
        
        // Add us to our parent scope.
        if (prev_)
          prev_->addChild(scope_);
      } else {
        // We didn't have a scope. Transfer any children to our parent.
        prev_->children_.extend(ke::Move(children_));
      }
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

    SymbolEnv *prev() const {
      return prev_;
    }
    Scope *scope() const {
      return scope_;
    }
    Scope::Kind kind() const {
      return kind_;
    }
    void setScope(Scope *scope) {
      assert(!scope_);
      assert(scope->kind() == kind_);
      scope_ = scope;
    }

   private:
    Scope *scope_;
    Scope::Kind kind_;
    Vector<Scope *> children_;
  };

  enum ReturnStatus {
    NoReturn,
    VoidReturn,
    ValueReturn
  };

  class FunctionLink : public ke::StackLinked<FunctionLink>
  {
   public:
    FunctionLink(FunctionLink **prevp)
     : StackLinked(prevp),
       status_(NoReturn)
    {
    }
    ReturnStatus returns() const {
      return status_;
    }
    void returns(ReturnStatus status) {
      if (status > status_)
        status_ = status;
    }
   private:
    ReturnStatus status_;
  };

 public:
  NameResolver(CompileContext &cc, TranslationUnit *unit)
   : cc_(cc),
     pool_(cc.pool()),
     unit_(unit),
     env_(nullptr),
     fun_(nullptr),
     layout_scope_(nullptr)
  {
    atom_String_ = cc_.add("String");
    atom_Float_ = cc_.add("Float");
    atom_any_ = cc_.add("any");
    atom_Function_ = cc_.add("Function");
    atom_bool_ = cc_.add("bool");

    globals_ = GlobalScope::New(pool_);
  }

  bool analyze() {
    // At the import level, we declare system types.
    declareSystemTypes(globals_);

    unit_->setGlobalScope(globals_);

    SymbolEnv env(&env_, globals_);

    for (size_t i = 0; i < unit_->tree()->statements()->length(); i++) {
      Statement *stmt = unit_->tree()->statements()->at(i);
      stmt->accept(this);

      if (!cc_.canContinueProcessing())
        return false;
    }

    resolveUnknownTags();
    resolveUnboundNames();
    return true;
  }

  void visitNameProxy(NameProxy *proxy) override {
    if (Symbol *sym = lookup(proxy->name())) {
      proxy->bind(sym);
    } else {
      // Place this symbol in the unresolved list, in case it binds to a
      // global we haven't seen yet.
      unresolved_names_.append(proxy);
    }
  }

  void visitFunctionStatement(FunctionStatement *node) override {
    assert(env_->scope()->isGlobal());

    // Function statements are always named, so add the name to the outer scope.
    FunctionSymbol *sym = new (pool_) FunctionSymbol(node, env_->scope(), node->name());

    registerGlobalFunction(sym);
    node->setSymbol(sym);
    visitFunction(node);
  }


  void visitFunction(FunctionNode *node) {
    FunctionSignature *signature = node->signature();

    // Scope for the function's arguments.
    SymbolEnv argEnv(&env_, FunctionScope::New(pool_));
    registerArguments(signature);

    // Scope for function body.
    SymbolEnv localEnv(&env_, Scope::Block);
    FunctionLink fun(&fun_);
    if (node->body())
      node->body()->accept(this);

    // Function statements have quirky semantics around their return type. For
    // reasonable compatibility with SP1, we use the following heuristics for
    // when no explicit type is declared.
    if (signature->returnType()->resolver() == TOK_IMPLICIT_INT) {
      bool cell_required = (node->token() == TOK_FORWARD || node->token() == TOK_NATIVE);
      if (cell_required || fun.returns() == ValueReturn)
        signature->returnType()->setBuiltinType(TOK_INT);
      else
        signature->returnType()->setBuiltinType(TOK_VOID);
    } else {
      visitTypeIfNeeded(signature->returnType());
    }

    node->setScopes(argEnv.scope()->toFunction(), localEnv.scope());
  }

  void visitLayoutBody(LayoutDecls *body) {
    for (size_t i = 0; i < body->length(); i++) {
      LayoutDecl *decl = body->at(i);
      decl->accept(this);
    }
  }

 private:
 public:
  void visitFieldDecl(FieldDecl *decl) override {
    visitTypeIfNeeded(decl->spec());

    if (Symbol *sym = layout_scope_->localLookup(decl->name())) {
      cc_.report(decl->loc(), rmsg::redefined_layout_decl)
        << "field"
        << decl->name()
        << sym->kindName()
        << cc_.note(sym->node()->loc(), rmsg::previous_location);
      return;
    }

    Atom *name = decl->name();
    if (!name) {
      // Originally, SourcePawn had a concept called "funcenums" to work around
      // the lack of a true top type. They were untagged unions of function
      // types. Sourceawn 1.7 future-proofed this syntax the best it could by
      // introducing something we interpret as a "block union". A block union
      // simply has a list of types, none of them named.
      layout_scope_->addAnonymousField(decl);
      return;
    }

    FieldSymbol *sym =
      new (pool_) FieldSymbol(decl, layout_scope_, decl->name());
    decl->setSymbol(sym);
    layout_scope_->addSymbol(sym);
  }

  void visitPropertyDecl(PropertyDecl *decl) override {
    visitTypeIfNeeded(decl->spec());
    if (decl->getter()->isFunction())
      visitFunction(decl->getter()->fun());
    if (decl->setter()->isFunction())
      visitFunction(decl->setter()->fun());

    if (Symbol *sym = layout_scope_->localLookup(decl->name())) {
      cc_.report(decl->loc(), rmsg::redefined_layout_decl)
        << "property"
        << decl->name()
        << sym->kindName()
        << cc_.note(sym->node()->loc(), rmsg::previous_location);
      return;
    }

    PropertySymbol *sym =
      new (pool_) PropertySymbol(decl, layout_scope_, decl->name());
    decl->setSymbol(sym);
    layout_scope_->addSymbol(sym);
  }

  void visitMethodDecl(MethodDecl *decl) override {
    if (decl->method()->isFunction())
      visitFunction(decl->method()->fun());

    // Once we support overloading, this will have to change.
    if (Symbol *sym = layout_scope_->localLookup(decl->name())) {
      cc_.report(decl->loc(), rmsg::redefined_layout_decl)
        << "method"
        << decl->name()
        << sym->kindName()
        << cc_.note(sym->node()->loc(), rmsg::previous_location);
      return;
    }

    MethodSymbol *sym =
      new (pool_) MethodSymbol(decl, layout_scope_, decl->name());
    decl->setSymbol(sym);
    layout_scope_->addSymbol(sym);
  }

  void visitRecordDecl(RecordDecl *layout) override {
    TypeSymbol *sym = new (pool_) TypeSymbol(layout, getOrCreateScope(), layout->name());
    registerSymbol(sym);
    layout->setSymbol(sym);

    // Record-types cannot nest yet.
    assert(!layout_scope_);
    SaveAndSet<LayoutScope *> save(&layout_scope_, LayoutScope::New(pool_));

    // Record types cannot have methods yet, so there is no need to link this
    // scope into the scope chain.
    layout->setScope(layout_scope_);

    visitLayoutBody(layout->body());

    if (layout->token() == TOK_UNION) {
      if (layout_scope_->hasMixedAnonymousFields()) {
        cc_.report(layout->loc(), rmsg::union_cannot_mix_fields);
        return;
      }
    } else {
      // Parser should disallow this.
      assert(!layout_scope_->anonymous_fields());
    }
  }

  void visitMethodmapDecl(MethodmapDecl *methodmap) override {
    if (methodmap->parent())
      methodmap->parent()->accept(this);

    defineMethodmap(methodmap);

    // Methodmaps cannot be nested anywhere.
    assert(!layout_scope_);
    SaveAndSet<LayoutScope *> save(&layout_scope_, LayoutScope::New(pool_));

    // Note that we do not insert the layout scope into the scope chain. For
    // simplicity SP1 did not, and we can't break that.
    methodmap->setScope(layout_scope_);

    visitLayoutBody(methodmap->body());
  }

  void visitTypedefStatement(TypedefStatement *node) override {
    visitTypeIfNeeded(node->spec());

    TypeSymbol *sym = new (pool_) TypeSymbol(node, getOrCreateScope(), node->name());
    registerSymbol(sym);
    node->setSymbol(sym);
  }

  void visitVariableDeclaration(VariableDeclaration *first) override {
    for (VariableDeclaration *iter = first; iter; iter = iter->next()) {
      // Visit the type so we don't bind the type to the variable name.
      visitTypeIfNeeded(iter->spec());

      // Note: we look at the initializer BEFORE entering the symbol, so a
      // naive "new x = x" will not bind (unless there is an x in an outer
      // scope).
      if (iter->initialization())
        iter->initialization()->accept(this);

      VariableSymbol *sym = new (pool_) VariableSymbol(iter, getOrCreateScope(), iter->name());
      registerSymbol(sym);
      iter->setSymbol(sym);
    }
  }

  void visitEnumStatement(EnumStatement *node) override {
    Scope *scope = getOrCreateScope();

    // Note: we do not let enums override methodmap declarations. Once a
    // methodmap has been declared, if no enum had been seen, we cannot
    // add enum values after the fact. This is handled implicitly by
    // registerSymbol(), and the fact that methodmaps define an enum.
    if (node->name()) {
      TypeSymbol *sym = new (pool_) TypeSymbol(node, scope, node->name());
      registerSymbol(sym);
      node->setSymbol(sym);
    } else {
      // If the enum does not have a name, we give it an anonymous symbol.
      Atom *name = cc_.createAnonymousName(node->loc());
      node->setSymbol(new (pool_) TypeSymbol(node, scope, name));
    }

    for (size_t i = 0; i < node->entries()->length(); i++) {
      EnumConstant *cn = node->entries()->at(i);
      if (cn->expression())
        cn->expression()->accept(this);

      ConstantSymbol *cs = new (pool_) ConstantSymbol(cn, scope, cn->name());
      registerSymbol(cs);
      cn->setSymbol(cs);
    }
  }

  void visitEnumConstant(EnumConstant *node) override {
    // Unreachable.
    assert(false);
  }

  // General AST traversing of simple nodes.

  void visitAssignment(Assignment *node) override {
    node->lvalue()->accept(this);
    node->expression()->accept(this);
  }
  void visitBinaryExpression(BinaryExpression *node) override {
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitReturnStatement(ReturnStatement *node) override {
    if (node->expression()) {
      node->expression()->accept(this);
      fun_->returns(ValueReturn);
    } else {
      fun_->returns(VoidReturn);
    }
  }
  void visitDeleteStatement(DeleteStatement *node) override {
    node->expression()->accept(this);
  }
  void visitForStatement(ForStatement *node) override {
    SymbolEnv env(&env_, Scope::Block);

    if (node->initialization())
      node->initialization()->accept(this);
    if (node->condition())
      node->condition()->accept(this);
    if (node->update())
      node->update()->accept(this);
    node->body()->accept(this);

    node->setScope(env.scope());
  }
  void visitBlockStatement(BlockStatement *node) override {
    SymbolEnv env(&env_, Scope::Block);

    for (size_t i = 0; i < node->statements()->length(); i++)
      node->statements()->at(i)->accept(this);

    node->setScope(env.scope());
  }
  void visitExpressionStatement(ExpressionStatement *node) override {
    node->expression()->accept(this);
  }
  void visitCallExpression(CallExpression *node) override {
    node->callee()->accept(this);
    for (size_t i = 0; i < node->arguments()->length(); i++)
      node->arguments()->at(i)->accept(this);
  }
  void visitIfStatement(IfStatement *node) override {
    node->ifTrue()->accept(this);
    if (node->ifFalse())
      node->ifFalse()->accept(this);
  }
  void visitFieldExpression(FieldExpression *node) override {
    node->base()->accept(this);
  }
  void visitIndexExpression(IndexExpression *node) override {
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitWhileStatement(WhileStatement *node) override {
    node->condition()->accept(this);
    node->body()->accept(this);
  }
  void visitIncDecExpression(IncDecExpression *node) override {
    node->expression()->accept(this);
  }
  void visitUnaryExpression(UnaryExpression *node) override {
    node->expression()->accept(this);
  }
  void visitTernaryExpression(TernaryExpression *node) override {
    node->condition()->accept(this);
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitSwitchStatement(SwitchStatement *node) override {
    if (node->defaultCase())
      node->defaultCase()->accept(this);

    for (size_t i = 0; i < node->cases()->length(); i++) {
      node->cases()->at(i)->expression()->accept(this);
      node->cases()->at(i)->statement()->accept(this);
    }
  }
  void visitArrayLiteral(ArrayLiteral *node) override {
    for (size_t i = 0; i < node->expressions()->length(); i++)
      node->expressions()->at(i)->accept(this);
  }
  void visitStructInitializer(StructInitializer *node) override {
    for (size_t i = 0; i < node->pairs()->length(); i++)
      node->pairs()->at(i)->expr()->accept(this);
  }
  void visitSizeofExpression(SizeofExpression *node) override {
    node->proxy()->accept(this);
  }

  // No-op nodes.
  void visitIntegerLiteral(IntegerLiteral *node) override {
  }
  void visitTokenLiteral(TokenLiteral *node) override {
  }
  void visitFloatLiteral(FloatLiteral *node) override {
  }
  void visitBreakStatement(BreakStatement *node) override {
  }
  void visitContinueStatement(ContinueStatement *node) override {
  }
  void visitStringLiteral(StringLiteral *node) override {
  }
  void visitCharLiteral(CharLiteral *node) override {
  }
  void visitThisExpression(ThisExpression *node) override {
  }

 private:
  Scope *getOrCreateScope() {
    if (env_->scope())
      return env_->scope();

    switch (env_->kind()) {
      case Scope::Block:
        env_->setScope(BlockScope::New(pool_));
        break;
      case Scope::Function:
        env_->setScope(FunctionScope::New(pool_));
        break;
      default:
        assert(false);
    }

    return env_->scope();
  }

  Symbol *lookup(Atom *name) {
    for (SymbolEnv *env = env_; env != nullptr; env = env->prev()) {
      if (!env->scope())
        continue;
      if (Symbol *sym = env->scope()->localLookup(name))
        return sym;
    }
    return nullptr;
  }

  void declareSystemType(Scope *scope, const char *name, Type *type) {
    Atom *tag = cc_.add(name);

    TypeSymbol *sym = new (pool_) TypeSymbol(nullptr, scope, tag, type);
    scope->addSymbol(sym);
  }

  void declareSystemType(Scope *scope, const char *name, PrimitiveType prim) {
    declareSystemType(scope, name, cc_.types()->getPrimitive(prim));
  }

  void declareSystemTypes(Scope *scope) {
    declareSystemType(scope, "float", PrimitiveType::Float);
    declareSystemType(scope, "int", PrimitiveType::Int32);
    declareSystemType(scope, "bool", PrimitiveType::Bool);
    declareSystemType(scope, "char", PrimitiveType::Char);
    declareSystemType(scope, "void", cc_.types()->getVoid());

    // These are pseudo-deprecated, but we still have them for compatibility.
    declareSystemType(scope, "_", PrimitiveType::Int32);
    declareSystemType(scope, "any", cc_.types()->getUnchecked());
    declareSystemType(scope, "Function", cc_.types()->getMetaFunction());
  }

  void visitTypeIfNeeded(TypeSpecifier *spec) {
    if (spec->needsBinding())
      visitType(spec);
  }

  void visitType(TypeSpecifier *spec) {
    assert(spec->needsBinding());

    switch (spec->resolver()) {
      case TOK_LABEL:
        if (Type *type = typeForLabelAtom(spec->proxy()->name())) {
          // Just resolve this type ahead of time - it's a builtin with an old-
          // style label.
          spec->setResolved(type);
        } else {
          visitNameProxy(spec->proxy());
          if (!spec->proxy()->sym()) {
            // SourcePawn 1 compatibility: we don't go as far as keeping
            // separate type and variable/fun symbol tables, but we do lazily
            // create tags that don't exist if there are no other bindings
            // available.
            Atom *atom = spec->proxy()->name();
            AtomMap<NameProxy *>::Insert p = user_tags_.findForAdd(atom);
            if (!p.found())
              user_tags_.add(p, atom, spec->proxy());
          }
        }
        break;

      case TOK_NAME:
        visitNameProxy(spec->proxy());
        break;

      case TOK_FUNCTION:
      {
        FunctionSignature *sig = spec->signature();
        visitTypeIfNeeded(sig->returnType());

        // Visit the types of arguments and check for duplicate argument names.
        AtomSet seen;
        for (size_t i = 0; i < sig->parameters()->length(); i++) {
          VariableDeclaration *decl = sig->parameters()->at(i);
          visitTypeIfNeeded(decl->spec());

          AtomSet::Insert p = seen.findForAdd(decl->name());
          if (p.found()) {
            cc_.report(decl->loc(), rmsg::duplicate_argument)
              << decl->name();
            continue;
          }
          seen.add(p, decl->name());
        }
        break;
      }
    }

    if (spec->dims()) {
      for (size_t i = 0; i < spec->dims()->length(); i++) {
        Expression *expr = spec->dims()->at(i);
        if (expr)
          expr->accept(this);
      }
    }
  }

  Type *typeForLabelAtom(Atom *atom) {
    if (atom == atom_String_)
      return cc_.types()->getPrimitive(PrimitiveType::Char);
    if (atom == atom_Float_)
      return cc_.types()->getPrimitive(PrimitiveType::Float);
    if (atom == atom_any_)
      return cc_.types()->getUnchecked();
    if (atom == atom_Function_)
      return cc_.types()->getMetaFunction();
    if (atom == atom_bool_)
      return cc_.types()->getPrimitive(PrimitiveType::Bool);
    return nullptr;
  }

  bool registerGlobalFunction(FunctionSymbol *sym) {
    Scope *scope = getOrCreateScope();

    Symbol *other = scope->localLookup(sym->name());
    if (!other)
      return scope->addSymbol(sym);

    // If |other| is not a function, it's an error.
    FunctionSymbol *orig = other->asFunction();
    if (!orig) {
      reportRedeclaration(sym, other);
      return true;
    }

    // If both have bodies, it's an error.
    FunctionStatement *sym_node = sym->node()->toFunctionStatement();
    FunctionStatement *orig_node = orig->node()->toFunctionStatement();
    if (sym_node->body() && orig_node->body()) {
      reportRedeclaration(sym, other);
      return true;
    }

    // Build a shadow list, containing all symbols with this name.
    if (!orig->shadows()) {
      orig->setShadows(new (pool_) PoolList<Symbol *>());
      orig->shadows()->append(orig);
    }
    orig->shadows()->append(sym);
    sym_node->setShadowed(orig);
    return true;
  }

  bool registerSymbol(Symbol *sym) {
    Scope *scope = getOrCreateScope();

    if (Symbol *other = scope->localLookup(sym->name())) {
      // Report, but allow errors to continue.
      reportRedeclaration(sym, other);
      return true;
    }

    return scope->addSymbol(sym);
  }

  void reportRedeclaration(Symbol *sym, Symbol *other) {
    cc_.report(sym->node()->loc(), rmsg::redeclared_name)
      << sym->name()
      << (cc_.note(other->node()->loc(), rmsg::previous_location));
  }

  void registerArguments(FunctionSignature *sig) {
    Scope *scope = getOrCreateScope();
    for (size_t i = 0; i < sig->parameters()->length(); i++) {
      VariableDeclaration *var = sig->parameters()->at(i);
      visitTypeIfNeeded(var->spec());

      VariableSymbol *sym = new (pool_) VariableSymbol(var, scope, var->name());
      registerSymbol(sym);
      var->setSymbol(sym);
    }
  }

  void resolveUnknownTags() {
    for (AtomMap<NameProxy *>::iterator iter = user_tags_.iter(); !iter.empty(); iter.next()) {
      Atom *atom = iter->key;
      if (globals_->lookup(atom))
        continue;

      NameProxy *origin = iter->value;
      EnumType *type = cc_.types()->newEnum(atom);
      Symbol *sym = new (pool_) TypeSymbol(origin, globals_, atom, type);
      globals_->addSymbol(sym);
    }
  }

  void resolveUnboundNames() {
    // Resolve unresolved global names.
    AtomSet seen;
    for (size_t i = 0; i < unresolved_names_.length(); i++) {
      NameProxy *proxy = unresolved_names_[i];
      Symbol *sym = globals_->lookup(proxy->name());
      if (!sym) {
        AtomSet::Insert p = seen.findForAdd(proxy->name());
        if (p.found())
          continue;
        seen.add(p, proxy->name());

        cc_.report(proxy->loc(), rmsg::name_not_found)
          << proxy->name();
        continue;
      }

      proxy->bind(sym);
    }
  }

  void defineMethodmap(MethodmapDecl *methodmap) {
    // Methodmaps are only allowed in the global scope. They have very odd
    // semantics (by design, as part of the transitional syntax): they
    // create an enum, or they extend an existing enum, in any declaration
    // order.
    //
    // If the symbol already exists, it must be a direct enum type. We do
    // not accept typedefs.
    assert(getOrCreateScope() == globals_);

    Symbol *prev = globals_->lookup(methodmap->name());
    if (!prev) {
      TypeSymbol *sym = new (pool_) TypeSymbol(methodmap, globals_, methodmap->name());
      registerSymbol(sym);
      methodmap->setSymbol(sym);
      return;
    }

    TypeSymbol *sym = prev->asType();
    if (!sym) {
      cc_.report(methodmap->loc(), rmsg::methodmap_on_non_type)
        << sym->name();
      return;
    }

    // Builtin types do not have AST nodes.
    if (!sym->node()) {
      cc_.report(methodmap->loc(), rmsg::methodmap_on_non_enum)
        << sym->name();
      return;
    }

    EnumStatement *stmt = sym->node()->asEnumStatement();
    if (!stmt) {
      if (sym->node()->asMethodmapDecl()) {
        // We had something like:
        //   methodmap X {}
        //   methodmap X {}
        //
        // We can give a slightly more specific error for this case.
        cc_.report(methodmap->loc(), rmsg::methodmap_already_defined)
          << methodmap->name();
      } else {
        cc_.report(methodmap->loc(), rmsg::methodmap_on_non_enum)
          << sym->name();
      }
      return;
    }

    // Mark that our enum statement has a methodmap.
    stmt->setMethodmap(methodmap);

    // Point the layout at the enum type.
    methodmap->setSymbol(sym);
  }

 private:
  CompileContext &cc_;
  PoolAllocator &pool_;
  TranslationUnit *unit_;
  SymbolEnv *env_;
  FunctionLink *fun_;
  GlobalScope *globals_;

  LayoutScope *layout_scope_;
  
  Vector<NameProxy *> unresolved_names_;

  Atom *atom_String_;
  Atom *atom_Float_;
  Atom *atom_any_;
  Atom *atom_Function_;
  Atom *atom_bool_;

  AtomMap<NameProxy *> user_tags_;
};

bool
ke::ResolveNames(CompileContext &cc, TranslationUnit *unit)
{
  NameResolver populator(cc, unit);
  if (!populator.analyze())
    return false;

  return cc.phasePassed();
}

