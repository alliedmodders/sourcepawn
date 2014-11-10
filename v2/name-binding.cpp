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
     fun_(nullptr)
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
    if (!declareSystemTypes(globals_))
      return false;

    unit_->setGlobalScope(globals_);

    SymbolEnv env(&env_, globals_);

    for (size_t i = 0; i < unit_->tree()->statements()->length(); i++) {
      Statement *stmt = unit_->tree()->statements()->at(i);
      stmt->accept(this);
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

  void visitLayoutStatement(LayoutStatement *layout) override {
    Type *type = nullptr;
    switch (layout->spec()) {
      case TOK_UNION:
        type = cc_.types()->newUnion(layout->name());
        break;

      case TOK_STRUCT:
        type = cc_.types()->newStruct(layout->name());
        break;

      case TOK_METHODMAP:
      {
        // Methodmaps are only allowed in the global scope. They have very odd
        // semantics (by design, as part of the transitional syntax): they
        // create an enum, or they extend an existing enum, in any declaration
        // order.
        //
        // If the symbol already exists, it must be a direct enum type. We do
        // not accept typedefs.
        if (Symbol *unk_sym = getOrCreateScope()->lookup(layout->name())) {
          TypeSymbol *sym = unk_sym->asType();
          if (!sym) {
            cc_.reportError(layout->loc(), Message_MethodmapOnNonType, unk_sym->name()->chars());
            break;
          }

          EnumType *enum_type = sym->type()->asEnum();
          if (!enum_type) {
            cc_.reportError(layout->loc(), Message_MethodmapOnNonEnum, GetTypeName(sym->type()));
            break;
          }
          if (enum_type->hasMethodmap()) {
            cc_.reportError(layout->loc(), Message_MethodmapAlreadyDefined, GetTypeName(sym->type()));
            break;
          }

          // We'll actually construct the methodmap later. We don't set |type|,
          // since the symbol is already declared.
          enum_type->setHasMethodmap();
        } else {
          // Create a placeholder enum type. Note that it was created for a
          // methodmap, so we don't error out re-declaring it in the enum.
          type = cc_.types()->newEnum(layout->name());
          type->toEnum()->setCreatedForMethodmap();
        }
        break;
      }

      default:
        assert(false);
    }

    if (type) {
      TypeSymbol *sym = new (pool_) TypeSymbol(layout, getOrCreateScope(), layout->name(), type);
      registerSymbol(sym);
      layout->setSymbol(sym);
    }

    // Traverse the layout body.
    for (size_t i = 0; i < layout->body()->length(); i++) {
      LayoutEntry *entry = layout->body()->at(i);
      switch (entry->type()) {
        case LayoutEntry::Field:
          visitTypeIfNeeded(entry->spec());
          break;

        case LayoutEntry::Accessor:
          visitTypeIfNeeded(entry->spec());
          if (entry->getter().isFunction())
            visitFunction(entry->getter().fun());
          if (entry->setter().isFunction())
            visitFunction(entry->setter().fun());
          break;

        case LayoutEntry::Method:
          if (entry->method().isFunction())
            visitFunction(entry->method().fun());
          break;
      }
    }
  }

  void visitTypedefStatement(TypedefStatement *node) override {
    visitTypeIfNeeded(node->spec());

    // Create a placeholder type; we can't fill it in until later.
    TypedefType *type = cc_.types()->newTypedef(node->name());

    TypeSymbol *sym = new (pool_) TypeSymbol(node, getOrCreateScope(), node->name(), type);
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

    Type *type;
    if (node->name()) {
      // Note: we do not let enums override methodmap declarations. Once a
      // methodmap has been declared, if no enum had been seen, we cannot
      // add enum values after the fact.
      //
      // This would not be hard to implement, but it is semantically weird,
      // and methodmaps and tags are semantically weird enough as it is.
      type = EnumType::New(node->name());

      TypeSymbol *sym = new (pool_) TypeSymbol(node, scope, node->name(), type);
      registerSymbol(sym);
      node->setSymbol(sym);
    } else {
      // We don't give these an anonyous type - it's basically an int list at
      // this point.
      type = cc_.types()->getPrimitive(PrimitiveType::Int32);
    }

    for (size_t i = 0; i < node->entries()->length(); i++) {
      EnumStatement::Entry &entry = node->entries()->at(i);
      if (entry.expr)
        entry.expr->accept(this);

      ConstantSymbol *cs = new (pool_) ConstantSymbol(entry.proxy, scope, entry.proxy->name(), type);
      registerSymbol(cs);
      entry.sym = cs;
    }
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
        env_->setScope(LocalScope::New(pool_));
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

  bool declareSystemType(Scope *scope, const char *name, Type *type) {
    Atom *tag = cc_.add(name);
    if (!tag)
      return false;

    TypeSymbol *sym = new (pool_) TypeSymbol(nullptr, scope, tag, type);
    return scope->addSymbol(sym);
  }

  bool declareSystemType(Scope *scope, const char *name, PrimitiveType prim) {
    return declareSystemType(scope, name, cc_.types()->getPrimitive(prim));
  }

  bool declareSystemTypes(Scope *scope) {
    if (!declareSystemType(scope, "float", PrimitiveType::Float))
      return false;
    if (!declareSystemType(scope, "int", PrimitiveType::Int32))
      return false;
    if (!declareSystemType(scope, "bool", PrimitiveType::Bool))
      return false;
    if (!declareSystemType(scope, "char", PrimitiveType::Char))
      return false;
    if (!declareSystemType(scope, "void", cc_.types()->getVoid()))
      return false;

    // These are pseudo-deprecated, but we still have them for compatibility.
    if (!declareSystemType(scope, "_", PrimitiveType::Int32))
      return false;
    if (!declareSystemType(scope, "any", cc_.types()->getUnchecked()))
      return false;
    if (!declareSystemType(scope, "Function", cc_.types()->getMetaFunction()))
      return false;

    return true;
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
            cc_.reportError(decl->loc(), Message_ArgumentNameDeclaredTwice, decl->name()->chars());
            continue;
          }
          seen.add(p, decl->name());
        }
        break;
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
    if (other->node()->loc().file != sym->node()->loc().file) {
      // :TODO: shorten paths.
      cc_.reportError(sym->node()->loc(), Message_RedeclaredNameWithFile,
        sym->name()->chars(),
        other->node()->loc().file->path(),
        other->node()->loc().line,
        other->node()->loc().col);
    } else {
      cc_.reportError(sym->node()->loc(), Message_RedeclaredName,
        sym->name()->chars(),
        other->node()->loc().line,
        other->node()->loc().col);
    }
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

        cc_.reportError(proxy->loc(), Message_IdentifierNotFound, proxy->name()->chars());
        continue;
      }

      proxy->bind(sym);
    }
  }

 private:
  CompileContext &cc_;
  PoolAllocator &pool_;
  TranslationUnit *unit_;
  SymbolEnv *env_;
  FunctionLink *fun_;
  GlobalScope *globals_;
  
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

  return cc.nerrors() == 0;
}

