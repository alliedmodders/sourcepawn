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
#include "name-binding.h"
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

    void purge() {
      scope_ = nullptr;
      children_.clear();
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

  // No-op nodes.
  void visitIntegerLiteral(IntegerLiteral *node) override {
  }
  void visitBooleanLiteral(BooleanLiteral *node) override {
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
        if (Type *type = typeForLabelAtom(spec->name()->name())) {
          // Just resolve this type ahead of time - it's a builtin with an old-
          // style label.
          spec->setResolved(type);
        } else {
          // Otherwise, add this to the list of labels that we might have to
          // create types for.
          AtomSet::Insert i = user_tags_.findForAdd(spec->name()->name());
          if (!i.found())
            user_tags_.add(i, spec->name()->name());
        }
        break;

      case TOK_NAME:
        visitNameProxy(spec->name());
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

  AtomSet user_tags_;
};

bool
ke::PopulateNamesAndTypes(CompileContext &cc, TranslationUnit *unit)
{
  NameResolver populator(cc, unit);
  if (!populator.analyze())
    return false;

  return cc.nerrors() == 0;
}

static const int INFER_ARRAY_SIZE = 0;
static const int EVAL_ARRAY_SIZE = -1;
static const int DYNAMIC_ARRAY_SIZE = -2;

class TypeResolver : public AstVisitor
{
 public:
  class AutoLinkScope
  {
   public:
    AutoLinkScope(AutoLinkScope **prevp, Scope *scope)
     : prevp_(prevp),
       prev_(*prevp),
       scope_(scope)
    {
      if (!scope_)
        return;

      *prevp_ = this;
      if (prev_)
        scope_->setParent(prev_->scope());
    }
    ~AutoLinkScope() {
      if (!scope_)
        return;
      assert(*prevp_ == this);
      *prevp_ = prev_;
    }
    Scope *scope() const {
      return scope_;
    }

   private:
    AutoLinkScope **prevp_;
    AutoLinkScope *prev_;
    Scope *scope_;
  };

 public:
  TypeResolver(CompileContext &cc, TranslationUnit *unit)
   : pool_(cc.pool()),
     cc_(cc),
     unit_(unit),
     link_(nullptr),
     fun_(nullptr)
  {
  }

  bool analyze() {
    AutoLinkScope globalScope(&link_, unit_->globalScope());

    for (size_t i = 0; i < unit_->tree()->statements()->length(); i++) {
      Statement *stmt = unit_->tree()->statements()->at(i);
      stmt->accept(this);
    }

    return true;
  }

  void visitVariableDeclaration(VariableDeclaration *node) {
#if 0
    // Bind the initializer before registering the declaration, so that we
    // can error on bogus initializers (new x = x).
    if (node->initialization())
      node->initialization()->accept(this);

    Type *type = bindType(node->type());
    if (!type)
      return;

    if (node->dims() || type->isArray()) {
      int dims[MAX_ARRAY_DEPTH];
      int levels = evaluateDimensions(node->loc(), type, node->dims(), dims);
      if (levels < 0)
        return;

      // Do some extra inference based on the initializer, if present.
      Expression *init = node->initialization();
      if (init &&
        init->isArrayLiteral() &&
        init->toArrayLiteral()->isFixed())
      {
        Type *contained = type->isArray()
                          ? type->toArray()
                          : type;
        if (!inferFixedArrayDimensions(init->toArrayLiteral(), contained, dims, levels))
          return;
      }

      // If we got extra dimensions, we need to build a new type.
      if (node->dims()) {
        if ((type = buildArrayType(type, dims, node->dims()->length())) == nullptr)
          return;
      }
    }

    // If the node already has a symbol (meaning it was a global), then
    // we don't have to do anything more.
    if (node->sym()) {
      assert(node->sym()->scope()->kind() == Scope::GLOBAL);
      node->sym()->setType(type);
      return;
    }

    VariableSymbol *sym = new (pool_) VariableSymbol(link_->scope(), node->name(), node->loc(), type);
    node->setSymbol(sym);
     
    if (Symbol *other = link_->scope()->localLookup(sym->name())) {
      // Report, but allow errors to continue.
      cc_.reportError(sym->loc(), Message_RedeclaredName,
              sym->name()->chars(),
              other->loc().pos.line,
              other->loc().pos.col);
      return;
    }
    
    if (!link_->scope()->addSymbol(sym))
      return;
#endif
  }

  void visitEnumStatement(EnumStatement *node) {
    int value = 0;
    for (size_t i = 0; i < node->entries()->length(); i++) {
      EnumStatement::Entry &entry = node->entries()->at(i);
      if (entry.expr) {
        BoxedPrimitive out;
        ConstantEvaluator ceval(cc_, link_->scope(), ConstantEvaluator::Required);
        switch (ceval.Evaluate(entry.expr, &out)) {
          case ConstantEvaluator::Ok:
            if (!out.isInt()) {
              cc_.reportError(entry.proxy->loc(), Message_EnumConstantMustBeInt);
              continue;
            }
            break;
          case ConstantEvaluator::NotConstant:
            cc_.reportError(entry.proxy->loc(), Message_EnumValueMustBeConstant);
            break;
          default:
            continue;
        }
        value = out.toInt();
      }

      entry.sym->setValue(BoxedPrimitive::Int(value));
      value++;
    }
  }

  void visitLayoutStatement(LayoutStatement *layout) {
    for (size_t i = 0; i < layout->body()->length(); i++) {
      LayoutEntry *entry = layout->body()->at(i);
      switch (entry->type()) {
        case LayoutEntry::Accessor:
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

  void visitFunction(FunctionNode *node) {
    AutoLinkScope funScope(&link_, node->funScope());
    AutoLinkScope varScope(&link_, node->varScope());

    resolveTypesInSignature(node->signature());

    SaveAndSet<FunctionNode *> save(&fun_, node);

    if (node->body())
      node->body()->accept(this);
  }

  void visitFunctionStatement(FunctionStatement *node) {
    visitFunction(node);
  }

  void visitTypedefStatement(TypedefStatement *tdef) {
    Type *type = resolveType(tdef->spec());
    if (!type) {
      // Even if we couldn't resolve, put a placeholder there so we can keep
      // trying to bind names.
      if (!tdef->sym()->type())
        tdef->sym()->setType(TypedefType::New(tdef->sym()->name()));
      return;
    }

    if (tdef->sym()->type()) {
      // We already resolved this type earlier. We have to make sure now that it
      // does not resolve cyclically.
      if (isCyclicType(type, tdef->sym()->type())) {
        cc_.reportError(tdef->loc(), Message_CannotResolveCyclicType);
        return;
      }
      tdef->sym()->type()->toTypedef()->resolve(type);
    }
    tdef->sym()->setType(type);
  }

  void visitAssignment(Assignment *node) {
    node->lvalue()->accept(this);
    node->expression()->accept(this);
  }
  void visitBinaryExpression(BinaryExpression *node) {
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitReturnStatement(ReturnStatement *node) {
    if (node->expression())
      node->expression()->accept(this);
  }
  void visitForStatement(ForStatement *node) {
    AutoLinkScope scope(&link_, node->scope());

    if (node->initialization())
      node->initialization()->accept(this);
    if (node->condition())
      node->condition()->accept(this);
    if (node->update())
      node->update()->accept(this);
    node->body()->accept(this);
  }
  void visitBlockStatement(BlockStatement *node) {
    AutoLinkScope scope(&link_, node->scope());
    for (size_t i = 0; i < node->statements()->length(); i++)
      node->statements()->at(i)->accept(this);
  }
  void visitIntegerLiteral(IntegerLiteral *node) {
  }
  void visitExpressionStatement(ExpressionStatement *node) {
    node->expression()->accept(this);
  }
  void visitCallExpression(CallExpression *node) {
    node->callee()->accept(this);
    for (size_t i = 0; i < node->arguments()->length(); i++)
      node->arguments()->at(i)->accept(this);
  }
  void visitIfStatement(IfStatement *node) {
    node->condition()->accept(this);
    node->ifTrue()->accept(this);
    if (node->ifFalse())
      node->ifFalse()->accept(this);
  }
  void visitIndexExpression(IndexExpression *node) {
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitWhileStatement(WhileStatement *node) {
    node->condition()->accept(this);
    node->body()->accept(this);
  }
  void visitIncDecExpression(IncDecExpression *node) {
    node->expression()->accept(this);
  }
  void visitUnaryExpression(UnaryExpression *node) {
    if (node->tag())
      node->tag()->accept(this);
    node->expression()->accept(this);
  }
  void visitTernaryExpression(TernaryExpression *node) {
    node->condition()->accept(this);
    node->left()->accept(this);
    node->right()->accept(this);
  }
  void visitBooleanLiteral(BooleanLiteral *node) {
  }
  void visitSwitchStatement(SwitchStatement *node) {
    node->expression()->accept(this);
    if (node->defaultCase())
      node->defaultCase()->accept(this);
    for (size_t i = 0; i < node->cases()->length(); i++) {
      // We don't test case expressions because they are literals.
      Case *c = node->cases()->at(i);
      c->statement()->accept(this);
    }
  }
  void visitArrayLiteral(ArrayLiteral *node) override {
    for (size_t i = 0; i < node->expressions()->length(); i++)
      node->expressions()->at(i)->accept(this);
  }

  // No-op cases.
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
  void visitNameProxy(NameProxy *node) override {
  }

 private:
  void resolveTypesInSignature(FunctionSignature *sig) {
    resolveType(sig->returnType());
    for (size_t i = 0; i < sig->parameters()->length(); i++) {
      VariableDeclaration *param = sig->parameters()->at(i);
      resolveType(param->spec());
    }
  }

  Type *resolveNameToSymbol(TypeSymbol *sym) {
    if (!sym->type() && sym->node()->asTypedefStatement()) {
      // Create a placeholder. This allows us to resolve dependent types that
      // have not yet been resolved. For example,
      //
      // typedef X function Y ();
      // typedef Y int
      //
      // Here, we'll create a Typedef for Y, which will be filled in later.
      sym->setType(TypedefType::New(sym->name()));
    }

    return sym->type();
  }

  Type *resolveNameToType(NameProxy *proxy) {
    // Resolve the name.
    visitNameProxy(proxy);
    if (!proxy->sym())
      return nullptr;

    TypeSymbol *sym = proxy->sym()->asType();
    if (!sym) {
      cc_.reportError(proxy->loc(), Message_IdentifierIsNotAType, proxy->sym()->name()->chars());
      return nullptr;
    }
    return resolveNameToSymbol(sym);
  }

  Type *resolveBaseType(TypeSpecifier *spec) {
    assert(!spec->resolved());

    switch (spec->resolver()) {
      case TOK_LABEL:
      case TOK_NAME:
        return resolveNameToType(spec->name());

      case TOK_VOID:
        return cc_.types()->getVoid();
      case TOK_IMPLICIT_INT:
      case TOK_INT:
        return cc_.types()->getPrimitive(PrimitiveType::Int32);
      case TOK_BOOL:
        return cc_.types()->getPrimitive(PrimitiveType::Bool);
      case TOK_CHAR:
        return cc_.types()->getPrimitive(PrimitiveType::Char);
      case TOK_FLOAT:
        return cc_.types()->getPrimitive(PrimitiveType::Float);

      case TOK_FUNCTION:
      {
        FunctionSignature *sig = spec->signature();
        resolveTypesInSignature(sig);

        return FunctionType::New(sig);
      }

      default:
        assert(false);
        return nullptr;
    }
  }

  // Returns true if it could be resolved to a constant integer; false
  // otherwise. |outp| is unmodified on failure.
  bool resolveConstantArraySize(Expression *expr, int *outp) {
    // We specify Required here, since we will not evaluate the expression
    // otherwise, and we need to report errors.
    BoxedPrimitive value;
    ConstantEvaluator ceval(cc_, link_->scope(), ConstantEvaluator::Required);
    switch (ceval.Evaluate(expr, &value)) {
      case ConstantEvaluator::Ok:
        break;
      case ConstantEvaluator::NotConstant:
        cc_.reportError(expr->loc(), Message_ArraySizeMustBeConstant);
        return false;
      default:
        return false;
    }

    if (!value.isInt()) {
      cc_.reportError(expr->loc(), Message_ArraySizeMustBeInteger);
      return false;
    }
    if (value.toInt() <= 0) {
      cc_.reportError(expr->loc(), Message_ArraySizeMustBePositive);
      return false;
    }

    *outp = value.toInt();
    return true;
  }

  Type *resolveArrayComponentTypes(TypeSpecifier *spec, Type *type) {
    if (type->isVoid())
      cc_.reportError(spec->arrayLoc(), Message_CannotCreateArrayOfVoid);

    size_t rank = spec->rank() - 1;
    do {
      int arraySize = ArrayType::kUnsized;
      Expression *expr = spec->sizeOfRank(rank);
      if (expr)
        resolveConstantArraySize(expr, &arraySize);
      type = cc_.types()->newArray(type, arraySize);
    } while (rank--);
    return type;
  }

  Type *resolveType(TypeSpecifier *spec) {
    if (spec->resolved())
      return spec->resolved();

    Type *baseType = resolveBaseType(spec);
    if (!baseType) {
      // Create a placeholder so we don't have to check null everywhere.
      baseType = TypedefType::New(cc_.add("__unresolved_type__"));
    }

    // Should not have a reference here.
    assert(!baseType->isReference());

    Type *type = baseType;
    if (spec->rank())
      type = resolveArrayComponentTypes(spec, type);

    if (spec->isByRef()) {
      // We refuse to allow by-ref arrays; arrays are always by-ref. We check
      // this once here, and once again parsing parameters in case we had
      // placeholders.
      //
      // Note: We also forbid this for objects (is ref basically deprecated?
      // I guess so).
      if (type->canUseInReferenceType())
        type = cc_.types()->newReference(type);
      else
        cc_.reportError(spec->byRefLoc(), Message_TypeCannotBeReference, GetTypeClassName(type));
    }

    if (spec->isConst())
      type = cc_.types()->newQualified(type, Qualifiers::Const);

    // If we've wrapped the type, and it was a placeholder, we have to make
    // sure later that the transformation we just made is still valid.
    if (baseType->isUnresolvedTypedef() && baseType != type)
      placeholder_checks_.append(LazyPlaceholderCheck(spec, baseType->toTypedef()));

    return type;
  }

  // If traversing the type nesting of |check| reveals a pointer to |first|,
  // we consider the type cyclic and unresolvable.
  bool isCyclicType(Type *check, Type *first) {
    if (check == first || check->canonical() == first)
      return true;

    if (check->isArray())
      return isCyclicType(check->toArray()->contained(), first);
    if (check->isResolvedTypedef())
      return isCyclicType(check->toTypedef()->actual(), first);
    if (check->isReference())
      return isCyclicType(check->toReference()->contained(), first);
    if (check->isFunction()) {
      FunctionSignature *signature = check->toFunction()->signature();
      if (isCyclicType(signature->returnType()->resolved(), first))
        return true;
      for (size_t i = 0; i < signature->parameters()->length(); i++) {
        VariableSymbol *sym = signature->parameters()->at(i)->sym();
        if (isCyclicType(sym->type(), first))
          return true;
      }
    }

    return false;
  }

#if 0
  int evaluateDimensions(const SourceLocation &loc, Type *typeArg, ExpressionList *exprs, int dims[MAX_ARRAY_DEPTH]) {
    unsigned level = 0;
    Type *type = typeArg;
    while (type->isArray()) {
      ArrayType *atype = type->toArray();
      dims[level] = atype->isFixedLength() ? atype->fixedLength() : DYNAMIC_ARRAY_SIZE;
      type = atype->contained();
      level++;
    }

    if (!exprs)
      return level;

    if (level + exprs->length() > MAX_ARRAY_DEPTH) {
      cc_.reportError(loc, Message_MaximumArrayDepth);
      return -1;
    }

    bool hadEmptySize = false;
    for (size_t i = 0; i < exprs->length(); i++) {
      Expression *expr = exprs->at(i);
      if (!expr) {
        dims[i] = INFER_ARRAY_SIZE;
        hadEmptySize = true;
        continue;
      }

      BoxedPrimitive box;
      ConstantEvaluator ceval(cc_, link_->scope(), ConstantEvaluator::Speculative);
      if (ceval.Evaluate(expr, &box) == ConstantEvaluator::Ok && box.type() == PrimitiveType::Int32) {
        int size = box.toInt();
        if (size <= 0) {
          cc_.reportError(expr->loc(), Message_BadArraySize);
          return false;
        }

        dims[i] = size;
        continue;
      }

      if (hadEmptySize) {
        cc_.reportError(expr->loc(), Message_BadDynamicInitializer);
        return false;
      }

      dims[i] = EVAL_ARRAY_SIZE;
    }

    return level + exprs->length();
  }

  // This function computes missing dimension sizes based on a literal
  // initializer. We don't really handle any size rules here; we just
  // do a really dumb inference.
  bool inferFixedArrayDimensions(ArrayLiteral *lit, Type *contained, int dims[MAX_ARRAY_DEPTH], int levels) {
    for (int i = 0; i < levels; i++) {
      if (dims[i] == EVAL_ARRAY_SIZE || dims[i] == DYNAMIC_ARRAY_SIZE) {
        // The following are invalid:
        //   new x[<expr>] = {...}
        //   new Type:x = {...}  // where Type is a non-fixed array type
        cc_.reportError(lit->loc(), Message_CannotInitializeEvalArray);
        return false;
      }

      if (dims[i] == INFER_ARRAY_SIZE) {
        // Character arrays are always dynamic when inferred from string literals.
        if (i == levels - 1 && contained->isChar())
          dims[i] = DYNAMIC_ARRAY_SIZE;
        else
          dims[i] = lit->expressions()->length();
      }

      if (i != levels - 1) {
        // Fixed array literals are guaranteed to have at least one expression.
        Expression *next = lit->expressions()->at(0);
        if (!next->isArrayLiteral()) {
          // Whelp... this array is just malformed. Not the best error message
          // but whatever.
          cc_.reportError(next->loc(), Message_ArraySizeCannotBeDetermined);
          return false;
        }
        lit = next->toArrayLiteral();
      }
    }

    return true;
  }

  ArrayType *buildArrayType(Type *contained, int dims[MAX_ARRAY_DEPTH], size_t postlevels) {
    Type *type = contained;
    for (size_t i = postlevels - 1; i < postlevels; i--) {
      int size = dims[i] > 0 ? dims[i] : ArrayType::DYNAMIC_ARRAY_SIZE;
      if ((type = cc_.types()->newArray(contained, size)) == nullptr)
        return nullptr;
    }
    return type->toArray();
  }
#endif

  bool fillFunctionType(FunctionType *fun, const FunctionSignature &sig) {
#if 0
    Type *type = nullptr;
    if (!fun->returnType()) {
      if ((type = bindType(sig.returnType())) == nullptr)
        return false;
      fun->setReturnType(type);
    }
#endif

#if 0
    // :TODO: some kind of length check...
    TypeList *params = new (pool_) TypeList(sig.parameters()->length());
    for (size_t i = 0; i < sig.parameters()->length(); i++) {
      Parameter *param = sig.parameters()->at(i);
      if ((type = bindType(param->type())) == nullptr)
        return false;
#endif

#if 0
      if (type->isArray() || param->dims()) {
        int dims[MAX_ARRAY_DEPTH];
        int levels = evaluateDimensions(param->loc(), type, param->dims(), dims);
        if (levels < 0)
          return false;

        // If we got extra dimensions, we need to build a new type.
        // Unlike variables, we do not infer anything about array
        // sizes from its default initializer.
        if (param->dims()) {
          if ((type = buildArrayType(type, dims, param->dims()->length())) == nullptr)
            return false;
        }
        assert(!param->reference());
      } else {
        if (param->reference()) {
          if ((type = cc_.types()->newReference(type)) == nullptr)
            return false;
        }
      }

      params->set(i, type);
      param->sym()->setType(type);
    }

    fun->setParameterTypes(params);
#endif
    return true;
  }

 private:
  PoolAllocator &pool_;
  CompileContext &cc_;
  TranslationUnit *unit_;
  AutoLinkScope *link_;
  FunctionNode *fun_;

  struct LazyPlaceholderCheck {
    LazyPlaceholderCheck(TypeSpecifier *spec, TypedefType *placeholder)
      : spec(spec),
        placeholder(placeholder)
    {}
    TypeSpecifier *spec;
    TypedefType *placeholder;
  };
  Vector<LazyPlaceholderCheck> placeholder_checks_;
};

bool
ke::BindNamesAndTypes(CompileContext &cc, TranslationUnit *unit)
{
  TypeResolver binder(cc, unit);
  if (!binder.analyze())
    return false;

  return cc.nerrors() == 0;
}
