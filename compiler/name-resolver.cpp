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
#include "name-resolver.h"

using namespace ke;
using namespace sp;

NameResolver::NameResolver(CompileContext &cc)
 : cc_(cc),
   pool_(cc.pool()),
   layout_scope_(nullptr)
{
  atom_String_ = cc_.add("String");
  atom_Float_ = cc_.add("Float");
  atom_any_ = cc_.add("any");
  atom_Function_ = cc_.add("Function");
  atom_bool_ = cc_.add("bool");
}

void
NameResolver::OnEnterParser()
{
  globals_ = GlobalScope::New(pool_);

  OnEnterScope(Scope::Global);
  env_[0].setScope(globals_);

  declareSystemTypes(globals_);
}

void
NameResolver::OnLeaveParser()
{
  OnLeaveScope();
  assert(env_.empty());

  if (!cc_.phasePassed())
    return;

  resolveUnknownTags();
  resolveUnboundNames();
}

void
NameResolver::declareSystemTypes(Scope *scope)
{
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

void
NameResolver::declareSystemType(Scope *scope, const char *name, PrimitiveType prim)
{
  declareSystemType(scope, name, cc_.types()->getPrimitive(prim));
}

void
NameResolver::declareSystemType(Scope *scope, const char *name, Type *type)
{
  Atom *tag = cc_.add(name);

  TypeSymbol *sym = new (pool_) TypeSymbol(nullptr, scope, tag, type);
  scope->addSymbol(sym);
}

void
NameResolver::OnEnterScope(Scope::Kind kind)
{
  env_.append(SymbolEnv(kind));
}

Scope *
NameResolver::OnLeaveScope()
{
  SymbolEnv &env = env_.back();
  SymbolEnv *prev = env_.length() >= 2
                    ? &env_[env_.length() - 2]
                    : nullptr;
  Scope *scope = env.scope();
  if (scope) {
    // Fix up children.
    for (size_t i = 0; i < env.children().length(); i++)
      env.children()[i]->setParent(scope);
        
    // Add us to our parent scope.
    if (prev)
      prev->addChild(scope);
  } else {
    // We didn't have a scope. Transfer any children to our parent. Note that
    // we must have a parent, since we stop at the global scope.
    prev->children().extend(ke::Move(env.children()));
  }
  env_.pop();
  return scope;
}

void
NameResolver::OnLeaveOrphanScope()
{
  // Don't connect the current up to anything.
  env_.pop();
}

Scope *
NameResolver::getOrCreateScope()
{
  SymbolEnv &env = env_.back();
  if (env.scope())
    return env.scope();

  switch (env.kind()) {
    case Scope::Block:
      env.setScope(BlockScope::New(pool_));
      break;
    case Scope::Function:
      env.setScope(FunctionScope::New(pool_));
      break;
    default:
      assert(false);
  }

  return env.scope();
}

Symbol *
NameResolver::lookup(Atom *name)
{
  for (size_t i = env_.length(); i > 0; i--) {
    SymbolEnv &env = env_[i - 1];
    if (!env.scope())
      continue;
    if (Symbol *sym = env.scope()->localLookup(name))
      return sym;
  }
  return nullptr;
}

void
NameResolver::reportRedeclaration(Symbol *sym, Symbol *other)
{
  if (other->node()) {
    cc_.report(sym->node()->loc(), rmsg::redeclared_name)
      << sym->name()
      << (cc_.note(other->node()->loc(), rmsg::previous_location));
  } else {
    cc_.report(sym->node()->loc(), rmsg::redeclared_builtin)
      << sym->name();
  }
}

bool
NameResolver::registerSymbol(Symbol *sym)
{
  Scope *scope = getOrCreateScope();

  if (Symbol *other = scope->localLookup(sym->name())) {
    // Report, but allow errors to continue.
    reportRedeclaration(sym, other);
    return true;
  }

  return scope->addSymbol(sym);
}

void
NameResolver::resolveUnknownTags()
{
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

void
NameResolver::resolveUnboundNames()
{
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

void
NameResolver::OnNameProxy(NameProxy *proxy)
{
  if (Symbol *sym = lookup(proxy->name())) {
    proxy->bind(sym);
  } else {
    // Place this symbol in the unresolved list, in case it binds to a
    // global we haven't seen yet.
    unresolved_names_.append(proxy);
  }
}

void
NameResolver::OnTagProxy(NameProxy *proxy)
{
  if (Symbol *sym = lookup(proxy->name())) {
    proxy->bind(sym);
    return;
  }

  // SourcePawn 1 compatibility: we don't go as far as keeping separate type
  // and variable/fun symbol tables, but we do lazily create tags that don't
  // exist if there are no other bindings available.
  AtomMap<NameProxy *>::Insert p = user_tags_.findForAdd(proxy->name());
  if (!p.found())
    user_tags_.add(p, proxy->name(), proxy);
  unresolved_names_.append(proxy);
}

void
NameResolver::OnEnumDecl(EnumStatement *node)
{
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
}

void
NameResolver::OnEnumValueDecl(EnumConstant *cs)
{
  ConstantSymbol *sym = new (pool_) ConstantSymbol(cs, getOrCreateScope(), cs->name());
  registerSymbol(sym);
  cs->setSymbol(sym);
}

void
NameResolver::OnVarDecl(VariableDeclaration *var)
{
  // Note: the parser has already bound |var->init()| at this point, meaning
  // that aside from globals it should be impossible to self-initialize like:
  //    int x = x;
  //
  // :TODO: do check this for globals.
  VariableSymbol *sym =
    new (pool_) VariableSymbol(var, getOrCreateScope(), var->name());
  registerSymbol(sym);
  var->setSymbol(sym);
}

void
NameResolver::OnEnterMethodmap(MethodmapDecl *methodmap)
{
  // Methodmaps cannot be nested anywhere.
  assert(!layout_scope_);
  layout_scope_ = LayoutScope::New(pool_);

  defineMethodmap(methodmap);

  // Note that we do not insert the layout scope into the scope chain. For
  // simplicity SP1 did not, and we can't break that.
  methodmap->setScope(layout_scope_);
}

void
NameResolver::OnLeaveMethodmap(MethodmapDecl *methodmap)
{
  layout_scope_ = nullptr;
}

void
NameResolver::defineMethodmap(MethodmapDecl *methodmap)
{
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

void
NameResolver::OnEnterRecordDecl(RecordDecl *decl)
{
  TypeSymbol *sym = new (pool_) TypeSymbol(decl, getOrCreateScope(), decl->name());
  registerSymbol(sym);
  decl->setSymbol(sym);

  // Record-types cannot nest yet.
  assert(!layout_scope_);
  layout_scope_ = LayoutScope::New(pool_);

  // Record types cannot have methods yet, so there is no need to link this
  // scope into the scope chain.
  decl->setScope(layout_scope_);
}

void
NameResolver::OnLeaveRecordDecl(RecordDecl *decl)
{
  if (decl->token() == TOK_UNION) {
    if (layout_scope_->hasMixedAnonymousFields()) {
      cc_.report(decl->loc(), rmsg::union_cannot_mix_fields);
      return;
    }
  } else {
    // Parser should disallow this.
    assert(!layout_scope_->anonymous_fields());
  }

  layout_scope_ = nullptr;
}

void
NameResolver::OnPropertyDecl(PropertyDecl *decl)
{
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

void
NameResolver::OnFieldDecl(FieldDecl *decl)
{
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

  if (Symbol *sym = layout_scope_->localLookup(name)) {
    cc_.report(decl->loc(), rmsg::redefined_layout_decl)
      << "field"
      << name
      << sym->kindName()
      << cc_.note(sym->node()->loc(), rmsg::previous_location);
    return;
  }

  FieldSymbol *sym = new (pool_) FieldSymbol(decl, layout_scope_, name);
  decl->setSymbol(sym);
  layout_scope_->addSymbol(sym);
}

void
NameResolver::OnMethodDecl(MethodDecl *decl)
{
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

void
NameResolver::registerFunction(FunctionSymbol *sym)
{
  Scope *scope = sym->scope();
  assert(scope == globals_);

  Symbol *other = scope->localLookup(sym->name());
  if (!other) {
    scope->addSymbol(sym);
    return;
  }

  // If |other| is not a function, it's an error.
  FunctionSymbol *orig = other->asFunction();
  if (!orig) {
    reportRedeclaration(sym, other);
    return;
  }

  // If both have bodies, it's an error.
  FunctionStatement *sym_node = sym->node()->toFunctionStatement();
  FunctionStatement *orig_node = orig->node()->toFunctionStatement();
  if (sym_node->body() && orig_node->body()) {
    reportRedeclaration(sym, other);
    return;
  }

  // Build a shadow list, containing all symbols with this name.
  if (!orig->shadows()) {
    orig->setShadows(new (pool_) PoolList<Symbol *>());
    orig->shadows()->append(orig);
  }
  orig->shadows()->append(sym);
  sym_node->setShadowed(orig);
}

void
NameResolver::OnEnterFunctionDecl(FunctionStatement *node)
{
  // Currently, we only allow these at global scope.
  Scope *scope = getOrCreateScope();
  assert(scope == globals_);

  FunctionSymbol *sym =
    new (pool_) FunctionSymbol(node, scope, node->name());
  registerFunction(sym);
  node->setSymbol(sym);

  encountered_return_value_ = false;
}

void
NameResolver::OnLeaveFunctionDecl(FunctionStatement *node)
{
  FunctionSignature *sig = node->signature();

  // Function statements have quirky semantics around their return type. For
  // reasonable compatibility with SP1, we use the following heuristics for
  // when no explicit type is declared.
  TypeExpr &rt = sig->returnType();
  if ((rt.resolved() && rt.resolved()->isImplicitInt()) ||
      (rt.spec() && rt.spec()->resolver() == TOK_IMPLICIT_INT))
  {
    bool cell_required = (node->token() == TOK_FORWARD || node->token() == TOK_NATIVE);
    if (cell_required || encountered_return_value_)
      rt.setResolved(cc_.types()->getPrimitive(PrimitiveType::Int32));
    else
      rt.setResolved(cc_.types()->getVoid());
  }
}

void
NameResolver::OnReturnStmt(ReturnStatement *stmt)
{
  if (stmt->expression())
    encountered_return_value_ = true;
}

void
NameResolver::OnTypedefDecl(TypedefStatement *node)
{
  TypeSymbol *sym = new (pool_) TypeSymbol(node, getOrCreateScope(), node->name());
  registerSymbol(sym);
  node->setSymbol(sym);
}

TypeExpr
NameResolver::HandleTypeExpr(const TypeSpecifier &spec)
{
  return TypeExpr(new (pool_) TypeSpecifier(spec));
}
