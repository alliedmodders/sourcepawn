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
#include "ast.h"
#include "scopes.h"
#include "symbols.h"

using namespace ke;

Scope::Scope(PoolAllocator &pool, Kind kind, Scope *enclosing)
  : pool_(pool),
  kind_(kind),
  enclosing_(enclosing)
{
}

bool
Scope::initialize()
{
  if (enclosing_ && !enclosing_->children_.append(this))
    return false;
  return true;
}

void
Scope::unlink()
{
  assert(empty());

  assert(enclosing_->children_[enclosing_->children_.length() - 1] == this);
  enclosing_->children_.pop();

  // OOM here is swallowed and checked after compilation.
  // :TODO: make the above true.
  for (size_t i = 0; i < children_.length(); i++)
    enclosing_->children_.append(children_[i]);
}

Scope *
Scope::unlinkIfEmpty()
{
  if (empty()) {
    unlink();
    return nullptr;
  }
  return this;
}

Symbol *
Scope::localLookup(Atom * name)
{
  for (size_t i = 0; i < names_.length(); i++) {
    if (names_[i]->name() == name)
      return names_[i];
  }
  return nullptr;
}

Symbol  *
Scope::lookup(Atom * name)
{
  for (Scope *scope = this; scope; scope = scope->enclosing()) {
    if (Symbol *bound = scope->localLookup(name))
      return bound;
  }
  return nullptr;
}

#if 0
void
Scope::setUsesLifoHeap()
{
  // Only one lifo slot is needed per scope.
#if 0
  if (lifoSlot_)
    return;
#endif

  // If the parent of this scope is the function scope, then this is the
  // function's block scope, and the lifo slot is part of the Frame instead.
  // Note we can get a function scope here if we're making a temporary in a
  // function with no local variables.
  if (kind() == Function || enclosing()->kind() == Function)
    return;

  Local<Type> type(ZONE(), ZONE()->types()->getPrimitive(PrimitiveType_Native));
  if (!type)
    return;

  Local<String> name(ZONE());
#if 0
  lifoSlot_ = new (pool_) Variable(this, name, SourcePosition());
  lifoSlot_->setType(type);
#endif
}
#endif

bool
Scope::addSymbol(Symbol *sym)
{
  return names_.append(sym);
}

LocalScope::LocalScope(PoolAllocator &pool)
 : Scope(pool, Scope::Block, nullptr)
{
}

LocalScope *
LocalScope::New(PoolAllocator &pool)
{
  LocalScope *scope = new (pool) LocalScope(pool);
  if (!scope->initialize())
    return nullptr;

  return scope;
}

FunctionScope::FunctionScope(PoolAllocator &pool)
  : Scope(pool, Scope::Function, nullptr)
{
}

FunctionScope *
FunctionScope::New(PoolAllocator &pool)
{
  FunctionScope *scope = new (pool) FunctionScope(pool);
  if (!scope->initialize())
    return nullptr;

  return scope;
}

GlobalScope::GlobalScope(PoolAllocator &pool)
  : Scope(pool, Scope::Global, nullptr)
{
}

GlobalScope *
GlobalScope::New(PoolAllocator &pool)
{
  GlobalScope *scope = new (pool) GlobalScope(pool);
  if (!scope->initialize())
    return nullptr;

  return scope;
}

#if 0
Symbol *
GlobalScope::findExported(Atom * name)
{
  for (size_t i = 0; i < exported_.length(); i++) {
    if (exported_[i]->name() == name)
      return exported_[i];
  }
  return nullptr;
}
#endif
