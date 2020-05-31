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
#include "parser/ast.h"
#include "scopes.h"
#include "symbols.h"

using namespace ke;
using namespace sp;

Scope::Scope(PoolAllocator& pool, Scope* enclosing)
 : pool_(pool),
   enclosing_(enclosing)
{
}

Symbol*
Scope::localLookup(Atom * name)
{
  for (size_t i = 0; i < names_.size(); i++) {
    if (names_[i]->name() == name)
      return names_[i];
  }
  return nullptr;
}

Symbol*
Scope::lookup(Atom* name)
{
  for (Scope* scope = this; scope; scope = scope->enclosing()) {
    if (Symbol* bound = scope->localLookup(name))
      return bound;
  }
  return nullptr;
}

void
Scope::addSymbol(Symbol* sym)
{
  names_.push_back(sym);
}

BlockScope::BlockScope(PoolAllocator& pool)
 : Scope(pool, nullptr)
{
}

BlockScope*
BlockScope::New(PoolAllocator& pool)
{
  return new (pool) BlockScope(pool);
}

ArgumentScope::ArgumentScope(PoolAllocator& pool)
  : Scope(pool, nullptr)
{
}

ArgumentScope*
ArgumentScope::New(PoolAllocator& pool)
{
  return new (pool) ArgumentScope(pool);
}

GlobalScope::GlobalScope(PoolAllocator& pool)
 : Scope(pool, nullptr)
{
}

GlobalScope*
GlobalScope::New(PoolAllocator& pool)
{
  return new (pool) GlobalScope(pool);
}

LayoutScope::LayoutScope(PoolAllocator& pool)
 : Scope(pool, nullptr),
   anonymous_fields_(nullptr)
{
}

LayoutScope*
LayoutScope::New(PoolAllocator& pool)
{
  return new (pool) LayoutScope(pool);
}

void
LayoutScope::addAnonymousField(ast::FieldDecl* decl)
{
  if (!anonymous_fields_)
    anonymous_fields_ = new (POOL()) PoolList<ast::FieldDecl*>();
  anonymous_fields_->push_back(decl);
}

bool
LayoutScope::hasMixedAnonymousFields() const
{
  if (!anonymous_fields_)
    return false;
  return names_.size() > 0;
}
