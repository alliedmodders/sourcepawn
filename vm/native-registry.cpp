// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "native-registry.h"
#include "plugin-runtime.h"
#include "environment.h"
#include "code-stubs.h"

using namespace sp;
using namespace ke;
using namespace SourcePawn;

NativeRegistry::NativeRegistry(Environment* env, const char* name)
 : env_(env),
   name_(name),
   binding_generation_(0)
{
  table_.init(128);
}

PassRef<INativeGroup>
NativeRegistry::NewNativeGroup(const char* name)
{
  return new NativeGroup(this, name);
}

void
NativeRegistry::AddNativeVector(INativeGroup* aGroup, const NativeDef* defs, size_t count)
{
  NativeGroup* group = aGroup ? aGroup->ToNativeGroup() : nullptr;
  assert(!group || group->registry() == this);

  for (size_t i = 0; i < count; i++)
    addNative(group, &defs[i]);

  if (group)
    group->addNatives(NativeGroup::NativeVector(defs, count));
}

void
NativeRegistry::AddNativeList(INativeGroup* aGroup, const NativeDef* defs)
{
  NativeGroup* group = aGroup ? aGroup->ToNativeGroup() : nullptr;
  assert(!group || group->registry() == this);

  size_t count = 0;
  for (const NativeDef* iter = defs; iter->name; iter++, count++)
    addNative(group, iter);

  if (group)
    group->addNatives(NativeGroup::NativeVector(defs, count));
}

void
NativeRegistry::AddLegacyNatives(INativeGroup* group, const sp_nativeinfo_t* defs)
{
  size_t count = 0;
  for (const sp_nativeinfo_t* iter = defs; iter->name; iter++)
    count++;

  NativeDef *newlist = new NativeDef[count];
  for (size_t i = 0; i < count; i++) {
    newlist[i].name = defs[i].name;
    newlist[i].method = (void *)defs[i].func;
    newlist[i].sig = nullptr;
  }

  AddNativeVector(group, newlist, count);

  if (group)
    group->ToNativeGroup()->addHeldPointer(newlist);
  else
    held_pointers_.append(newlist);
}

void
NativeRegistry::addNative(NativeGroup* group, const NativeDef* def)
{
  NativeTable::Insert i = table_.findForAdd(def->name);
  if (i.found()) {
    assert(false);
    return;
  }
  if (!table_.add(i))
    return;

  i->def = def;
  i->group = group;
}

bool
NativeRegistry::AddRoutedNative(INativeGroup* aGroup,
                                const char* name,
                                SPVM_FAKENATIVE_FUNC callback,
                                void* data)
{
  NativeGroup* group = aGroup ? aGroup->ToNativeGroup() : nullptr;

  AutoPtr<RoutedNative> rn(new RoutedNative);
  rn->chunk = env_->stubs()->CreateFakeNativeStub(callback, data);
  if (!rn->chunk.address())
    return false;

  rn->name = new char[strlen(name)+1];
  strcpy(rn->name, name);

  rn->def = new NativeDef;
  rn->def->method = rn->chunk.address();
  rn->def->name = rn->name;
  rn->def->sig = nullptr;

  CharsAndLength key(rn->name);
  NativeTable::Insert i = table_.findForAdd(key);
  if (i.found())
    return false;

  if (!table_.add(i))
    return false;

  i->def = rn->def;
  i->group = group;
  i->rn = rn.take();

  if (group)
    group->addNatives(NativeGroup::NativeVector(i->def, 1));
  return true;
}

bool
NativeRegistry::Bind(PluginRuntime* rt)
{
  // This generation number is valid only for this runtime.
  binding_generation_++;

  bool all_bound = true;
  for (size_t i = 0; i < rt->GetNativesNum(); i++) {
    NativeEntry* entry = rt->NativeAt(i);
    if (entry->binding)
      continue;

    if (!bind(rt, entry)) {
      if (!(entry->flags & SP_NTVFLAG_DYNAMIC))
        all_bound = false;
    }
  }
  return all_bound;
}

bool
NativeRegistry::bind(PluginRuntime* rt, NativeEntry* entry)
{
  assert(!entry->binding || (entry->flags & SP_NTVFLAG_DYNAMIC));

  CharsAndLength name(entry->name);
  NativeTable::Result r = table_.find(name);
  if (!r.found())
    return false;

  entry->status = SP_NATIVE_BOUND;
  entry->binding = r->def;

  entry->weak_ref = nullptr;
  if (r->group) {
    // If the native is optional, this becomes a weak reference. Otherwise,
    // it's a strong reference.
    if (entry->flags & SP_NTVFLAG_DYNAMIC) {
      entry->weak_ref = r->group->addWeakRef(rt);
    } else if (r->group->bindingGeneration() != binding_generation_) {
      r->group->addStrongRef(rt);
      r->group->setBindingGeneration(binding_generation_);
    }
  }
  return true;
}

void
NativeRegistry::RemoveNatives(const Ref<INativeGroup>& aGroup)
{
  NativeGroup* group = aGroup->ToNativeGroup();
  group->detachWeakDependents();

  assert(!group->HasDependents());

  // Remove natives after having detached dependents. Otherwise, we could have
  // freed a RoutedNative's allocated NativeDef.
  for (NativeTable::iterator iter(&table_); !iter.empty(); iter.next()) {
    if (iter->group != group)
      continue;
    iter.erase();
  } 
}