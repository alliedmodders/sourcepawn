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
#include "native-group.h"
#include "native-registry.h"
#include "plugin-runtime.h"
#include "environment.h"

using namespace sp;
using namespace ke;
using namespace SourcePawn;

NativeGroup::NativeGroup(NativeRegistry* registry, const char* name)
 : name_(name),
   registry_(registry),
   binding_generation_(0)
{
  weak_refs_.init();
}

bool
NativeGroup::HasDependents() const
{
  return strong_refs_.elements() > 0;
}

void
NativeGroup::EnumerateDependents(FuncPtr<void(SourcePawn::IPluginRuntime*)> callback) const
{
  for (StrongRefSet::iterator iter = strong_refs_.iter(); !iter.empty(); iter.next())
    callback(*iter);
}

auto
NativeGroup::addWeakRef(PluginRuntime* rt) -> PassRef<WeakRef>
{
  WeakRefMap::Insert p = weak_refs_.findForAdd(rt);
  if (p.found())
    return p->value;

  Ref<WeakRef> ref = new WeakRef(rt, this);
  weak_refs_.add(p, rt, ref);
  return ref;
}

void
NativeGroup::dropLastWeakRef(PluginRuntime* rt)
{
  WeakRefMap::Result r = weak_refs_.find(rt);
  weak_refs_.remove(r);
}

void
NativeGroup::detachWeakDependents()
{
  // Since wiping a plugin's native list could zap a WeakRef, and mutate the
  // hashtable during iteration, we build a list first.
  Vector<PluginRuntime*> runtimes;
  for (WeakRefMap::iterator iter = weak_refs_.iter(); !iter.empty(); iter.next())
    runtimes.append(iter->key);

  for (size_t i = 0; i < runtimes.length(); i++) {
    PluginRuntime* rt = runtimes[i];
    for (size_t j = 0; j < rt->GetNativesNum(); j++) {
      NativeEntry* entry = rt->NativeAt(j);
      if (entry->weak_ref && entry->weak_ref->group() == this) {
        assert(entry->flags & SP_NTVFLAG_DYNAMIC);
        entry->binding = nullptr;
        entry->status = SP_NATIVE_UNBOUND;
        entry->weak_ref = nullptr;
      }
    }
  }
}

void
NativeGroup::addStrongRef(PluginRuntime* rt)
{
  StrongRefSet::Insert p = strong_refs_.findForAdd(rt);
  if (p.found())
    return;

  strong_refs_.add(p, rt);
  rt->addDependency(new StrongRef(rt, this));
}

void
NativeGroup::dropStrongRef(PluginRuntime* rt)
{
  StrongRefSet::Result p = strong_refs_.find(rt);
  strong_refs_.remove(p);
}

void
NativeGroup::forEachNative(FuncPtr<void(const NativeDef*)> callback)
{
  for (size_t i = 0; i < natives_.length(); i++) {
    for (size_t j = 0; j < natives_[i].length; j++)
      callback(&natives_[i].defs[j]);
  }
}

NativeGroup::WeakRef::WeakRef(PluginRuntime* rt, NativeGroup* group)
 : rt_(rt),
   group_(group)
{
}

NativeGroup::WeakRef::~WeakRef()
{
  group_->dropLastWeakRef(rt_);
}

NativeGroup::StrongRef::StrongRef(PluginRuntime* rt, NativeGroup* group)
 : rt_(rt),
   group_(group)
{
}

NativeGroup::StrongRef::~StrongRef()
{
  group_->dropStrongRef(rt_);
}