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
#ifndef _include_sourcepawn_vm_native_group_h_
#define _include_sourcepawn_vm_native_group_h_

#include <sp_vm_api.h>
#include <am-hashmap.h>
#include <am-hashset.h>
#include <am-refcounting.h>
#include <am-vector.h>
#include <am-function.h>
#include <string.h>

namespace sp {

using namespace SourcePawn;
using namespace ke;

class PluginRuntime;
struct NativeEntry;

class NativeGroup :
  public SourcePawn::INativeGroup,
  public ke::Refcounted<NativeGroup>
{
 public:
  NativeGroup(NativeRegistry* cache, const char* name);

  KE_IMPL_REFCOUNTING(NativeGroup);

  NativeGroup* ToNativeGroup() override {
    return this;
  }
  bool HasDependents() const override;
  void EnumerateDependents(FuncPtr<void(SourcePawn::IPluginRuntime*)> callback) const override;

  NativeRegistry* registry() const {
    return registry_;
  }
  void forEachNative(FuncPtr<void(const NativeDef*)> callback);

  void addHeldPointer(NativeDef* def) {
    AutoArray<NativeDef> ptr(def);
    held_pointers_.append(ke::Move(ptr));
  }

  unsigned bindingGeneration() const {
    return binding_generation_;
  }
  void setBindingGeneration(unsigned binding_generation) {
    binding_generation_ = binding_generation;
  }

 public:
  // We allocate one WeakRef for each runtime that depends on this native
  // group. When that plugin drops all references to the WeakRef, it will
  // silently remove its dependency on to the native group.
  class WeakRef : public ke::Refcounted<WeakRef>
  {
   public:
    WeakRef(PluginRuntime* rt, NativeGroup* group);
    ~WeakRef();

    NativeGroup* group() const {
      return group_;
    }

   private:
    PluginRuntime* rt_;
    Ref<NativeGroup> group_;
  };

  PassRef<WeakRef> addWeakRef(PluginRuntime* rt);
  void dropLastWeakRef(PluginRuntime* rt);
  void detachWeakDependents();

 public:
  // Strong references are for dependents that cannot be safely accessed once
  // they are bound to this group. We require that these dependents be unloaded
  // before dissolving a StrongRef.
  //
  // Most dependents are strong dependents, so rather than attach them to each
  // native we attach a list in the PluginRuntime.
  class StrongRef : public ke::Refcounted<StrongRef>
  {
   public:
    StrongRef(PluginRuntime* rt, NativeGroup* group);
    ~StrongRef();

   private:
    PluginRuntime* rt_;
    Ref<NativeGroup> group_;
  };

  void addStrongRef(PluginRuntime* rt);
  void dropStrongRef(PluginRuntime* rt);

 public:
  struct NativeVector {
    NativeVector()
     : defs(nullptr),
       length(0)
    {}
    NativeVector(const NativeDef* defs, size_t count)
     : defs(defs),
       length(count)
    {}
    const NativeDef* defs;
    size_t length;
  };

  void addNatives(const NativeVector& natives) {
    natives_.append(natives);
  }

 private:
  const char* name_;
  Ref<NativeRegistry> registry_;
  Vector<NativeVector> natives_;

  struct RuntimePolicy {
    static bool matches(PluginRuntime* key, PluginRuntime* other) {
      return key == other;
    }
    static uint32_t hash(PluginRuntime* key) {
      return HashPointer(key);
    }
  };

  // Note: we don't store the WeakRef in a Ref<>, since we want the WeakRef's
  // dtor to be responsible for fixing up the dependency map. If we used a
  // Ref<> here its dtor would never fire.
  typedef HashMap<PluginRuntime*, WeakRef*, RuntimePolicy> WeakRefMap;
  WeakRefMap weak_refs_;

  typedef HashSet<PluginRuntime*, RuntimePolicy> StrongRefSet;
  mutable StrongRefSet strong_refs_;

  unsigned binding_generation_;

  // For storing lists that we had to construct dynamically.
  ke::Vector<AutoArray<NativeDef>> held_pointers_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_native_group_h_
