// vim: set ts=4 sw=4 tw=99 et:
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
#ifndef _include_sourcepawn_includes_native_api_h_
#define _include_sourcepawn_includes_native_api_h_

#include <stdint.h>
#include <am-refcounting.h>
#include <am-function.h>

namespace sp {
class NativeGroup;
class NativeRegistry;
}

namespace SourcePawn {

class IPluginRuntime;

// @brief An INativeGroup is used to unbind or remove natives that are part of
// the same subsystem, library, or module.
//
// Native groups are created within native registries, and cannot be used
// across registries.
class INativeGroup : public ke::IRefcounted
{
 public:
  // @brief Return a pointer to the implementation of this class.
  virtual sp::NativeGroup* ToNativeGroup() = 0;

  // @brief Returns true if the native group has any dependents.
  //
  // @return True if any runtimes depend on this native group, false otherwise.
  virtual bool HasDependents() const = 0;

  // @brief Enumerate all dependent plugin runtimes.
  //
  // @callback        Callback to be invoked for each dependent.
  virtual void EnumerateDependents(ke::FuncPtr<void(IPluginRuntime*)> callback) const = 0;
};

// @brief Describes the calling convention of a native.
struct NativeSignature
{
  // Flags from the NativeSpecFlags namespace.
  uint32_t flags;

  // Signature layout. Unused.
  const uint8_t* signature;

  // Length of signature. Unused.
  size_t length;
};

// @brief Describes a native definition.
struct NativeDef
{
  // The native's name.
  const char* name;

  // The native's method pointer.
  void* method;

  // The native's signature. If null, this native uses the old legacy
  // signature:
  //   int32_t (*)(SourcePawn::IPluginContext*, const int32_t*)
  // (Where int32_t is aliased to cell_t.)
  const NativeSignature* sig;
};

// @brief Describes a native cache.
class INativeRegistry : public ke::IRefcounted
{
 public:
  // @brief Returns a pointer to the structure's implementation.
  virtual sp::NativeRegistry* ToNativeRegistry() = 0;

  // @brief Allocate a native grouping object.
  //
  // @param delegate   A pointer to an object to receive notifications.
  // @param name       Native group name, for internal debugging. The pointer
  //                   must be statically scoped.
  // @return           A new reference-counted INativeGroup object.
  virtual ke::PassRef<INativeGroup> NewNativeGroup(const char* name) = 0;

  // @brief Add natives to the native cache.
  //
  // @param group      The group that will own this native. This may be
  //                   null. If non-null, the caller must own at least
  //                   one reference for the duration of this call.
  // @param defs       The native definition. Must be statically scoped.
  // @param count      Number of natives in the vector.
  virtual void AddNativeVector(INativeGroup* group, const NativeDef* def, size_t count) = 0;

  // @brief Add a list of natives to the native cache.
  //
  // @param group      The group that will own this native. This may be
  //                   null. If non-null, the caller must own at least
  //                   one reference for the duration of this call.
  // @param defs       Pointer to an array of native definitions. The list
  //                   is terminated by NativeDef::name being null. Must be
  //                   statically scoped.
  virtual void AddNativeList(INativeGroup* group, const NativeDef* defs) = 0;

  // @brief Add a legacy native list to the native cache. The list is
  // terminated by a null "name" field.
  //
  // This internally converts a LegacyNativeDef to a NativeDef. The result of
  // this conversion is allocated in the given registry, or if provided, the
  // the group instead.
  //
  // @param group      The group that will own this native. This may be
  //                   null. If non-null, the caller must own at least
  //                   one reference for the duration of this call.
  // @param defs       Native definitions.
  virtual void AddLegacyNatives(INativeGroup* group, const sp_nativeinfo_t* defs) = 0;

  // @brief Add a native router. This will invoke the given callback when the
  // the native is invoked, with the given callback data.
  //
  // @param group      The group that will own this native. This may be
  //                   null. If non-null, the caller must own at least
  //                   one reference for the duration of this call.
  // @param name       The native's name.
  // @param callback   Callback to be invoked with the name.
  // @param data       Data to be passed to the callback.
  // @return           True on success, false otherwise.
  virtual bool AddRoutedNative(INativeGroup* group,
                               const char* name,
                               SPVM_FAKENATIVE_FUNC callback,
                               void* data) = 0;

  // @brief Remove all natives owned by the given group.
  //
  // The caller must ensure that any actively bound natives in this group are
  // bound with the SP_NTVFLAG_OPTIONAL flag. Otherwise, this call will fail.
  // In a debug build an assert will fire.
  //
  // @param group      The group that is being unregistered. Cannot be null.
  virtual void RemoveNatives(const ke::Ref<INativeGroup>& group) = 0;
};

} // namespace SourcePawn

#endif // _include_sourcepawn_includes_native_api_h_
