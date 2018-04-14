// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2018 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#ifndef _include_sourcepawn_vm_builtins_h_
#define _include_sourcepawn_vm_builtins_h_

#include <sp_vm_types.h>
#include <amtl/am-hashmap.h>
#include <string.h>

namespace sp {

class BuiltinNatives
{
 public:
  BuiltinNatives();

  bool Initialize();

  SPVM_NATIVE_FUNC Lookup(const char* name);

 private:
  struct NativeMapPolicy {
    static inline bool matches(const char* lookup, const char* key) {
      return strcmp(lookup, key) == 0;
    }
    static inline uint32_t hash(const char* key) {
      return ke::FastHashCharSequence(key, strlen(key));
    }
  };
  typedef ke::HashMap<const char*,
                      SPVM_NATIVE_FUNC,
                      NativeMapPolicy> NativeMap;

  NativeMap map_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_builtins_h_
