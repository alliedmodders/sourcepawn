// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_vm_builtins_h_
#define _include_sourcepawn_vm_builtins_h_

#include <amtl/am-hashmap.h>
#include <sp_vm_types.h>
#include <string.h>

namespace sp::v1 {

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
    typedef ke::HashMap<const char*, SPVM_NATIVE_FUNC, NativeMapPolicy> NativeMap;

    NativeMap map_;
};

} // namespace sp::v1

#endif // _include_sourcepawn_vm_builtins_h_
