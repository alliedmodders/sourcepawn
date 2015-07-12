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
#ifndef _include_sourcepawn_vm_native_cache_h_
#define _include_sourcepawn_vm_native_cache_h_

#include <sp_vm_api.h>
#include <am-hashtable.h>
#include <am-refcounting.h>

namespace sp {

using namespace SourcePawn;

struct NativeInfo : public ke::Refcounted<NativeInfo>
{
  NativeInfo()
    : name(nullptr),
      spec(nullptr),
      stub(nullptr)
  {}
  ~NativeInfo() {
    assert(!stub);
  }

  const char* name;
  const NativeSpec* spec;
  void* stub;
};

class NativeCache
{
 public:
  NativeCache();

  void add(const char* name, const NativeSpec* spec);
  ke::PassRef<NativeInfo> find(const char* name);

 private:
  struct CharsAndLength
  {
    CharsAndLength(const char* ptr)
     : ptr(ptr),
       length(strlen(ptr))
    {}
    const char* ptr;
    size_t length;
  };

  struct Policy {
    typedef ke::Ref<NativeInfo> Payload;
    static uint32_t hash(const CharsAndLength& key) {
      return ke::FastHashCharSequence(key.ptr, key.length);
    }
    static bool matches(const CharsAndLength& key, const Payload& other) {
      return strcmp(key.ptr, other->name) == 0;
    }
  };

  typedef ke::HashTable<Policy> NativeTable;

  NativeTable table_;
};

}

#endif // _include_sourcepawn_vm_native_cache_h_