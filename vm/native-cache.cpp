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
#include "native-cache.h"

using namespace SourcePawn;
using namespace sp;
using namespace ke;

NativeCache::NativeCache()
{
  table_.init(128);
}

void
NativeCache::add(const char* name, const NativeSpec* spec)
{
  CharsAndLength key(name);
  NativeTable::Insert i = table_.findForAdd(key);
  if (i.found()) {
    // This should not happen in a properly written host application.
    assert(false);
    return;
  }

  Ref<NativeInfo> info = new NativeInfo();
  info->name = name;
  info->spec = spec;

  table_.add(i, info);
}

PassRef<NativeInfo>
NativeCache::find(const char* name)
{
  CharsAndLength key(name);
  NativeTable::Result r = table_.find(key);
  if (!r.found())
    return nullptr;
  return *r;
}
