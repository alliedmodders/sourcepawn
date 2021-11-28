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
#include "compiled-function.h"
#include "environment.h"
#include <amtl/am-platform.h>

using namespace sp;

CompiledFunction::CompiledFunction(const CodeChunk& code,
                                   cell_t pcode_offs,
                                   FixedArray<LoopEdge>* edges,
                                   FixedArray<CipMapEntry>* cipmap)
 : code_(code),
   code_offset_(pcode_offs),
   edges_(edges),
   cip_map_(cipmap),
   cip_map_sorted_(false)
{
}

CompiledFunction::~CompiledFunction()
{
}

static int cip_map_entry_sort_cmp(const void* a1, const void* a2)
{
  const CipMapEntry* c1 = reinterpret_cast<const CipMapEntry*>(a1);
  const CipMapEntry* c2 = reinterpret_cast<const CipMapEntry*>(a2);
  if (c1->pcoffs < c2->pcoffs)
    return -1;
  if (c1->pcoffs == c2->pcoffs)
    return 0;
  return 1;
}

static int cip_map_entry_cmp(const void* a1, const void* aEntry)
{
  uint32_t pcoffs = (uint32_t)(intptr_t)a1;
  const CipMapEntry* entry = reinterpret_cast<const CipMapEntry*>(aEntry);
  if (pcoffs < entry->pcoffs)
    return -1;
  if (pcoffs == entry->pcoffs)
    return 0;
  return pcoffs > entry->pcoffs;
}

ucell_t
CompiledFunction::FindCipByPc(void* pc)
{
  if (uintptr_t(pc) < uintptr_t(code_.address()))
    return kInvalidCip;

  uint32_t pcoffs = intptr_t(pc) - intptr_t(code_.address());
  if (pcoffs > code_.bytes())
    return kInvalidCip;

  if (!cip_map_sorted_) {
    qsort(cip_map_->buffer(),
          cip_map_->size(),
          sizeof(CipMapEntry),
          cip_map_entry_sort_cmp);
    cip_map_sorted_ = true;
  }

  void* ptr = bsearch(
    (void*)(uintptr_t)pcoffs,
    cip_map_->buffer(),
    cip_map_->size(),
    sizeof(CipMapEntry),
    cip_map_entry_cmp);
  assert(ptr);

  if (!ptr) {
    // Shouldn't happen, but fail gracefully.
    return kInvalidCip;
  }

  return code_offset_ + reinterpret_cast<CipMapEntry*>(ptr)->cipoffs;
}
