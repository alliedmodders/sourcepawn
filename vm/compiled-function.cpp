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
#if defined(KE_ARCH_X86)
# include "x86/jit_x86.h"
#endif

using namespace sp;

CompiledFunction::CompiledFunction(const CodeChunk& code,
                                   cell_t pcode_offs,
                                   FixedArray<LoopEdge> *edges,
                                   FixedArray<CipMapEntry> *cipmap)
  : code_(code),
    code_offset_(pcode_offs),
    edges_(edges),
    cip_map_(cipmap)
{
}

CompiledFunction::~CompiledFunction()
{
}

CompiledFunction *
CompiledFunction::Compile(PluginRuntime *prt, cell_t pcode_offs, int *err)
{
  Compiler cc(prt, pcode_offs);
  CompiledFunction *fun = cc.emit(err);
  if (!fun)
    return nullptr;

  // Grab the lock before linking code in, since the watchdog timer will look
  // at this list on another thread.
  ke::AutoLock lock(Environment::get()->lock());

  prt->AddJittedFunction(fun);
  return fun;
}

static int cip_map_entry_cmp(const void *a1, const void *aEntry)
{
  uint32_t pcoffs = (uint32_t)(intptr_t)a1;
  const CipMapEntry *entry = reinterpret_cast<const CipMapEntry *>(aEntry);
  if (pcoffs < entry->pcoffs)
    return -1;
  if (pcoffs == entry->pcoffs)
    return 0;
  return pcoffs > entry->pcoffs;
}

ucell_t
CompiledFunction::FindCipByPc(void *pc)
{
  if (uintptr_t(pc) < uintptr_t(code_.address()))
    return kInvalidCip;

  uint32_t pcoffs = intptr_t(pc) - intptr_t(code_.address());
  if (pcoffs > code_.bytes())
    return kInvalidCip;

  void *ptr = bsearch(
    (void *)(uintptr_t)pcoffs,
    cip_map_->buffer(),
    cip_map_->length(),
    sizeof(CipMapEntry),
    cip_map_entry_cmp);
  assert(ptr);

  if (!ptr) {
    // Shouldn't happen, but fail gracefully.
    return kInvalidCip;
  }

  return code_offset_ + reinterpret_cast<CipMapEntry *>(ptr)->cipoffs;
}
