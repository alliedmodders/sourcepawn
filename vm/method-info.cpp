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
#include "environment.h"
#include "compiled-function.h"
#include "method-info.h"
#include "method-verifier.h"
#include "graph-builder.h"

namespace sp {

MethodInfo::MethodInfo(PluginRuntime* rt, uint32_t codeOffset)
 : rt_(rt),
   pcode_offset_(codeOffset),
   checked_(false),
   validation_error_(SP_ERROR_NONE),
   max_stack_(0)
{
}

MethodInfo::~MethodInfo()
{
}

void
MethodInfo::setCompiledFunction(CompiledFunction* fun)
{
  assert(!jit_);

  // Grab the lock before linking code in, since the watchdog timer will look
  // at this on another thread.
  std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
  jit_.reset(fun);
}

void
MethodInfo::InternalValidate()
{
  MethodVerifier verifier(rt_, pcode_offset_);
  graph_ = verifier.verify();
  if (graph_) {
    max_stack_ = verifier.max_stack();
  } else {
    validation_error_ = verifier.error();
  }

  checked_ = true;
}

} // namespace sp
