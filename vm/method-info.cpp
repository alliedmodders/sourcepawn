// vim: set sts=4 ts=8 sw=4 tw=99 et:
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
#include "method-info.h"
#include "compiled-function.h"
#include "environment.h"
#include "graph-builder.h"
#include "method-verifier.h"

namespace sp {

MethodInfo::MethodInfo(PluginRuntime* rt, uint32_t codeOffset)
 : rt_(rt),
   pcode_offset_(codeOffset),
   checked_(false),
   validation_error_(SP_ERROR_NONE),
   max_stack_(0)
{}

MethodInfo::~MethodInfo()
{}

void
MethodInfo::setCompiledFunction(CompiledFunction* fun) {
    assert(!jit_);

    // Grab the lock before linking code in, since the watchdog timer will look
    // at this on another thread.
    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
    jit_.reset(fun);
}

void
MethodInfo::InternalValidate() {
    checked_ = true;

    MethodVerifier verifier(rt_, pcode_offset_);
    graph_ = verifier.verify();
    if (!graph_) {
        validation_error_ = verifier.error();
        return;
    }
    max_stack_ = verifier.max_stack();
    local_sizes_ = std::move(verifier.local_sizes());
    BuildLocalOffsetTable();
}

void MethodInfo::BuildLocalOffsetTable() {
    local_offsets_ =
        ke::FixedArray<cell_t>(local_sizes_.size());

    cell_t offset = 0;
    for (size_t i = 0; i < local_sizes_.size(); i++) {
        offset -= local_sizes_[i];
        local_offsets_[i] = offset;
    }
}

cell_t MethodInfo::StackOffset(cell_t slot) {
    if (rt_->code().version < SmxConsts::CODE_VERSION_TYPED_STACK)
        return slot;

    if (slot < 0)
        return (-slot - 1 + 3) * sizeof(cell_t);

    return local_offsets_.at(slot);
}

cell_t MethodInfo::StackSizeForLocalSlots() {
    if (local_offsets_.empty())
        return 0;
    return local_offsets_.back();
}

} // namespace sp
