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
#include "v2/method-info.h"
#include "compiled-function.h"
#include "environment.h"
#include "graph-builder.h"
#include "v2/method-verifier.h"

namespace sp::v2 {

MethodInfo::MethodInfo(Runtime* rt, uint32_t method_index)
 : rt_(rt),
   method_index_(method_index),
   max_stack_(0),
   max_eval_stack_depth_(0),
   max_eval_stack_bytes_(0)
{
}

uint32_t MethodInfo::pcode_offset() const {
    return rt_->image()->GetMethod(method_index_)->pcode_start;
}

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
    if (checked_.has_value())
        return;

    MethodVerifier verifier(rt_, method_index_);
    graph_ = verifier.verify();
    if (!graph_) {
        checked_ = {false};
        return;
    }

    checked_ = {true};
    max_stack_ = verifier.max_stack();
    max_eval_stack_depth_ = verifier.max_eval_stack_depth();
    max_eval_stack_bytes_ = verifier.max_eval_stack_bytes();
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

    if (slot < 0) {
        // -1 is because we can't encode 0-based arguments, because 0 is local.
        // +1 because we skip the argument count.
        return (-slot - 1 + 1) * sizeof(cell_t);
    }

    return local_offsets_.at(slot);
}

cell_t MethodInfo::StackSizeForLocalSlots() {
    if (local_offsets_.empty())
        return 0;
    return local_offsets_.back();
}

} // namespace sp::v2
