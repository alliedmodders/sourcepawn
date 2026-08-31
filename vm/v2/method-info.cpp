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
#include "v2/lowering/llcode.h"
#include "v2/method-verifier.h"


namespace sp::v2 {

MethodInfo::MethodInfo(Runtime* rt, uint32_t method_index)
 : rt_(rt),
   method_index_(method_index),
   code_kind_(CodeKind::None),
   max_stack_(0),
   max_eval_stack_depth_(0),
   max_eval_stack_bytes_(0)
 {
    code_.jit = nullptr;
 }

uint32_t MethodInfo::pcode_offset() const {
    return rt_->image()->GetMethod(method_index_)->pcode_start;
}

MethodInfo::~MethodInfo() {
    if (code_kind_ == CodeKind::Jit)
        delete code_.jit;
    else if (code_kind_ == CodeKind::Interp)
        delete code_.interp;
}

void
MethodInfo::setCompiledFunction(CompiledFunction* fun) {
    assert(code_kind_ == CodeKind::None);

    // Grab the lock before linking code in, since the watchdog timer will look
    // at this on another thread.
    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
    code_.jit = fun;
    code_kind_ = CodeKind::Jit;
}

void MethodInfo::setInterpCode(std::unique_ptr<InterpCode> code) {
    assert(code_kind_ == CodeKind::None);

    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
    code_.interp = code.release();
    code_kind_ = CodeKind::Interp;
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
    local_types_ = std::move(verifier.local_types());
    arg_types_ = std::move(verifier.arg_types());
    mutated_args_ = std::move(verifier.mutated_args());
}

const TypeDesc* MethodInfo::GetTypeOfLocal(cell_t offset) const {
    if (offset < 0) {
        uint32_t arg_slot = -offset - 1;
        assert(arg_slot < arg_types_.size());
        return arg_types_[arg_slot];
    }
    assert((uint32_t)offset < local_types_.size());
    return local_types_[offset];
}

uint32_t MethodInfo::TranslateInterpCip(const uint8_t* cip) const {
    assert(interp());
    const uint8_t* ll_bytes = interp()->bytes();
    assert(cip >= ll_bytes && cip < ll_bytes + interp()->size());
    uint32_t ll_offset = (uint32_t)(cip - ll_bytes);
    return interp()->LookupHighOffset(ll_offset);
}

} // namespace sp::v2
