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

MethodInfo::MethodInfo(Runtime* rt, uint32_t method_index, const TypeDesc* signature)
 : rt_(rt),
   method_index_(method_index),
   max_stack_(0),
   max_eval_stack_depth_(0),
   max_eval_stack_bytes_(0),
   signature_(signature)
 {}

uint32_t MethodInfo::pcode_offset() const {
    return rt_->image()->GetMethod(method_index_)->pcode_start;
}

MethodInfo::~MethodInfo() {
    if (fn_obj_)
        fn_obj_->method = nullptr;
}

void MethodInfo::setCompiledFunction(CompiledFunction* fun) {
    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
    jit_.reset(fun);
}

void MethodInfo::set_llcode(std::unique_ptr<LLCode> code) {
    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
    llcode_ = std::move(code);
}

const Handle<SpFunction>& MethodInfo::GetFunction() {
    if (!fn_obj_) {
        fn_obj_ = rt_->heap().New<SpFunction>(signature());
        if (fn_obj_)
            fn_obj_->method = this;
    }
    return fn_obj_;
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
    assert(llcode());
    const uint8_t* ll_bytes = llcode()->bytes();
    assert(cip >= ll_bytes && cip < ll_bytes + llcode()->size());
    uint32_t ll_offset = (uint32_t)(cip - ll_bytes);
    return llcode()->LookupHighOffset(ll_offset);
}

uint32_t MethodInfo::TranslateJitCip(uint32_t cip) const {
    assert(llcode());
    return llcode()->LookupHighOffset(cip);
}

const char* MethodInfo::GetName() const {
    auto method = rt_->image()->GetMethod(method_index_);
    if (!method)
        return nullptr;
    return rt_->image()->names() + method->name;
}

const char* MethodInfo::GetFilePath() const {
    return rt_->image()->LookupFile(pcode_offset());
}

} // namespace sp::v2
