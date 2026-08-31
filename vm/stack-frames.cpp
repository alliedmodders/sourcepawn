// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include "stack-frames.h"

#include "base-runtime.h"
#include "compiled-function.h"
#include "environment.h"
#include "legacy/method-info.h"
#include "legacy/plugin-runtime.h"
#include "objects.h"
#include "v2/method-info.h"
#if defined(KE_ARCH_X86)
#    include "x86/frames-x86.h"
#elif defined(KE_ARCH_X64)
#    include "x64/frames-x64.h"
#elif !defined(SP_JIT_V1) && !defined(SP_JIT_V2)
#    include "null-frame-layout.h"
#endif

using namespace ke;
using namespace sp;
using namespace SourcePawn;

InvokeFrame::InvokeFrame(BaseRuntime* cx)
 : prev_(Environment::get()->top()),
   cx_(cx)
{
    Environment::get()->enterInvoke(this);
}

InvokeFrame::~InvokeFrame() {
    assert(Environment::get()->top() == this);
    Environment::get()->leaveInvoke();
}

InterpInvokeFrame::InterpInvokeFrame(BaseRuntime* cx, v1::MethodInfo* method,
                                     const uint8_t* const* cip)
  : InvokeFrame(cx),
    cip_(cip),
    native_index_(-1)
{
    legacy_method_ = method;
}

InterpInvokeFrame::InterpInvokeFrame(BaseRuntime* cx, SpFunction* fn, const uint8_t* const* cip)
  : InvokeFrame(cx),
    callee_(fn),
    cip_(cip),
    native_index_(-1)
{}

InterpInvokeFrame::~InterpInvokeFrame() {
    assert(native_index_ == -1);
}

BaseMethodInfo* InterpInvokeFrame::method() const {
    return legacy_method_;
}

void
InterpInvokeFrame::enterNativeCall(uint32_t native_index) {
    assert(native_index_ == -1);
    native_index_ = native_index;
}

void
InterpInvokeFrame::leaveNativeCall() {
    assert(native_index_ != -1);
    native_index_ = -1;
}

JitInvokeFrame::JitInvokeFrame(BaseRuntime* cx)
 : InvokeFrame(cx),
   prev_exit_fp_(Environment::get()->exit_fp()) {
}

JitInvokeFrame::~JitInvokeFrame() {
    Environment::get()->leaveJitInvoke(this);
}

InterpFrameIterator::InterpFrameIterator(InterpInvokeFrame* ivk)
 : ivk_(ivk) {
    if (ivk_->native_index_ != -1)
        current_ = FrameType::Native;
    else
        current_ = FrameType::Scripted;
}

bool
InterpFrameIterator::done() const {
    return current_ == FrameType::Scripted;
}

void
InterpFrameIterator::next() {
    assert(!done());
    current_ = FrameType::Scripted;
}

FrameType
InterpFrameIterator::type() const {
    return current_;
}

cell_t InterpFrameIterator::cip() const {
    assert(current_ == FrameType::Scripted);
    const uint8_t* ptr = *ivk_->cip_;
    return ivk_->method()->TranslateInterpCip(ptr);
}

uint32_t
InterpFrameIterator::native_index() const {
    assert(current_ == FrameType::Native);
    return ivk_->native_index_;
}

// This constructor is for find_entry_fp() in the JIT.
JitFrameIterator::JitFrameIterator(Environment* env)
 : JitFrameIterator(env->top()->cx()->GetBaseRuntime(), env->exit_fp())
{}

JitFrameIterator::JitFrameIterator(BaseRuntime* rt, intptr_t* exit_fp)
 : rt_(rt),
   cur_frame_(FrameLayout::FromFp(exit_fp))
{
    assert(cur_frame_->frame_type() == JitFrameType::Exit);
    assert(cur_frame_->return_address);
    assert(cur_frame_->prev_fp);

    pc_ = nullptr;
    cip_ = kInvalidCip;
}

bool
JitFrameIterator::done() const {
    return cur_frame_->frame_type() == JitFrameType::Entry;
}

void JitFrameIterator::next() {
    assert(!done());

    pc_ = cur_frame_->return_address;
    cip_ = kInvalidCip;
    cur_frame_ = FrameLayout::FromFp(cur_frame_->prev_fp);
}

FrameType
JitFrameIterator::type() const {
    switch ((JitFrameType)cur_frame_->frame_type()) {
        case JitFrameType::Scripted:
            return FrameType::Scripted;
        case JitFrameType::Exit:
            if (GetExitFrameType(cur_frame_->function_id()) == ExitFrameType::Native)
                return FrameType::Native;
            return FrameType::Internal;
        default:
            return FrameType::Internal;
    }
}

BaseMethodInfo* JitFrameIterator::method() const {
    if (cur_frame_->frame_type() != JitFrameType::Scripted)
        return nullptr;
    return rt_->GetMethodFromFrameId(cur_frame_->function_id());
}

cell_t JitFrameIterator::cip() const {
    ke::RefPtr<BaseMethodInfo> method = rt_->GetMethodFromFrameId(cur_frame_->function_id());
    if (!method)
        return 0;

    CompiledFunction* fn = method->jit();
    if (!fn)
        return 0;

    if (cip_ == kInvalidCip) {
        if (pc_)
            cip_ = method->TranslateJitCip(fn->FindCipByPc(pc_));
        else
            cip_ = 0;
    }
    return cip_;
}

uint32_t
JitFrameIterator::native_index() const {
    assert(type() == FrameType::Native);
    return GetExitFramePayload(cur_frame_->function_id());
}

FrameIterator::FrameIterator()
 : ivk_(nullptr),
   runtime_(nullptr),
   next_exit_fp_(nullptr)
{
    Reset();
}

void
FrameIterator::nextInvokeFrame() {
    runtime_ = ivk_->cx()->GetBaseRuntime();
    if (JitInvokeFrame* jvk = ivk_->AsJitInvokeFrame()) {
        frame_cursor_ = std::make_unique<JitFrameIterator>(runtime_, next_exit_fp_);
        next_exit_fp_ = jvk->prev_exit_fp();
        return;
    }
    if (InterpInvokeFrame* ivk = ivk_->AsInterpInvokeFrame()) {
        frame_cursor_ = std::make_unique<InterpFrameIterator>(ivk);
        return;
    }
}

cell_t
FrameIterator::cip() const {
    return frame_cursor_->cip();
}

void
FrameIterator::Next() {
    if (frame_cursor_->done()) {
        frame_cursor_ = nullptr;

        ivk_ = ivk_->prev();
        if (ivk_)
            nextInvokeFrame();
        return;
    }

    frame_cursor_->next();
}

void
FrameIterator::Reset() {
    ivk_ = Environment::get()->top();
    runtime_ = nullptr;
    next_exit_fp_ = Environment::get()->exit_fp();
    frame_cursor_ = nullptr;

    if (ivk_)
        nextInvokeFrame();
}

unsigned
FrameIterator::LineNumber() const {
    if (!IsScriptedFrame())
        return 0;

    ucell_t cip = frame_cursor_->cip();
    if (cip == kInvalidCip)
        return 0;

    uint32_t line;
    if (!runtime_->image()->LookupLine(cip, &line))
        return 0;

    return line;
}

const char*
FrameIterator::FilePath() const {
    if (!IsScriptedFrame())
        return nullptr;

    ucell_t cip = frame_cursor_->cip();
    if (cip == kInvalidCip)
        return frame_cursor_->method()->GetFilePath();

    return runtime_->image()->LookupFile(cip);
}

const char*
FrameIterator::FunctionName() const {
    assert(ivk_);
    if (IsNativeFrame()) {
        uint32_t native_index = frame_cursor_->native_index();
        const sp_native_t* native = runtime_->GetNative(native_index);
        if (!native)
            return nullptr;
        return native->name;
    }

    if (IsScriptedFrame()) {
        return frame_cursor_->method()->GetName();
    }

    return nullptr;
}

bool
FrameIterator::IsNativeFrame() const {
    return frame_cursor_->type() == FrameType::Native;
}

bool
FrameIterator::IsScriptedFrame() const {
    return frame_cursor_->type() == FrameType::Scripted;
}

IPluginContext*
FrameIterator::Context() const {
    if (!ivk_)
        return nullptr;
    return ivk_->cx();
}

bool
FrameIterator::IsInternalFrame() const {
    return frame_cursor_->type() == FrameType::Internal;
}
