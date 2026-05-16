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
#include "base-context.h"
#include "environment.h"

using namespace sp;
using namespace SourcePawn;

BasePluginContext::BasePluginContext()
 : env_(Environment::get()) {
}

BasePluginContext::~BasePluginContext() {
}

void
BasePluginContext::SetKey(int k, void* value) {
    if (k < 1 || k > 4)
        return;

    m_keys[k - 1] = value;
    m_keys_set[k - 1] = true;
}

bool
BasePluginContext::GetKey(int k, void** value) {
    if (k < 1 || k > 4 || m_keys_set[k - 1] == false)
        return false;

    *value = m_keys[k - 1];
    return true;
}

bool
BasePluginContext::IsDebugging() {
    return true;
}

ISourcePawnEngine2*
BasePluginContext::APIv2() {
    return env_->APIv2();
}

void
BasePluginContext::ReportError(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    env_->ReportErrorVA(fmt, ap);
    va_end(ap);
}

void
BasePluginContext::ReportErrorVA(const char* fmt, va_list ap) {
    env_->ReportErrorVA(fmt, ap);
}

void
BasePluginContext::ReportFatalError(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    env_->ReportErrorVA(SP_ERROR_FATAL, fmt, ap);
    va_end(ap);
}

void
BasePluginContext::ReportFatalErrorVA(const char* fmt, va_list ap) {
    env_->ReportErrorVA(SP_ERROR_FATAL, fmt, ap);
}

void
BasePluginContext::ReportErrorNumber(int error) {
    env_->ReportError(error);
}

void
BasePluginContext::ClearLastNativeError() {
    if (env_->hasPendingException())
        env_->clearPendingException();
}

cell_t
BasePluginContext::ThrowNativeErrorEx(int error, const char* msg, ...) {
    va_list ap;
    va_start(ap, msg);
    if (msg)
        env_->ReportErrorVA(error, msg, ap);
    else
        env_->ReportError(error);
    va_end(ap);
    return 0;
}

cell_t
BasePluginContext::ThrowNativeError(const char* msg, ...) {
    va_list ap;
    va_start(ap, msg);
    env_->ReportErrorVA(SP_ERROR_NATIVE, msg, ap);
    va_end(ap);
    return 0;
}

int
BasePluginContext::GetLastNativeError() {
    Environment* env = env_;
    if (!env->hasPendingException())
        return SP_ERROR_NONE;
    return env->getPendingExceptionCode();
}

cell_t
BasePluginContext::BlamePluginError(SourcePawn::IPluginFunction* pf, const char* msg, ...) {
    va_list ap;
    va_start(ap, msg);
    env_->BlamePluginErrorVA(pf, msg, ap);
    va_end(ap);
    return 0;
}

IFrameIterator*
BasePluginContext::CreateFrameIterator() {
    FrameIterator* it = new FrameIterator();
    return it;
}

void
BasePluginContext::DestroyFrameIterator(IFrameIterator* it) {
    delete static_cast<FrameIterator*>(it);
}

int BasePluginContext::LocalToArrayPtr(cell_t base, ARRAY_PTR* out) {
    cell_t* phys;
    if (int err = LocalToPhysAddr(base, &phys))
        return err;
    *out = reinterpret_cast<ARRAY_PTR>(phys);
    return SP_ERROR_NONE;
}

void* BasePluginContext::GetArrayData(ARRAY_PTR handle, uint32_t* size) {
    if (size)
        *size = 0;
    return reinterpret_cast<void*>(handle);
}
