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
#include "base-runtime.h"
#include "environment.h"
#include "stack-frames.h"
#include "md5/md5.h"
#include <stdarg.h>

using namespace sp;
using namespace SourcePawn;

BaseRuntime::BaseRuntime(SmxImage* image)
 : env_(Environment::get()),
   image_(image),
   computed_code_hash_(false),
   computed_data_hash_(false)
{
    code_ = image_->DescribeCode();
    data_ = image_->DescribeData();

    memset(code_hash_, 0, sizeof(code_hash_));
    memset(data_hash_, 0, sizeof(data_hash_));

    for (int i = 0; i < 4; i++) {
        m_keys[i] = nullptr;
        m_keys_set[i] = false;
    }
}

BaseRuntime::~BaseRuntime() {
}

void
BaseRuntime::SetKey(int k, void* value) {
    if (k < 1 || k > 4)
        return;

    m_keys[k - 1] = value;
    m_keys_set[k - 1] = true;
}

bool
BaseRuntime::GetKey(int k, void** value) {
    if (k < 1 || k > 4 || m_keys_set[k - 1] == false)
        return false;

    *value = m_keys[k - 1];
    return true;
}

void
BaseRuntime::ReportError(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    env_->ReportErrorVA(fmt, ap);
    va_end(ap);
}

void
BaseRuntime::ReportErrorVA(const char* fmt, va_list ap) {
    env_->ReportErrorVA(fmt, ap);
}

void
BaseRuntime::ReportFatalError(const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    env_->ReportErrorVA(SP_ERROR_FATAL, fmt, ap);
    va_end(ap);
}

void
BaseRuntime::ReportFatalErrorVA(const char* fmt, va_list ap) {
    env_->ReportErrorVA(SP_ERROR_FATAL, fmt, ap);
}

void
BaseRuntime::ReportErrorNumber(int error) {
    env_->ReportError(error);
}

cell_t
BaseRuntime::ThrowNativeErrorEx(int error, const char* msg, ...) {
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
BaseRuntime::ThrowNativeError(const char* msg, ...) {
    va_list ap;
    va_start(ap, msg);
    env_->ReportErrorVA(SP_ERROR_NATIVE, msg, ap);
    va_end(ap);
    return 0;
}

int
BaseRuntime::GetLastNativeError() {
    Environment* env = env_;
    if (!env->hasPendingException())
        return SP_ERROR_NONE;
    return env->getPendingExceptionCode();
}

cell_t
BaseRuntime::BlamePluginError(SourcePawn::IPluginFunction* pf, const char* msg, ...) {
    va_list ap;
    va_start(ap, msg);
    env_->BlamePluginErrorVA(pf, msg, ap);
    va_end(ap);
    return 0;
}

IFrameIterator*
BaseRuntime::CreateFrameIterator() {
    return new FrameIterator();
}

void
BaseRuntime::DestroyFrameIterator(IFrameIterator* it) {
    delete static_cast<FrameIterator*>(it);
}

SourcePawn::ISourcePawnEngine* BaseRuntime::GetEnvironment() {
    return env_;
}

unsigned char* BaseRuntime::GetCodeHash() {
    if (!computed_code_hash_) {
        MD5 md5_pcode;
        md5_pcode.update((const unsigned char*)code_.bytes, code_.length);
        md5_pcode.finalize();
        md5_pcode.raw_digest(code_hash_);
        computed_code_hash_ = true;
    }
    return code_hash_;
}

unsigned char* BaseRuntime::GetDataHash() {
    if (!computed_data_hash_) {
        MD5 md5_data;
        md5_data.update((const unsigned char*)data_.bytes, data_.length);
        md5_data.finalize();
        md5_data.raw_digest(data_hash_);
        computed_data_hash_ = true;
    }
    return data_hash_;
}

void BaseRuntime::SetPauseState(bool paused) {
    paused_ = paused;
}

bool BaseRuntime::IsPaused() {
    return paused_;
}
