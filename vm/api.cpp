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
#include "api.h"
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <zlib/zlib.h>
#include "environment.h"
#if defined __GNUC__
#    include <unistd.h>
#endif

#if defined WIN32
#    define WIN32_LEAN_AND_MEAN
#    include <windows.h>
#elif defined __GNUC__
#    include <sys/mman.h>
#endif

#if defined __linux__
#    include <malloc.h>
#endif

#if defined(SOURCEMOD_BUILD)
#    include <sourcemod_version.h>
#    define SOURCEPAWN_VERSION SOURCEMOD_VERSION
#endif
#include <amtl/am-string.h>
#include "code-stubs.h"

using namespace sp;
using namespace SourcePawn;

// ////// //
// API v1
// ////// //

SourcePawnEngine::SourcePawnEngine() {
}

void*
SourcePawnEngine::AllocatePageMemory(size_t size) {
    CodeChunk chunk = Environment::get()->AllocateCode(size + sizeof(CodeChunk));
    CodeChunk* hidden = (CodeChunk*)chunk.address();
    new (hidden) CodeChunk(chunk);
    return hidden + 1;
}

void
SourcePawnEngine::SetReadExecute(void* ptr) {
    /* already re */
}

void
SourcePawnEngine::SetReadWrite(void* ptr) {
    /* already rw */
}

void
SourcePawnEngine::FreePageMemory(void* ptr) {
    assert(ptr);
    CodeChunk* hidden = (CodeChunk*)((uint8_t*)ptr - sizeof(CodeChunk));
    hidden->~CodeChunk();
}

IDebugListener*
SourcePawnEngine::SetDebugListener(IDebugListener* pListener) {
    IDebugListener* old = Environment::get()->debugger();
    Environment::get()->SetDebugger(pListener);
    return old;
}

int
SourcePawnEngine::SetDebugBreakHandler(SPVM_DEBUGBREAK handler) {
    if (!Environment::get()->IsDebugBreakEnabled())
        return SP_ERROR_NOTDEBUGGING;

    Environment::get()->SetDebugBreakHandler(handler);
    return SP_ERROR_NONE;
}

unsigned int
SourcePawnEngine::GetEngineAPIVersion() {
    return 5;
}

// ////// //
// API v2
// ////// //

SourcePawnEngine2::SourcePawnEngine2() {
}

size_t
sp::UTIL_FormatVA(char* buffer, size_t maxlength, const char* fmt, va_list ap) {
    size_t len = vsnprintf(buffer, maxlength, fmt, ap);

    if (len >= maxlength) {
        buffer[maxlength - 1] = '\0';
        return maxlength - 1;
    }
    return len;
}

size_t
sp::UTIL_Format(char* buffer, size_t maxlength, const char* fmt, ...) {
    va_list ap;

    va_start(ap, fmt);
    size_t len = UTIL_FormatVA(buffer, maxlength, fmt, ap);
    va_end(ap);

    return len;
}

static IPluginRuntime*
LoadImage(std::unique_ptr<SmxImage> image, const char* file, char* error, size_t maxlength) {
    if (!image->validate()) {
        const char* errorMessage = image->errorMessage();
        if (!errorMessage)
            errorMessage = "binary parse error";
        UTIL_Format(error, maxlength, "%s", errorMessage);
        return nullptr;
    }

    PluginRuntime* pRuntime = new PluginRuntime(image.release());
    if (!pRuntime->Initialize()) {
        delete pRuntime;

        UTIL_Format(error, maxlength, "out of memory");
        return nullptr;
    }

    size_t len = strlen(file);
    for (size_t i = len - 1; i < len; i--) {
        if (file[i] == '/'
#if defined WIN32
            || file[i] == '\\'
#endif
        ) {
            pRuntime->SetNames(file, &file[i + 1]);
            break;
        }
    }

    if (*pRuntime->Name() == '\0')
        pRuntime->SetNames(file, file);

    return pRuntime;
}

IPluginRuntime*
SourcePawnEngine2::LoadBinaryFromFile(const char* file, char* error, size_t maxlength) {
    FILE* fp = fopen(file, "rb");

    if (!fp) {
        UTIL_Format(error, maxlength, "file not found");
        return nullptr;
    }

    std::unique_ptr<SmxImage> image(new SmxImage(fp));
    fclose(fp);

    return LoadImage(std::move(image), file, error, maxlength);
}

IPluginRuntime*
SourcePawnEngine2::LoadBinaryFromMemory(const char* file, uint8_t* addr, size_t size,
                                        void (*dtor)(uint8_t*), char* error, size_t maxlength) {
    std::unique_ptr<SmxImage> image;

    if (dtor)
        image = std::make_unique<SmxImage>(addr, size, dtor);
    else
        image = std::make_unique<SmxImage>(addr, size);

    return LoadImage(std::move(image), file, error, maxlength);
}

#if !defined(SOURCEPAWN_VERSION)
#    define SOURCEPAWN_VERSION "SourcePawn 1.10"
#endif

const char*
SourcePawnEngine2::GetEngineName() {
    const char* info = "";
#if !defined(SP_HAS_JIT)
    info = ", interp-x86";
#else
    if (!Environment::get()->IsJitEnabled()) {
        info = ", interp-x86";
    } else {
#    if defined(KE_ARCH_X86)
        info = ", jit-x86";
#    else
        info = ", unknown";
#    endif
    }
#endif

    // This is not pretty, but the API is per-thread, and this only breaks if
    // the caller observes the engine name, without requerying it, after
    // changing the JIT status. We do this because the SourceMod build exposes
    // the version as an extern, not a string literal.
    ke::SafeSprintf(engine_name_, sizeof(engine_name_), "%s%s", SOURCEPAWN_VERSION, info);
    return engine_name_;
}

const char*
SourcePawnEngine2::GetVersionString() {
    return SOURCEPAWN_VERSION;
}

IDebugListener*
SourcePawnEngine2::SetDebugListener(IDebugListener* listener) {
    IDebugListener* old = Environment::get()->debugger();
    Environment::get()->SetDebugger(listener);
    return old;
}

unsigned int
SourcePawnEngine2::GetAPIVersion() {
    return SOURCEPAWN_ENGINE2_API_VERSION;
}

const char*
SourcePawnEngine2::GetErrorString(int err) {
    return Environment::get()->GetErrorString(err);
}

bool
SourcePawnEngine2::InstallWatchdogTimer(size_t timeout_ms) {
    return Environment::get()->InstallWatchdogTimer(timeout_ms);
}

bool
SourcePawnEngine2::SetJitEnabled(bool enabled) {
    Environment::get()->SetJitEnabled(enabled);
    return Environment::get()->IsJitEnabled() == enabled;
}

bool
SourcePawnEngine2::IsJitEnabled() {
    return Environment::get()->IsJitEnabled();
}

void
SourcePawnEngine2::EnableProfiling() {
    Environment::get()->EnableProfiling();
}

void
SourcePawnEngine2::DisableProfiling() {
    Environment::get()->DisableProfiling();
}

void
SourcePawnEngine2::SetProfilingTool(IProfilingTool* tool) {
    Environment::get()->SetProfiler(tool);
}

ISourcePawnEnvironment*
SourcePawnEngine2::Environment() {
    return Environment::get();
}
