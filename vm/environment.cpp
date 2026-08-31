// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2006-2026 AlliedModders LLC
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
#include "api.h"
#include "code-stubs.h"
#include "compiled-function.h"
#include "debug-metadata.h"
#include "legacy/method-info.h"
#include "legacy/plugin-runtime.h"
#include "v2/method-info.h"
#include "v2/runtime.h"
#include "watchdog_timer.h"
#if defined(SP_HAS_JIT)
#    include "legacy/jit.h"
#    include "v2/jit.h"
#endif
#include <stdarg.h>
#include "legacy/builtins.h"
#include "debugging.h"
#include "legacy/interpreter.h"
#include "v2/interp/interpreter.h"

using namespace sp;
using namespace SourcePawn;

static Environment* sEnvironment = nullptr;

Environment::Environment()
 : debug_break_enabled_(false),
   debug_break_handler_(nullptr),
   debugger_(nullptr),
   eh_top_(nullptr),
   exception_code_(SP_ERROR_NONE),
   debug_metadata_flags_(JIT_DEBUG_DELETE_ON_EXIT | JIT_DEBUG_PERF_BASIC),
   profiler_(nullptr),
   profiling_enabled_(false),
   code_stubs_(nullptr),
   top_(nullptr)
{
    jit_enabled_ = IsJitAvailable();
}

Environment::~Environment() {
}

Environment*
Environment::New() {
    assert(!sEnvironment);
    if (sEnvironment)
        return nullptr;

    sEnvironment = new Environment();
    if (!sEnvironment->Initialize()) {
        delete sEnvironment;
        sEnvironment = nullptr;
        return nullptr;
    }

    return sEnvironment;
}

Environment*
Environment::get() {
    return sEnvironment;
}

bool
Environment::Initialize() {
    watchdog_timer_ = std::make_unique<WatchdogTimer>(this);
    builtins_ = std::make_unique<v1::BuiltinNatives>();
    code_alloc_ = std::make_unique<CodeAllocator>();

    if (!builtins_->Initialize())
        return false;

    return true;
}

void
Environment::Shutdown() {
    watchdog_timer_->Shutdown();
    builtins_ = nullptr;
    code_stubs_ = nullptr;
    code_alloc_ = nullptr;

    assert(sEnvironment == this);
    sEnvironment = nullptr;
}

bool Environment::SetJitEnabled(bool enabled) {
    jit_enabled_ = enabled && IsJitAvailable();
    return jit_enabled_ == enabled;
}

bool
Environment::EnableDebugBreak() {
    // Can't change this after any plugins are loaded.
    if (!v1_runtimes_.empty() || !v2_runtimes_.empty())
        return false;

    debug_break_enabled_ = true;
    return true;
}

void
Environment::SetDebugMetadataFlags(int flags) {
    debug_metadata_flags_ = flags;
}

void
Environment::EnableProfiling() {
    profiling_enabled_ = !!profiler_;
}

void
Environment::DisableProfiling() {
    profiling_enabled_ = false;
}

bool
Environment::InstallWatchdogTimer(int timeout_ms) {
    return watchdog_timer_->Initialize(timeout_ms);
}

static const char* sErrorMsgTable[] = {
    NULL,
    "Unrecognizable file format",
    "Decompressor was not found",
    "Not enough space on the heap",
    "Invalid parameter or parameter type",
    "Invalid plugin address",
    "Object or index not found",
    "Invalid index or index not found",
    "Not enough space on the stack",
    "Debug section not found or debug not enabled",
    "Invalid instruction",
    "Invalid memory access",
    "Stack went below stack boundary",
    "Heap went below heap boundary",
    "Divide by zero",
    "Array index is out of bounds",
    "Instruction contained invalid parameter",
    "Stack memory leaked by native",
    "Heap memory leaked by native",
    "Dynamic array is too big",
    "Tracker stack is out of bounds",
    "Native is not bound",
    "Maximum number of parameters reached",
    "Native detected error",
    "Plugin not runnable",
    "Call was aborted",
    "Plugin format is too old",
    "Plugin format is too new",
    "Out of memory",
    "Integer overflow",
    "Script execution timed out",
    "Custom error",
    "Fatal error",
    "Invalid array size",
};

const char*
Environment::GetErrorString(int error) {
    if (error < 1 || error > int(sizeof(sErrorMsgTable) / sizeof(sErrorMsgTable[0])))
        return NULL;
    return sErrorMsgTable[error];
}

CodeChunk
Environment::AllocateCode(size_t size) {
    return code_alloc_->Allocate(size);
}

void
Environment::WriteDebugMetadata(void* address, uint64_t length, const char* symbol,
                                const CodeDebugMap& mapping) {
    // Some other debug info consumers we might want to implement here:
    //
    // * Intel VTune
    //   https://github.com/intel/ittapi
    //   Would give us profiling coverage on non-Linux.
    //   Very similar input (and use case) as perf, requires linking Intel code in though.
    //
    // * GDB JIT Interface
    //   https://sourceware.org/gdb/current/onlinedocs/gdb/JIT-Interface.html
    //   Lets GDB show JIT frames when debugging, with source info.
    //   Requires generating full ELF + DWARF objects in memory.

#if defined(KE_LINUX) && defined(SP_HAS_JIT)
    if (!perf_jit_file_ && (debug_metadata_flags_ & JIT_DEBUG_PERF_BASIC) != 0) {
        perf_jit_file_ =
            std::make_unique<PerfJitFile>((debug_metadata_flags_ & JIT_DEBUG_DELETE_ON_EXIT) != 0);
    }

    if (perf_jit_file_) {
        perf_jit_file_->Write(address, length, symbol);
    }

    if (!perf_jitdump_file_ && (debug_metadata_flags_ & JIT_DEBUG_PERF_JITDUMP) != 0) {
        perf_jitdump_file_ = std::make_unique<PerfJitdumpFile>(
            (debug_metadata_flags_ & JIT_DEBUG_DELETE_ON_EXIT) != 0);
    }

    if (perf_jitdump_file_) {
        perf_jitdump_file_->Write(address, length, symbol, mapping);
    }
#endif
}

void
Environment::RegisterRuntime(v1::PluginRuntime* rt) {
    mutex_.AssertCurrentThreadOwns();
    v1_runtimes_.append(rt);
}

void
Environment::DeregisterRuntime(v1::PluginRuntime* rt) {
    mutex_.AssertCurrentThreadOwns();
    v1_runtimes_.remove(rt);
}

void
Environment::RegisterRuntime(v2::Runtime* rt) {
    mutex_.AssertCurrentThreadOwns();
    v2_runtimes_.append(rt);
}

void
Environment::DeregisterRuntime(v2::Runtime* rt) {
    mutex_.AssertCurrentThreadOwns();
    v2_runtimes_.remove(rt);
}

static inline void
SwapLoopEdge(uint8_t* code, LoopEdge& e) {
    int32_t* loc = reinterpret_cast<int32_t*>(code + e.offset - 4);
    int32_t new_disp32 = e.disp32;
    e.disp32 = *loc;
    *loc = new_disp32;
}

void
Environment::PatchAllJumpsForTimeout() {
    mutex_.AssertCurrentThreadOwns();
    for (auto rt : v1_runtimes_) {
        for (const auto& method : rt->AllMethods()) {
            CompiledFunction* fun = method->jit();
            if (!fun)
                continue;

            uint8_t* base = reinterpret_cast<uint8_t*>(fun->GetEntryAddress());
            for (size_t j = 0; j < fun->NumLoopEdges(); j++)
                SwapLoopEdge(base, fun->GetLoopEdge(j));
        }
    }
    for (auto rt : v2_runtimes_) {
        for (const auto& method : rt->AllMethods()) {
            if (!method)
                continue;
            CompiledFunction* fun = method->jit();
            if (!fun)
                continue;

            uint8_t* base = reinterpret_cast<uint8_t*>(fun->GetEntryAddress());
            for (size_t j = 0; j < fun->NumLoopEdges(); j++)
                SwapLoopEdge(base, fun->GetLoopEdge(j));
        }
    }
}

void
Environment::UnpatchAllJumpsFromTimeout() {
    mutex_.AssertCurrentThreadOwns();
    for (auto rt : v1_runtimes_) {
        for (const auto& method : rt->AllMethods()) {
            CompiledFunction* fun = method->jit();
            if (!fun)
                continue;

            uint8_t* base = reinterpret_cast<uint8_t*>(fun->GetEntryAddress());
            for (size_t j = 0; j < fun->NumLoopEdges(); j++)
                SwapLoopEdge(base, fun->GetLoopEdge(j));
        }
    }
    for (auto rt : v2_runtimes_) {
        for (const auto& method : rt->AllMethods()) {
            if (!method)
                continue;
            CompiledFunction* fun = method->jit();
            if (!fun)
                continue;

            uint8_t* base = reinterpret_cast<uint8_t*>(fun->GetEntryAddress());
            for (size_t j = 0; j < fun->NumLoopEdges(); j++)
                SwapLoopEdge(base, fun->GetLoopEdge(j));
        }
    }
}

bool Environment::Invoke(v1::PluginContext* cx, const RefPtr<v1::MethodInfo>& method, cell_t* result) {
#if defined(SP_HAS_JIT)
    if (jit_enabled_) {
        if (!code_stubs_) {
            code_stubs_ = std::make_unique<CodeStubs>(this);

            // We delay initializing this to here to avoid executing any generated code if the embedder
            // doesn't want the JIT enabled. The debug metadata flags must be set before this point.
            if (!code_stubs_->Initialize()) {
                code_stubs_ = nullptr;
                return false;
            }
        }

        if (v1::CompilerBase::SupportsPlugin(cx) && !method->jit()) {
            int err = SP_ERROR_NONE;
            if (!v1::CompilerBase::Compile(cx, method, &err)) {
                cx->ReportErrorNumber(err);
                return false;
            }
        }

        if (CompiledFunction* fn = method->jit()) {
            JitInvokeFrame ivkframe(cx, fn->GetCodeOffset());

            assert(top_ && top_->cx() == cx);

            InvokeStubV1Fn invoke = code_stubs_->InvokeStubV1();
            invoke(cx, fn->GetEntryAddress(), result);

            return exception_code_ == SP_ERROR_NONE;
        }
    }
#endif

    // The JIT performs its own validation. Handle the interpreter here.
    {
        if (!method->Validate())
            return false;
    }

    return v1::Interpreter::Run(cx, method, result);
}

bool Environment::Invoke(v2::Runtime* cx, const RefPtr<v2::MethodInfo>& method, cell_t* result) {
#if defined(SP_HAS_JIT)
    if (jit_enabled_) {
        if (!code_stubs_) {
            code_stubs_ = std::make_unique<CodeStubs>(this);

            // We delay initializing this to here to avoid executing any generated code if the embedder
            // doesn't want the JIT enabled. The debug metadata flags must be set before this point.
            if (!code_stubs_->Initialize()) {
                code_stubs_ = nullptr;
                return false;
            }
        }

        if (v2::CompilerBase::SupportsPlugin(cx) && !method->jit()) {
            int err = SP_ERROR_NONE;
            if (!v2::CompilerBase::Compile(cx, method, &err)) {
                cx->ReportErrorNumber(err);
                return false;
            }
        }

        if (CompiledFunction* fn = method->jit()) {
            JitInvokeFrame ivkframe(cx, fn->GetCodeOffset());

            assert(top_ && top_->cx() == cx);

            InvokeStubV2Fn invoke = code_stubs_->InvokeStubV2();
            invoke(cx, fn->GetEntryAddress(), result);

            return exception_code_ == SP_ERROR_NONE;
        }
    }
#endif

    // The JIT performs its own validation. Handle the interpreter here.
    {
        if (!method->Validate())
            return false;
    }

    return v2::Interpreter::Run(cx, method, result);
}

static BaseRuntime* LoadImage(std::unique_ptr<SmxImage> image, const char* file,
                                    bool data_only)
{
    if (!image->validate())
        return nullptr;

    std::unique_ptr<BaseRuntime> pRuntime;
    if (image->hdr()->version < SmxConsts::SP_VERSION_2) {
        pRuntime = std::make_unique<sp::v1::PluginRuntime>(image.release());
    } else {
        pRuntime = std::make_unique<sp::v2::Runtime>(image.release(), data_only);
    }

    ExceptionHandler eh(Environment::get());
    if (!pRuntime->Initialize()) {
        if (!eh.HasException())
            Environment::get()->ReportError(SP_ERROR_OUT_OF_MEMORY);

        eh.Rethrow();
        return nullptr;
    }

    assert(!eh.HasException());

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

    if (!data_only && !pRuntime->CallGlobalCtor())
        return nullptr;

    return pRuntime.release();
}

BaseRuntime*
Environment::LoadBinaryFromFile(const char* file, bool data_only) {
    FILE* fp = fopen(file, "rb");
    if (!fp) {
        ReportError(SP_ERROR_NOT_FOUND, "could not open file");
        return nullptr;
    }

    auto image = std::make_unique<SmxImage>(fp);
    return LoadImage(std::move(image), file, data_only);
}

BaseRuntime*
Environment::LoadBinaryFromMemory(const char* file, uint8_t* addr, size_t size,
                                  void (*dtor)(uint8_t*), bool data_only) {
    std::unique_ptr<SmxImage> image;
    if (dtor)
        image = std::make_unique<SmxImage>(addr, size, dtor);
    else
        image = std::make_unique<SmxImage>(addr, size);
    return LoadImage(std::move(image), file, data_only);
}

void
Environment::ReportError(int code) {
    const char* message = GetErrorString(code);
    if (!message) {
        char buffer[255];
        UTIL_Format(buffer, sizeof(buffer), "Unknown error code %d", code);
        ReportError(code, buffer);
    } else {
        ReportError(code, message);
    }
}

ErrorReport::ErrorReport(int code, const char* message, BaseRuntime* cx, IPluginFunction* pf)
 : code_(code),
   message_(message),
   context_(cx),
   blame_(pf)
{
}

const char*
ErrorReport::Message() const {
    return message_;
}

IPluginFunction*
ErrorReport::Blame() const {
    return blame_;
}

bool
ErrorReport::IsFatal() const {
    switch (code_) {
        case SP_ERROR_HEAPLOW:
        case SP_ERROR_INVALID_ADDRESS:
        case SP_ERROR_STACKLOW:
        case SP_ERROR_INVALID_INSTRUCTION:
        case SP_ERROR_MEMACCESS:
        case SP_ERROR_STACKMIN:
        case SP_ERROR_HEAPMIN:
        case SP_ERROR_INSTRUCTION_PARAM:
        case SP_ERROR_STACKLEAK:
        case SP_ERROR_HEAPLEAK:
        case SP_ERROR_TRACKER_BOUNDS:
        case SP_ERROR_PARAMS_MAX:
        case SP_ERROR_ABORTED:
        case SP_ERROR_OUT_OF_MEMORY:
        case SP_ERROR_FATAL:
            return true;
        default:
            return false;
    }
}

IPluginContext*
ErrorReport::Context() const {
    return context_;
}

int
ErrorReport::Code() const {
    return code_;
}

void
Environment::ReportErrorVA(const char* fmt, va_list ap) {
    ReportErrorVA(SP_ERROR_USER, fmt, ap);
}

void
Environment::ReportErrorVA(int code, const char* fmt, va_list ap) {
    // :TODO: right-size the string rather than rely on this buffer.
    char buffer[1024];
    UTIL_FormatVA(buffer, sizeof(buffer), fmt, ap);
    ReportError(code, buffer);
}

void
Environment::ReportErrorFmt(int code, const char* message, ...) {
    va_list ap;
    va_start(ap, message);
    ReportErrorVA(code, message, ap);
    va_end(ap);
}

void
Environment::ReportError(int code, const char* message) {
    ErrorReport report(code, message, top_ ? top_->cx() : nullptr, nullptr);
    DispatchReport(report);
}

void
Environment::BlamePluginErrorVA(SourcePawn::IPluginFunction* pf, const char* fmt, va_list ap) {
    // :TODO: right-size the string rather than rely on this buffer.
    char buffer[1024];
    UTIL_FormatVA(buffer, sizeof(buffer), fmt, ap);
    ErrorReport report(SP_ERROR_USER, buffer, top_ ? top_->cx() : nullptr, pf);
    DispatchReport(report);
}

void
Environment::DispatchReport(const ErrorReport& report) {
    FrameIterator iter;

    // If this fires, someone forgot to propagate an error.
    assert(!hasPendingException());

    // Save the exception state.
    if (eh_top_) {
        exception_code_ = report.Code();
        UTIL_Format(exception_message_, sizeof(exception_message_), "%s", report.Message());
    }

    // For now, we always report exceptions even if they might be handled.
    if (debugger_)
        debugger_->ReportError(report, iter);

    // See if the plugin is being debugged
    if (top_)
        InvokeDebugger(top_->cx(), &report);
}

void
Environment::EnterExceptionHandlingScope(ExceptionHandler* handler) {
    handler->next_ = eh_top_;
    eh_top_ = handler;
}

void
Environment::LeaveExceptionHandlingScope(ExceptionHandler* handler) {
    assert(handler == eh_top_);
    eh_top_ = eh_top_->next_;

    // To preserve compatibility with older API, we clear the exception state
    // when there is no EH handler.
    if (!eh_top_ || handler->catch_)
        exception_code_ = SP_ERROR_NONE;
}

bool
Environment::HasPendingException(const ExceptionHandler* handler) {
    // Note here and elsewhere - this is not a sanity assert. In the future, the
    // API may need to query the handler.
    assert(handler == eh_top_);
    return hasPendingException();
}

const char*
Environment::GetPendingExceptionMessage(const ExceptionHandler* handler) {
    // Note here and elsewhere - this is not a sanity assert. In the future, the
    // API may need to query the handler.
    assert(handler == eh_top_);
    assert(HasPendingException(handler));
    return exception_message_;
}

int Environment::GetPendingExceptionCode(const ExceptionHandler* handler) {
    assert(handler == eh_top_);
    assert(HasPendingException(handler));
    return exception_code_;
}

bool Environment::hasPendingException() const {
    return exception_code_ != SP_ERROR_NONE;
}

void Environment::clearPendingException() {
    exception_code_ = SP_ERROR_NONE;
}

void Environment::ClearPendingException(ExceptionHandler* handler) {
    assert(handler == eh_top_);
    clearPendingException();
}

int Environment::getPendingExceptionCode() const {
    return exception_code_;
}

void Environment::enterInvoke(InvokeFrame* frame) {
    if (!top_)
        frame_id_++;
    top_ = frame;
}

void Environment::leaveJitInvoke(JitInvokeFrame* frame) {
    assert(frame == top_);
    exit_fp_ = frame->prev_exit_fp();
}

void Environment::leaveInvoke() {
    top_ = top_->prev();
}

bool Environment::IsJitAvailable() {
#if defined(SP_HAS_JIT)
    return v1::CompilerBase::IsSupported();
#else
    return false;
#endif
}

void* Environment::AllocatePageMemory(size_t size) {
    CodeChunk chunk = AllocateCode(size + sizeof(CodeChunk));
    CodeChunk* hidden = (CodeChunk*)chunk.address();
    new (hidden) CodeChunk(chunk);
    return hidden + 1;
}

void Environment::SetReadExecute(void* ptr) {
    /* already re */
}

void Environment::SetReadWrite(void* ptr) {
    /* already rw */
}

void Environment::FreePageMemory(void* ptr) {
    assert(ptr);
    CodeChunk* hidden = (CodeChunk*)((uint8_t*)ptr - sizeof(CodeChunk));
    hidden->~CodeChunk();
}

IDebugListener* Environment::SetDebugListener(IDebugListener* pListener) {
    IDebugListener* old = debugger_;
    SetDebugger(pListener);
    return old;
}

int Environment::SetDebugBreakHandler(SPVM_DEBUGBREAK handler) {
    if (!IsDebugBreakEnabled())
        return SP_ERROR_NOTDEBUGGING;

    debug_break_handler_ = handler;
    return SP_ERROR_NONE;
}

#if !defined(SOURCEPAWN_VERSION)
#    define SOURCEPAWN_VERSION "SourcePawn 1.10"
#endif

const char* Environment::GetEngineName() {
    const char* info = "";
#if !defined(SP_HAS_JIT)
    info = ", interp-x86";
#else
    if (!IsJitEnabled()) {
        info = ", interp-x86";
    } else {
#    if defined(KE_ARCH_X86)
        info = ", jit-x86";
#    else
        info = ", unknown";
#    endif
    }
#endif

    ke::SafeSprintf(engine_name_, sizeof(engine_name_), "%s%s", SOURCEPAWN_VERSION, info);
    return engine_name_;
}

const char* Environment::GetVersionString() {
    return SOURCEPAWN_VERSION;
}

void Environment::SetProfilingTool(IProfilingTool* tool) {
    SetProfiler(tool);
}
