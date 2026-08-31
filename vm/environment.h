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
#ifndef _include_sourcepawn_vm_environment_h_
#define _include_sourcepawn_vm_environment_h_

#include <memory>

#include <amtl/am-cxx.h>
#include <amtl/am-inlinelist.h>
#include <amtl/am-mutex.h>
#include <sp_vm_api.h>
#include "code-allocator.h"
#include "heap.h"
#include "legacy/plugin-runtime.h"
#include "stack-frames.h"
#include "type-cache.h"

namespace sp {

using namespace SourcePawn;

namespace v1 {
class BuiltinNatives;
class PluginRuntime;
class MethodInfo;
}
namespace v2 {
class Runtime;
class MethodInfo;
}

class CodeStubs;
class WatchdogTimer;
class ErrorReport;
struct CodeDebugMapping;
using CodeDebugMap = std::vector<CodeDebugMapping>;

#if defined(KE_LINUX)
class PerfJitFile;
class PerfJitdumpFile;
#endif

// An Environment encapsulates everything that's needed to load and run
// instances of plugins on a single thread. There can be at most one
// environment per thread.
//
// Currently, the VM is single threaded in that no more than one
// Environment can be created per process.
class Environment : public ISourcePawnEnvironment
{
  public:
    Environment();
    ~Environment();

    static Environment* New();

    // @brief Destroy the environment, releasing all resources and freeing
    // all plugin memory. This should not be called while plugins have
    // active code running on the stack.
    void Shutdown();

    uint32_t GetApiVersion() override { return kApiVersion; }

    // ISourcePawnEnvironment (from Engine/Engine2)
    void* AllocatePageMemory(size_t size) override;
    void SetReadWrite(void* ptr) override;
    void SetReadExecute(void* ptr) override;
    void FreePageMemory(void* ptr) override;

    // Access the current Environment.
    static Environment* get();

    bool InstallWatchdogTimer(int timeout_ms);

    void EnterExceptionHandlingScope(ExceptionHandler* handler) override;
    void LeaveExceptionHandlingScope(ExceptionHandler* handler) override;
    bool HasPendingException(const ExceptionHandler* handler) override;
    const char* GetPendingExceptionMessage(const ExceptionHandler* handler) override;
    int GetPendingExceptionCode(const ExceptionHandler* handler) override;

    /**
     * @brief Enables the line debugger callbacks. This must be called
     * before any plugins are loaded.
     */
    bool EnableDebugBreak();

    /**
     * @brief See JIT_DEBUG_* flags.
     * Must be set before any plugin code is executed.
     */
    void SetDebugMetadataFlags(int flags);
    void ClearPendingException(ExceptionHandler* handler) override;

    // Runtime functions.
    const char* GetErrorString(int err) override;
    void ReportError(int code);
    void ReportError(int code, const char* message);
    void ReportErrorFmt(int code, const char* message, ...);
    void ReportErrorVA(const char* fmt, va_list ap);
    void ReportErrorVA(int code, const char* fmt, va_list ap);
    void BlamePluginErrorVA(SourcePawn::IPluginFunction* pf, const char* fmt, va_list ap);

    // Engine2 methods that weren't in the interface but were in the implementation.
    const char* GetEngineName();
    const char* GetVersionString();
    IDebugListener* SetDebugListener(IDebugListener* listener);
    bool SetJitEnabled(bool enabled);
    void SetProfilingTool(IProfilingTool* tool);

    // Allocate and free executable memory.
    CodeChunk AllocateCode(size_t size);
    void WriteDebugMetadata(void* address, uint64_t length, const char* symbol,
                            const CodeDebugMap& mapping);

    CodeStubs* stubs() {
        return code_stubs_.get();
    }
    CodeStubs* EnsureStubs();
    v1::BuiltinNatives* builtins() {
        return builtins_.get();
    }

    // Runtime management.
    void RegisterRuntime(v1::PluginRuntime* rt);
    void DeregisterRuntime(v1::PluginRuntime* rt);
    void RegisterRuntime(v2::Runtime* rt);
    void DeregisterRuntime(v2::Runtime* rt);
    void PatchAllJumpsForTimeout();
    void UnpatchAllJumpsFromTimeout();
    ke::Mutex& lock() {
        return mutex_;
    }

    Heap& heap() { return heap_; }
    const RawHeapPtr<uint8_t[]>& stack() const { return stack_; }

    uint32_t& sp() { return sp_; }
    uint32_t sp_base() const { return sp_base_; }
    uint32_t sp_top() const { return sp_top_; }
    uint32_t* addressOfSp() { return &sp_; }
    uint32_t* addressOfSpBase() { return &sp_base_; }
    uint32_t* addressOfSpTop() { return &sp_top_; }
    bool addStack(uint32_t amount);
    bool dropStack(uint32_t amount);
    static inline size_t offsetOfSp() { return offsetof(Environment, sp_); }
    static inline size_t offsetOfSpBase() { return offsetof(Environment, sp_base_); }
    static inline size_t offsetOfSpTop() { return offsetof(Environment, sp_top_); }

    bool Invoke(v1::PluginRuntime* cx, const RefPtr<v1::MethodInfo>& method, cell_t* result);
    bool Invoke(v2::Runtime* cx, Handle<SpFunction> fn, uint32_t frm, cell_t* result);

    // Loading.
    BaseRuntime* LoadBinaryFromFile(const char* file, bool data_only = false);
    BaseRuntime* LoadBinaryFromMemory(const char* file, uint8_t* addr, size_t size,
                                      void (*dtor)(uint8_t*), bool data_only = false);

    // Helpers.
    void SetProfiler(IProfilingTool* profiler) {
        profiler_ = profiler;
    }
    IProfilingTool* profiler() const {
        return profiler_;
    }
    bool IsProfilingEnabled() const {
        return profiling_enabled_;
    }
    void EnableProfiling();
    void DisableProfiling();

    bool IsJitAllowed() const {
        return jit_allowed_;
    }
    void SetDebugger(IDebugListener* debugger) {
        debugger_ = debugger;
    }
    IDebugListener* debugger() const {
        return debugger_;
    }

    bool IsDebugBreakEnabled() const {
        return debug_break_enabled_;
    }
    int SetDebugBreakHandler(SPVM_DEBUGBREAK handler);
    SPVM_DEBUGBREAK debugbreak() const {
        return debug_break_handler_;
    }

    int GetDebugMetadataFlags() const {
        return debug_metadata_flags_;
    }

    WatchdogTimer* watchdog() const {
        return watchdog_timer_.get();
    }

    bool hasPendingException() const;
    void clearPendingException();
    int getPendingExceptionCode() const;

    // These are indicators used for the watchdog timer.
    uintptr_t FrameId() const { return frame_id_; }
    bool RunningCode() const { return !!top_; }

    void enterInvoke(InvokeFrame* frame);
    void leaveJitInvoke(JitInvokeFrame* frame);
    void leaveInvoke();

    InvokeFrame* top() const { return top_; }
    intptr_t* exit_fp() const { return exit_fp_; }

    bool spew_interp_ops() const {
        return spew_interp_ops_;
    }
    void set_spew_interp_ops(bool spew) {
        spew_interp_ops_ = spew;
    }

    TypeCache* types() { return &types_; }
    VirtMem& virt_mem() { return virt_mem_; }

  public:
    static inline size_t offsetOfTopFrame() {
        return offsetof(Environment, top_);
    }
    static inline size_t offsetOfExceptionCode() {
        return offsetof(Environment, exception_code_);
    }
    static inline size_t offsetOfExit() {
        return offsetof(Environment, exit_fp_);
    }

    void* addressOfExit() {
        return &exit_fp_;
    }
    void* addressOfExceptionCode() {
        return &exception_code_;
    }
    void DispatchDeferredReport();

  private:
    bool Initialize();

    void DispatchReport(const ErrorReport& report);

  private:
    std::unique_ptr<WatchdogTimer> watchdog_timer_;
    std::unique_ptr<v1::BuiltinNatives> builtins_;
    ke::Mutex mutex_;

    bool debug_break_enabled_;
    SPVM_DEBUGBREAK debug_break_handler_;

    IDebugListener* debugger_;
    ExceptionHandler* eh_top_;
    int exception_code_;
    char exception_message_[1024];
    char engine_name_[256];

    int debug_metadata_flags_;

#if defined(KE_LINUX)
    // There can only be one of each of these per process, as the filenames are
    // only distinguished by PID (although jitdump does internally support per-
    // thread metadata). Once we support multiple environments per process we'll
    // need to globalise these and add internal locking.
    std::unique_ptr<PerfJitFile> perf_jit_file_;
    std::unique_ptr<PerfJitdumpFile> perf_jitdump_file_;
#endif

    IProfilingTool* profiler_;
    bool jit_allowed_;
    bool profiling_enabled_;
    bool spew_interp_ops_ = false;

    std::unique_ptr<CodeAllocator> code_alloc_;
    std::unique_ptr<CodeStubs> code_stubs_;

    ke::InlineList<v1::PluginRuntime> v1_runtimes_;
    ke::InlineList<v2::Runtime> v2_runtimes_;

    uintptr_t frame_id_;

    InvokeFrame* top_;
    intptr_t* exit_fp_;

    // Global type cache.
    TypeCache types_;

    VirtMem virt_mem_;
    Heap heap_;
    RawHeapPtr<uint8_t[]> stack_;
    uint32_t sp_base_ = 0;
    uint32_t sp_top_ = 0;
    uint32_t sp_ = 0;
};

class EnterProfileScope
{
  public:
    EnterProfileScope(const char* group, const char* name) {
        if (Environment::get()->IsProfilingEnabled()) {
            Environment::get()->profiler()->EnterScope(group, name);
            scope_entered_ = true;
        }
    }

    ~EnterProfileScope() {
        if (scope_entered_ && Environment::get()->IsProfilingEnabled())
            Environment::get()->profiler()->LeaveScope();
    }

  private:
    bool scope_entered_ = false;
};

class ErrorReport : public SourcePawn::IErrorReport
{
  public:
    ErrorReport(int code, const char* message, BaseRuntime* cx, SourcePawn::IPluginFunction* pf);

  public: //IErrorReport
    const char* Message() const override;
    int Code() const override;
    IPluginFunction* Blame() const override;
    bool IsFatal() const override;
    IPluginContext* Context() const override;

  private:
    int code_;
    const char* message_;
    BaseRuntime* context_;
    IPluginFunction* blame_;
};

} // namespace sp

#endif // _include_sourcepawn_vm_environment_h_
