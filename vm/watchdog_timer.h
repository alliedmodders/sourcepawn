// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_watchdog_timer_posix_h_
#define _include_sourcepawn_watchdog_timer_posix_h_

#include <stddef.h>
#include <stdint.h>

#include <condition_variable>
#include <thread>

namespace SourcePawn {
typedef class IPluginRuntime IPluginContext;
class IErrorReport;
} // namespace SourcePawn

namespace sp {

class Environment;

typedef bool (*WatchdogCallback)();

class WatchdogTimer
{
    // Allow line debugger callback to disable timeouts.
    friend int InvokeDebugger(SourcePawn::IPluginContext* ctx,
                              const SourcePawn::IErrorReport* report);

  public:
    WatchdogTimer(Environment* env);
    ~WatchdogTimer();

    bool Initialize(size_t timeout_ms);
    void Shutdown();

    // Called from main thread.
    bool NotifyTimeoutReceived();
    bool HandleInterrupt();

  private:
    // Watchdog thread.
    void Run();

  private:
    Environment* env_;

    bool terminate_;
    size_t timeout_ms_;
    std::thread::id mainthread_;
    bool ignore_timeout_;

    std::unique_ptr<std::thread> thread_;
    std::mutex mutex_;
    std::condition_variable cv_;

    // Accessed only on the watchdog thread.
    uintptr_t last_frame_id_;
    bool second_timeout_;

    // Accessed only on the main thread.
    bool timedout_;
};

} // namespace sp

#endif // _include_sourcepawn_watchdog_timer_posix_h_
