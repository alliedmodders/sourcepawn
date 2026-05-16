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
#include "scripted-invoker.h"

#include <stdio.h>
#include <string.h>

#include <utility>

#include "environment.h"
#include "method-info.h"
#include "plugin-runtime.h"

/********************
* FUNCTION CALLING*
********************/

using namespace sp;
using namespace SourcePawn;

ScriptedInvoker::ScriptedInvoker(PluginRuntime* runtime, funcid_t id, uint32_t pub_id)
 : env_(Environment::get()),
   context_(runtime),
   m_FnId(id)
{
    runtime->GetPublicByIndex(pub_id, &public_);

    size_t rt_len = strlen(runtime->Name());
    size_t len = rt_len + strlen("::") + strlen(public_->name);

    full_name_ = std::make_unique<char[]>(len + 1);
    strcpy(full_name_.get(), runtime->Name());
    strcpy(full_name_.get() + rt_len, "::");
    strcpy(full_name_.get() + rt_len + 2, public_->name);
}

ScriptedInvoker::~ScriptedInvoker() {
}

bool
ScriptedInvoker::IsRunnable() {
    return !context_->runtime()->IsPaused();
}

IPluginContext*
ScriptedInvoker::GetParentContext() {
    return context_;
}

int ScriptedInvoker::PushCell(cell_t cell) {
    default_args_.PushCell(cell);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

int ScriptedInvoker::PushCellByRef(cell_t* cell, int flags) {
    default_args_.PushCellByRef(cell, flags);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

int ScriptedInvoker::PushFloat(float number) {
    default_args_.PushFloat(number);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

int ScriptedInvoker::PushFloatByRef(float* number, int flags) {
    default_args_.PushFloatByRef(number, flags);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

int ScriptedInvoker::PushArray(cell_t* inarray, unsigned int cells, int copyback) {
    default_args_.PushArray(inarray, cells, copyback ? SM_PARAM_COPYBACK : 0);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

int ScriptedInvoker::PushInt64(int64_t value) {
    default_args_.PushInt64(value);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

int ScriptedInvoker::PushString(const char* string) {
    default_args_.PushString(string);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

int ScriptedInvoker::PushStringEx(char* buffer, size_t length, int sz_flags, int cp_flags) {
    default_args_.PushString(buffer, length, sz_flags | cp_flags);
    return default_args_.error ? SP_ERROR_PARAMS_MAX : SP_ERROR_NONE;
}

void ScriptedInvoker::Cancel() {
    default_args_.Reset();
}

bool ScriptedInvoker::Invoke(const CallArgs& args, cell_t* result) {
    assert(!env_->hasPendingException());

    if (!IsRunnable()) {
        env_->ReportError(SP_ERROR_NOT_RUNNABLE);
        return false;
    }
    if (args.error) {
        env_->ReportError(SP_ERROR_PARAMS_MAX);
        return false;
    }

    context_->EnterHeapScope();
    ke::ScopeGuard leave_heap_scope([this]() -> void {
        context_->LeaveHeapScope();
    });

    std::array<cell_t, SP_MAX_EXEC_PARAMS> params;
    assert(args.argc <= params.size());

    std::array<std::pair<void*, uint32_t>, SP_MAX_EXEC_PARAMS> cows;
    uint32_t ncows = 0;

    for (uint32_t i = 0; i < args.argc; i++) {
        const auto& arg = args.argv[i];

        // Simple case, no memory allocation needed.
        if (arg.type == CallArgs::ARG_CELL) {
            params[i] = arg.u.value;
            continue;
        }

        void* addr = nullptr;
        uint32_t nbytes = 0;
        switch (arg.type) {
            case CallArgs::ARG_CELL_BY_REF: {
                nbytes = sizeof(cell_t);
                if ((addr = context_->heapAllocEx(nbytes, &params[i])) == nullptr)
                    return false;
                *reinterpret_cast<cell_t*>(addr) = *reinterpret_cast<cell_t*>(arg.u.addr);
                break;
            }
            case CallArgs::ARG_INT64: {
                nbytes = sizeof(int64_t);
                if ((addr = context_->heapAllocEx(nbytes, &params[i])) == nullptr)
                    return false;
                *reinterpret_cast<int64_t*>(addr) = *reinterpret_cast<int64_t*>(arg.u.addr);
                break;
            }
            case CallArgs::ARG_ARRAY: {
                nbytes = arg.array_size * sizeof(cell_t);
                int err = context_->AllocArray(nbytes, &params[i],
                                               reinterpret_cast<cell_t**>(&addr));
                if (err != SP_ERROR_NONE) {
                    env_->ReportError(err);
                    return false;
                }
                memcpy(addr, arg.u.addr, arg.array_size * sizeof(cell_t));
                break;
            }
            case CallArgs::ARG_CHAR_ARRAY: {
                uint32_t ncells = (arg.array_size + sizeof(cell_t) - 1) / sizeof(cell_t);
                nbytes = ncells * sizeof(cell_t);

                int err = context_->AllocArray(nbytes, &params[i],
                                               reinterpret_cast<cell_t**>(&addr));
                if (err != SP_ERROR_NONE) {
                    env_->ReportError(err);
                    return false;
                }

                if (arg.flags & SM_PARAM_STRING_COPY) {
                    if (arg.flags & SM_PARAM_STRING_UTF8) {
                        context_->StringToLocalUTF8(params[i], arg.array_size,
                                                    reinterpret_cast<const char *>(arg.u.addr),
                                                    NULL);
                    } else if (arg.flags & SM_PARAM_STRING_BINARY) {
                        memcpy(addr, arg.u.addr, arg.array_size);
                    } else {
                        context_->StringToLocal(params[i], arg.array_size,
                                                reinterpret_cast<const char *>(arg.u.addr));
                    }
                } else {
                    *reinterpret_cast<cell_t*>(addr) = 0;
                }
                break;
            }
            default:
                env_->ReportError(SP_ERROR_PARAM);
                return false;
        }

        if (arg.flags & SM_PARAM_COPYBACK)
            cows[ncows++] = std::pair<void*, uint32_t>{addr, i};
    }

    {
        const char* debugName = this->DebugName();
        size_t debugNameLength = strlen(debugName) + 2;
        volatile char* volatile debugNameForCrashDumps = (char*)alloca(debugNameLength);
        SafeStrcpy((char*)debugNameForCrashDumps + 1, debugNameLength - 1, debugName);
    }

    if (!context_->Invoke(m_FnId, params.data(), args.argc, result))
        return false;

    assert(!env_->hasPendingException());

    for (uint32_t i = 0; i < ncows; i++) {
        void* src = cows[i].first;
        const auto& arg = args.argv[cows[i].second];

        switch (arg.type) {
            case CallArgs::ARG_CELL_BY_REF:
                *reinterpret_cast<cell_t*>(arg.u.addr) = *reinterpret_cast<cell_t*>(src);
                break;
            case CallArgs::ARG_INT64:
                *reinterpret_cast<int64_t*>(arg.u.addr) = *reinterpret_cast<int64_t*>(src);
                break;
            case CallArgs::ARG_ARRAY:
                memcpy(arg.u.addr, src, arg.array_size * sizeof(cell_t));
                break;
            case CallArgs::ARG_CHAR_ARRAY:
                memcpy(arg.u.addr, src, arg.array_size);
                break;
            default:
                assert(false);
        }
    }

    return !env_->hasPendingException();
}

int ScriptedInvoker::Execute(cell_t* result) {
    Environment* env = Environment::get();
    env->clearPendingException();

    // For backward compatibility, we have to clear the exception state.
    // Otherwise code like this:
    //
    // static cell_t native(cx, params) {
    //   for (auto callback : callbacks) {
    //     callback->Execute();
    //   }
    // }
    //
    // Could unintentionally leak a pending exception back to the caller,
    // which wouldn't have happened before the Great Exception Refactoring.
    ExceptionHandler eh(context_);
    if (!Invoke(result)) {
        assert(env->hasPendingException());
        return env->getPendingExceptionCode();
    }

    return SP_ERROR_NONE;
}

bool ScriptedInvoker::Invoke(cell_t* result) {
    bool ok = Invoke(default_args_, result);
    default_args_.Reset();
    return ok;
}

IPluginRuntime* ScriptedInvoker::GetParentRuntime() {
    return context_->runtime();
}

funcid_t ScriptedInvoker::GetFunctionID() {
    return m_FnId;
}

RefPtr<MethodInfo> ScriptedInvoker::AcquireMethod() {
    if (!method_)
        method_ = context_->runtime()->AcquireMethod(public_->code_offs);
    return method_;
}
