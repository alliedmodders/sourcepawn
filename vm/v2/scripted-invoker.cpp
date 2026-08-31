// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
//
#include "scripted-invoker.h"

#include <stdio.h>
#include <string.h>

#include <vector>

#include "environment.h"
#include "v2/method-info.h"
#include "v2/runtime.h"
#include "objects.h"

/********************
 * ScriptedInvoker  *
 ********************/

using namespace sp::v2;
using namespace SourcePawn;

ScriptedInvoker::ScriptedInvoker(Runtime* runtime, uint32_t method_index)
 : context_(runtime->context()),
   method_index_(method_index)
{
}

ScriptedInvoker::~ScriptedInvoker() {
}

bool
ScriptedInvoker::IsRunnable() {
    return !context_->IsPaused();
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
    Environment* env = context_->env();

    assert(!env->hasPendingException());

    if (!IsRunnable()) {
        env->ReportError(SP_ERROR_NOT_RUNNABLE);
        return false;
    }
    if (!AcquireMethod()->Validate()) {
        return false;
    }
    if (args.error) {
        env->ReportError(SP_ERROR_PARAMS_MAX);
        return false;
    }

    context_->EnterHeapScope();
    ke::ScopeGuard leave_heap_scope([this]() -> void {
        context_->LeaveHeapScope();
    });

    ke::SaveRestore<uint32_t> save_sp(env->sp());

    std::array<cell_t, SP_MAX_EXEC_PARAMS> params;
    assert(args.argc <= params.size());

    std::vector<Handle<SpArray>> retained_arrays;

    std::array<std::pair<void*, uint32_t>, SP_MAX_EXEC_PARAMS> cows;
    uint32_t ncows = 0;

    const auto& expected_arg_types = method_->arg_types();
    for (uint32_t i = 0; i < args.argc; i++) {
        const auto& arg = args.argv[i];

        // Simple case, no memory allocation needed.
        if (arg.type == CallArgs::ARG_CELL) {
            params[i] = arg.u.value;
            continue;
        }

        void* addr = nullptr;
        switch (arg.type) {
            case CallArgs::ARG_CELL_BY_REF: {
                params[i] = env->sp();
                if (!env->addStack(sizeof(cell_t)))
                    return false;
                addr = env->heap().ToPhysAddr<void*>(params[i]);
                *reinterpret_cast<cell_t*>(addr) = *reinterpret_cast<cell_t*>(arg.u.addr);
                break;
            }
            case CallArgs::ARG_INT64: {
                params[i] = env->sp();
                if (!env->addStack(sizeof(int64_t)))
                    return false;
                addr = env->heap().ToPhysAddr<void*>(params[i]);
                *reinterpret_cast<int64_t*>(addr) = arg.u.i64;
                break;
            }
            case CallArgs::ARG_ARRAY: {
                auto expected_td = expected_arg_types[i];
                if (expected_td->IsFlatArray()) {
                    uint32_t flat_bytes = expected_td->array_size() * sizeof(cell_t);

                    params[i] = env->sp();
                    if (!env->addStack(flat_bytes))
                        return false;

                    addr = env->heap().ToPhysAddr<void*>(params[i]);
                    uint32_t nbytes =
                        std::min<size_t>(arg.array_size * sizeof(cell_t), flat_bytes);
                    memcpy(addr, arg.u.addr, nbytes);
                } else {
                    auto elt_type = env->types()->GetPrimitive(TypeKind::Int32);
                    auto type = env->types()->GetArray(elt_type);
                    auto array = context_->NewArray(type, arg.array_size);
                    if (!array)
                        return false;

                    addr = context_->heap().ToPhysAddr<void*>(array->data);
                    memcpy(addr, arg.u.addr, arg.array_size * sizeof(cell_t));

                    params[i] = context_->heap().ToLocalAddr(array.get());
                    retained_arrays.emplace_back(std::move(array));
                }
                break;
            }
            case CallArgs::ARG_CHAR_ARRAY: {
                uint32_t max_size, nbytes;

                auto expected_td = expected_arg_types[i];
                if (expected_td->IsFlatArray()) {
                    uint32_t flat_bytes = expected_td->array_size() * sizeof(char);
                    uint32_t aligned_bytes =
                        (flat_bytes + sizeof(cell_t) - 1) & ~(sizeof(cell_t) - 1);

                    params[i] = env->sp();
                    if (!env->addStack(aligned_bytes))
                        return false;

                    addr = env->heap().ToPhysAddr<void*>(params[i]);
                    max_size = expected_td->array_size();
                    nbytes = std::min<size_t>(arg.array_size, flat_bytes);
                } else {
                    auto elt_type = env->types()->GetPrimitive(TypeKind::Char8);
                    auto type = env->types()->GetArray(elt_type);
                    auto array = context_->NewArray(type, arg.array_size);
                    if (!array)
                        return false;

                    addr = context_->heap().ToPhysAddr<void*>(array->data);
                    max_size = arg.array_size;
                    nbytes = arg.array_size;

                    params[i] = context_->heap().ToLocalAddr(array.get());
                    retained_arrays.emplace_back(std::move(array));
                }

                if (arg.flags & SM_PARAM_STRING_COPY) {
                    cell_t dest = context_->heap().ToLocalAddr(addr);
                    if (arg.flags & SM_PARAM_STRING_UTF8) {
                        context_->StringToLocalUTF8(dest, max_size,
                                                    reinterpret_cast<const char *>(arg.u.addr),
                                                    NULL);
                    } else if (arg.flags & SM_PARAM_STRING_BINARY) {
                        memcpy(addr, arg.u.addr, nbytes);
                    } else {
                        context_->StringToLocal(dest, max_size,
                                                reinterpret_cast<const char *>(arg.u.addr));
                    }
                } else {
                    *reinterpret_cast<char*>(addr) = 0;
                }
                break;
            }

            default:
                env->ReportError(SP_ERROR_PARAM);
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

    if (!context_->InvokeMethod(method_index_, params.data(), args.argc, result))
        return false;

    assert(!env->hasPendingException());

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

    return !env->hasPendingException();
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
    return context_;
}

const char* ScriptedInvoker::DebugName() {
    if (debug_name_.empty()) {
        auto image = context_->image();
        auto method = image->getRttiRow<smx_rtti_method>(image->rtti_methods(), method_index_);

        debug_name_ = context_->Name();
        debug_name_ += "::";
        debug_name_ += image->names() + method->name;
    }
    return debug_name_.c_str();
}

RefPtr<MethodInfo>
ScriptedInvoker::AcquireMethod() {
    if (!method_)
        method_ = context_->AcquireMethod(method_index_);
    return method_;
}

funcid_t ScriptedInvoker::GetFunctionID() {
    return (method_index_ << 1) | 1;
}
