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
#include "v2/runtime.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>

#include <deque>
#include <unordered_set>

#include <amtl/am-bits.h>
#include <smx/smx-v2-opcodes.h>
#include "legacy/builtins.h"
#include "compiled-function.h"
#include "environment.h"
#include "md5/md5.h"
#include "objects.h"
#include "runtime-helpers.h"
#include "v2/method-info.h"
#include "v2/method-verifier.h"
#include "watchdog_timer.h"

namespace sp {
namespace v2 {

using namespace SourcePawn;

Runtime::Runtime(std::shared_ptr<SmxImage> image, bool data_only)
 : BaseRuntime(std::move(image)),
   env_(Environment::get()),
   heap_(env_->virt_mem()),
   data_only_(data_only)
{

    std::lock_guard<ke::Mutex> lock(env_->lock());
    env_->RegisterRuntime(this);
}

Runtime::~Runtime() {
    for (size_t i = 0; i < global_vars_.size(); i++) {
        if (!global_vars_[i].td->IsHeapItem())
            continue;
        cell_t* slot = heap_.ToPhysAddr<cell_t*>(global_vars_[i].addr);
        if (auto obj = heap_.ToPhysAddr<HeapItem*>(*slot))
            obj->Release();
    }
    for (size_t i = 0; i < string_addrs_.size(); i++) {
        auto array = heap_.ToPhysAddr<SpArray*>(string_addrs_[i]);
        if (array)
            array->Release();
    }

    // The watchdog thread takes the global JIT lock while it patches all
    // runtimes. It is not enough to ensure that the unlinking of the runtime is
    // protected; we cannot delete functions or code while the watchdog might be
    // executing. Therefore, the entire destructor is guarded.
    std::lock_guard<ke::Mutex> lock(env_->lock());

    env_->DeregisterRuntime(this);

    assert(heap_.IsEmpty());
}

bool Runtime::Initialize() {
    for (size_t i = 0; i < image_->rtti_methods()->row_count; i++) {
        auto method = image_->getRttiRow<smx_rtti_method>(image_->rtti_methods(), i);

        if (method->flags & kRttiMethod_Native) {
            uint32_t native_index = (uint32_t)natives_.size();
            native_map_[i] = native_index;

            NativeEntry native;
            native.name = image_->names() + method->name;
            native.status = SP_NATIVE_UNBOUND;
            native.flags = 0;
            native.user = nullptr;
            natives_.emplace_back(native);
            continue;
        }

        uint8_t visibility = method->flags & kRttiMethodVisibilityMask;
        if (visibility != kRttiMethodVisibility_Public)
            continue;

        sp_public_t pb = {
            funcid_t((i << 1) | 1),
            method->pcode_start,
            image_->names() + method->name
        };
        publics_.emplace_back(pb);
    }
    natives_.shrink_to_fit();
    publics_.shrink_to_fit();

    std::sort(publics_.begin(), publics_.end(),
              [](const sp_public_t& a, const sp_public_t& b) -> bool
    {
        return strcmp(a.name, b.name) < 0;
    });

    if (!InitializeContext())
        return false;
    if (!InitializeGlobals())
        return false;

    return true;
}

bool Runtime::InitializeContext() {
    if (!heap_.Initialize())
        return false;

    return true;
}

bool Runtime::InitializeGlobals() {
    uint32_t num_globals = 0;
    if (image_->rtti_globals())
        num_globals = image_->rtti_globals()->row_count;

    uint32_t global_bytes = 0;

    global_vars_ = ke::FixedArray<GlobalDesc>(num_globals);
    for (uint32_t i = 0; i < num_globals; i++) {
        auto global = image_->getRttiRow<smx_rtti_global>(image_->rtti_globals(), i);
        assert(global);

        auto td = LoadTypeFromId(global->type_id);
        if (!td)
            return false;
        global_vars_[i].td = td;

        if (!ke::IsUint32AddSafe(global_bytes, td->slot_size())) {
            ReportErrorNumber(SP_ERROR_OUT_OF_MEMORY);
            return false;
        }
        global_bytes += td->slot_size();
    }

    global_buffer_ = heap_.MakeRawPtr<uint8_t[]>(global_bytes);
    if (!global_buffer_) {
        ReportErrorNumber(SP_ERROR_OUT_OF_MEMORY);
        return false;
    }

    uint32_t next_global_addr = heap_.ToLocalAddr(global_buffer_.get());
    for (uint32_t i = 0; i < num_globals; i++) {
        global_vars_[i].addr = next_global_addr;
        next_global_addr += global_vars_[i].td->slot_size();

        auto global = image_->getRttiRow<smx_rtti_global>(image_->rtti_globals(), i);
        if ((global->flags & kRttiGlobal_VisibilityMask) == kRttiGlobal_Public) {
            PubvarEntry entry;
            entry.pubvar.name = image_->names() + global->name;
            entry.global_index = i;
            entry.pubvar.offs = heap_.ToPhysAddr<cell_t*>(global_vars_[i].addr);
            entry.local_addr = global_vars_[i].addr;
            pubvars_.push_back(entry);
        }
    }

    std::sort(pubvars_.begin(), pubvars_.end(),
              [](const PubvarEntry& a, const PubvarEntry& b) -> bool {
                  return strcmp(a.pubvar.name, b.pubvar.name) < 0;
              });

    pubvars_.shrink_to_fit();

    uint32_t num_strings = 0;
    if (image_->rtti_stringpool())
        num_strings = image_->rtti_stringpool()->row_count;

    string_addrs_ = ke::FixedArray<uint32_t>(num_strings);
    for (uint32_t i = 0; i < num_strings; i++) {
        auto string = image_->getRttiRow<smx_rtti_string>(image_->rtti_stringpool(), i);
        assert(string);

        auto blob = image_->ReadDataBlob(string->offset);
        if (!blob)
            return false;

        auto char_type = GetPrimitiveType(TypeKind::Char8);
        auto td = GetFixedArrayType(char_type, (uint32_t)blob->size() + 1);
        assert(td);

        auto array = NewArray(td, (uint32_t)td->array_size());
        if (!array)
            return false;

        auto data = heap_.ToPhysAddr<char*>(array->data);
        memcpy(data, blob->data(), blob->size());
        *(data + blob->size()) = '\0';

        string_addrs_[i] = heap_.ToLocalAddr(array.release());
    }

    /* Initialize the null references */
    uint32_t index;
    if (FindPubvarByName("NULL_VECTOR", &index) == SP_ERROR_NONE) {
        sp_pubvar_t* pubvar;
        GetPubvarByIndex(index, &pubvar);
        m_pNullVec = pubvar->offs;
    }

    if (FindPubvarByName("NULL_STRING", &index) == SP_ERROR_NONE) {
        sp_pubvar_t* pubvar;
        GetPubvarByIndex(index, &pubvar);
        m_pNullString = pubvar->offs;
    }

    return true;
}

ke::RefPtr<BaseMethodInfo> Runtime::GetMethodFromFrameId(uint32_t frame_id) const {
    return GetMethodByIndex(frame_id);
}

ke::RefPtr<BaseMethodInfo> Runtime::GetMethodByIndex(uint32_t method_index) const {
    if (method_index >= methods_.size())
        return nullptr;
    return methods_[method_index];
}

RefPtr<MethodInfo> Runtime::AcquireMethod(uint32_t method_index) {
    if (method_index < methods_.size() && methods_[method_index])
        return methods_[method_index];

    const smx_rtti_method* rtti_method = image_->GetMethod(method_index);
    if (!rtti_method)
        return nullptr;

    const TypeDesc* signature = LoadMethodSignature(method_index);
    if (!signature)
        return nullptr;

    RefPtr<MethodInfo> method = new MethodInfo(this, method_index, signature);

    // Grab the lock before linking code in, since the watchdog timer will look
    // at this list on another thread.
    {
        std::lock_guard<ke::Mutex> lock(env_->lock());
        if (method_index >= methods_.size())
            methods_.resize(method_index + 1);
        methods_[method_index] = method;
    }
    return method;
}

const std::vector<RefPtr<MethodInfo>>& Runtime::AllMethods() const {
    env_->lock().AssertCurrentThreadOwns();
    return methods_;
}

int Runtime::FindNativeByName(const char* name, uint32_t* index) {
    for (uint32_t i = 0; i < (uint32_t)natives_.size(); i++) {
        if (strcmp(natives_[i].name, name) == 0) {
            if (index)
                *index = i;
            return SP_ERROR_NONE;
        }
    }
    return SP_ERROR_NOT_FOUND;
}

int Runtime::GetNativeByIndex(uint32_t index, sp_native_t** native) {
    if (index >= (uint32_t)natives_.size())
        return SP_ERROR_INDEX;

    if (native)
        *native = &natives_[index];

    return SP_ERROR_NONE;
}

int Runtime::UpdateNativeBinding(uint32_t index, SPVM_NATIVE_FUNC pfn, uint32_t flags,
                                   void* data) {
    if (index >= (uint32_t)natives_.size())
        return SP_ERROR_INDEX;

    NativeEntry* native = &natives_[index];

    // The native must either be unbound, or it must be ephemeral or optional.
    // Otherwise, we've already baked its address in at callsites and it's too
    // late to fix them.
    if (native->status == SP_NATIVE_BOUND &&
        !(native->flags & (SP_NTVFLAG_OPTIONAL | SP_NTVFLAG_EPHEMERAL))) {
        return SP_ERROR_PARAM;
    }

    native->legacy_fn = pfn;
    native->callback = nullptr;
    native->status = pfn ? SP_NATIVE_BOUND : SP_NATIVE_UNBOUND;
    native->flags = flags;
    native->user = data;
    return SP_ERROR_NONE;
}

int Runtime::UpdateNativeBindingObject(uint32_t index, INativeCallback* callback, uint32_t flags,
                                         void* data) {
    RefPtr<INativeCallback> holder(callback);
    if (index >= (uint32_t)natives_.size())
        return SP_ERROR_INDEX;

    NativeEntry* native = &natives_[index];

    // The native must either be unbound, or it must be ephemeral or optional.
    // Otherwise, we've already baked its address in at callsites and it's too
    // late to fix them.
    // #include "ref.h"
    if (native->status == SP_NATIVE_BOUND &&
        !(native->flags & (SP_NTVFLAG_OPTIONAL | SP_NTVFLAG_EPHEMERAL))) {
        return SP_ERROR_PARAM;
    }

    native->legacy_fn = nullptr;
    native->callback = callback;
    native->status = callback ? SP_NATIVE_BOUND : SP_NATIVE_UNBOUND;
    native->flags = flags;
    native->user = data;
    return SP_ERROR_NONE;
}

const sp_native_t* Runtime::GetNative(uint32_t index) {
    if (index >= (uint32_t)natives_.size())
        return nullptr;

    return &natives_[index];
}

uint32_t Runtime::GetNativesNum() {
    return (uint32_t)natives_.size();
}

int Runtime::FindPublicByName(const char* name, uint32_t* index) {
    auto cmp = [](const sp_public_t& a, const char* target) -> bool {
        return strcmp(a.name, target) < 0;
    };
    auto it = std::lower_bound(publics_.begin(), publics_.end(), name, cmp);
    if (it == publics_.end() || strcmp(it->name, name) != 0)
        return SP_ERROR_NOT_FOUND;

    *index = (uint32_t)std::distance(publics_.begin(), it);
    return SP_ERROR_NONE;
}

int Runtime::GetPublicByIndex(uint32_t index, sp_public_t** out) {
    if (index >= publics_.size())
        return SP_ERROR_INDEX;

    if (out)
      *out = &publics_[index];

    return SP_ERROR_NONE;
}

uint32_t Runtime::GetPublicsNum() {
    return (uint32_t)publics_.size();
}

void Runtime::ResolvePubvar(PubvarEntry& entry) {
    if (entry.resolved)
        return;

    auto td = global_vars_[entry.global_index].td;
    if (td->IsNonFlatArray()) {
        uint32_t array_local = *heap_.ToPhysAddr<cell_t*>(entry.local_addr);
        SpArray* array = heap_.ToPhysAddr<SpArray*>(array_local);
        entry.pubvar.offs = heap_.ToPhysAddr<cell_t*>(array->data);
        entry.local_addr = array->data;
    }
    entry.resolved = true;
}

int Runtime::GetPubvarByIndex(uint32_t index, sp_pubvar_t** out) {
    if (index >= pubvars_.size())
        return SP_ERROR_INDEX;

    ResolvePubvar(pubvars_[index]);

    if (out)
        *out = &pubvars_[index].pubvar;
    return SP_ERROR_NONE;
}

int Runtime::FindPubvarByName(const char* name, uint32_t* index) {
    auto cmp = [](const PubvarEntry& a, const char* target) -> bool {
        return strcmp(a.pubvar.name, target) < 0;
    };
    auto it = std::lower_bound(pubvars_.begin(), pubvars_.end(), name, cmp);
    if (it == pubvars_.end() || strcmp(it->pubvar.name, name) != 0)
        return SP_ERROR_NOT_FOUND;

    if (index)
        *index = (uint32_t)std::distance(pubvars_.begin(), it);
    return SP_ERROR_NONE;
}

int Runtime::GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr) {
    if (index >= pubvars_.size())
        return SP_ERROR_INDEX;

    ResolvePubvar(pubvars_[index]);

    *local_addr = pubvars_[index].local_addr;
    if (phys_addr)
        *phys_addr = pubvars_[index].pubvar.offs;
    return SP_ERROR_NONE;
}

uint32_t
Runtime::GetPubVarsNum() {
    return (uint32_t)pubvars_.size();
}


IPluginFunction* Runtime::GetFunctionById(funcid_t func_id) {
    return GetScriptedInvoker(func_id);
}

ScriptedInvoker* Runtime::GetScriptedInvoker(funcid_t func_id) {
    if (!(func_id & 1))
        return nullptr;

    uint32_t method_index = func_id >> 1;
    return GetFunctionByMethodIndex(method_index);
}

ScriptedInvoker* Runtime::GetFunctionByMethodIndex(uint32_t method_index) {
    if (method_index >= image_->rtti_methods()->row_count)
        return nullptr;
    if (method_index >= entrypoints_.size())
        entrypoints_.resize(method_index + 1);
    if (!entrypoints_[method_index])
        entrypoints_[method_index] = std::make_unique<ScriptedInvoker>(this, method_index);
    return entrypoints_[method_index].get();
}

IPluginFunction* Runtime::GetFunctionByName(const char* public_name) {
    uint32_t index;

    if (FindPublicByName(public_name, &index) != SP_ERROR_NONE)
        return nullptr;

    assert(index < publics_.size());
    return GetScriptedInvoker(publics_[index].funcid);
}

bool Runtime::IsDebugging() {
    return true;
}


size_t Runtime::GetMemUsage() {
    return sizeof(*this) + image_->ImageSize();
}



bool Runtime::PerformFullValidation() {
    for (uint32_t i = 0; i < image_->rtti_methods()->row_count; i++) {
        const smx_rtti_method* method = image_->GetMethod(i);
        if (method->flags & kRttiMethod_Native)
            continue;


        ExceptionHandler eh(env_);

        MethodVerifier verifier(this, i);
        if (!verifier.verify()) {
            const char* name = image_->names() + method->name;
            int code = SP_ERROR_FATAL;
            std::string message = "unknown error";
            if (eh.HasException()) {
                code = eh.Code();
                message = eh.Message();
                eh.ClearException();
            }

            env_->ReportErrorFmt(code, "%s: %s", name, message.c_str());
            eh.Rethrow();
            return false;
        }
    }
    return true;
}

bool Runtime::GetNativeIndex(uint32_t method_index, uint32_t* index) const {
    auto iter = native_map_.find(method_index);
    if (iter == native_map_.end())
        return false;
    *index = iter->second;
    return true;
}

bool Runtime::UsesDirectArrays() {
    return true;
}

bool Runtime::CallGlobalCtor() {
    cell_t ignore_result = 0;

    auto ctor_index = image_->FindRttiMethod(".ctor");
    if (!ctor_index)
        return true;

    return InvokeMethod(*ctor_index, &ignore_result, 0, &ignore_result);
}

int Runtime::LocalToPhysAddr(cell_t local_addr, cell_t** phys_addr) {
    if (auto array = LocalToCompatArray(local_addr)) {
        if (phys_addr)
            *phys_addr = heap_.ToPhysAddr<cell_t*>(array->data);
        return SP_ERROR_NONE;
    }
    if (phys_addr)
        *phys_addr = heap_.ToPhysAddr<cell_t*>(local_addr);
    return SP_ERROR_NONE;
}

int Runtime::LocalToString(cell_t local_addr, char** addr) {
    if (auto array = LocalToCompatArray(local_addr)) {
        if (addr)
            *addr = heap_.ToPhysAddr<char*>(array->data);
        return SP_ERROR_NONE;
    }
    if (addr)
        *addr = heap_.ToPhysAddr<char*>(local_addr);
    return SP_ERROR_NONE;
}

int Runtime::StringToLocal(cell_t local_addr, size_t bytes, const char* source) {
    if (bytes == 0)
        return SP_ERROR_NONE;

    size_t len = strlen(source);
    char* dest;
    if (auto array = LocalToCompatArray(local_addr))
        dest = heap_.ToPhysAddr<char*>(array->data);
    else
        dest = heap_.ToPhysAddr<char*>(local_addr);

    if (len >= bytes)
        len = bytes - 1;

    memmove(dest, source, len);
    dest[len] = '\0';
    return SP_ERROR_NONE;
}

static inline int
__CheckValidChar(char* c) {
    int count;
    int bytecount = 0;

    for (count = 1; (*c & 0xC0) == 0x80; count++)
        c--;

    switch (*c & 0xF0) {
        case 0xC0:
        case 0xD0: {
            bytecount = 2;
            break;
        }
        case 0xE0: {
            bytecount = 3;
            break;
        }
        case 0xF0: {
            bytecount = 4;
            break;
        }
    }

    if (bytecount != count)
        return count;

    return 0;
}

int Runtime::StringToLocalUTF8(cell_t local_addr, size_t maxbytes, const char* source,
                               size_t* wrtnbytes) {
    if (maxbytes == 0)
        return SP_ERROR_NONE;

    size_t len = strlen(source);
    char* dest;
    if (auto array = LocalToCompatArray(local_addr))
        dest = heap_.ToPhysAddr<char*>(array->data);
    else
        dest = heap_.ToPhysAddr<char*>(local_addr);

    bool needtocheck = false;
    if ((size_t)len >= maxbytes) {
        len = maxbytes - 1;
        needtocheck = true;
    }

    memmove(dest, source, len);
    if ((dest[len - 1] & 1 << 7) && needtocheck)
        len -= __CheckValidChar(dest + len - 1);
    dest[len] = '\0';

    if (wrtnbytes)
        *wrtnbytes = len;

    return SP_ERROR_NONE;
}

int Runtime::LocalToStringNULL(cell_t local_addr, char** addr) {
    int err;
    if ((err = LocalToString(local_addr, addr)) != SP_ERROR_NONE)
        return err;

    if ((cell_t*)*addr == m_pNullString)
        *addr = NULL;

    return SP_ERROR_NONE;
}

cell_t* Runtime::GetNullRef(SP_NULL_TYPE type) {
    if (type == SP_NULL_VECTOR)
        return m_pNullVec;

    return NULL;
}

bool Runtime::IsInExec() {
    for (InvokeFrame* ivk = env_->top(); ivk; ivk = ivk->prev()) {
        if (ivk->cx() == this)
            return true;
    }
    return false;
}

bool Runtime::InvokeMethod(uint32_t method_index, const cell_t* params,
                           unsigned int num_params, cell_t* result)
{
    EnterProfileScope profileScope("SourcePawn", "EnterJIT");

    if (!env_->watchdog()->HandleInterrupt()) {
        ReportErrorNumber(SP_ERROR_TIMEOUT);
        return false;
    }

    ScriptedInvoker* cfun = GetFunctionByMethodIndex(method_index);
    if (!cfun) {
        ReportErrorNumber(SP_ERROR_NOT_FOUND);
        return false;
    }

    if (IsPaused()) {
        ReportErrorNumber(SP_ERROR_NOT_RUNNABLE);
        return false;
    }

    // Yuck. We have to do this for compatibility, otherwise something like
    // ForwardSys or any sort of multi-callback-fire code would die. Later,
    // we'll expose an Invoke() or something that doesn't do this.
    env_->clearPendingException();

    cell_t ignore_result;
    if (result == NULL)
        result = &ignore_result;

    /* We got this far.  It's time to start profiling. */
    EnterProfileScope scriptScope("SourcePawn", cfun->DebugName());

    /* See if we have to compile the callee. */
    RefPtr<MethodInfo> method = cfun->AcquireMethod();
    if (!method) {
        ReportErrorNumber(SP_ERROR_INVALID_ADDRESS);
        return false;
    }


    ke::SaveRestore<uint32_t> save_sp(env_->sp());

    uint32_t frame_base = env_->sp();
    if (!env_->addStack(num_params * sizeof(cell_t)))
        return false;
    cell_t* sp = env_->heap().ToPhysAddr<cell_t*>(frame_base);

    for (unsigned int i = 0; i < num_params; i++)
        sp[i] = params[i];

    // Enter the execution engine.
    bool ok = env_->Invoke(this, method, frame_base, result);


    return ok;
}

cell_t* Runtime::GetLocalParams() {
    assert(false);
    return nullptr;
}

bool Runtime::HeapAlloc2dArray(unsigned int length, unsigned int stride, cell_t* local_addr,
                                const cell_t* init) {
    assert(!heap_scopes_.empty());
    if (heap_scopes_.empty()) {
        ReportError("HeapAlloc2dArray called outside of a heap scope");
        return false;
    }

    if (length > INT_MAX || stride > INT_MAX) {
        ReportErrorNumber(SP_ERROR_ARRAY_TOO_BIG);
        return false;
    }

    const TypeDesc* elt_td = GetArrayType(GetPrimitiveType(TypeKind::Any));
    const TypeDesc* td = GetArrayType(elt_td);

    Handle<SpArray> array = NewArray(td, length);
    if (!array)
        return false;

    heap_scopes_.back().push_back(array);
    *local_addr = heap_.ToLocalAddr(array.get());

    cell_t* array_phys = heap_.ToPhysAddr<cell_t*>(array->data);
    for (unsigned int i = 0; i < length; i++) {
        Handle<SpArray> elt = NewArray(elt_td, stride);
        if (!elt)
            return false;

        heap_scopes_.back().push_back(elt);

        if (init) {
            cell_t* elt_phys = heap_.ToPhysAddr<cell_t*>(elt->data);
            memcpy(elt_phys, &init[i * stride], stride * sizeof(cell_t));
        }

        array_phys[i] = heap_.ToLocalAddr(elt.release());
    }
    return true;
}

void Runtime::EnterHeapScope() {
    heap_scopes_.emplace_back();
}

void Runtime::LeaveHeapScope() {
    assert(!heap_scopes_.empty());
    heap_scopes_.pop_back();
}

cell_t Runtime::GetNullFunctionValue() {
    return 0;
}

bool Runtime::IsNullFunctionId(funcid_t func) {
    return func == static_cast<funcid_t>(GetNullFunctionValue());
}

bool Runtime::GetFunctionByIdOrNull(funcid_t func, IPluginFunction** out) {
    if (IsNullFunctionId(func)) {
        *out = nullptr;
        return true;
    }

    *out = GetFunctionById(func);
    if (!*out) {
        ReportError("Invalid function id: 0x%08x", func);
        return false;
    }
    return true;
}

IPluginFunction* Runtime::GetFunctionByIdOrError(funcid_t func_id) {
    if (auto fn = GetFunctionById(func_id))
        return fn;
    ReportError("Invalid function id: 0x%08x", func_id);
    return nullptr;
}

int Runtime::LocalToArrayPtr(cell_t base, ARRAY_PTR* out) {
    *out = reinterpret_cast<ARRAY_PTR>(static_cast<uintptr_t>(base));
    return SP_ERROR_NONE;
}

void* Runtime::GetArrayData(ARRAY_PTR handle, uint32_t* size) {
    cell_t base = static_cast<cell_t>(reinterpret_cast<uintptr_t>(handle));
    if (base & kNativePointerTag) {
        uint32_t local_addr = base & ~kNativePointerTag;
        SpArray* array = heap_.ToPhysAddr<SpArray*>(local_addr);
        if (size)
            *size = array->length;
        return heap_.ToPhysAddr<void*>(array->data);
    }
    if (size)
        *size = 0;
    return heap_.ToPhysAddr<void*>(base);
}

const TypeDesc* Runtime::LoadType(FastRtti& parser) {
    uint8_t b;
    if (!parser.GetNextByte(&b)) {
        ReportError("Invalid type data");
        return nullptr;
    }

    // We completely ignore const in the VM. It's just documentation.
    if (b == cb::kConst && !parser.GetNextByte(&b)) {
        ReportError("Invalid type data");
        return nullptr;
    }

    switch (b) {
        case cb::kBool:
            return GetPrimitiveType(TypeKind::Bool);
        case cb::kInt32:
            return GetPrimitiveType(TypeKind::Int32);
        case cb::kFloat32:
            return GetPrimitiveType(TypeKind::Float32);
        case cb::kChar8:
            return GetPrimitiveType(TypeKind::Char8);
        case cb::kAny:
            return GetPrimitiveType(TypeKind::Any);
        case cb::kTopFunction:
            return GetPrimitiveType(TypeKind::TopFunction);
        case cb::kEnum: {
            uint32_t index;
            if (!parser.ReadUint32_Leb128(&index))
                ReportError("invalid type data");
            if (index >= image_->rtti_enums()->row_count)
                ReportError("invalid enum index in type data");
            // Rewrite to int32 for now.
            return GetPrimitiveType(TypeKind::Int32);
        }
        case cb::kClassdef:
        case cb::kEnumStruct: {
            uint32_t index;
            if (!parser.ReadUint32_Leb128(&index)) {
                ReportError("invalid type data");
                return nullptr;
            }
            if (!image_->rtti_classdefs() || index >= image_->rtti_classdefs()->row_count) {
                ReportError("invalid classdef index in type data");
                return nullptr;
            }
            auto classdef = image_->getClassdef(index);
            return GetEnumStructType(classdef);
        }
        case cb::kInt64:
            return GetPrimitiveType(TypeKind::Int64);
        case cb::kFixedArray: {
            uint32_t size;
            if (!parser.ReadUint32_Leb128(&size) || !size) {
                ReportError("Invalid type data");
                return nullptr;
            }
            auto td = LoadType(parser);
            if (!td)
                return nullptr;
            return GetFixedArrayType(td, size);
        }
        case cb::kFlatArray: {
            uint32_t size;
            if (!parser.ReadUint32_Leb128(&size) || !size) {
                ReportError("Invalid type data");
                return nullptr;
            }
            auto td = LoadType(parser);
            if (!td)
                return nullptr;
            return GetFlatArrayType(td, size);
        }
        case cb::kArray: {
            auto td = LoadType(parser);
            if (!td)
                return nullptr;
            return GetArrayType(td);
        }
        case cb::kFunctionPtr: {
            uint32_t index;
            if (!parser.ReadUint32_Leb128(&index)) {
                ReportError("Invalid type data");
                return nullptr;
            }
            return GetPrimitiveType(TypeKind::TopFunction);
        }
        case cb::kTypeset: {
            uint32_t index;
            if (!parser.ReadUint32_Leb128(&index)) {
                ReportError("Invalid type data");
                return nullptr;
            }
            return GetPrimitiveType(TypeKind::TopFunction);
        }
        default:
            assert(false);

            ReportError("Invalid type data byte: %x", b);
            return nullptr;
    }
}

const TypeDesc* Runtime::LoadArgType(FastRtti& parser) {
    uint8_t b;
    if (!parser.GetByte(&b)) {
        ReportError("Invalid type data");
        return nullptr;
    }

    if (b == cb::kConst) {
        parser.NextByte();
        if (!parser.GetByte(&b)) {
            ReportError("Invalid type data");
            return nullptr;
        }
    }

    if (b == cb::kByRef)
        parser.NextByte();

    const TypeDesc* td = LoadType(parser);
    if (!td)
        return nullptr;

    if (b == cb::kByRef)
        return GetReferenceType(td);
    return td;
}

const TypeDesc* Runtime::LoadTypeFromId(uint32_t type_id) {
    FastRtti parser = image_->GetTypeIdParser(type_id);
    return LoadType(parser);
}

const TypeDesc* Runtime::LoadMethodSignature(uint32_t method_index) {
    const smx_rtti_method* method = image_->GetMethod(method_index);
    if (!method) {
        ReportError("invalid method index");
        return nullptr;
    }

    // :TODO: acquire method

    auto parser = image_->GetTypeParser(method->signature);

    uint32_t expected_argc;
    if (!parser.ReadFunctionSignatureArgCount(&expected_argc)) {
        ReportError("invalid function signature");
        return nullptr;
    }

    uint8_t variadic;
    if (!parser.GetByte(&variadic)) {
        ReportError("invalid function signature");
        return nullptr;
    }

    if (variadic == cb::kLegacyVariadic)
        parser.NextByte();

    uint8_t type_byte;
    if (!parser.GetByte(&type_byte)) {
        ReportError("invalid function signature");
        return nullptr;
    }

    const TypeDesc* return_type = nullptr;
    if (type_byte != cb::kVoid) {
        return_type = LoadType(parser);
        if (!return_type)
            return nullptr;
    } else {
        parser.NextByte();
        return_type = GetPrimitiveType(TypeKind::Void);
    }

    std::vector<const TypeDesc*> args;
    for (uint32_t i = 0; i < expected_argc; i++) {
        const TypeDesc* arg = LoadArgType(parser);
        if (!arg)
            return nullptr;
        args.push_back(arg);
    }

    if (variadic == cb::kLegacyVariadic)
        args.push_back(GetPrimitiveType(TypeKind::LegacyVarArgs));

    return env_->types()->CreateFunction(return_type, args, (method->flags & kRttiMethod_Native) != 0);
}

const TypeDesc* Runtime::GetReferenceType(const TypeDesc* td) {
    return env_->types()->GetReference(td);
}

const TypeDesc* Runtime::GetPrimitiveType(TypeKind kind) {
    return env_->types()->GetPrimitive(kind);
}

const TypeDesc* Runtime::GetArrayType(const TypeDesc* elt) {
    return env_->types()->GetArray(elt);
}

const TypeDesc* Runtime::GetFixedArrayType(const TypeDesc* elt, uint32_t size) {
    return env_->types()->GetFixedArray(elt, size);
}

const TypeDesc* Runtime::GetFlatArrayType(const TypeDesc* elt, uint32_t size) {
    return env_->types()->GetFlatArray(elt, size);
}

const TypeDesc* Runtime::GetSliceType(const TypeDesc* elt) {
    return env_->types()->GetSlice(elt);
}

const TypeDesc* Runtime::GetStringLitType(uint16_t index) {
    if (string_addrs_.size() > 0 && string_addrs_[index] != 0) {
        uint32_t addr = string_addrs_[index];
        auto array = heap_.ToPhysAddr<SpArray*>(addr);
        return array->td;
    }

    auto string = image_->getRttiRow<smx_rtti_string>(image_->rtti_stringpool(), index);
    auto blob = image_->ReadDataBlob(string->offset);
    if (!blob)
        return nullptr;

    auto char_type = GetPrimitiveType(TypeKind::Char8);
    return GetFixedArrayType(char_type, (uint32_t)blob->size() + 1);
}

const TypeDesc* Runtime::GetEnumStructType(const smx_rtti_classdef* classdef) {
    return env_->types()->GetEnumStruct(this, classdef);
}

const TypeDesc* Runtime::GetTypeOfGlobal(uint16_t index) {
    auto global = image_->getRttiRow<smx_rtti_global>(image_->rtti_globals(), index);
    return LoadTypeFromId(global->type_id);
}

Handle<SpArray> Runtime::NewArray(const TypeDesc* td, uint32_t size) {
    assert(td->kind() == TypeKind::Array ||
           (td->kind() == TypeKind::FixedArray && size == td->array_size()));
    uint32_t elt_size = td->array_elt()->element_size();

    if (!ke::IsUintMultiplySafe(size, elt_size)) {
        env_->ReportError(SP_ERROR_INVALID_ARRAY_SIZE);
        return nullptr;
    }

    uint32_t data_size = size * elt_size;
    if (data_size >= INT_MAX) {
        env_->ReportError(SP_ERROR_INVALID_ARRAY_SIZE);
        return nullptr;
    }

    auto base = heap_.New<SpArray>(td, size * elt_size);
    if (!base) {
        env_->ReportError(SP_ERROR_INVALID_ARRAY_SIZE);
        return nullptr;
    }
    base->length = size;

    if (size) {
        base->data = heap_.ToLocalAddr(base.get()) + sizeof(SpArray);

        auto data_ptr = heap_.ToPhysAddr<void*>(base->data);
        memset(data_ptr, 0, data_size);

        auto array_elt = td->array_elt();
        if (array_elt->kind() == TypeKind::FixedArray) {
            uint32_t* slots = reinterpret_cast<uint32_t*>(data_ptr);
            for (uint32_t i = 0; i < size; i++) {
                auto p = NewArray(array_elt, array_elt->array_size());
                if (!p)
                    return nullptr;
                slots[i] = heap_.ToLocalAddr(p.release());
            }
        }
    } else {
        base->data = 0;
    }
    return base;
}

Handle<SpArray> Runtime::NewBulkArray(const TypeDesc* td, uint8_t dims, cell_t* sizes) {
    if (*sizes < 0) {
        ReportErrorNumber(SP_ERROR_ARRAY_BOUNDS);
        return nullptr;
    }

    uint32_t size = *sizes;
    auto array = NewArray(td, size);
    if (!array)
        return nullptr;

    if (!size || dims == 1)
        return array;

    auto inner = td->array_elt();
    if (inner->kind() != TypeKind::Array && inner->kind() != TypeKind::FixedArray)
        return array;

    assert(dims > 1);

    uint32_t* parent_slots = heap_.ToPhysAddr<uint32_t*>(array->data);
    for (uint32_t i = 0; i < size; i++) {
        auto child = NewBulkArray(inner, dims - 1, sizes + 1);
        if (!child)
            return nullptr;
        parent_slots[i] = heap_.ToLocalAddr(child.release());
    }
    return array;
}

void Runtime::FillArray(SpArray* array, uint32_t data_offset) {
    assert(array->td->kind() == TypeKind::FixedArray);

    BinaryReader br = image_->GetDataReader(data_offset);
    auto data_bytes = br.readCompactUint32();
    assert(data_bytes);

    auto elt_size = array->td->array_elt()->element_size();
    assert(*data_bytes % elt_size == 0);
    [[maybe_unused]] auto elt_count = *data_bytes / elt_size;
    assert(elt_count <= array->length);

    auto data = heap_.ToPhysAddr<uint8_t*>(array->data);
    memcpy(data, br.cursor(), *data_bytes);
}

void Runtime::FillFlatArray(cell_t local_addr, const TypeDesc* td, uint32_t data_offset) {
    assert(td->IsFlatArray());
    assert(!td->array_elt()->IsHeapItem());

    BinaryReader br = image_->GetDataReader(data_offset);
    auto data_bytes = br.readCompactUint32();
    assert(data_bytes);

    auto elt_size = td->array_elt()->element_size();
    assert(*data_bytes % elt_size == 0);
    [[maybe_unused]] auto elt_count = *data_bytes / elt_size;
    assert(elt_count <= td->array_size());

    auto data = heap_.ToPhysAddr<uint8_t*>(local_addr);
    memcpy(data, br.cursor(), *data_bytes);
}

void* Runtime::GetArrayElem(SpArray* array, uint32_t index) {
    assert(index < array->length);

    uint32_t elt_size = array->td->array_elt()->element_size();
    uint8_t* data = heap_.ToPhysAddr<uint8_t*>(array->data);
    return data + (index * elt_size);
}

Handle<SpArray> Runtime::NewSlice(SpArray* array, uint32_t index) {
    assert(!array->td->array_elt()->IsArrayish());
    if (index >= array->length) {
        ReportOutOfBoundsError(index, array->length);
        return nullptr;
    }

    auto td = array->td;
    if (td->kind() != TypeKind::ArraySlice)
        td = GetSliceType(td->array_elt());

    auto slice = heap_.New<SpArray>(td);
    if (!slice)
        return nullptr;
    slice->length = array->length - index;
    slice->data = heap_.ToLocalAddr(GetArrayElem(array, index));
    return slice;
}

Handle<SpArray> Runtime::NewSliceEs(uint32_t data, uint32_t size) {
    auto td = GetSliceType(GetPrimitiveType(TypeKind::Any));
    auto slice = heap_.New<SpArray>(td);
    if (!slice)
        return nullptr;

    slice->length = size;
    slice->data = data;
    return slice;
}

Handle<SpArray> Runtime::NewFlatSlice(cell_t local_addr, const TypeDesc* td, uint32_t index) {
    assert(td->IsFlatArray());
    assert(index <= td->array_size());

    const TypeDesc* slice_td = GetSliceType(td->array_elt());
    Handle<SpArray> slice = heap_.New<SpArray>(slice_td);
    if (!slice)
        return nullptr;
    slice->length = td->array_size() - index;
    slice->data = local_addr + index * td->array_elt()->element_size();
    return slice;
}

SpArray* Runtime::LocalToCompatArray(cell_t local_addr) {
    if (local_addr & kNativePointerTag) {
        uint32_t handle = local_addr & ~kNativePointerTag;
        return heap_.ToPhysAddr<SpArray*>(handle);
    }
    return nullptr;
}

} // namespace v2
} // namespace sp
