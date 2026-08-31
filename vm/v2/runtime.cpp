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

#include <smx/smx-v2-opcodes.h>
#include "legacy/builtins.h"
#include "compiled-function.h"
#include "environment.h"
#include "md5/md5.h"
#include "v2/method-info.h"
#include "v2/method-verifier.h"
#include "watchdog_timer.h"

namespace sp {
namespace v2 {

using namespace SourcePawn;

static const size_t kMinHeapSize = 16384;
#define CELLBOUNDMAX (INT_MAX / sizeof(cell_t))

Runtime::Runtime(SmxImage* image)
 : BaseRuntime(image),
   data_size_(data().length)
{

    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
    Environment::get()->RegisterRuntime(this);
}

Runtime::~Runtime() {
    // The watchdog thread takes the global JIT lock while it patches all
    // runtimes. It is not enough to ensure that the unlinking of the runtime is
    // protected; we cannot delete functions or code while the watchdog might be
    // executing. Therefore, the entire destructor is guarded.
    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());

    Environment::get()->DeregisterRuntime(this);

    delete[] memory_;
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

    pubvars_ = std::make_unique<sp_pubvar_t[]>(image_->NumPubvars());
    if (!pubvars_)
        return false;
    memset(pubvars_.get(), 0, sizeof(sp_pubvar_t) * image_->NumPubvars());

    if (!InitializeContext())
        return false;
    if (!InitializeGlobals())
        return false;

    return true;
}

bool Runtime::InitializeContext() {
    if (!heap_.Initialize())
        return false;

    auto sp_base = heap_.Allocate(kDefaultStackSize);
    if (!sp_base)
        return false;

    sp_base_ = heap_.ToLocalAddr(sp_base);
    sp_top_ = sp_base_ + kDefaultStackSize;
    sp_ = sp_top_;
    return true;
}

bool Runtime::InitializeGlobals() {
    global_addrs_ = ke::FixedArray<uint32_t>(image_->rtti_globals()->row_count);
    for (uint32_t i = 0; i < image_->rtti_globals()->row_count; i++) {
        auto global = image_->getRttiRow<smx_rtti_global>(image_->rtti_globals(), i);
        assert(global);

        uint8_t* p = heap_.Allocate(sizeof(cell_t));
        if (!p)
            return false;
        global_addrs_[i] = heap_.ToLocalAddr(p);
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

    RefPtr<MethodInfo> method = new MethodInfo(this, method_index);

    // Grab the lock before linking code in, since the watchdog timer will look
    // at this list on another thread.
    {
        std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
        if (method_index >= methods_.size())
            methods_.resize(method_index + 1);
        methods_[method_index] = method;
    }
    return method;
}

const std::vector<RefPtr<MethodInfo>>& Runtime::AllMethods() const {
    Environment::get()->lock().AssertCurrentThreadOwns();
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

int Runtime::GetPubvarByIndex(uint32_t index, sp_pubvar_t** out) {
    if (index >= image_->NumPubvars())
        return SP_ERROR_INDEX;

    sp_pubvar_t* pubvar = &pubvars_[index];
    if (!pubvar->name) {
        uint32_t offset;
        image_->GetPubvar(index, &offset, &pubvar->name);
        if (int err = LocalToPhysAddr(offset, &pubvar->offs))
            return err;
    }

    if (out)
        *out = pubvar;
    return SP_ERROR_NONE;
}

int Runtime::FindPubvarByName(const char* name, uint32_t* index) {
    size_t idx;
    if (!image_->FindPubvar(name, &idx))
        return SP_ERROR_NOT_FOUND;

    if (index)
        *index = idx;
    return SP_ERROR_NONE;
}

int Runtime::GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr) {
    if (index >= image_->NumPubvars())
        return SP_ERROR_INDEX;

    uint32_t offset;
    image_->GetPubvar(index, &offset, nullptr);

    if (int err = LocalToPhysAddr(offset, phys_addr))
        return err;
    *local_addr = offset;
    return SP_ERROR_NONE;
}

uint32_t
Runtime::GetPubVarsNum() {
    return image_->NumPubvars();
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
    Environment* env = Environment::get();
    for (uint32_t i = 0; i < image_->rtti_methods()->row_count; i++) {
        const smx_rtti_method* method = image_->GetMethod(i);
        if (method->flags & kRttiMethod_Native)
            continue;


        ExceptionHandler eh(env);

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

            env->ReportErrorFmt(code, "%s: %s", name, message.c_str());
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
    cell_t ignore_result;

    auto ctor_index = image_->FindRttiMethod(".ctor");
    if (!ctor_index)
        return true;

    return InvokeMethod(*ctor_index, &ignore_result, 0, &ignore_result);
}

int Runtime::HeapAlloc(unsigned int cells, cell_t* local_addr, cell_t** phys_addr) {
    if (!IsUint32MultiplySafe(cells, sizeof(cell_t)))
        return SP_ERROR_HEAPLOW;

    uint32_t alloc_size = cells * sizeof(cell_t);
    if (!IsUint32AddSafe(cells, sizeof(HeapImpl::Position)))
        return SP_ERROR_HEAPLOW;
    alloc_size += sizeof(HeapImpl::Position);

    auto save_pos = heap_.GetPosition();
    uint8_t* p = heap_.Allocate(alloc_size);
    if (!p)
        return SP_ERROR_HEAPLOW;

    *reinterpret_cast<HeapImpl::Position*>(p) = save_pos;
    p += sizeof(HeapImpl::Position);

    *local_addr = heap_.ToLocalAddr(p);
    *phys_addr = reinterpret_cast<cell_t*>(p);
    return SP_ERROR_NONE;
}

int Runtime::HeapPop(cell_t local_addr) {
    uint8_t* p = heap_.ToPhysAddr<uint8_t*>(local_addr);
    p -= sizeof(HeapImpl::Position);
    auto pos = *reinterpret_cast<HeapImpl::Position*>(p);

    heap_.RestorePosition(pos);
    return SP_ERROR_NONE;
}

int Runtime::HeapRelease(cell_t local_addr) {
    return SP_ERROR_PARAM;
}

int Runtime::LocalToPhysAddr(cell_t local_addr, cell_t** phys_addr) {
    if (phys_addr)
        *phys_addr = heap_.ToPhysAddr<cell_t*>(local_addr);
    return SP_ERROR_NONE;
}

int Runtime::LocalToString(cell_t local_addr, char** addr) {
    if (addr)
        *addr = heap_.ToPhysAddr<char*>(local_addr);
    return SP_ERROR_NONE;
}

int Runtime::StringToLocal(cell_t local_addr, size_t bytes, const char* source) {
    if (bytes == 0)
        return SP_ERROR_NONE;

    size_t len = strlen(source);
    char* dest = heap_.ToPhysAddr<char*>(local_addr);

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
    auto dest = heap_.ToPhysAddr<char*>(local_addr);

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

    /* Save our previous state. */
    uint32_t save_sp = sp_;
#ifndef NDEBUG
    uint32_t save_hp_scope = hp_scope_;
    auto heap_pos = heap_.GetPosition();
#endif

    /* Push parameters */
    if (!addStack(-int32_t((num_params + 1) * sizeof(cell_t))))
        return false;
    cell_t* sp = heap_.ToPhysAddr<cell_t*>(sp_);

    sp[0] = num_params;
    for (unsigned int i = 0; i < num_params; i++)
        sp[i + 1] = params[i];

    // Enter the execution engine. Callee is responsible for saving and
    // restoring hp_scope_.
    bool ok = env_->Invoke(this, method, result);

#ifndef NDEBUG
    assert(hp_scope_ == save_hp_scope);
    assert(heap_pos == heap_.GetPosition());
#endif
    sp_ = save_sp;
    return ok;
}

cell_t* Runtime::GetLocalParams() {
    assert(false);
    return nullptr;
}

bool Runtime::enterHeapScope() {
    auto pos = heap_.GetPosition();

    auto node = heap_.AllocTyped<HeapScope>();
    if (!node)
        return false;

    node->pos = pos;
    node->prev_hp_scope = hp_scope_;

    hp_scope_ = heap_.ToLocalAddr(node);
    return true;
}

void Runtime::leaveHeapScope() {
    assert(hp_scope_ != 0);

    auto node = *heap_.ToPhysAddr<HeapScope*>(hp_scope_);
    heap_.RestorePosition(node.pos);
    hp_scope_ = node.prev_hp_scope;
}

// We divide multi-dimensional arrays into two regions: the IV (indirection
// vector) region, and the data region. The IV region contains all the
// intermediate links to access the final dimension. The data region contains
// every cell in the last dimension.
//
// We split things this way because, all the intermediate vectors must be
// allocated up-front, and it is easier to memset() the data area in one
// big block.
//
// For a 1D array, the IV space is 0.
// For a 2D array of size [X][Y], the IV space is X cells.
// For a 3D array of size [X][Y][Z], the IV space is:
//    (X + (Y * X))
// For a 4D array of size [X][Y][Z][A], the IV space is:
//    (X + ((Y + (Z * Y)) * X))
//
// This function generates IV vectors recursively. When processing intermediate
// dimensions, we reserve the indirection vector in |iv_cursor|, then for each
// slot, recursively ask for the next array it should point to.
//
// If the next dimension is also intermediate, it will point into the IV space.
// If the next dimension is terminal, we will instead allocate the array in the
// data space, and return its base address.
struct abs_iv_data_t {
    cell_t addr;
    uint8_t* ptr;
    cell_t iv_cursor;
    cell_t data_cursor;
    const cell_t* dims;
    cell_t dimcount;
};

static cell_t
GenerateAbsoluteIndirectionVectors(abs_iv_data_t& info, cell_t dim) {
    if (dim == 0) {
        cell_t next_addr = info.data_cursor;
        info.data_cursor += info.dims[0] * sizeof(cell_t);
        return next_addr;
    }

    cell_t iv_base_offset = info.iv_cursor;
    info.iv_cursor += info.dims[dim] * sizeof(cell_t);

    for (cell_t i = 0; i < info.dims[dim]; i++) {
        cell_t next_array_offset = GenerateAbsoluteIndirectionVectors(info, dim - 1);
        cell_t iv_cell = iv_base_offset + i * sizeof(cell_t);
        cell_t next_array_addr = info.addr + next_array_offset;
        *reinterpret_cast<cell_t*>(info.ptr + iv_cell) = next_array_addr;
    }
    return iv_base_offset;
}

int Runtime::generateFullArray(uint32_t argc, cell_t* argv, int autozero) {
    // Calculate how many cells are needed.
    if (argv[0] <= 0)
        return SP_ERROR_ARRAY_TOO_BIG;

    // cells is the total number of cells required.
    // iv_size is the number of bytes needed to hold indirection vectors,
    // and is a subset of cells*sizeof(cell).
    uint32_t cells = argv[0];
    cell_t iv_size = 0;

    for (uint32_t dim = 1; dim < argc; dim++) {
        cell_t dimsize = argv[dim];
        if (dimsize <= 0)
            return SP_ERROR_ARRAY_TOO_BIG;
        if (!ke::IsUint32MultiplySafe(cells, dimsize))
            return SP_ERROR_ARRAY_TOO_BIG;
        cells *= uint32_t(dimsize);
        if (!ke::IsUint32AddSafe(cells, dimsize))
            return SP_ERROR_ARRAY_TOO_BIG;
        cells += uint32_t(dimsize);
        iv_size *= dimsize;
        iv_size += dimsize * sizeof(cell_t);
    }

    if (!ke::IsUint32MultiplySafe(cells, sizeof(cell_t)))
        return SP_ERROR_ARRAY_TOO_BIG;

    uint32_t bytes = cells * sizeof(cell_t);
    if (!ke::IsUint32AddSafe(hp_, bytes))
        return SP_ERROR_ARRAY_TOO_BIG;

    uint32_t new_hp = hp_ + bytes;
    if (new_hp >= sp_ - STACKMARGIN)
        return SP_ERROR_HEAPLOW;

    cell_t* base = reinterpret_cast<cell_t*>(memory_ + hp_);

    if (autozero) {
        memset(reinterpret_cast<uint8_t*>(base) + iv_size, 0, bytes - iv_size);
    }

    abs_iv_data_t info;
    info.addr = hp_;
    info.ptr = reinterpret_cast<uint8_t*>(base);
    info.iv_cursor = 0;
    info.data_cursor = iv_size;
    info.dims = argv;
    info.dimcount = argc;
    GenerateAbsoluteIndirectionVectors(info, argc - 1);

    assert(info.iv_cursor == iv_size);
    assert(info.data_cursor == (cell_t)bytes);

    argv[argc - 1] = hp_;
    hp_ = new_hp;
    return SP_ERROR_NONE;
}

int Runtime::generateArray(cell_t dims, cell_t* stk, bool autozero) {
    if (dims == 1) {
        uint32_t size = *stk;
        if (size <= 0)
            return SP_ERROR_INVALID_ARRAY_SIZE;
        if (!ke::IsUint32MultiplySafe(size, 4))
            return SP_ERROR_ARRAY_TOO_BIG;
        *stk = hp_;

        uint32_t bytes = size * 4;

        if (uintptr_t(memory_ + hp_ + bytes) >= uintptr_t(stk))
            return SP_ERROR_HEAPLOW;

        hp_ += bytes;

        if (autozero)
            memset(memory_ + *stk, 0, bytes);

        return SP_ERROR_NONE;
    }

    if (int err = generateFullArray(dims, stk, autozero))
        return err;

    return SP_ERROR_NONE;
}

bool Runtime::getCellValue(cell_t address, cell_t* out) {
    assert((uintptr_t)(const void*)out % sizeof(cell_t) == 0);

    cell_t* ptr = heap_.ToPhysAddr<cell_t*>(address);
    if (!ptr)
        return false;

    if ((uintptr_t)(const void*)ptr % sizeof(cell_t) == 0) {
        *out = *ptr;
    } else {
        for (size_t i = 0; i < sizeof(cell_t); ++i) {
            ((unsigned char*)out)[i] = ((unsigned char*)ptr)[i];
        }
    }

    return true;
}

bool Runtime::setCellValue(cell_t address, cell_t value) {
    cell_t* ptr = heap_.ToPhysAddr<cell_t*>(address);
    if (!ptr)
        return false;

    *ptr = value;
    return true;
}

bool Runtime::heapAlloc(cell_t amount, cell_t* out) {
    return heapAllocEx(amount, out) != nullptr;
}

cell_t* Runtime::heapAllocEx(cell_t amount, cell_t* out) {
    cell_t new_hp = hp_ + amount;

    if (amount < 0) {
        // Note: signed compare, in case new_hp is negative.
        if (new_hp < cell_t(data_size_)) {
            ReportErrorNumber(SP_ERROR_HEAPMIN);
            return nullptr;
        }
    } else {
        if (new_hp + STACKMARGIN > sp_) {
            ReportErrorNumber(SP_ERROR_HEAPLOW);
            return nullptr;
        }
    }

    *out = hp_;
    hp_ = new_hp;
    return reinterpret_cast<cell_t*>(memory_ + *out);
}

cell_t* Runtime::acquireAddrRange(cell_t address, uint32_t bounds) {
    return heap_.ToPhysAddr<cell_t*>(address);
}

bool Runtime::addStack(cell_t amount) {
    assert(ke::IsAligned(amount, sizeof(cell_t)));

    uint32_t new_sp = sp_ + amount;
    if (new_sp >= sp_top_) {
        ReportErrorNumber(amount < 0 ? SP_ERROR_STACKLOW : SP_ERROR_STACKMIN);
        return false;
    }

    sp_ += new_sp;
    return true;
}

bool Runtime::initArray(cell_t array_addr, cell_t dat_addr, cell_t iv_size, cell_t data_copy_size,
                         cell_t data_fill_size, cell_t fill_value) {
    int err;

    cell_t* iv_vec;
    if ((err = LocalToPhysAddr(array_addr, &iv_vec)) != SP_ERROR_NONE) {
        ReportErrorNumber(err);
        return false;
    }

    // Note: we don't use LocalToPhysAddr here because the address could be the
    // very end of DAT and it could throw an error.
    cell_t* data_vec = iv_vec + iv_size;
    assert(iv_vec <= data_vec);

    cell_t* mem_end = reinterpret_cast<cell_t*>(memory_ + mem_size_);
    if (data_vec + data_copy_size + data_fill_size - 1 >= mem_end) {
        ReportErrorNumber(SP_ERROR_INVALID_ADDRESS);
        return false;
    }

    // Only attempt address conversions if there's a template to copy from.
    if (iv_size || data_copy_size) {
        cell_t* tpl_iv_vec;
        if ((err = LocalToPhysAddr(dat_addr, &tpl_iv_vec)) != SP_ERROR_NONE) {
            ReportErrorNumber(err);
            return false;
        }

        cell_t* tpl_data_vec = tpl_iv_vec + iv_size;
        assert(tpl_iv_vec <= tpl_data_vec);

        cell_t* dat_end = reinterpret_cast<cell_t*>(memory_ + data_size_);
        if (tpl_data_vec + data_copy_size - 1 >= dat_end) {
            ReportErrorNumber(SP_ERROR_INVALID_ADDRESS);
            return false;
        }

        while (iv_vec < data_vec) {
            *iv_vec = *tpl_iv_vec + array_addr;
            iv_vec++;
            tpl_iv_vec++;
        }
        memcpy(data_vec, tpl_data_vec, data_copy_size * sizeof(cell_t));
    }

    if (!data_fill_size)
        return true;

    cell_t* fill_pos = data_vec + data_copy_size;
    if (fill_value) {
        cell_t* fill_end = fill_pos + data_fill_size;
        while (fill_pos < fill_end)
            *fill_pos++ = fill_value;
    } else {
        memset(fill_pos, 0, data_fill_size * sizeof(cell_t));
    }
    return true;
}

bool Runtime::HeapAlloc2dArray(unsigned int length, unsigned int stride, cell_t* local_addr,
                                const cell_t* init) {
    if (length > INT_MAX || stride > INT_MAX) {
        ReportErrorNumber(SP_ERROR_ARRAY_TOO_BIG);
        return false;
    }

    cell_t argv[2] = {(cell_t)stride, (cell_t)length};
    int rv = generateFullArray(2, argv, !init);
    if (rv != SP_ERROR_NONE) {
        ReportErrorNumber(rv);
        return false;
    }

    cell_t array_base = argv[1];
    *local_addr = array_base;

    cell_t* array_phys;
    if ((rv = LocalToPhysAddr(array_base, &array_phys)) != SP_ERROR_NONE) {
        ReportErrorNumber(rv);
        return false;
    }

    if (!init)
        return true;

    for (unsigned int i = 0; i < length; i++) {
        cell_t elt_base = array_phys[i];

        cell_t* elt_phys;
        if ((rv = LocalToPhysAddr(elt_base, &elt_phys)) != SP_ERROR_NONE) {
            ReportErrorNumber(rv);
            return false;
        }

        memcpy(elt_phys, &init[i * stride], stride * sizeof(cell_t));
    }
    return true;
}

void Runtime::EnterHeapScope() {
    enterHeapScope();
}

void Runtime::LeaveHeapScope() {
    leaveHeapScope();
}

cell_t Runtime::GetNullFunctionValue() {
    return 0;
}

bool Runtime::IsNullFunctionId(funcid_t func) {
    return func == GetNullFunctionValue();
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

int
Runtime::AllocArray(unsigned int cells, cell_t* local_addr, cell_t** phys_addr)
{
    if (cells > CELLBOUNDMAX)
        return SP_ERROR_ARRAY_TOO_BIG;
    cell_t realmem = cells * sizeof(cell_t);
    cell_t addr;
    if (heapAllocEx(realmem, &addr) == nullptr)
        return SP_ERROR_HEAPLOW;

    if (local_addr)
        *local_addr = addr;
    if (phys_addr)
        *phys_addr = (cell_t*)(memory_ + addr);

    return SP_ERROR_NONE;
}

bool
Runtime::Invoke(funcid_t fnid, const cell_t* params, unsigned int num_params,
                cell_t* result)
{
    EnterProfileScope profileScope("SourcePawn", "EnterJIT");

    if (!env_->watchdog()->HandleInterrupt()) {
        ReportErrorNumber(SP_ERROR_TIMEOUT);
        return false;
    }

    assert((fnid & 1) != 0);

    unsigned public_id = fnid >> 1;
    ScriptedInvoker* cfun = GetFunctionByMethodIndex(public_id);
    if (!cfun) {
        ReportErrorNumber(SP_ERROR_NOT_FOUND);
        return false;
    }

    if (IsPaused()) {
        ReportErrorNumber(SP_ERROR_NOT_RUNNABLE);
        return false;
    }

    env_->clearPendingException();

    cell_t ignore_result;
    if (result == NULL)
        result = &ignore_result;

    EnterProfileScope scriptScope("SourcePawn", cfun->DebugName());

    RefPtr<MethodInfo> method = cfun->AcquireMethod();
    if (!method) {
        ReportErrorNumber(SP_ERROR_INVALID_ADDRESS);
        return false;
    }

    uint32_t save_sp = sp_;
#ifndef NDEBUG
    uint32_t save_hp_scope = hp_scope_;
    auto heap_pos = heap_.GetPosition();
#endif

    if (!addStack(-int32_t((num_params + 1) * sizeof(cell_t))))
        return false;
    cell_t* sp = heap_.ToPhysAddr<cell_t*>(sp_);

    sp[0] = num_params;
    for (unsigned int i = 0; i < num_params; i++)
        sp[i + 1] = params[i];

    bool ok = env_->Invoke(this, method, result);

#ifndef NDEBUG
    assert(hp_scope_ == save_hp_scope);
    assert(heap_pos == heap_.GetPosition());
#endif
    sp_ = save_sp;
    return ok;
}

int
Runtime::LocalToArrayPtr(cell_t base, ARRAY_PTR* out)
{
    cell_t* phys;
    if (int err = LocalToPhysAddr(base, &phys))
        return err;
    *out = reinterpret_cast<ARRAY_PTR>(phys);
    return SP_ERROR_NONE;
}

void*
Runtime::GetArrayData(ARRAY_PTR handle, uint32_t* size)
{
    if (size)
        *size = 0;
    return reinterpret_cast<void*>(handle);
}

size_t Runtime::HeapSize() const {
    return heap_.committed();
}

size_t Runtime::DataSize() const {
    return data_size_;
}

} // namespace v2
} // namespace sp
