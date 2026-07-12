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
#include "plugin-runtime.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <deque>
#include <unordered_set>

#include <smx/smx-headers.h>
#include <smx/smx-v1-opcodes.h>
#include "builtins.h"
#include "compiled-function.h"
#include "environment.h"
#include "md5/md5.h"
#include "method-info.h"
#include "method-verifier.h"
#include "watchdog_timer.h"

using namespace sp;
using namespace SourcePawn;

#define CELLBOUNDMAX (INT_MAX / sizeof(cell_t))

static const size_t kMinHeapSize = 16384;

PluginRuntime::PluginRuntime(SmxImage* image)
 : BaseRuntime(image),
   memory_(nullptr),
   data_size_(0),
   mem_size_(0),
   m_pNullVec(nullptr),
   m_pNullString(nullptr)
{
    data_size_ = data_.length;
    mem_size_ = image_->HeapSize();

    // Compute and align a minimum memory amount.
    if (mem_size_ < data_size_)
        mem_size_ = data_size_;
    mem_size_ = ke::Align(mem_size_, sizeof(cell_t));

    // Add a minimum heap size if needed.
    if (mem_size_ < data_size_ + kMinHeapSize)
        mem_size_ = data_size_ + kMinHeapSize;
    assert(ke::IsAligned(mem_size_, sizeof(cell_t)));

    hp_ = data_size_;
    sp_ = mem_size_ - sizeof(cell_t);
    stp_ = sp_;
    frm_ = sp_;
    hp_scope_ = -1;

    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
    Environment::get()->RegisterRuntime(this);
}

PluginRuntime::~PluginRuntime() {
    // The watchdog thread takes the global JIT lock while it patches all
    // runtimes. It is not enough to ensure that the unlinking of the runtime is
    // protected; we cannot delete functions or code while the watchdog might be
    // executing. Therefore, the entire destructor is guarded.
    std::lock_guard<ke::Mutex> lock(Environment::get()->lock());

    Environment::get()->DeregisterRuntime(this);

    for (uint32_t i = 0; i < image_->NumPublics(); i++)
        delete entrypoints_[i];

    delete[] memory_;
}

bool
PluginRuntime::Initialize() {
    if (!ke::IsAligned(code_.bytes, sizeof(cell_t))) {
        // Align the code section.
        aligned_code_ = std::make_unique<uint8_t[]>(code_.length);
        if (!aligned_code_)
            return false;

        memcpy(aligned_code_.get(), code_.bytes, code_.length);
        code_.bytes = aligned_code_.get();
    }

    natives_ = std::make_unique<NativeEntry[]>(image_->NumNatives());
    if (!natives_)
        return false;

    publics_ = std::make_unique<sp_public_t[]>(image_->NumPublics());
    if (!publics_)
        return false;
    memset(publics_.get(), 0, sizeof(sp_public_t) * image_->NumPublics());

    pubvars_ = std::make_unique<sp_pubvar_t[]>(image_->NumPubvars());
    if (!pubvars_)
        return false;
    memset(pubvars_.get(), 0, sizeof(sp_pubvar_t) * image_->NumPubvars());

    entrypoints_ = std::make_unique<ScriptedInvoker*[]>(image_->NumPublics());
    if (!entrypoints_)
        return false;
    memset(entrypoints_.get(), 0, sizeof(ScriptedInvoker*) * image_->NumPublics());

    memory_ = new uint8_t[mem_size_];
    if (!memory_)
        return false;
    memset(memory_ + data_size_, 0, mem_size_ - data_size_);
    memcpy(memory_, data_.bytes, data_size_);

    /* Initialize the null references */
    uint32_t index;
    if (FindPubvarByName("NULL_VECTOR", &index) == SP_ERROR_NONE) {
        sp_pubvar_t* pubvar;
        GetPubvarByIndex(index, &pubvar);
        m_pNullVec = pubvar->offs;
    } else {
        m_pNullVec = NULL;
    }

    if (FindPubvarByName("NULL_STRING", &index) == SP_ERROR_NONE) {
        sp_pubvar_t* pubvar;
        GetPubvarByIndex(index, &pubvar);
        m_pNullString = pubvar->offs;
    } else {
        m_pNullString = NULL;
    }

    SetupFloatNativeRemapping();

    if (!function_map_.init(32))
        return false;

    return true;
}

struct NativeMapping {
    const char* name;
    unsigned opcode;
};

static const NativeMapping sNativeMap[] = {
    // Older versions for SourceMod.
    {"FloatAbs", OP_FABS},
    {"FloatAdd", OP_FLOATADD},
    {"FloatSub", OP_FLOATSUB},
    {"FloatMul", OP_FLOATMUL},
    {"FloatDiv", OP_FLOATDIV},
    {"float", OP_FLOAT},
    {"FloatCompare", OP_FLOATCMP},
    {"RoundToCeil", OP_RND_TO_CEIL},
    {"RoundToZero", OP_RND_TO_ZERO},
    {"RoundToFloor", OP_RND_TO_FLOOR},
    {"RoundToNearest", OP_RND_TO_NEAREST},
    {"__FLOAT_GT__", OP_FLOAT_GT},
    {"__FLOAT_GE__", OP_FLOAT_GE},
    {"__FLOAT_LT__", OP_FLOAT_LT},
    {"__FLOAT_LE__", OP_FLOAT_LE},
    {"__FLOAT_EQ__", OP_FLOAT_EQ},
    {"__FLOAT_NE__", OP_FLOAT_NE},
    {"__FLOAT_NOT__", OP_FLOAT_NOT},

    // Newer versions for spshell/sp2.
    {"__float_add", OP_FLOATADD},
    {"__float_sub", OP_FLOATSUB},
    {"__float_mul", OP_FLOATMUL},
    {"__float_div", OP_FLOATDIV},
    {"__float_mod", OP_NOP}, // No asm version
    {"__float_ctor", OP_FLOAT},
    {"__float_gt", OP_FLOAT_GT},
    {"__float_ge", OP_FLOAT_GE},
    {"__float_lt", OP_FLOAT_LT},
    {"__float_le", OP_FLOAT_LE},
    {"__float_eq", OP_FLOAT_EQ},
    {"__float_ne", OP_FLOAT_NE},
    {"__float_not", OP_FLOAT_NOT},
    {NULL, 0},
};

void
PluginRuntime::SetupFloatNativeRemapping() {
    float_table_ = std::make_unique<floattbl_t[]>(image_->NumNatives());
    BuiltinNatives* builtins = Environment::get()->builtins();

    for (size_t i = 0; i < image_->NumNatives(); i++) {
        const char* name = image_->GetNative(i);
        const NativeMapping* iter = sNativeMap;
        while (iter->name) {
            if (strcmp(name, iter->name) == 0) {
                if (builtins->Lookup(name) || iter->opcode == OP_NOP) {
                    float_table_[i].found = true;
                    float_table_[i].index = iter->opcode;
                }
                break;
            }
            iter++;
        }
    }
}

static cell_t
NativeMustBeReplaced(IPluginContext* cx, const cell_t* params) {
    cx->ThrowNativeError("This native was not replaced");
    return 0;
}

void
PluginRuntime::InstallBuiltinNatives() {
    Environment* env = Environment::get();
    for (size_t i = 0; i < image_->NumNatives(); i++) {
        if (!float_table_[i].found)
            continue;

        const char* name = image_->GetNative(i);
        SPVM_NATIVE_FUNC func = env->builtins()->Lookup(name);
        if (!func)
            func = NativeMustBeReplaced;
        UpdateNativeBinding(i, func, 0, nullptr);
    }
}

unsigned
PluginRuntime::GetNativeReplacement(size_t index) {
    if (!float_table_[index].found || float_table_[index].index == OP_NOP)
        return (unsigned)OP_NOP;
    return float_table_[index].index;
}

RefPtr<MethodInfo>
PluginRuntime::GetMethod(cell_t pcode_offset) const {
    FunctionMap::Result r = function_map_.find(pcode_offset);
    if (!r.found())
        return nullptr;
    return r->value;
}

RefPtr<MethodInfo>
PluginRuntime::AcquireMethod(cell_t pcode_offset) {
    FunctionMap::Insert p = function_map_.findForAdd(pcode_offset);
    if (p.found())
        return p->value;

    // Do some quick validation to make sure this is a valid offset. The only
    // real reason to do this is so we don't fill the hash set with bogus
    // methods.
    if (pcode_offset < 0 || size_t(pcode_offset) >= code_.length ||
        !IsAligned(pcode_offset, sizeof(cell_t))) {
        return nullptr;
    }

    const cell_t* address = reinterpret_cast<const cell_t*>(code_.bytes + pcode_offset);
    if (*address != OP_PROC)
        return nullptr;

    RefPtr<MethodInfo> method = new MethodInfo(this, pcode_offset);
    if (!function_map_.add(p, pcode_offset, method))
        return nullptr;

    // Grab the lock before linking code in, since the watchdog timer will look
    // at this list on another thread.
    {
        std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
        methods_.push_back(method);
    }
    return method;
}

const std::vector<RefPtr<MethodInfo>>&
PluginRuntime::AllMethods() const {
    Environment::get()->lock().AssertCurrentThreadOwns();
    return methods_;
}

int
PluginRuntime::FindNativeByName(const char* name, uint32_t* index) {
    size_t idx;
    if (!image_->FindNative(name, &idx))
        return SP_ERROR_NOT_FOUND;

    if (index)
        *index = idx;

    return SP_ERROR_NONE;
}

int
PluginRuntime::UpdateNativeBinding(uint32_t index, SPVM_NATIVE_FUNC pfn, uint32_t flags,
                                   void* data) {
    if (index >= image_->NumNatives())
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

int
PluginRuntime::UpdateNativeBindingObject(uint32_t index, INativeCallback* callback, uint32_t flags,
                                         void* data) {
    RefPtr<INativeCallback> holder(callback);
    if (index >= image_->NumNatives())
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

const sp_native_t*
PluginRuntime::GetNative(uint32_t index) {
    if (index >= image_->NumNatives())
        return nullptr;

    if (!natives_[index].name)
        natives_[index].name = image_->GetNative(index);

    return &natives_[index];
}

uint32_t
PluginRuntime::GetNativesNum() {
    return image_->NumNatives();
}

int
PluginRuntime::FindPublicByName(const char* name, uint32_t* index) {
    size_t idx;
    if (!image_->FindPublic(name, &idx))
        return SP_ERROR_NOT_FOUND;

    if (index)
        *index = idx;
    return SP_ERROR_NONE;
}

int
PluginRuntime::GetPublicByIndex(uint32_t index, sp_public_t** out) {
    if (index >= image_->NumPublics())
        return SP_ERROR_INDEX;

    sp_public_t& entry = publics_[index];
    if (!entry.name) {
        uint32_t offset;
        image_->GetPublic(index, &offset, &entry.name);
        entry.code_offs = offset;
        entry.funcid = (index << 1) | 1;
    }

    if (out)
        *out = &entry;
    return SP_ERROR_NONE;
}

uint32_t
PluginRuntime::GetPublicsNum() {
    return image_->NumPublics();
}

int
PluginRuntime::GetPubvarByIndex(uint32_t index, sp_pubvar_t** out) {
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

int
PluginRuntime::FindPubvarByName(const char* name, uint32_t* index) {
    size_t idx;
    if (!image_->FindPubvar(name, &idx))
        return SP_ERROR_NOT_FOUND;

    if (index)
        *index = idx;
    return SP_ERROR_NONE;
}

int
PluginRuntime::GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr) {
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
PluginRuntime::GetPubVarsNum() {
    return image_->NumPubvars();
}

IPluginFunction*
PluginRuntime::GetFunctionById(funcid_t func_id) {
    ScriptedInvoker* pFunc = NULL;

    if (func_id & 1) {
        func_id >>= 1;
        if (func_id >= image_->NumPublics())
            return NULL;
        pFunc = entrypoints_[func_id];
        if (!pFunc) {
            entrypoints_[func_id] = new ScriptedInvoker(this, (func_id << 1) | 1, func_id);
            pFunc = entrypoints_[func_id];
        }
    }

    return pFunc;
}

ScriptedInvoker*
PluginRuntime::GetPublicFunction(size_t index) {
    assert(index < image_->NumPublics());
    ScriptedInvoker* pFunc = entrypoints_[index];
    if (!pFunc) {
        sp_public_t* pub = NULL;
        GetPublicByIndex(index, &pub);
        if (pub)
            entrypoints_[index] = new ScriptedInvoker(this, (index << 1) | 1, index);
        pFunc = entrypoints_[index];
    }

    return pFunc;
}

IPluginFunction*
PluginRuntime::GetFunctionByName(const char* public_name) {
    uint32_t index;

    if (FindPublicByName(public_name, &index) != SP_ERROR_NONE)
        return NULL;

    return GetPublicFunction(index);
}

bool
PluginRuntime::IsDebugging() {
    return true;
}

size_t
PluginRuntime::GetMemUsage() {
    return sizeof(*this) + image_->ImageSize() + (aligned_code_ ? code_.length : 0) + HeapSize();
}

bool
PluginRuntime::PerformFullValidation() {
    std::unordered_set<cell_t> seen;
    std::deque<cell_t> work;

    Environment* env = Environment::get();
    for (size_t i = 0; i < GetPublicsNum(); i++) {
        int err;
        sp_public_t* fun;
        if ((err = GetPublicByIndex(i, &fun)) != SP_ERROR_NONE) {
            env->ReportErrorFmt(SP_ERROR_USER,
                                "Could not get public function at index %" KE_FMT_SIZET "\n", i);
            return false;
        }
        assert(seen.find(fun->code_offs) == seen.end());
        seen.insert(fun->code_offs);
        work.push_back(fun->code_offs);
    }

    auto onExternFuncRef = [&seen, &work](cell_t offset) -> void {
        if (seen.find(offset) != seen.end())
            return;
        seen.insert(offset);
        work.push_back(offset);
    };

    while (!work.empty()) {
        cell_t offset = work.front();
        work.pop_front();

        const char* name = image_->LookupFunction(offset);
        if (!name)
            name = "<unknown>";

        MethodVerifier verifier(this, offset);
        verifier.collectExternalFuncRefs(onExternFuncRef);

        if (!verifier.verify()) {
            env->ReportErrorFmt(SP_ERROR_USER, "Method %s failed verification: %s\n", name,
                                env->GetErrorString(verifier.error()));
            return false;
        }
    }
    return true;
}

bool
PluginRuntime::UsesDirectArrays() {
    auto features = image()->DescribeCode().features;
    return !!(features & SmxConsts::kCodeFeatureDirectArrays);
}

bool
PluginRuntime::UsesHeapScopes() {
    auto features = image()->DescribeCode().features;
    return !!(features & SmxConsts::kCodeFeatureHeapScopes);
}

int PluginRuntime::AllocArray(unsigned int cells, cell_t* local_addr, cell_t** phys_addr) {
    cell_t* addr;
    ucell_t realmem;

    assert(cells < CELLBOUNDMAX);

    realmem = cells * sizeof(cell_t);

    /**
   * Check if the space between the heap and stack is sufficient.
   */
    if ((cell_t)(sp_ - hp_ - realmem) < STACK_MARGIN)
        return SP_ERROR_HEAPLOW;

    addr = (cell_t*)(memory_ + hp_);
    /* store size of allocation in cells */
    *addr = (cell_t)cells;
    addr++;
    hp_ += sizeof(cell_t);

    *local_addr = hp_;

#ifdef DEBUG
    memset(addr, 0xcd, realmem);
#endif

    if (phys_addr)
        *phys_addr = addr;

    hp_ += realmem;

    return SP_ERROR_NONE;
}

int
PluginRuntime::LocalToPhysAddr(cell_t local_addr, cell_t** phys_addr) {
    if (((local_addr >= hp_) && (local_addr < sp_)) || (local_addr < 0) ||
        ((ucell_t)local_addr >= mem_size_)) {
        return SP_ERROR_INVALID_ADDRESS;
    }

    if (phys_addr)
        *phys_addr = (cell_t*)(memory_ + local_addr);

    return SP_ERROR_NONE;
}

int
PluginRuntime::LocalToString(cell_t local_addr, char** addr) {
    if (((local_addr >= hp_) && (local_addr < sp_)) || (local_addr < 0) ||
        ((ucell_t)local_addr >= mem_size_)) {
        return SP_ERROR_INVALID_ADDRESS;
    }
    *addr = (char*)(memory_ + local_addr);

    return SP_ERROR_NONE;
}

int
PluginRuntime::StringToLocal(cell_t local_addr, size_t bytes, const char* source) {
    char* dest;
    size_t len;

    if (((local_addr >= hp_) && (local_addr < sp_)) || (local_addr < 0) ||
        ((ucell_t)local_addr >= mem_size_)) {
        return SP_ERROR_INVALID_ADDRESS;
    }

    if (bytes == 0)
        return SP_ERROR_NONE;

    len = strlen(source);
    dest = (char*)(memory_ + local_addr);

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

int
PluginRuntime::StringToLocalUTF8(cell_t local_addr, size_t maxbytes, const char* source,
                                 size_t* wrtnbytes) {
    char* dest;
    size_t len;
    bool needtocheck = false;

    if (((local_addr >= hp_) && (local_addr < sp_)) || (local_addr < 0) ||
        ((ucell_t)local_addr >= mem_size_)) {
        return SP_ERROR_INVALID_ADDRESS;
    }

    if (maxbytes == 0)
        return SP_ERROR_NONE;

    len = strlen(source);
    dest = (char*)(memory_ + local_addr);

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

int
PluginRuntime::LocalToStringNULL(cell_t local_addr, char** addr) {
    int err;
    if ((err = LocalToString(local_addr, addr)) != SP_ERROR_NONE)
        return err;

    if ((cell_t*)*addr == m_pNullString)
        *addr = NULL;

    return SP_ERROR_NONE;
}

cell_t*
PluginRuntime::GetNullRef(SP_NULL_TYPE type) {
    if (type == SP_NULL_VECTOR)
        return m_pNullVec;

    return NULL;
}

bool
PluginRuntime::IsInExec() {
    for (InvokeFrame* ivk = env_->top(); ivk; ivk = ivk->prev()) {
        if (ivk->cx() == this)
            return true;
    }
    return false;
}

bool
PluginRuntime::Invoke(funcid_t fnid, const cell_t* params, unsigned int num_params,
                      cell_t* result) {
    EnterProfileScope profileScope("SourcePawn", "EnterJIT");

    if (!env_->watchdog()->HandleInterrupt()) {
        ReportErrorNumber(SP_ERROR_TIMEOUT);
        return false;
    }

    assert((fnid & 1) != 0);

    unsigned public_id = fnid >> 1;
    ScriptedInvoker* cfun = GetPublicFunction(public_id);
    if (!cfun) {
        ReportErrorNumber(SP_ERROR_NOT_FOUND);
        return false;
    }

    if (IsPaused()) {
        ReportErrorNumber(SP_ERROR_NOT_RUNNABLE);
        return false;
    }

    if ((cell_t)(hp_ + 16 * sizeof(cell_t)) > (cell_t)(sp_ - (sizeof(cell_t) * (num_params + 1)))) {
        ReportErrorNumber(SP_ERROR_STACKLOW);
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
    cell_t save_sp = sp_;
    cell_t save_hp = hp_;
    cell_t save_frm = frm_;
    cell_t save_hp_scope = hp_scope_;

    /* Push parameters */
    sp_ -= sizeof(cell_t) * (num_params + 1);
    cell_t* sp = (cell_t*)(memory_ + sp_);

    sp[0] = num_params;
    for (unsigned int i = 0; i < num_params; i++)
        sp[i + 1] = params[i];

    // Enter the execution engine.
    bool ok = env_->Invoke(this, method, result);

    sp_ = save_sp;
    hp_ = save_hp;
    frm_ = save_frm;
    hp_scope_ = save_hp_scope;
    return ok;
}

IPluginRuntime*
PluginRuntime::GetRuntime() {
    return this;
}

cell_t*
PluginRuntime::GetLocalParams() {
    return (cell_t*)(memory_ + frm_ + (2 * sizeof(cell_t)));
}

int
PluginRuntime::popTrackerAndSetHeap() {
    assert(sp_ >= hp_);
    assert(hp_ >= cell_t(data_size_));

    if (hp_ - cell_t(data_size_) < (cell_t)sizeof(cell_t))
        return SP_ERROR_TRACKER_BOUNDS;

    hp_ -= sizeof(cell_t);
    cell_t amt = *reinterpret_cast<cell_t*>(memory_ + hp_);

    if (amt < 0 || hp_ - cell_t(data_size_) < amt)
        return SP_ERROR_TRACKER_BOUNDS;

    hp_ -= amt;
    return SP_ERROR_NONE;
}

int
PluginRuntime::pushTracker(uint32_t amount) {
    assert(!UsesHeapScopes());
    if (amount > INT_MAX)
        return SP_ERROR_TRACKER_BOUNDS;
    if (sp_ - hp_ < STACK_MARGIN)
        return SP_ERROR_TRACKER_BOUNDS;

    *reinterpret_cast<cell_t*>(memory_ + hp_) = amount;
    hp_ += sizeof(cell_t);
    return SP_ERROR_NONE;
}

bool
PluginRuntime::enterHeapScope() {
    auto old_hp_scope = hp_scope_;

    if (!heapAlloc(sizeof(cell_t), &hp_scope_))
        return false;

    cell_t* scope = throwIfBadAddress(hp_scope_);
    if (!scope)
        return false;

    *scope = old_hp_scope;
    return true;
}

bool
PluginRuntime::leaveHeapScope() {
    cell_t* scope = throwIfBadAddress(hp_scope_);
    if (!scope)
        return false;

    auto prev_hp_scope = *scope;
    hp_ = hp_scope_;
    hp_scope_ = prev_hp_scope;

    if (hp_scope_ != -1 && !throwIfBadAddress(hp_scope_))
        return false;
    return true;
}

struct array_creation_t {
    const cell_t* dim_list; /* Dimension sizes */
    cell_t dim_count;       /* Number of dimensions */
    cell_t* data_offs;      /* Current offset AFTER the indirection vectors (data) */
    cell_t* base;           /* array base */
};

static cell_t
GenerateInnerArrayIndirectionVectors(array_creation_t* ar, int dim, cell_t cur_offs) {
    cell_t write_offs = cur_offs;
    cell_t* data_offs = ar->data_offs;

    cur_offs += ar->dim_list[dim];

    // Dimension n-x where x > 2 will have sub-vectors.
    // Otherwise, we just need to reference the data section.
    if (ar->dim_count > 2 && dim < ar->dim_count - 2) {
        // For each index at this dimension, write offstes to our sub-vectors.
        // After we write one sub-vector, we generate its sub-vectors recursively.
        // At the end, we're given the next offset we can use.
        for (int i = 0; i < ar->dim_list[dim]; i++) {
            ar->base[write_offs] = (cur_offs - write_offs) * sizeof(cell_t);
            write_offs++;
            cur_offs = GenerateInnerArrayIndirectionVectors(ar, dim + 1, cur_offs);
        }
    } else {
        // In this section, there are no sub-vectors, we need to write offsets
        // to the data.  This is separate so the data stays in one big chunk.
        // The data offset will increment by the size of the last dimension,
        // because that is where the data is finally computed as.
        for (int i = 0; i < ar->dim_list[dim]; i++) {
            ar->base[write_offs] = (*data_offs - write_offs) * sizeof(cell_t);
            write_offs++;
            *data_offs = *data_offs + ar->dim_list[dim + 1];
        }
    }

    return cur_offs;
}

static cell_t
calc_indirection(const array_creation_t* ar, cell_t dim) {
    cell_t size = ar->dim_list[dim];
    if (dim < ar->dim_count - 2)
        size += ar->dim_list[dim] * calc_indirection(ar, dim + 1);
    return size;
}

static cell_t
GenerateArrayIndirectionVectors(cell_t* arraybase, cell_t dims[], cell_t _dimcount) {
    array_creation_t ar;
    cell_t data_offs;

    /* Reverse the dimensions */
    std::vector<cell_t> dim_list;
    for (int i = _dimcount - 1; i >= 0; i--)
        dim_list.emplace_back(dims[i]);

    ar.base = arraybase;
    ar.dim_list = &dim_list[0];
    ar.dim_count = _dimcount;
    ar.data_offs = &data_offs;

    data_offs = calc_indirection(&ar, 0);
    GenerateInnerArrayIndirectionVectors(&ar, 0, 0);
    return data_offs;
}

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

int
PluginRuntime::generateFullArray(uint32_t argc, cell_t* argv, int autozero) {
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
    if (new_hp >= sp_ - STACK_MARGIN)
        return SP_ERROR_HEAPLOW;

    cell_t* base = reinterpret_cast<cell_t*>(memory_ + hp_);

    if (autozero) {
        memset(reinterpret_cast<uint8_t*>(base) + iv_size, 0, bytes - iv_size);
    }

    if (image_->DescribeCode().features & SmxConsts::kCodeFeatureDirectArrays) {
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
    } else {
        cell_t offs = GenerateArrayIndirectionVectors(base, argv, argc);
        assert(size_t(offs) == cells);
        (void)offs;
    }

    argv[argc - 1] = hp_;
    hp_ = new_hp;

    if (!UsesHeapScopes()) {
        if (int err = pushTracker(bytes))
            return err;
    }
    return SP_ERROR_NONE;
}

int
PluginRuntime::generateArray(cell_t dims, cell_t* stk, bool autozero) {
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
        if (!UsesHeapScopes()) {
            if (int err = pushTracker(bytes))
                return err;
        }

        if (autozero)
            memset(memory_ + *stk, 0, bytes);

        return SP_ERROR_NONE;
    }

    if (int err = generateFullArray(dims, stk, autozero))
        return err;

    return SP_ERROR_NONE;
}

bool
PluginRuntime::pushAmxFrame() {
    if (!pushStack(frm_))
        return false;

    // Save the entry heap scope.
    cell_t saved_hp_scope;
    if (!heapAlloc(sizeof(cell_t), &saved_hp_scope))
        return false;
    cell_t* saved_hp_scope_addr = throwIfBadAddress(saved_hp_scope);
    if (!saved_hp_scope_addr)
        return false;
    *saved_hp_scope_addr = hp_scope_;

    if (!pushStack(saved_hp_scope))
        return false;

    frm_ = sp_;
    return true;
}

bool
PluginRuntime::popAmxFrame() {
    sp_ = frm_;

    cell_t saved_hp_scope;
    if (!popStack(&saved_hp_scope))
        return false;

    cell_t* saved_hp_scope_addr = throwIfBadAddress(saved_hp_scope);
    if (!saved_hp_scope_addr)
        return false;

    // Actual scope address is pushed onto the heap.
    hp_scope_ = *saved_hp_scope_addr;
    // The heap address containing the scope address is where we can reset the
    // heap to. pushAmxFrame calls heapAlloc, which returns |hp| and sets |hp|
    // to |hp + 4|. So by restoring |hp| here, we're also restoring the original
    // heap address.
    hp_ = saved_hp_scope;

    if ((uint32_t)hp_ < data_size_ || hp_ > sp_) {
        ReportErrorNumber(SP_ERROR_INVALID_ADDRESS);
        return false;
    }

    if (!popStack(&frm_))
        return false;

    cell_t nargs;
    if (!popStack(&nargs))
        return false;

    if (nargs < 0 || cell_t(sp_ + nargs * sizeof(cell_t)) > stp_) {
        ReportErrorNumber(SP_ERROR_STACKMIN);
        return false;
    }

    sp_ += nargs * sizeof(cell_t);
    return true;
}

bool
PluginRuntime::pushStack(cell_t value) {
    if (sp_ <= cell_t(hp_ + sizeof(cell_t))) {
        ReportErrorNumber(SP_ERROR_STACKLOW);
        return false;
    }
    sp_ -= sizeof(cell_t);

    *reinterpret_cast<cell_t*>(memory_ + sp_) = value;
    return true;
}

bool
PluginRuntime::popStack(cell_t* out) {
    if (sp_ >= stp_) {
        ReportErrorNumber(SP_ERROR_STACKMIN);
        return false;
    }
    *out = *reinterpret_cast<cell_t*>(memory_ + sp_);

    sp_ += sizeof(cell_t);
    return true;
}

bool
PluginRuntime::getFrameValue(cell_t offset, cell_t* out) {
    cell_t* addr = throwIfBadAddress(frm_ + offset);
    if (!addr)
        return false;

    *out = *addr;
    return true;
}

bool
PluginRuntime::setFrameValue(cell_t offset, cell_t value) {
    cell_t* addr = throwIfBadAddress(frm_ + offset);
    if (!addr)
        return false;

    *addr = value;
    return true;
}

bool
PluginRuntime::getCellValue(cell_t address, cell_t* out) {
    assert((uintptr_t)(const void*)out % sizeof(cell_t) == 0);

    cell_t* ptr = throwIfBadAddress(address);
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

bool
PluginRuntime::setCellValue(cell_t address, cell_t value) {
    cell_t* ptr = throwIfBadAddress(address);
    if (!ptr)
        return false;

    *ptr = value;
    return true;
}

bool
PluginRuntime::heapAlloc(cell_t amount, cell_t* out) {
    return heapAllocEx(amount, out) != nullptr;
}

cell_t*
PluginRuntime::heapAllocEx(cell_t amount, cell_t* out) {
    cell_t new_hp = hp_ + amount;

    if (amount < 0) {
        // Note: signed compare, in case new_hp is negative.
        if (new_hp < cell_t(data_size_)) {
            ReportErrorNumber(SP_ERROR_HEAPMIN);
            return nullptr;
        }
    } else {
        if (new_hp + STACK_MARGIN > sp_) {
            ReportErrorNumber(SP_ERROR_HEAPLOW);
            return nullptr;
        }
    }

    *out = hp_;
    hp_ = new_hp;
    return reinterpret_cast<cell_t*>(memory_ + *out);
}

cell_t*
PluginRuntime::acquireAddrRange(cell_t address, uint32_t bounds) {
    cell_t* addr = throwIfBadAddress(address);
    if (!addr)
        return nullptr;
    if (bounds && !throwIfBadAddress(address + bounds - 1))
        return nullptr;
    return addr;
}

cell_t*
PluginRuntime::throwIfBadAddress(cell_t addr) {
    if (addr < 0 || (addr >= hp_ && addr < sp_) || addr >= stp_) {
        ReportErrorNumber(SP_ERROR_INVALID_ADDRESS);
        return nullptr;
    }
    return reinterpret_cast<cell_t*>(memory_ + addr);
}

bool
PluginRuntime::addStack(cell_t amount) {
    cell_t new_sp = sp_ + amount;

    if (amount < 0) {
        // Note: signed compare, in case new_sp is negative.
        if (new_sp < hp_ + STACK_MARGIN) {
            ReportErrorNumber(SP_ERROR_STACKLOW);
            return false;
        }
    } else {
        if (new_sp > stp_) {
            ReportErrorNumber(SP_ERROR_STACKMIN);
            return false;
        }
    }

    sp_ = new_sp;
    return true;
}

bool
PluginRuntime::initArray(cell_t array_addr, cell_t dat_addr, cell_t iv_size, cell_t data_copy_size,
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

bool
PluginRuntime::HeapAlloc2dArray(unsigned int length, unsigned int stride, cell_t* local_addr,
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

    bool direct_arrays = UsesDirectArrays();
    for (unsigned int i = 0; i < length; i++) {
        cell_t elt_base;

        if (direct_arrays)
            elt_base = array_phys[i];
        else
            elt_base = array_base + (i * sizeof(cell_t)) + array_phys[i];

        cell_t* elt_phys;
        if ((rv = LocalToPhysAddr(elt_base, &elt_phys)) != SP_ERROR_NONE) {
            ReportErrorNumber(rv);
            return false;
        }

        memcpy(elt_phys, &init[i * stride], stride * sizeof(cell_t));
    }
    return true;
}

void
PluginRuntime::EnterHeapScope() {
    enterHeapScope();
}

void
PluginRuntime::LeaveHeapScope() {
    leaveHeapScope();
}

cell_t
PluginRuntime::GetNullFunctionValue() {
    if (image_->DescribeCode().features & SmxConsts::kCodeFeatureNullFunctions) {
        return 0;
    }
    return -1;
}

bool
PluginRuntime::IsNullFunctionId(funcid_t func) {
    return func == GetNullFunctionValue();
}

bool
PluginRuntime::GetFunctionByIdOrNull(funcid_t func, IPluginFunction** out) {
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

IPluginFunction*
PluginRuntime::GetFunctionByIdOrError(funcid_t func_id) {
    if (auto fn = GetFunctionById(func_id))
        return fn;
    ReportError("Invalid function id: 0x%08x", func_id);
    return nullptr;
}

ke::RefPtr<BaseMethodInfo>
PluginRuntime::GetMethodFromFrameId(uint32_t frame_id) const {
    return GetMethod(frame_id);
}

int PluginRuntime::LocalToArrayPtr(cell_t base, ARRAY_PTR* out) {
    cell_t* phys;
    if (int err = LocalToPhysAddr(base, &phys))
        return err;
    *out = reinterpret_cast<ARRAY_PTR>(phys);
    return SP_ERROR_NONE;
}

void* PluginRuntime::GetArrayData(ARRAY_PTR handle, uint32_t* size) {
    if (size)
        *size = 0;
    return reinterpret_cast<void*>(handle);
}
