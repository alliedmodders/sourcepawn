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

#include <unordered_set>
#include <deque>

#include <smx/smx-v1-opcodes.h>
#include "builtins.h"
#include "compiled-function.h"
#include "environment.h"
#include "md5/md5.h"
#include "method-info.h"
#include "method-verifier.h"
#include "plugin-context.h"

using namespace sp;
using namespace SourcePawn;

PluginRuntime::PluginRuntime(LegacyImage* image)
 : image_(image),
   paused_(false),
   computed_code_hash_(false),
   computed_data_hash_(false)
{
  code_ = image_->DescribeCode();
  data_ = image_->DescribeData();
  memset(code_hash_, 0, sizeof(code_hash_));
  memset(data_hash_, 0, sizeof(data_hash_));

  std::lock_guard<ke::Mutex> lock(Environment::get()->lock());
  Environment::get()->RegisterRuntime(this);
}

PluginRuntime::~PluginRuntime()
{
  // The watchdog thread takes the global JIT lock while it patches all
  // runtimes. It is not enough to ensure that the unlinking of the runtime is
  // protected; we cannot delete functions or code while the watchdog might be
  // executing. Therefore, the entire destructor is guarded.
  std::lock_guard<ke::Mutex> lock(Environment::get()->lock());

  Environment::get()->DeregisterRuntime(this);

  for (uint32_t i = 0; i < image_->NumPublics(); i++)
    delete entrypoints_[i];
}

bool
PluginRuntime::Initialize()
{
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

  context_ = std::make_unique<PluginContext>(this);
  if (!context_->Initialize())
    return false;

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
  { "FloatAbs",       OP_FABS },
  { "FloatAdd",       OP_FLOATADD },
  { "FloatSub",       OP_FLOATSUB },
  { "FloatMul",       OP_FLOATMUL },
  { "FloatDiv",       OP_FLOATDIV },
  { "float",          OP_FLOAT },
  { "FloatCompare",   OP_FLOATCMP },
  { "RoundToCeil",    OP_RND_TO_CEIL },
  { "RoundToZero",    OP_RND_TO_ZERO },
  { "RoundToFloor",   OP_RND_TO_FLOOR },
  { "RoundToNearest", OP_RND_TO_NEAREST },
  { "__FLOAT_GT__",   OP_FLOAT_GT },
  { "__FLOAT_GE__",   OP_FLOAT_GE },
  { "__FLOAT_LT__",   OP_FLOAT_LT },
  { "__FLOAT_LE__",   OP_FLOAT_LE },
  { "__FLOAT_EQ__",   OP_FLOAT_EQ },
  { "__FLOAT_NE__",   OP_FLOAT_NE },
  { "__FLOAT_NOT__",  OP_FLOAT_NOT },

  // Newer versions for spshell/sp2.
  { "__float_add",    OP_FLOATADD },
  { "__float_sub",    OP_FLOATSUB },
  { "__float_mul",    OP_FLOATMUL },
  { "__float_div",    OP_FLOATDIV },
  { "__float_mod",    OP_NOP }, // No asm version
  { "__float_ctor",   OP_FLOAT },
  { "__float_gt",     OP_FLOAT_GT },
  { "__float_ge",     OP_FLOAT_GE },
  { "__float_lt",     OP_FLOAT_LT },
  { "__float_le",     OP_FLOAT_LE },
  { "__float_eq",     OP_FLOAT_EQ },
  { "__float_ne",     OP_FLOAT_NE },
  { "__float_not",    OP_FLOAT_NOT },
  { NULL,             0 },
};

void
PluginRuntime::SetupFloatNativeRemapping()
{
  float_table_ = std::make_unique<floattbl_t[]>(image_->NumNatives());
  for (size_t i = 0; i < image_->NumNatives(); i++) {
    const char* name = image_->GetNative(i);
    const NativeMapping* iter = sNativeMap;
    while (iter->name) {
      if (strcmp(name, iter->name) == 0) {
        float_table_[i].found = true;
        float_table_[i].index = iter->opcode;
        break;
      }
      iter++;
    }
  }
}

static cell_t
NativeMustBeReplaced(IPluginContext* cx, const cell_t* params)
{
  cx->ThrowNativeError("This native was not replaced");
  return 0;
}

void
PluginRuntime::InstallBuiltinNatives()
{
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
PluginRuntime::GetNativeReplacement(size_t index)
{
  if (!float_table_[index].found || float_table_[index].index == OP_NOP)
    return (unsigned)OP_NOP;
  return float_table_[index].index;
}

void
PluginRuntime::SetNames(const char* fullname, const char* name)
{
  name_ = name;
  full_name_ = fullname;
}

RefPtr<MethodInfo>
PluginRuntime::GetMethod(cell_t pcode_offset) const
{
  FunctionMap::Result r = function_map_.find(pcode_offset);
  if (!r.found())
    return nullptr;
  return r->value;
}

RefPtr<MethodInfo>
PluginRuntime::AcquireMethod(cell_t pcode_offset)
{
  FunctionMap::Insert p = function_map_.findForAdd(pcode_offset);
  if (p.found())
    return p->value;

  // Do some quick validation to make sure this is a valid offset. The only
  // real reason to do this is so we don't fill the hash set with bogus
  // methods.
  if (pcode_offset < 0 ||
      size_t(pcode_offset) >= code_.length ||
      !IsAligned(pcode_offset, sizeof(cell_t)))
  {
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
PluginRuntime::AllMethods() const
{
  Environment::get()->lock().AssertCurrentThreadOwns();
  return methods_;
}

int
PluginRuntime::FindNativeByName(const char* name, uint32_t* index)
{
  size_t idx;
  if (!image_->FindNative(name, &idx))
    return SP_ERROR_NOT_FOUND;

  if (index)
    *index = idx;

  return SP_ERROR_NONE;
}

int
PluginRuntime::GetNativeByIndex(uint32_t index, sp_native_t** native)
{
  return SP_ERROR_PARAM;
}

int
PluginRuntime::UpdateNativeBinding(uint32_t index, SPVM_NATIVE_FUNC pfn, uint32_t flags, void* data)
{
  if (index >= image_->NumNatives())
    return SP_ERROR_INDEX;

  NativeEntry* native = &natives_[index];

  // The native must either be unbound, or it must be ephemeral or optional.
  // Otherwise, we've already baked its address in at callsites and it's too
  // late to fix them.
  if (native->status == SP_NATIVE_BOUND &&
      !(native->flags & (SP_NTVFLAG_OPTIONAL|SP_NTVFLAG_EPHEMERAL)))
  {
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
                                         void* data)
{
  RefPtr<INativeCallback> holder(callback);
  if (index >= image_->NumNatives())
    return SP_ERROR_INDEX;

  NativeEntry* native = &natives_[index];

  // The native must either be unbound, or it must be ephemeral or optional.
  // Otherwise, we've already baked its address in at callsites and it's too
  // late to fix them.
  if (native->status == SP_NATIVE_BOUND &&
      !(native->flags & (SP_NTVFLAG_OPTIONAL|SP_NTVFLAG_EPHEMERAL)))
  {
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
PluginRuntime::GetNative(uint32_t index)
{
  if (index >= image_->NumNatives())
    return nullptr;

  if (!natives_[index].name)
    natives_[index].name = image_->GetNative(index);

  return &natives_[index];
}

uint32_t
PluginRuntime::GetNativesNum()
{
  return image_->NumNatives();
}

int
PluginRuntime::FindPublicByName(const char* name, uint32_t* index)
{
  size_t idx;
  if (!image_->FindPublic(name, &idx))
    return SP_ERROR_NOT_FOUND;

  if (index)
    *index = idx;
  return SP_ERROR_NONE;
}

int
PluginRuntime::GetPublicByIndex(uint32_t index, sp_public_t** out)
{
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
PluginRuntime::GetPublicsNum()
{
  return image_->NumPublics();
}

int
PluginRuntime::GetPubvarByIndex(uint32_t index, sp_pubvar_t** out)
{
  if (index >= image_->NumPubvars())
    return SP_ERROR_INDEX;

  sp_pubvar_t* pubvar = &pubvars_[index];
  if (!pubvar->name) {
    uint32_t offset;
    image_->GetPubvar(index, &offset, &pubvar->name);
    if (int err = context_->LocalToPhysAddr(offset, &pubvar->offs))
      return err;
  }

  if (out)
    *out = pubvar;
  return SP_ERROR_NONE;
}

int
PluginRuntime::FindPubvarByName(const char* name, uint32_t* index)
{
  size_t idx;
  if (!image_->FindPubvar(name, &idx))
    return SP_ERROR_NOT_FOUND;

  if (index)
    *index = idx;
  return SP_ERROR_NONE;
}

int
PluginRuntime::GetPubvarAddrs(uint32_t index, cell_t* local_addr, cell_t** phys_addr)
{
  if (index >= image_->NumPubvars())
    return SP_ERROR_INDEX;

  uint32_t offset;
  image_->GetPubvar(index, &offset, nullptr);

  if (int err = context_->LocalToPhysAddr(offset, phys_addr))
    return err;
  *local_addr = offset;
  return SP_ERROR_NONE;
}

uint32_t
PluginRuntime::GetPubVarsNum()
{
  return image_->NumPubvars();
}

IPluginContext*
PluginRuntime::GetDefaultContext()
{
  return context_.get();
}

IPluginDebugInfo*
PluginRuntime::GetDebugInfo()
{
  return this;
}

IPluginFunction*
PluginRuntime::GetFunctionById(funcid_t func_id)
{
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
PluginRuntime::GetPublicFunction(size_t index)
{
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
PluginRuntime::GetFunctionByName(const char* public_name)
{
  uint32_t index;

  if (FindPublicByName(public_name, &index) != SP_ERROR_NONE)
    return NULL;

  return GetPublicFunction(index);
}

bool
PluginRuntime::IsDebugging()
{
  return true;
}

void
PluginRuntime::SetPauseState(bool paused)
{
  paused_ = paused;
}

bool
PluginRuntime::IsPaused()
{
  return paused_;
}

size_t
PluginRuntime::GetMemUsage()
{
  return sizeof(*this) +
         sizeof(PluginContext) +
         image_->ImageSize() +
         (aligned_code_ ? code_.length : 0) +
         context_->HeapSize();
}

unsigned char*
PluginRuntime::GetCodeHash()
{
  if (!computed_code_hash_) {
    MD5 md5_pcode;
    md5_pcode.update((const unsigned char*)code_.bytes, code_.length);
    md5_pcode.finalize();
    md5_pcode.raw_digest(code_hash_);
    computed_code_hash_ = true;
  }
  return code_hash_;
}

unsigned char*
PluginRuntime::GetDataHash()
{
  if (!computed_data_hash_) {
    MD5 md5_data;
    md5_data.update((const unsigned char*)data_.bytes, data_.length);
    md5_data.finalize();
    md5_data.raw_digest(data_hash_);
    computed_data_hash_ = true;
  }
  return data_hash_;
}

PluginContext*
PluginRuntime::GetBaseContext()
{
  return context_.get();
}

int
PluginRuntime::ApplyCompilationOptions(ICompilation* co)
{
  return SP_ERROR_NONE;
}

int
PluginRuntime::LookupLine(ucell_t addr, uint32_t* line)
{
  if (!image_->LookupLine(addr, line))
    return SP_ERROR_NOT_FOUND;
  return SP_ERROR_NONE;
}

int
PluginRuntime::LookupFunction(ucell_t addr, const char** out)
{
  const char* name = image_->LookupFunction(addr);
  if (!name)
    return SP_ERROR_NOT_FOUND;
  if (out)
    *out = name;
  return SP_ERROR_NONE;
}

int
PluginRuntime::LookupFile(ucell_t addr, const char** out)
{
  const char* name = image_->LookupFile(addr);
  if (!name)
    return SP_ERROR_NOT_FOUND;
  if (out)
    *out = name;
  return SP_ERROR_NONE;
}

size_t
PluginRuntime::NumFiles()
{
  return image_->NumFiles();
}

const char*
PluginRuntime::GetFileName(size_t index)
{
  return image_->GetFileName(index);
}

int
PluginRuntime::LookupFunctionAddress(const char* function, const char* file, ucell_t* addr)
{
  if (!image_->LookupFunctionAddress(function, file, addr))
    return SP_ERROR_NOT_FOUND;
  return SP_ERROR_NONE;
}

int
PluginRuntime::LookupLineAddress(const uint32_t line, const char* file, ucell_t* addr)
{
  if (!image_->LookupLineAddress(line, file, addr))
    return SP_ERROR_NOT_FOUND;
  return SP_ERROR_NONE;
}

bool
PluginRuntime::PerformFullValidation()
{
  std::unordered_set<cell_t> seen;
  std::deque<cell_t> work;

  Environment* env = Environment::get();
  for (size_t i = 0; i < GetPublicsNum(); i++) {
    int err;
    sp_public_t* fun;
    if ((err = GetPublicByIndex(i, &fun)) != SP_ERROR_NONE) {
      env->ReportErrorFmt(SP_ERROR_USER, "Could not get public function at index %" KE_FMT_SIZET "\n", i);
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

    const char* name;
    int err = GetDebugInfo()->LookupFunction(offset, &name);
    if (err != SP_ERROR_NONE)
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
PluginRuntime::UsesDirectArrays()
{
  auto features = image()->DescribeCode().features;
  return !!(features & SmxConsts::kCodeFeatureDirectArrays);
}

bool
PluginRuntime::UsesHeapScopes()
{
  auto features = image()->DescribeCode().features;
  return !!(features & SmxConsts::kCodeFeatureHeapScopes);
}
