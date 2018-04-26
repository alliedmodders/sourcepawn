// vim: set sts=2 ts=8 sw=2 tw=99 et:
// 
// Copyright (C) 2006-2018 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "builtins.h"
#include <am-float.h>
#include <math.h>

namespace sp {

using namespace SourcePawn;

extern sp_nativeinfo_t gBuiltinFloatNatives[];

BuiltinNatives::BuiltinNatives()
{
}

bool
BuiltinNatives::Initialize()
{
  if (!map_.init(32))
    return false;

  for (size_t i = 0; gBuiltinFloatNatives[i].name != nullptr; i++) {
    const sp_nativeinfo_t& entry = gBuiltinFloatNatives[i];
    NativeMap::Insert p = map_.findForAdd(entry.name);
    assert(!p.found());
    map_.add(p, entry.name, entry.func);
  }

  return true;
}

SPVM_NATIVE_FUNC
BuiltinNatives::Lookup(const char* name)
{
  NativeMap::Result r = map_.find(name);
  if (!r.found())
    return nullptr;
  return r->value;
}

static cell_t
FloatCtor(IPluginContext* pCtx, const cell_t* params)
{
  float val = static_cast<float>(params[1]);

  return sp_ftoc(val);
}

static cell_t
FloatAdd(IPluginContext* pCtx, const cell_t* params)
{
  float val = sp_ctof(params[1]) + sp_ctof(params[2]);

  return sp_ftoc(val);
}

static cell_t
FloatSub(IPluginContext* pCtx, const cell_t* params)
{
  float val = sp_ctof(params[1]) - sp_ctof(params[2]);

  return sp_ftoc(val);
}

static cell_t
FloatMul(IPluginContext* pCtx, const cell_t* params)
{
  float val = sp_ctof(params[1]) * sp_ctof(params[2]);

  return sp_ftoc(val);
}

static cell_t
FloatDiv(IPluginContext* pCtx, const cell_t* params)
{
  float val = sp_ctof(params[1]) / sp_ctof(params[2]);

  return sp_ftoc(val);
}

static cell_t
FloatGt(IPluginContext* pCtx, const cell_t* params)
{
  return !!(sp_ctof(params[1]) > sp_ctof(params[2]));
}

static cell_t
FloatGe(IPluginContext* pCtx, const cell_t* params)
{
  return !!(sp_ctof(params[1]) >= sp_ctof(params[2]));
}

static cell_t
FloatLt(IPluginContext* pCtx, const cell_t* params)
{
  return !!(sp_ctof(params[1]) < sp_ctof(params[2]));
}

static cell_t
FloatLe(IPluginContext* pCtx, const cell_t* params)
{
  return !!(sp_ctof(params[1]) <= sp_ctof(params[2]));
}

static cell_t
FloatEq(IPluginContext* pCtx, const cell_t* params)
{
  return !!(sp_ctof(params[1]) == sp_ctof(params[2]));
}

static cell_t
FloatNe(IPluginContext* pCtx, const cell_t* params)
{
  return !!(sp_ctof(params[1]) != sp_ctof(params[2]));
}

static cell_t
FloatNot(IPluginContext* pCtx, const cell_t* params)
{
  float val = sp_ctof(params[1]);
  if (ke::IsNaN(val))
    return 1;
  return val ? 0 : 1;
}

sp_nativeinfo_t gBuiltinFloatNatives[] = {
  {"__float_ctor",    FloatCtor},
  {"__float_mul",     FloatMul},
  {"__float_div",     FloatDiv},
  {"__float_add",     FloatAdd},
  {"__float_sub",     FloatSub},
  {"__float_gt",      FloatGt},
  {"__float_ge",      FloatGe},
  {"__float_lt",      FloatLt},
  {"__float_le",      FloatLe},
  {"__float_eq",      FloatEq},
  {"__float_ne",      FloatNe},
  {"__float_not",     FloatNot},
  {nullptr,           nullptr},
};

} // namespace sp
