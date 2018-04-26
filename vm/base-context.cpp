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
#include "base-context.h"
#include "environment.h"

using namespace sp;
using namespace SourcePawn;

BasePluginContext::BasePluginContext()
 : env_(Environment::get())
{
}

BasePluginContext::~BasePluginContext()
{
}

void
BasePluginContext::SetKey(int k, void* value)
{
  if (k < 1 || k > 4)
    return;

  m_keys[k - 1] = value;
  m_keys_set[k - 1] = true;
}

bool
BasePluginContext::GetKey(int k, void** value)
{
  if (k < 1 || k > 4 || m_keys_set[k - 1] == false)
    return false;

  *value = m_keys[k - 1];
  return true;
}

SourceMod::IdentityToken_t*
BasePluginContext::GetIdentity()
{
  SourceMod::IdentityToken_t* tok;

  if (GetKey(1, (void**)&tok))
    return tok;
  return NULL;
}

IVirtualMachine*
BasePluginContext::GetVirtualMachine()
{
  return NULL;
}

sp_context_t*
BasePluginContext::GetContext()
{
  return reinterpret_cast<sp_context_t*>((IPluginContext * )this);
}

bool
BasePluginContext::IsDebugging()
{
  return true;
}

int
BasePluginContext::SetDebugBreak(void* newpfn, void* oldpfn)
{
  return SP_ERROR_ABORTED;
}

IPluginDebugInfo*
BasePluginContext::GetDebugInfo()
{
  return NULL;
}

int
BasePluginContext::Execute(uint32_t code_addr, cell_t* result)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::BindNatives(const sp_nativeinfo_t* natives, unsigned int num, int overwrite)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::BindNative(const sp_nativeinfo_t* native)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::BindNativeToIndex(uint32_t index, SPVM_NATIVE_FUNC func)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::BindNativeToAny(SPVM_NATIVE_FUNC native)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::PushCell(cell_t value)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::PushCellsFromArray(cell_t array[], unsigned int numcells)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::PushCellArray(cell_t* local_addr, cell_t** phys_addr, cell_t array[], unsigned int numcells)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::PushString(cell_t* local_addr, char** phys_addr, const char* string)
{
  return SP_ERROR_ABORTED;
}

int
BasePluginContext::Execute2(IPluginFunction* function, const cell_t* params, unsigned int num_params, cell_t* result)
{
  ReportErrorNumber(SP_ERROR_ABORTED);
  return SP_ERROR_ABORTED;
}

ISourcePawnEngine2*
BasePluginContext::APIv2()
{
  return env_->APIv2();
}

void
BasePluginContext::ReportError(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  env_->ReportErrorVA(fmt, ap);
  va_end(ap);
}

void
BasePluginContext::ReportErrorVA(const char* fmt, va_list ap)
{
  env_->ReportErrorVA(fmt, ap);
}

void
BasePluginContext::ReportFatalError(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  env_->ReportErrorVA(SP_ERROR_FATAL, fmt, ap);
  va_end(ap);
}

void
BasePluginContext::ReportFatalErrorVA(const char* fmt, va_list ap)
{
  env_->ReportErrorVA(SP_ERROR_FATAL, fmt, ap);
}

void
BasePluginContext::ReportErrorNumber(int error)
{
  env_->ReportError(error);
}

void
BasePluginContext::ClearLastNativeError()
{
  if (env_->hasPendingException())
    env_->clearPendingException();
}

cell_t
BasePluginContext::ThrowNativeErrorEx(int error, const char* msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  if (msg)
    env_->ReportErrorVA(error, msg, ap);
  else
    env_->ReportError(error);
  va_end(ap);
  return 0;
}

cell_t
BasePluginContext::ThrowNativeError(const char* msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  env_->ReportErrorVA(SP_ERROR_NATIVE, msg, ap);
  va_end(ap);
  return 0;
}

int
BasePluginContext::GetLastNativeError()
{
  Environment* env = env_;
  if (!env->hasPendingException())
    return SP_ERROR_NONE;
  return env->getPendingExceptionCode();
}

cell_t
BasePluginContext::BlamePluginError(SourcePawn::IPluginFunction* pf, const char* msg, ...)
{
  va_list ap;
  va_start(ap, msg);
  env_->BlamePluginErrorVA(pf, msg, ap);
  va_end(ap);
  return 0;
}

IFrameIterator*
BasePluginContext::CreateFrameIterator()
{
  FrameIterator* it = new FrameIterator();
  return it;
}

void
BasePluginContext::DestroyFrameIterator(IFrameIterator* it)
{
  delete static_cast<FrameIterator*>(it);
}
