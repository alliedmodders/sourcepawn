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
#ifndef _INCLUDE_SOURCEPAWN_BASECONTEXT_H_
#define _INCLUDE_SOURCEPAWN_BASECONTEXT_H_

#include <sp_vm_api.h>

using namespace SourcePawn;
namespace sp {

// Since the legacy API has a lot of cruft in it, we use a base class to handle common
// code and stub deprecated functions.
class BasePluginContext : public SourcePawn::IPluginContext
{
public:
  BasePluginContext();
  virtual ~BasePluginContext();

  // Generic API access.
  void SetKey(int k, void* value) override;
  bool GetKey(int k, void** value) override;
  SourceMod::IdentityToken_t* GetIdentity() override;
  SourcePawn::ISourcePawnEngine2* APIv2() override;
  void ReportError(const char* fmt, ...) override;
  void ReportErrorVA(const char* fmt, va_list ap) override;
  void ReportFatalError(const char* fmt, ...) override;
  void ReportFatalErrorVA(const char* fmt, va_list ap) override;
  void ReportErrorNumber(int error) override;
  void ClearLastNativeError() override;
  cell_t ThrowNativeErrorEx(int error, const char* msg, ...) override;
  cell_t ThrowNativeError(const char* msg, ...) override;
  int GetLastNativeError() override;
  cell_t BlamePluginError(SourcePawn::IPluginFunction* pf, const char* msg, ...) override;
  IFrameIterator* CreateFrameIterator() override;
  void DestroyFrameIterator(IFrameIterator* it) override;
  
  // Removed functions.
  int PushCell(cell_t value) override;
  int PushCellArray(cell_t* local_addr, cell_t** phys_addr, cell_t array[], unsigned int numcells) override;
  int PushString(cell_t* local_addr, char** phys_addr, const char* string) override;
  int PushCellsFromArray(cell_t array[], unsigned int numcells) override;
  int BindNatives(const sp_nativeinfo_t* natives, unsigned int num, int overwrite) override;
  int BindNative(const sp_nativeinfo_t* native) override;
  int BindNativeToAny(SPVM_NATIVE_FUNC native) override;
  int BindNativeToIndex(uint32_t index, SPVM_NATIVE_FUNC native) override;
  SourcePawn::IVirtualMachine* GetVirtualMachine() override;
  sp_context_t* GetContext() override;
  bool IsDebugging() override;
  int SetDebugBreak(void* newpfn, void* oldpfn) override;
  SourcePawn::IPluginDebugInfo* GetDebugInfo() override;
  int Execute(uint32_t code_addr, cell_t* result) override;
  int Execute2(SourcePawn::IPluginFunction* function, const cell_t* params, unsigned int num_params, cell_t* result) override;

protected:
  Environment* env_;

private:
  void* m_keys[4];
  bool m_keys_set[4];
};

} // namespace sp

#endif // _INCLUDE_SOURCEPAWN_BASECONTEXT_H_
