// vim: set ts=4 sw=4 tw=99 noet:
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
#ifndef _INCLUDE_SOURCEPAWN_VM_DEBUG_API_H_
#define _INCLUDE_SOURCEPAWN_VM_DEBUG_API_H_

#include "sp_vm_types.h"

// Current version of the IDebugSymbol interface.
#define SOURCEPAWN_DEBUG_SYMBOL_VERSION 0x001
// Current version of the ISymbolType interface.
#define SOURCEPAWN_DEBUG_TYPE_VERSION 0x001

namespace SourcePawn
{
  class ISymbolType {
  public:
    virtual int ApiVersion() = 0;
    virtual bool isInt32() = 0;
    virtual bool isFloat() = 0;
    virtual bool isBoolean() = 0;
    virtual bool isEnum() = 0;
    virtual bool isString() = 0;
    virtual bool isArray() = 0;
    virtual uint32_t dimcount() = 0;
    virtual uint32_t dimension(uint32_t dim) = 0;
  };

  enum SymbolScope {
    Global,
    Local,
    Static,
    Arg
  };

  class IDebugSymbol {
  public:
    virtual int ApiVersion() = 0;
    virtual const char* name() = 0;
    virtual SymbolScope scope() = 0;
    virtual cell_t address() = 0;
    virtual cell_t codestart() = 0;
    virtual cell_t codeend() = 0;
    virtual ISymbolType& type() = 0;
  };

  class IDebugSymbolIterator
  {
  public:
    virtual ~IDebugSymbolIterator()
    {}
    virtual bool Done() = 0;
    virtual IDebugSymbol* Next() = 0;
    virtual void Reset() = 0;
  };
}

#endif //_INCLUDE_SOURCEPAWN_VM_DEBUG_API_H_
