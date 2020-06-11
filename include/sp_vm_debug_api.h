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
  // Represents the type of a variable.
  class ISymbolType {
  public:
    // @brief Returns the version of the type interface. (SOURCEPAWN_DEBUG_TYPE_VERSION)
    virtual int ApiVersion() = 0;

    // @brief Returns whether the type represents an integer.
    virtual bool isInt32() = 0;

    // @brief Returns whether the type represents a float.
    virtual bool isFloat() = 0;

    // @brief Returns whether the type represents a boolean.
    virtual bool isBoolean() = 0;

    // @brief Returns whether the type represents a string.
    virtual bool isString() = 0;

    // @brief Returns whether the type represents an enum.
    virtual bool isEnum() = 0;

    // @brief Returns whether the type represents a function.
    virtual bool isFunction() = 0;

    // @brief Returns whether the type represents a struct.
    virtual bool isStruct() = 0;

    // @brief Returns whether the type represents a class object.
    virtual bool isObject() = 0;

    // @brief Returns whether the argument is passed by reference.
    // Only valid for argument types in a function signature.
    virtual bool isReference() = 0;

    // @brief Returns whether the symbol is an array.
    virtual bool isArray() = 0;

    // @brief Returns the number of dimensions of the array.
    virtual uint32_t dimcount() = 0;

    // @brief Returns the size of a dimension.
    virtual uint32_t dimension(uint32_t dim) = 0;

    // @brief Returns the name of the type.
    // Only valid for non-primitive types.
    virtual const char* name() = 0;
  };

  // Visibility scope of a symbol.
  enum SymbolScope {
    Global,
    Local,
    Static,
    Argument
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
