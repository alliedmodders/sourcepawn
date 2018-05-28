// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2004-2018 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#ifndef _include_sourcepawn_smxv1_debug_symbols_h_
#define _include_sourcepawn_smxv1_debug_symbols_h_

#include <sp_vm_debug_api.h>
#include <smx/smx-legacy-debuginfo.h>
#include <amtl/am-vector.h>
#include <amtl/am-autoptr.h>

using namespace SourcePawn;

namespace sp {
  class SmxV1Image;
  class SmxV1DebugSymbol;

  class SmxV1SymbolType
    : public ISymbolType
  {
  public:
    SmxV1SymbolType(const SmxV1Image* const image, const sp_fdbg_symbol_t* sym);
    SmxV1SymbolType(const SmxV1Image* const image, const sp_u_fdbg_symbol_t* sym);
  public:
    virtual int ApiVersion() {
      return SOURCEPAWN_DEBUG_TYPE_VERSION;
    }
    virtual bool isInt32() {
      return type_ == Integer;
    }
    virtual bool isFloat() {
      return type_ == Float;
    }
    virtual bool isBoolean() {
      return type_ == Boolean;
    }
    virtual bool isEnum() {
      return false;
    }
    virtual bool isString() {
      return type_ == String;
    }
    virtual bool isArray() {
      return dimcount_ > 0;
    }
    virtual uint32_t dimcount() {
      return dimcount_;
    }
    virtual uint32_t dimension(uint32_t dim) {
      if (dim >= dimcount_)
        return 0;
      return dimensions_[dim];
    }

  private:
    template <typename SymbolType, typename DimType>
    void guessLegacyType(const SmxV1Image* const image, const SymbolType* sym);

  private:
    enum BaseType {
      Integer,
      Float,
      Boolean,
      String
    };
    BaseType type_;
    uint32_t dimcount_;
    ke::AutoPtr<uint32_t> dimensions_;
  };

  class SmxV1DebugSymbol
    : public IDebugSymbol
  {
  public:
    SmxV1DebugSymbol(const SmxV1Image* const image, const sp_fdbg_symbol_t* sym);
    SmxV1DebugSymbol(const SmxV1Image* const image, const sp_u_fdbg_symbol_t* sym);
  public:
    virtual ~SmxV1DebugSymbol() {}
    virtual int ApiVersion() {
      return SOURCEPAWN_DEBUG_SYMBOL_VERSION;
    }
    virtual const char* name() {
      return name_;
    }
    virtual SymbolScope scope() {
      return scope_;
    }
    virtual cell_t address() {
      return address_;
    }
    virtual cell_t codestart() {
      return codestart_;
    }
    virtual cell_t codeend() {
      return codeend_;
    }
    virtual ISymbolType& type() {
      return type_;
    }

  private:
    const char* name_;
    SymbolScope scope_;
    cell_t address_;
    cell_t codestart_;
    cell_t codeend_;
    SmxV1SymbolType type_;
  };

  class SmxV1SymbolIterator
    : public IDebugSymbolIterator
  {
  public:
    SmxV1SymbolIterator(const SmxV1Image* const image, ucell_t address);

    // IDebugSymbolIterator
  public:
    virtual bool Done();
    virtual IDebugSymbol* Next();
    virtual void Reset();

  private:
    template <typename SymbolType, typename DimType>
    IDebugSymbol* nextSymbol();
    template <typename SymbolType, typename DimType>
    void advanceCursor();
    template <typename SymbolType, typename DimType>
    void skipFunctions();

  private:
    const SmxV1Image* const image_;
    ucell_t address_;
    const uint8_t* start_;
    const uint8_t* cursor_;
    const uint8_t* cursor_end_;
    ke::Vector<ke::AutoPtr<SmxV1DebugSymbol>> symbols_;
  };
}

#endif // _include_sourcepawn_smxv1_debug_symbols_h_
