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

#include <memory>
#include <vector>

#include <sp_vm_debug_api.h>
#include <smx/smx-legacy-debuginfo.h>
#include <smx/smx-typeinfo.h>

using namespace SourcePawn;

namespace sp {
  class SmxV1Image;
  class SmxV1DebugSymbol;
  class SmxV1SymbolType;
  class Rtti;

  class SmxV1EnumStructField
    : public IEnumStructField
  {
  public:
    SmxV1EnumStructField(const SmxV1Image* image, const smx_rtti_es_field* es_field);

    const char* name() const {
      return name_;
    }
    uint32_t offset() const {
      return offset_;
    }
    const ISymbolType* type() const {
      return type_.get();
    }
  private:
    const char* name_;
    uint32_t offset_;
    std::unique_ptr<const ISymbolType> type_;
  };

  class SmxV1SymbolType
    : public ISymbolType
  {
  public:
    SmxV1SymbolType(const SmxV1Image* image, const Rtti* sym);
    SmxV1SymbolType(const SmxV1Image* image, const sp_fdbg_symbol_t* sym);
    SmxV1SymbolType(const SmxV1Image* image, const sp_u_fdbg_symbol_t* sym);
  public:
    virtual int ApiVersion() const {
      return SOURCEPAWN_DEBUG_TYPE_VERSION;
    }
    virtual bool isInt32() const {
      return type_ == Integer;
    }
    virtual bool isFloat() const {
      return type_ == Float;
    }
    virtual bool isBoolean() const {
      return type_ == Boolean;
    }
    virtual bool isString() const {
      return type_ == Character;
    }
    virtual bool isEnum() const {
      return type_ == Enum;
    }
    virtual bool isFunction() const {
      return type_ == Function;
    }
    virtual bool isStruct() const {
      return type_ == Struct;
    }
    virtual bool isObject() const {
      return type_ == Object;
    }
    virtual bool isEnumStruct() const {
        return type_ == EnumStruct;
    }
    virtual bool isReference() const {
      return reference_;
    }
    virtual bool isArray() const {
      return !dimensions_.empty();
    }
    virtual size_t dimcount() const {
      return dimensions_.size();
    }
    virtual uint32_t dimension(uint32_t dim) const {
      if (dim >= dimensions_.size())
        return 0;
      return dimensions_[dim];
    }
    virtual const char* name() const {
      return name_;
    }
    virtual size_t esfieldcount() const {
      return es_fields_.size();
    }
    virtual const IEnumStructField* esfield(uint32_t idx) const {
      if (idx >= es_fields_.size())
        return nullptr;
      return es_fields_[idx].get();
    }

  private:
    enum BaseType {
      Integer,
      Float,
      Boolean,
      Character,
      Any,
      Enum,
      Typedef,
      Typeset,
      Struct,
      Object,
      Function,
      EnumStruct
    };
    BaseType fromRttiType(const SmxV1Image* image, const Rtti* type);
    template <typename SymbolType, typename DimType>
    void guessLegacyType(const SmxV1Image* image, const SymbolType* sym);

  private:
    BaseType type_;
    bool reference_;
    const char* name_;
    std::vector<uint32_t> dimensions_;
    std::vector<std::unique_ptr<const SmxV1EnumStructField>> es_fields_;
  };

  class SmxV1DebugSymbol
    : public IDebugSymbol
  {
  public:
    SmxV1DebugSymbol(SmxV1Image* image, const smx_rtti_debug_var* sym);
    SmxV1DebugSymbol(const SmxV1Image* image, const sp_fdbg_symbol_t* sym);
    SmxV1DebugSymbol(const SmxV1Image* image, const sp_u_fdbg_symbol_t* sym);
  public:
    virtual ~SmxV1DebugSymbol() {}
    virtual int ApiVersion() const {
      return SOURCEPAWN_DEBUG_SYMBOL_VERSION;
    }
    virtual const char* name() const {
      return name_;
    }
    virtual SymbolScope scope() const {
      return scope_;
    }
    virtual cell_t address() const {
      return address_;
    }
    virtual cell_t codestart() const {
      return codestart_;
    }
    virtual cell_t codeend() const {
      return codeend_;
    }
    virtual const ISymbolType* type() const {
      return type_.get();
    }

  private:
    const char* name_;
    SymbolScope scope_;
    cell_t address_;
    cell_t codestart_;
    cell_t codeend_;
    std::unique_ptr<const SmxV1SymbolType> type_;
  };

  // .dbg.globals and .dbg.locals iteration
  class SmxV1SymbolIterator
    : public IDebugSymbolIterator
  {
  public:
    SmxV1SymbolIterator(SmxV1Image* image, ucell_t scope_addr);

    // IDebugSymbolIterator
  public:
    virtual bool Done();
    virtual IDebugSymbol* Next();
    virtual void Reset();

  private:
    enum RttiIteratorState {
      Globals,
      Locals,
      End
    };
    bool validateScopeOfRow();
    bool findFirstLocal();

  private:
    SmxV1Image* image_;
    ucell_t scope_address_;
    std::vector<std::unique_ptr<SmxV1DebugSymbol>> symbols_;

    RttiIteratorState state_;
    const smx_rtti_table_header* rtti_section_;
    size_t rtti_row_index_;
    size_t rtti_end_index_;
  };

  // Legacy .dbg.symbols iteration
  class SmxV1LegacySymbolIterator
    : public IDebugSymbolIterator
  {
  public:
    SmxV1LegacySymbolIterator(SmxV1Image* image, ucell_t scope_addr);

    // IDebugSymbolIterator
  public:
    virtual bool Done();
    virtual const IDebugSymbol* Next();
    virtual void Reset();
    
  private:
    template <typename SymbolType, typename DimType>
    IDebugSymbol* nextSymbol();
    template <typename SymbolType, typename DimType>
    void advanceCursor();
    template <typename SymbolType, typename DimType>
    void findFirstSymbol();

  private:
    SmxV1Image* image_;
    ucell_t scope_address_;
    std::vector<std::unique_ptr<SmxV1DebugSymbol>> symbols_;

    const uint8_t* start_;
    const uint8_t* cursor_;
    const uint8_t* cursor_end_;
  };
}

#endif // _include_sourcepawn_smxv1_debug_symbols_h_
