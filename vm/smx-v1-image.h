// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2004-2015 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#ifndef _include_sourcepawn_smx_parser_h_
#define _include_sourcepawn_smx_parser_h_

#include <stdio.h>
#include <smx/smx-headers.h>
#include <smx/smx-v1.h>
#include <am-string.h>
#include <am-vector.h>
#include "file-utils.h"
#include "legacy-image.h"

namespace sp {

class SmxV1Image
  : public FileReader,
    public LegacyImage
{
 public:
  SmxV1Image(FILE *fp);

  // This must be called to initialize the reader.
  bool validate();

  const sp_file_hdr_t *hdr() const {
    return hdr_;
  }

  const char *errorMessage() const {
    return error_.chars();
  }

 public:
  Code DescribeCode() const override;
  Data DescribeData() const override;
  size_t NumNatives() const override;
  const char *GetNative(size_t index) const override;
  bool FindNative(const char *name, size_t *indexp) const override;
  size_t NumPublics() const override;
  void GetPublic(size_t index, uint32_t *offsetp, const char **namep) const override;
  bool FindPublic(const char *name, size_t *indexp) const override;
  size_t NumPubvars() const override;
  void GetPubvar(size_t index, uint32_t *offsetp, const char **namep) const override;
  bool FindPubvar(const char *name, size_t *indexp) const override;
  size_t HeapSize() const override;
  size_t ImageSize() const override;
  const char *LookupFile(uint32_t code_offset) override;
  const char *LookupFunction(uint32_t code_offset) override;
  bool LookupLine(uint32_t code_offset, uint32_t *line) override;

  // Additional information for interactive debugging.
  class Symbol;
  bool GetFunctionAddress(const char *function, const char *file, uint32_t *addr) override;
  bool GetLineAddress(const uint32_t line, const char *file, uint32_t *addr) override;
  const char *FindFileByPartialName(const char *partialname) override;
  bool GetVariable(const char *symname, uint32_t scopeaddr, ke::AutoPtr<Symbol> &sym);
  const char *GetDebugName(uint32_t nameoffs);
  const char *GetFileName(uint32_t index);
  uint32_t GetFileCount();

 public:
  const char *GetTagName(uint32_t tag);

 public:
   class Symbol
   {
   public:
     Symbol(sp_fdbg_symbol_t *sym) :
       addr_(sym->addr),
       tagid_(sym->tagid),
       codestart_(sym->codestart),
       codeend_(sym->codeend),
       ident_(sym->ident),
       vclass_(sym->vclass),
       dimcount_(sym->dimcount),
       name_(sym->name),
       sym_(sym),
       unpacked_sym_(nullptr)
     {}

     Symbol(sp_u_fdbg_symbol_t *sym) :
       addr_(sym->addr),
       tagid_(sym->tagid),
       codestart_(sym->codestart),
       codeend_(sym->codeend),
       ident_(sym->ident),
       vclass_(sym->vclass),
       dimcount_(sym->dimcount),
       name_(sym->name),
       sym_(nullptr),
       unpacked_sym_(sym)
     {}

     Symbol(Symbol *sym) :
       addr_(sym->addr_),
       tagid_(sym->tagid_),
       codestart_(sym->codestart_),
       codeend_(sym->codeend_),
       ident_(sym->ident_),
       vclass_(sym->vclass_),
       dimcount_(sym->dimcount_),
       name_(sym->name_),
       sym_(sym->sym_),
       unpacked_sym_(sym->unpacked_sym_)
     {}

     const int32_t addr() const {
       return addr_;
     }
     const int16_t tagid() const {
       return tagid_;
     }
     const uint32_t codestart() const {
       return codestart_;
     }
     const uint32_t codeend() const {
       return codeend_;
     }
     const uint8_t ident() const {
       return ident_;
     }
     const uint8_t vclass() const {
       return vclass_;
     }
     const uint16_t dimcount() const {
       return dimcount_;
     }
     const uint32_t name() const {
       return name_;
     }
     void setVClass(uint8_t vclass) {
       vclass_ = vclass;
       if (sym_)
         sym_->vclass = vclass;
       else
         unpacked_sym_->vclass = vclass;
     }
     const bool packed() const {
       return sym_ != nullptr;
     }
   private:
     int32_t   addr_;       /**< Address rel to DAT or stack frame */
     int16_t   tagid_;      /**< Tag id */
     uint32_t  codestart_;  /**< Start scope validity in code */
     uint32_t  codeend_;    /**< End scope validity in code */
     uint8_t   ident_;      /**< Variable type */
     uint8_t   vclass_;     /**< Scope class (local vs global) */
     uint16_t  dimcount_;   /**< Dimension count (for arrays) */
     uint32_t  name_;       /**< Offset into debug nametable */

     sp_fdbg_symbol_t *sym_;
     sp_u_fdbg_symbol_t *unpacked_sym_;
   };

   class SymbolIterator
   {
   public:
     SymbolIterator(uint8_t *start, uint32_t debug_symbols_section_size, bool packed)
      : cursor_(start),
      packed_(packed)
     {
       cursor_end_ = cursor_ + debug_symbols_section_size;
     }

     bool Done() {
       if (packed_) {
         return cursor_ + sizeof(sp_fdbg_symbol_t) > cursor_end_;
       }
       else {
         return cursor_ + sizeof(sp_u_fdbg_symbol_t) > cursor_end_;
       }
     }

     Symbol *Next() {
       if (packed_) {
         sp_fdbg_symbol_t *sym = reinterpret_cast<sp_fdbg_symbol_t *>(cursor_);
         if (sym->dimcount > 0)
           cursor_ += sizeof(sp_fdbg_arraydim_t) * sym->dimcount;
         cursor_ += sizeof(sp_fdbg_symbol_t);

         return new Symbol(sym);
       }
       else {
         sp_u_fdbg_symbol_t *sym = reinterpret_cast<sp_u_fdbg_symbol_t *>(cursor_);
         if (sym->dimcount > 0)
           cursor_ += sizeof(sp_u_fdbg_arraydim_t) * sym->dimcount;
         cursor_ += sizeof(sp_u_fdbg_symbol_t);

         return new Symbol(sym);
       }
     }

   private:
     uint8_t *cursor_;
     uint8_t *cursor_end_;

     bool packed_;
   };

   SymbolIterator symboliterator();

   class ArrayDim
   {
   public:
     ArrayDim(sp_fdbg_arraydim_t *dim)
       : tagid_(dim->tagid),
       size_(dim->size)
     {}

     ArrayDim(sp_u_fdbg_arraydim_t *dim)
       : tagid_(dim->tagid),
       size_(dim->size)
     {}

     int16_t tagid() {
       return tagid_;
     }
     uint32_t size() {
       return size_;
     }

   private:
     int16_t   tagid_;    /**< Tag id */
     uint32_t  size_;     /**< Size of dimension */
   };

   ke::Vector<ArrayDim *> *GetArrayDimensions(const Symbol *sym);

 private:
   struct Section
   {
     const char *name;
     uint32_t dataoffs;
     uint32_t size;
   };
  const Section *findSection(const char *name);

 public:
  template <typename T>
  class Blob
  {
   public:
    Blob()
     : header_(nullptr),
       section_(nullptr),
       blob_(nullptr),
       length_(0)
    {}
    Blob(const Section *header,
         const T *section,
         const uint8_t *blob,
         size_t length)
     : header_(header),
       section_(section),
       blob_(blob),
       length_(length)
    {}

    size_t size() const {
      return section_->size;
    }
    const T * operator ->() const {
      return section_;
    }
    const uint8_t *blob() const {
      return blob_;
    }
    size_t length() const {
      return length_;
    }
    bool exists() const {
      return !!header_;
    }

   private:
    const Section *header_;
    const T *section_;
    const uint8_t *blob_;
    size_t length_;
  };

  template <typename T>
  class List
  {
   public:
    List()
     : section_(nullptr),
       length_(0)
    {}
    List(const T *section, size_t length)
     : section_(section),
       length_(length)
    {}

    size_t length() const {
      return length_;
    }
    const T &operator[](size_t index) const {
      assert(index < length());
      return section_[index];
    }
    bool exists() const {
      return !!section_;
    }

   private:
    const T *section_;
    size_t length_;
  };

 public:
  const Blob<sp_file_code_t> &code() const {
    return code_;
  }
  const Blob<sp_file_data_t> &data() const {
    return data_;
  }
  const List<sp_file_publics_t> &publics() const {
    return publics_;
  }
  const List<sp_file_natives_t> &natives() const {
    return natives_;
  }
  const List<sp_file_pubvars_t> &pubvars() const {
    return pubvars_;
  }
  const List<sp_file_tag_t> &tags() const {
    return tags_;
  }

 protected:
  bool error(const char *msg) {
    error_ = msg;
    return false;
  }
  bool validateName(size_t offset);
  bool validateSection(const Section *section);
  bool validateCode();
  bool validateData();
  bool validatePublics();
  bool validatePubvars();
  bool validateNatives();
  bool validateDebugInfo();
  bool validateTags();

 private:
  template <typename SymbolType, typename DimType>
  const char *lookupFunction(const SymbolType *syms, uint32_t addr);
  template <typename SymbolType, typename DimType>
  bool getFunctionAddress(const SymbolType *syms, const char *name, uint32_t *addr, uint32_t *index);

 private:
  sp_file_hdr_t *hdr_;
  ke::AString error_;
  const char *header_strings_;
  ke::Vector<Section> sections_;

  const Section *names_section_;
  const char *names_;

  Blob<sp_file_code_t> code_;
  Blob<sp_file_data_t> data_;
  List<sp_file_publics_t> publics_;
  List<sp_file_natives_t> natives_;
  List<sp_file_pubvars_t> pubvars_;
  List<sp_file_tag_t> tags_;

  const Section *debug_names_section_;
  const char *debug_names_;
  const sp_fdbg_info_t *debug_info_;
  List<sp_fdbg_file_t> debug_files_;
  List<sp_fdbg_line_t> debug_lines_;
  const Section *debug_symbols_section_;
  const sp_fdbg_symbol_t *debug_syms_;
  const sp_u_fdbg_symbol_t *debug_syms_unpacked_;
};

} // namespace sp

#endif // _include_sourcepawn_smx_parser_h_
