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
#include <smx/smx-legacy-debuginfo.h>
#include <smx/smx-typeinfo.h>
#include <smx/smx-v1.h>
#include <am-string.h>
#include <am-vector.h>
#include <sp_vm_types.h>
#include "file-utils.h"
#include "legacy-image.h"

namespace sp {

class SmxV1Image
  : public FileReader,
    public LegacyImage
{
 public:
  SmxV1Image(FILE* fp);

  // This must be called to initialize the reader.
  bool validate();

  const sp_file_hdr_t* hdr() const {
    return hdr_;
  }

  const char* errorMessage() const {
    return error_.c_str();
  }

 public:
  Code DescribeCode() const override;
  Data DescribeData() const override;
  size_t NumNatives() const override;
  const char* GetNative(size_t index) const override;
  bool FindNative(const char* name, size_t* indexp) const override;
  size_t NumPublics() const override;
  void GetPublic(size_t index, uint32_t* offsetp, const char** namep) const override;
  bool FindPublic(const char* name, size_t* indexp) const override;
  size_t NumPubvars() const override;
  void GetPubvar(size_t index, uint32_t* offsetp, const char** namep) const override;
  bool FindPubvar(const char* name, size_t* indexp) const override;
  size_t HeapSize() const override;
  size_t ImageSize() const override;
  const char* LookupFile(uint32_t code_offset) override;
  const char* LookupFunction(uint32_t code_offset) override;
  bool LookupLine(uint32_t code_offset, uint32_t* line) override;
  bool LookupFunctionAddress(const char* function, const char* file, ucell_t* addr) override;
  bool LookupLineAddress(const uint32_t line, const char* file, ucell_t* addr) override;
  size_t NumFiles() const override;
  const char* GetFileName(size_t index) const override;

 private:
   struct Section
   {
     const char* name;
     uint32_t dataoffs;
     uint32_t size;
   };
  const Section* findSection(const char* name);

 public:
  template <typename T>
  class Blob
  {
   public:
    Blob()
     : header_(nullptr),
       section_(nullptr),
       blob_(nullptr),
       length_(0),
       features_(0)
    {}
    Blob(const Section* header,
         const T* section,
         const uint8_t* blob,
         size_t length,
         uint32_t features)
     : header_(header),
       section_(section),
       blob_(blob),
       length_(length),
       features_(features)
    {}

    size_t size() const {
      return section_->size;
    }
    const T * operator ->() const {
      return section_;
    }
    const uint8_t* blob() const {
      return blob_;
    }
    size_t length() const {
      return length_;
    }
    bool exists() const {
      return !!header_;
    }
    uint32_t features() const {
      return features_;
    }
    const Section* header() const {
      return header_;
    }

   private:
    const Section* header_;
    const T* section_;
    const uint8_t* blob_;
    size_t length_;
    uint32_t features_;
  };

  template <typename T>
  class List
  {
   public:
    List()
     : section_(nullptr),
       length_(0)
    {}
    List(const T* section, size_t length)
     : section_(section),
       length_(length)
    {}

    size_t length() const {
      return length_;
    }
    const T& operator[](size_t index) const {
      assert(index < length());
      return section_[index];
    }
    bool exists() const {
      return !!section_;
    }

   private:
    const T* section_;
    size_t length_;
  };

 public:
  const Blob<sp_file_code_t>& code() const {
    return code_;
  }
  const Blob<sp_file_data_t>& data() const {
    return data_;
  }
  const List<sp_file_publics_t>& publics() const {
    return publics_;
  }
  const List<sp_file_natives_t>& natives() const {
    return natives_;
  }
  const List<sp_file_pubvars_t>& pubvars() const {
    return pubvars_;
  }

 protected:
  bool error(const char* msg) {
    error_ = msg;
    return false;
  }
  bool validateName(size_t offset);
  bool validateSection(const Section* section);
  bool validateRttiHeader(const Section* section);
  bool validateCode();
  bool validateData();
  bool validatePublics();
  bool validatePubvars();
  bool validateNatives();
  bool validateRtti();
  bool validateRttiMethods();
  bool validateDebugInfo();
  bool validateTags();

 private:
  template <typename SymbolType, typename DimType>
  const char* lookupFunction(const SymbolType* syms, uint32_t addr);
  template <typename SymbolType, typename DimType>
  bool getFunctionAddress(const SymbolType* syms, const char* function, ucell_t* funcaddr, uint32_t& index);

  const smx_rtti_table_header* findRttiSection(const char* name) {
    const Section* section = findSection(name);
    if (!section)
      return nullptr;
    return reinterpret_cast<const smx_rtti_table_header*>(buffer() + section->dataoffs);
  }

  template <typename T>
  const T* getRttiRow(const smx_rtti_table_header* header, size_t index) {
    assert(index < header->row_count);
    const uint8_t* base = reinterpret_cast<const uint8_t*>(header) + header->header_size;
    return reinterpret_cast<const T*>(base + header->row_size * index);
  }

 private:
  sp_file_hdr_t* hdr_;
  std::string error_;
  const char* header_strings_;
  ke::Vector<Section> sections_;

  const Section* names_section_;
  const char* names_;

  Blob<sp_file_code_t> code_;
  Blob<sp_file_data_t> data_;
  List<sp_file_publics_t> publics_;
  List<sp_file_natives_t> natives_;
  List<sp_file_pubvars_t> pubvars_;
  List<sp_file_tag_t> tags_;

  const Section* debug_names_section_;
  const char* debug_names_;
  const sp_fdbg_info_t* debug_info_;
  List<sp_fdbg_file_t> debug_files_;
  List<sp_fdbg_line_t> debug_lines_;
  const Section* debug_symbols_section_;
  const sp_fdbg_symbol_t* debug_syms_;
  const sp_u_fdbg_symbol_t* debug_syms_unpacked_;

  const Section* rtti_data_;
  const smx_rtti_table_header* rtti_methods_;
};

} // namespace sp

#endif // _include_sourcepawn_smx_parser_h_
