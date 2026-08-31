// vim: set sts=4 ts=8 sw=4 tw=99 et:
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
#include <memory>

#include <memory>
#include <optional>
#include <string_view>
#include <unordered_map>

#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <smx/smx-headers.h>
#include <smx/smx-legacy-debuginfo.h>
#include <smx/smx-typeinfo.h>
#include <smx/smx-v1.h>
#include <sp_vm_types.h>
#include "binary-reader.h"
#include "file-utils.h"
#include "rtti.h"

namespace sp {

using namespace debug;

class SmxImage final :
    public FileReader,
    public std::enable_shared_from_this<SmxImage>
{
  public:
    SmxImage(FILE* fp);
    SmxImage(uint8_t* addr, size_t length);
    SmxImage(uint8_t* addr, size_t length, void (*dtor)(uint8_t*));

    struct Code {
        const uint8_t* bytes;
        size_t length;
        int version;
        uint32_t features;
    };
    struct Data {
        const uint8_t* bytes;
        size_t length;
    };

    // This must be called to initialize the reader.
    bool validate();

    const sp_file_hdr_t* hdr() const { return hdr_; }
    const char* names() const { return names_; }

  public:
    Code DescribeCode() const;
    Data DescribeData() const;
    size_t NumNatives() const;
    const char* GetNative(size_t index) const;
    bool FindNative(const char* name, size_t* indexp) const;
    size_t NumPublics() const;
    void GetPublic(size_t index, uint32_t* offsetp, const char** namep) const;
    bool FindPublic(const char* name, size_t* indexp) const;
    size_t NumPubvars() const;
    void GetPubvar(size_t index, uint32_t* offsetp, const char** namep) const;
    bool FindPubvar(const char* name, size_t* indexp) const;
    size_t HeapSize() const;
    size_t ImageSize() const;
    const char* LookupFile(uint32_t code_offset) const;
    const char* LookupFunction(uint32_t code_offset) const;
    bool LookupLine(uint32_t code_offset, uint32_t* line) const;
    bool LookupLineV2(uint32_t code_offset, uint32_t* line) const;
    bool IsLineBoundary(uint32_t addr) const;
    size_t NumFiles() const;
    const char* GetFileName(size_t index) const;
    size_t NumFunctions() const;
    const char* GetFunctionName(size_t index, const char** filename) const;
    bool HasRtti() const;
    std::optional<uint32_t> FindRttiMethod(const char* name) const;
    const smx_rtti_method* GetMethodRttiByOffset(uint32_t pcode_offset) const;
    std::optional<uint32_t> GetDebugMethodRow(uint32_t pcode_offset) const;
    std::optional<uint32_t> GetDebugMethodLineRow(uint32_t dbg_method_row, uint32_t rel_addr) const;

    // Note: throws an exception on failure.
    std::optional<std::string_view> ReadDataBlob(uint32_t offset) const;

    const smx_rtti_method* GetMethod(uint32_t method_index) const {
        if (!rtti_methods_ || method_index >= rtti_methods_->row_count)
            return nullptr;
        return getRttiRow<smx_rtti_method>(rtti_methods_, method_index);
    }
    uint32_t GetIndexOfMethod(const smx_rtti_method* method) const {
        assert(rtti_methods_);
        return method - getRttiRow<smx_rtti_method>(rtti_methods_, 0);
    }
    const smx_rtti_classdef* getClassdef(uint32_t index) const {
        if (!rtti_classdefs_ || index >= rtti_classdefs_->row_count)
            return nullptr;
        return getRttiRow<smx_rtti_classdef>(rtti_classdefs_, index);
    }
    const smx_rtti_field* getField(uint32_t index) const {
        if (!rtti_fields_ || index >= rtti_fields_->row_count)
            return nullptr;
        return getRttiRow<smx_rtti_field>(rtti_fields_, index);
    }
    uint32_t getClassdefFieldsEnd(uint32_t i) const;
    uint32_t getClassdefMethodsEnd(uint32_t i) const;
    const smx_rtti_classdef* FindClassdefForField(uint32_t field_index) const;
    const smx_rtti_classdef* FindClassdefForMethod(uint32_t method_index) const;

    struct FieldLookup {
        uint32_t field_index;
        const smx_rtti_classdef* classdef;
        const smx_rtti_field* field;
    };
    std::optional<FieldLookup> ResolveFieldRef(uint32_t table_id) const;

    FastRtti GetTypeParser(uint32_t offset);
    FastRtti GetTypeIdParser(uint32_t type_id);

  private:
    SmxImage();

    struct Section {
        const char* name;
        uint32_t dataoffs;
        uint32_t size;
    };
    const Section* findSection(const char* name) const;
    bool IsVoidSignature(uint32_t offset) const;

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
        Blob(const Section* header, const T* section, const uint8_t* blob, size_t length,
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
        const T* operator->() const {
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
    const RttiData* rttidata() const {
        return rtti_data_.get();
    }
    const smx_rtti_table_header* rtti_methods() const { return rtti_methods_; }
    const smx_rtti_table_header* rtti_enums() const { return rtti_enums_; }
    const smx_rtti_table_header* rtti_globals() const { return rtti_globals_; }
    const smx_rtti_table_header* rtti_stringpool() const { return rtti_stringpool_; }
    const smx_rtti_table_header* rtti_classdefs() const { return rtti_classdefs_; }
    const smx_rtti_table_header* rtti_fields() const { return rtti_fields_; }

    BinaryReader GetDataReader(uint32_t offset) {
        assert(IsValidDataOffset(offset));
        return BinaryReader(data_.blob() + offset, data_.blob() + data_.length());
    }
    bool IsValidDataOffset(uint32_t offset) const { return offset < data_.length(); }

  protected:
    bool error(const char* msg) const;
    bool error(const std::string& msg) const;
    bool errorf(const char* fmt, ...) const KE_PRINTF_FUNCTION(2, 3);
    bool validateName(size_t offset) const;
    bool validateSection(const Section* section) const;
    bool validateRttiHeader(const Section* section) const;
    bool validateCode();
    bool validateData();
    bool validatePublics();
    bool validatePubvars();
    bool validateNatives();
    bool validateRtti();
    bool validateRttiClassdefs();
    bool validateRttiEnums();
    bool validateRttiField(uint32_t index);
    bool validateRttiMethods();
    bool validateRttiNatives();
    bool validateRttiTypedefs();
    bool validateRttiTypesets();
    bool validateRttiGlobals();
    bool validateDebugInfo();
    bool validateDebugVariables(const smx_rtti_table_header* rtti_table);
    bool validateDebugMethods();
    bool validateSymbolAddress(int32_t address, uint8_t vclass);
    bool validateTags();

  public:
    template <typename SymbolType, typename DimType>
    const char* lookupFunction(const SymbolType* syms, uint32_t addr) const;
    template <typename SymbolType, typename DimType>
    uint32_t getFunctionCount(const SymbolType* syms) const;
    template <typename SymbolType, typename DimType>
    const char* getFunctionName(const SymbolType* syms, const char** filename,
                                uint32_t index) const;
    template <typename SymbolType, typename DimType>
    bool getFunctionAddress(const SymbolType* syms, const char* function, ucell_t* funcaddr,
                            uint32_t& index) const;

    const smx_rtti_table_header* findRttiSection(const char* name) const {
        const Section* section = findSection(name);
        if (!section)
            return nullptr;
        return reinterpret_cast<const smx_rtti_table_header*>(buffer() + section->dataoffs);
    }

    const smx_rtti_table_header* toRttiTable(const Section* section) const {
        return reinterpret_cast<const smx_rtti_table_header*>(buffer() + section->dataoffs);
    }

  public:
    template <typename T>
    const T* getRttiRow(const smx_rtti_table_header* header, size_t index) const {
        assert(index < header->row_count);
        const uint8_t* base = reinterpret_cast<const uint8_t*>(header) + header->header_size;
        return reinterpret_cast<const T*>(base + header->row_size * index);
    }

  private:
    sp_file_hdr_t* hdr_ = nullptr;
    const char* header_strings_ = nullptr;
    std::vector<Section> sections_;

    const Section* names_section_ = nullptr;
    const char* names_ = nullptr;

    Blob<sp_file_code_t> code_;
    Blob<sp_file_data_t> data_;
    List<sp_file_publics_t> publics_;
    List<sp_file_natives_t> natives_;
    List<sp_file_pubvars_t> pubvars_;
    List<sp_file_tag_t> tags_;
    mutable std::unordered_map<std::string_view, size_t> publics_cache_;
    mutable std::unordered_map<std::string_view, size_t> pubvars_cache_;

    const Section* debug_names_section_ = nullptr;
    const char* debug_names_ = nullptr;
    const sp_fdbg_info_t* debug_info_ = nullptr;
    List<sp_fdbg_file_t> debug_files_;
    List<sp_fdbg_line_t> debug_lines_;
    const Section* debug_symbols_section_ = nullptr;
    const sp_fdbg_symbol_t* debug_syms_ = nullptr;
    const sp_u_fdbg_symbol_t* debug_syms_unpacked_ = nullptr;

    std::unique_ptr<const RttiData> rtti_data_ = nullptr;
    const smx_rtti_table_header* rtti_classdefs_ = nullptr;
    const smx_rtti_table_header* rtti_enums_ = nullptr;
    const smx_rtti_table_header* rtti_fields_ = nullptr;
    const smx_rtti_table_header* rtti_methods_ = nullptr;
    const smx_rtti_table_header* rtti_typedefs_ = nullptr;
    const smx_rtti_table_header* rtti_typesets_ = nullptr;
    const smx_rtti_table_header* rtti_globals_ = nullptr;
    const smx_rtti_table_header* rtti_stringpool_ = nullptr;
    const smx_rtti_table_header* rtti_dbg_globals_ = nullptr;
    const smx_rtti_table_header* rtti_dbg_methods_ = nullptr;
    const smx_rtti_table_header* rtti_dbg_method_lines_ = nullptr;
    const smx_rtti_table_header* rtti_dbg_locals_ = nullptr;
};

} // namespace sp

#endif // _include_sourcepawn_smx_parser_h_
