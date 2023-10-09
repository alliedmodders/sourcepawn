// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2004-2015 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#include <utility>

#include <amtl/am-string.h>
#include "smx-v1-image.h"
#include <zlib/zlib.h>
#include "environment.h"

using namespace ke;
using namespace sp;

SmxV1Image::SmxV1Image(FILE* fp)
 : FileReader(fp)
{
}

SmxV1Image::SmxV1Image(uint8_t* addr, size_t length)
 : FileReader(addr, length)
{
}

SmxV1Image::SmxV1Image(uint8_t* addr, size_t length, void (*dtor)(uint8_t*))
 : FileReader(addr, length, dtor)
{
}

// Validating SMX v1 scripts is fairly expensive. We reserve real validation
// for v2.
bool
SmxV1Image::validate()
{
  if (length_ < sizeof(sp_file_hdr_t))
    return error("bad header");

  hdr_ = (sp_file_hdr_t*)buffer();
  if (hdr_->magic != SmxConsts::FILE_MAGIC)
    return error("bad header");

  switch (hdr_->version) {
    case SmxConsts::SP1_VERSION_1_0:
    case SmxConsts::SP1_VERSION_1_1:
    case SmxConsts::SP1_VERSION_1_7:
      break;
    default:
      return error("unsupported version");
  }

  switch (hdr_->compression) {
    case SmxConsts::FILE_COMPRESSION_GZ:
    {
      // We don't support junk in binaries, check that disksize matches the actual file size.
      // (this is to avoid a known crash in inflate() if told that data is bigger than it is)
      if (hdr_->disksize > length_)
        return error("illegal disk size");

      // The start of the compression cannot be larger than the file.
      if (hdr_->dataoffs > length_)
        return error("illegal compressed region");

      // The compressed region must start after the header.
      if (hdr_->dataoffs < sizeof(sp_file_hdr_t))
        return error("illegal compressed region");

      // The full size of the image must be at least as large as the start
      // of the compressed region.
      if (hdr_->imagesize < hdr_->dataoffs)
        return error("illegal image size");

      // Allocate the uncompressed image buffer.
      uint32_t compressedSize = hdr_->disksize - hdr_->dataoffs;
      std::unique_ptr<uint8_t, decltype(&DefaultFree)> uncompressed(
          static_cast<uint8_t*>(malloc(hdr_->imagesize)), DefaultFree);
      if (!uncompressed)
        return error("out of memory");

      // Decompress.
      const uint8_t* src = buffer() + hdr_->dataoffs;
      uint8_t* dest = uncompressed.get() + hdr_->dataoffs;
      uLongf destlen = hdr_->imagesize - hdr_->dataoffs;
      int rv = uncompress(
        (Bytef*)dest,
        &destlen,
        src,
        compressedSize);
      if (rv != Z_OK)
        return error("could not decode compressed region");

      // Copy the initial uncompressed region back in.
      memcpy(uncompressed.get(), buffer(), hdr_->dataoffs);

      // Replace the original buffer.
      length_ = hdr_->imagesize;
      buffer_ = std::move(uncompressed);
      hdr_ = (sp_file_hdr_t*)buffer();
      break;
    }

    case SmxConsts::FILE_COMPRESSION_NONE:
      break;

    default:
      return error("unknown compression type");
  }

  // Validate the string table.
  if (hdr_->stringtab >= length_)
    return error("invalid string table");
  header_strings_ = reinterpret_cast<const char*>(buffer() + hdr_->stringtab);

  // Validate sections header.
  if ((sizeof(sp_file_hdr_t) + hdr_->sections * sizeof(sp_file_section_t)) > length_)
    return error("invalid section table");

  size_t last_header_string = 0;
  const sp_file_section_t* sections =
    reinterpret_cast<const sp_file_section_t*>(buffer() + sizeof(sp_file_hdr_t));
  for (size_t i = 0; i < hdr_->sections; i++) {
    if (sections[i].nameoffs >= (hdr_->dataoffs - hdr_->stringtab))
      return error("invalid section name");

    if (sections[i].nameoffs > last_header_string)
      last_header_string = sections[i].nameoffs;

    sections_.push_back(Section());
    sections_.back().dataoffs = sections[i].dataoffs;
    sections_.back().size = sections[i].size;
    sections_.back().name = header_strings_ + sections[i].nameoffs;
  }

  // Validate sanity of section header strings.
  bool found_terminator = false;
  for (const char* iter = header_strings_ + last_header_string;
       iter < reinterpret_cast<const char*>(buffer() + hdr_->dataoffs);
       iter++)
  {
    if (*iter == '\0') {
      found_terminator = true;
      break;
    }
  }
  if (!found_terminator)
    return error("malformed section names header");

  names_section_ = findSection(".names");
  if (!names_section_)
    return error("could not find .names section");
  if (!validateSection(names_section_))
    return error("invalid names section");
  names_ = reinterpret_cast<const char*>(buffer() + names_section_->dataoffs);

  // The names section must be 0-length or be null-terminated.
  if (names_section_->size != 0 &&
      *(names_ + names_section_->size - 1) != '\0')
  {
    return error("malformed names section");
  }

  if (!validateCode())
    return false;
  if (!validateData())
    return false;
  if (!validatePublics())
    return false;
  if (!validatePubvars())
    return false;
  if (!validateNatives())
    return false;
  if (!validateRtti())
    return false;
  if (!validateDebugInfo())
    return false;
  if (!validateTags())
    return false;

  return true;
}

const SmxV1Image::Section*
SmxV1Image::findSection(const char* name) const
{
  for (size_t i = 0; i < sections_.size(); i++) {
    if (strcmp(sections_[i].name, name) == 0)
      return &sections_[i];
  }
  return nullptr;
}

bool
SmxV1Image::validateSection(const Section* section) const
{
  if (section->dataoffs >= length_)
    return false;
  if (section->size > length_ - section->dataoffs)
    return false;
  return true;
}

bool
SmxV1Image::validateRttiHeader(const Section* section) const
{
  if (!validateSection(section))
    return false;

  const smx_rtti_table_header* header =
    reinterpret_cast<const smx_rtti_table_header*>(buffer() + section->dataoffs);
  if (section->size < sizeof(smx_rtti_table_header))
    return false;
  if (section->size < header->header_size)
    return false;
  if (!IsUint32MultiplySafe(header->row_size, header->row_count))
    return false;

  uint32_t table_size = header->row_size * header->row_count;
  if (!IsUint32AddSafe(table_size, header->header_size))
    return false;
  if (section->size != header->header_size + table_size)
    return false;
  return true;
}

bool
SmxV1Image::validateData()
{
  // .data is required.
  const Section* section = findSection(".data");
  if (!section)
    return error("could not find data");
  if (!validateSection(section))
    return error("invalid data section");

  const sp_file_data_t* data =
    reinterpret_cast<const sp_file_data_t*>(buffer() + section->dataoffs);
  if (data->data > section->size)
    return error("invalid data blob");
  if (data->datasize > (section->size - data->data))
    return error("invalid data blob");

  const uint8_t* blob =
    reinterpret_cast<const uint8_t*>(data) + data->data;
  data_ = Blob<sp_file_data_t>(section, data, blob, data->datasize, 0);
  return true;
}

bool
SmxV1Image::validateCode()
{
  // .code is required.
  const Section* section = findSection(".code");
  if (!section)
    return error("could not find code");
  if (!validateSection(section))
    return error("invalid code section");

  const sp_file_code_t* code =
    reinterpret_cast<const sp_file_code_t*>(buffer() + section->dataoffs);
  if (code->codeversion < SmxConsts::CODE_VERSION_MINIMUM)
    return error("code version is too old, no longer supported");
  if (code->codeversion > SmxConsts::CODE_VERSION_CURRENT)
    return error("code version is too new, not supported");
  if (code->cellsize != 4)
    return error("unsupported cellsize");
  if (code->flags & ~CODEFLAG_DEBUG)
    return error("unsupported code settings");
  if (code->code > section->size)
    return error("invalid code blob");
  if (code->codesize > (section->size - code->code))
    return error("invalid code blob");

  uint32_t features = 0;
  if (code->codeversion >= SmxConsts::CODE_VERSION_FEATURE_MASK)
    features = code->features;

  uint32_t supported_features =
    SmxConsts::kCodeFeatureDirectArrays |
    SmxConsts::kCodeFeatureHeapScopes |
    SmxConsts::kCodeFeatureNullFunctions;
  if (features & ~supported_features)
    return error("unsupported feature set; code is too new");

  const uint8_t* blob =
    reinterpret_cast<const uint8_t*>(code) + code->code;
  code_ = Blob<sp_file_code_t>(section, code, blob, code->codesize, features);
  return true;
}

bool
SmxV1Image::validatePublics()
{
  const Section* section = findSection(".publics");
  if (!section)
    return true;
  if (!validateSection(section))
    return error("invalid .publics section");
  if ((section->size % sizeof(sp_file_publics_t)) != 0)
    return error("invalid .publics section");

  const sp_file_publics_t* publics =
    reinterpret_cast<const sp_file_publics_t*>(buffer() + section->dataoffs);
  size_t length = section->size / sizeof(sp_file_publics_t);

  for (size_t i = 0; i < length; i++) {
    if (!validateName(publics[i].name))
      return error("invalid public name");
  }

  publics_ = List<sp_file_publics_t>(publics, length);
  return true;
}

bool
SmxV1Image::validatePubvars()
{
  const Section* section = findSection(".pubvars");
  if (!section)
    return true;
  if (!validateSection(section))
    return error("invalid .pubvars section");
  if ((section->size % sizeof(sp_file_pubvars_t)) != 0)
    return error("invalid .pubvars section");

  const sp_file_pubvars_t* pubvars =
    reinterpret_cast<const sp_file_pubvars_t*>(buffer() + section->dataoffs);
  size_t length = section->size / sizeof(sp_file_pubvars_t);

  for (size_t i = 0; i < length; i++) {
    if (!validateName(pubvars[i].name))
      return error("invalid pubvar name");
  }

  pubvars_ = List<sp_file_pubvars_t>(pubvars, length);
  return true;
}

bool
SmxV1Image::validateNatives()
{
  const Section* section = findSection(".natives");
  if (!section)
    return true;
  if (!validateSection(section))
    return error("invalid .natives section");
  if ((section->size % sizeof(sp_file_natives_t)) != 0)
    return error("invalid .natives section");

  const sp_file_natives_t* natives =
    reinterpret_cast<const sp_file_natives_t*>(buffer() + section->dataoffs);
  size_t length = section->size / sizeof(sp_file_natives_t);

  for (size_t i = 0; i < length; i++) {
    if (!validateName(natives[i].name))
      return error("invalid native name");
  }

  natives_ = List<sp_file_natives_t>(natives, length);
  return true;
}

bool
SmxV1Image::validateName(size_t offset) const
{
  return offset < names_section_->size;
}

bool
SmxV1Image::validateRtti()
{
  const Section* rtti_data = findSection("rtti.data");
  if (!rtti_data)
    return true;
  if (!validateSection(rtti_data))
    return error("invalid rtti.data section");

  const uint8_t* blob =
    reinterpret_cast<const uint8_t*>(buffer() + rtti_data->dataoffs);
  rtti_data_ = std::make_unique<const RttiData>(blob, rtti_data->size);

  const char* mandatory_tables[] = {
    "rtti.methods",
    "rtti.natives",
  };
  for (size_t i = 0; i < sizeof(mandatory_tables) / sizeof(mandatory_tables[0]); i++) {
    const char* table_name = mandatory_tables[i];
    const Section* section = findSection(table_name);
    if (!section) {
      error_ = StringPrintf("missing %s section", table_name);
      return false;
    }
    if (!validateRttiHeader(section)) {
      error_ = StringPrintf("could not validate %s section", table_name);
      return false;
    }
  }

  rtti_methods_ = findRttiSection("rtti.methods");
  if (!validateRttiMethods())
    return false;

  rtti_natives_ = findRttiSection("rtti.natives");
  if (!validateRttiNatives())
    return false;

  const char* optional_tables[] = {
    "rtti.classdefs",
    "rtti.enums",
    "rtti.enumstructs",
    "rtti.enumstruct_fields",
    "rtti.fields",
    "rtti.typedefs",
    "rtti.typesets",
  };
  for (size_t i = 0; i < sizeof(optional_tables) / sizeof(optional_tables[0]); i++) {
    const char* table_name = optional_tables[i];
    const Section* section = findSection(table_name);
    if (!section)
      continue;
    if (!validateRttiHeader(section)) {
      error_ = StringPrintf("could not validate %s section", table_name);
      return false;
    }
  }

  rtti_enums_ = findRttiSection("rtti.enums");
  if (rtti_enums_ && !validateRttiEnums())
    return false;

  rtti_enumstruct_fields_ = findRttiSection("rtti.enumstruct_fields");
  rtti_enumstructs_ = findRttiSection("rtti.enumstructs");
  if (rtti_enumstructs_ && !validateRttiEnumStructs())
    return false;

  rtti_fields_ = findRttiSection("rtti.fields");
  rtti_classdefs_ = findRttiSection("rtti.classdefs");
  if (rtti_classdefs_ && !validateRttiClassdefs())
    return false;

  rtti_typedefs_ = findRttiSection("rtti.typedefs");
  if (rtti_typedefs_ && !validateRttiTypedefs())
    return false;

  rtti_typesets_ = findRttiSection("rtti.typesets");
  if (rtti_typesets_ && !validateRttiTypesets())
    return false;

  return true;
}

bool
SmxV1Image::validateRttiEnums()
{
  for (uint32_t i = 0; i < rtti_enums_->row_count; i++) {
    const smx_rtti_enum* enumType = getRttiRow<smx_rtti_enum>(rtti_enums_, i);
    if (!validateName(enumType->name))
      return error("invalid enum name");
  }
  return true;
}

bool
SmxV1Image::validateRttiEnumStructs()
{
  if (!rtti_enumstruct_fields_)
    return error("rtti.enumstruct_fields section missing");

  for (uint32_t i = 0; i < rtti_enumstructs_->row_count; i++) {
    const smx_rtti_enumstruct* enumstruct = getRttiRow<smx_rtti_enumstruct>(rtti_enumstructs_, i);
    if (!validateName(enumstruct->name))
      return error("invalid enum struct name");

    // Calculate how many fields this enumstruct has.
    uint32_t stopat = rtti_enumstruct_fields_->row_count;
    if (i != rtti_enumstructs_->row_count - 1) {
      const smx_rtti_enumstruct* next_enumstruct = getRttiRow<smx_rtti_enumstruct>(rtti_enumstructs_, i + 1);
      stopat = next_enumstruct->first_field;
    }
    if (enumstruct->first_field >= stopat)
      return error("invalid enum struct fields boundary");

    for (uint32_t j = enumstruct->first_field; j < stopat; j++) {
      if (!validateRttiEnumStructField(enumstruct, j))
        return false;
    }
  }
  return true;
}

bool
SmxV1Image::validateRttiEnumStructField(const smx_rtti_enumstruct* enumstruct, uint32_t index)
{
  if (index >= rtti_enumstruct_fields_->row_count)
    return error("invalid enum struct field index");

  const smx_rtti_es_field* field = getRttiRow<smx_rtti_es_field>(rtti_enumstruct_fields_, index);
  if (!validateName(field->name))
    return error("invalid enum struct field name");
  if (field->offset >= enumstruct->size * 4)
    return error("invalid enum struct field offset");
  if (!rtti_data_->validateType(field->type_id))
    return error("invalid enum struct field type");
  return true;
}

bool
SmxV1Image::validateRttiClassdefs()
{
  if (!rtti_fields_)
    return error("rtti.fields section missing");

  for (uint32_t i = 0; i < rtti_classdefs_->row_count; i++) {
    const smx_rtti_classdef* classdef = getRttiRow<smx_rtti_classdef>(rtti_classdefs_, i);
    // TODO: Validate flags.
    if (!validateName(classdef->name))
      return error("invalid classdef name");

    // Calculate how many fields this class has.
    uint32_t stopat = rtti_fields_->row_count;
    if (i != rtti_classdefs_->row_count - 1) {
      const smx_rtti_classdef* next_classdef = getRttiRow<smx_rtti_classdef>(rtti_classdefs_, i + 1);
      stopat = next_classdef->first_field;
    }
    if (classdef->first_field >= stopat)
      return error("invalid classdef fields boundary");

    for (uint32_t j = classdef->first_field; j < stopat; j++) {
      if (!validateRttiField(j))
        return false;
    }
  }
  return true;
}

bool
SmxV1Image::validateRttiField(uint32_t index)
{
  if (index >= rtti_fields_->row_count)
    return error("invalid classdef field index");

  // TODO: Validate flags.
  const smx_rtti_field* field = getRttiRow<smx_rtti_field>(rtti_fields_, index);
  if (!validateName(field->name))
    return error("invalid classdef field name");
  if (!rtti_data_->validateType(field->type_id))
    return error("invalid classdef field type");
  return true;
}

bool
SmxV1Image::validateRttiMethods()
{
  for (uint32_t i = 0; i < rtti_methods_->row_count; i++) {
    const smx_rtti_method* method = getRttiRow<smx_rtti_method>(rtti_methods_, i);
    if (!validateName(method->name))
      return error("invalid method name");
    if (!rtti_data_->validateFunctionOffset(method->signature))
      return error("invalid method signature type offset");
    if (method->pcode_start > method->pcode_end)
      return error("invalid method code range");
    if (method->pcode_start >= code_.header()->size)
      return error("invalid method code start");
    if (method->pcode_end > code_.header()->size)
      return error("invalid method code end");
  }
  return true;
}

bool
SmxV1Image::validateRttiNatives()
{
  for (uint32_t i = 0; i < rtti_natives_->row_count; i++) {
    const smx_rtti_native* native = getRttiRow<smx_rtti_native>(rtti_natives_, i);
    if (!validateName(native->name))
      return error("invalid native name");
    if (!rtti_data_->validateFunctionOffset(native->signature))
      return error("invalid native type offset");
  }
  return true;
}

bool
SmxV1Image::validateRttiTypedefs()
{
  for (uint32_t i = 0; i < rtti_typedefs_->row_count; i++) {
    const smx_rtti_typedef* typedefType = getRttiRow<smx_rtti_typedef>(rtti_typedefs_, i);
    if (!validateName(typedefType->name))
      return error("invalid typedef name");
    if (!rtti_data_->validateType(typedefType->type_id))
      return error("invalid typedef type");
  }
  return true;
}

bool
SmxV1Image::validateRttiTypesets()
{
  for (uint32_t i = 0; i < rtti_typesets_->row_count; i++) {
    const smx_rtti_typeset* typesetType = getRttiRow<smx_rtti_typeset>(rtti_typesets_, i);
    if (!validateName(typesetType->name))
      return error("invalid typeset name");
    if (!rtti_data_->validateTypesetOffset(typesetType->signature))
      return error("invalid typeset signatures");
  }
  return true;
}

bool
SmxV1Image::validateDebugInfo()
{
  const Section* dbginfo = findSection(".dbg.info");
  if (!dbginfo)
    return true;
  if (!validateSection(dbginfo))
    return error("invalid .dbg.info section");

  debug_info_ =
    reinterpret_cast<const sp_fdbg_info_t*>(buffer() + dbginfo->dataoffs);

  // Pre-RTTI, the debug tables used a separate string table. That is no longer
  // the case, but we support both scenarios.
  debug_names_section_ = findSection(".dbg.strings");
  if (debug_names_section_) {
    if (!validateSection(debug_names_section_))
      return error("invalid .dbg.strings section");
    debug_names_ = reinterpret_cast<const char*>(buffer() + debug_names_section_->dataoffs);

    // Name tables must be null-terminated.
    if (debug_names_section_->size != 0 &&
        *(debug_names_ + debug_names_section_->size - 1) != '\0')
    {
      return error("invalid .dbg.strings section");
    }
  } else {
    debug_names_section_ = names_section_;
    debug_names_ = names_;
  }

  const Section* files = findSection(".dbg.files");
  if (!files)
    return error("no debug file table");
  if (!validateSection(files))
    return error("invalid debug file table");
  if (files->size < sizeof(sp_fdbg_file_t) * debug_info_->num_files)
    return error("invalid debug file table size");
  debug_files_ = List<sp_fdbg_file_t>(
    reinterpret_cast<const sp_fdbg_file_t*>(buffer() + files->dataoffs),
    debug_info_->num_files);

  const Section* lines = findSection(".dbg.lines");
  if (!lines)
    return error("no debug lines table");
  if (!validateSection(lines))
    return error("invalid debug lines table");
  if (lines->size < sizeof(sp_fdbg_line_t) * debug_info_->num_lines)
    return error("invalid debug lines table size");
  debug_lines_ = List<sp_fdbg_line_t>(
    reinterpret_cast<const sp_fdbg_line_t*>(buffer() + lines->dataoffs),
    debug_info_->num_lines);

  debug_symbols_section_ = findSection(".dbg.symbols");
  if (debug_symbols_section_) {
    if (!validateSection(debug_symbols_section_))
      return error("invalid debug symbol table");
  } else {
    // New debug symbol tables are optional, but if present, they need to be
    // coherent.
    if (const Section* globals = findSection(".dbg.globals")) {
      if (!validateRttiHeader(globals))
        return error("invalid debug globals table");
      rtti_dbg_globals_ = toRttiTable(globals);
      if (Environment::get()->IsDebugBreakEnabled() && !validateDebugVariables(rtti_dbg_globals_))
        return false;
    }
    if (const Section* locals = findSection(".dbg.locals")) {
      if (!validateRttiHeader(locals))
        return error("invalid debug locals table");
      rtti_dbg_locals_ = toRttiTable(locals);
      if (Environment::get()->IsDebugBreakEnabled() && !validateDebugVariables(rtti_dbg_locals_))
        return false;
    }
    if (const Section* methods = findSection(".dbg.methods")) {
      if (!validateRttiHeader(methods))
        return error("invalid debug methods table");
      rtti_dbg_methods_ = toRttiTable(methods);
      if (Environment::get()->IsDebugBreakEnabled() && !validateDebugMethods())
        return false;
    }
  }

  if (debug_symbols_section_) {
    // See the note about unpacked debug sections in smx-headers.h.
    if (hdr_->version == SmxConsts::SP1_VERSION_1_0 &&
        !findSection(".dbg.natives"))
    {
      debug_syms_unpacked_ =
        reinterpret_cast<const sp_u_fdbg_symbol_t*>(buffer() + debug_symbols_section_->dataoffs);
    } else {
      debug_syms_ =
        reinterpret_cast<const sp_fdbg_symbol_t*>(buffer() + debug_symbols_section_->dataoffs);
    }
  }

  return true;
}

bool
SmxV1Image::validateDebugVariables(const smx_rtti_table_header* rtti_table)
{
  for (uint32_t i = 0; i < rtti_table->row_count; i++) {
    const smx_rtti_debug_var* debug_var = getRttiRow<smx_rtti_debug_var>(rtti_table, i);
    if (debug_var->vclass > kVarClass_Max)
      return error("invalid debug variable class");
    if (!validateSymbolAddress(debug_var->address, debug_var->vclass))
      return false;
    if (!validateName(debug_var->name))
      return error("invalid debug variable name");
    if (debug_var->code_start > debug_var->code_end)
      return error("invalid debug variable code range");
    if (debug_var->code_start >= code_.header()->size)
      return error("invalid debug variable code start");
    if (debug_var->code_end > code_.header()->size)
      return error("invalid debug variable code end");
    if (!rtti_data_->validateType(debug_var->type_id))
      return error("invalid debug variable type");
  }
  return true;
}

bool
SmxV1Image::validateSymbolAddress(int32_t address, uint8_t vclass)
{
  switch (vclass) {
  case kVarClass_Global:
  case kVarClass_Static:
    if ((uint32_t)address >= HeapSize())
      return error("invalid global variable address");
    break;
  case kVarClass_Local:
    if (address > 0)
      return error("invalid local variable address");
    break;
  case kVarClass_Arg:
    if (address <= 0)
      return error("invalid argument address");
  }
  return true;
}

bool
SmxV1Image::validateDebugMethods()
{
  for (uint32_t i = 0; i < rtti_dbg_methods_->row_count; i++) {
    const smx_rtti_debug_method* debug_method = getRttiRow<smx_rtti_debug_method>(rtti_dbg_methods_, i);
    if (debug_method->method_index >= rtti_methods_->row_count)
      return error("invalid debug method index");
    if (rtti_dbg_locals_ && debug_method->first_local >= rtti_dbg_locals_->row_count)
      return error("invalid first local index");
  }
  return true;
}

bool
SmxV1Image::validateTags()
{
  const Section* section = findSection(".tags");
  if (!section)
    return true;
  if (!validateSection(section))
    return error("invalid .tags section");
  if ((section->size % sizeof(sp_file_tag_t)) != 0)
    return error("invalid .tags section");

  const sp_file_tag_t* tags =
    reinterpret_cast<const sp_file_tag_t*>(buffer() + section->dataoffs);
  size_t length = section->size / sizeof(sp_file_tag_t);

  for (size_t i = 0; i < length; i++) {
    if (!validateName(tags[i].name))
      return error("invalid tag name");
  }

  tags_ = List<sp_file_tag_t>(tags, length);
  return true;
}

auto
SmxV1Image::DescribeCode() const -> Code
{
  Code code;
  code.bytes = code_.blob();
  code.length = code_.length();
  code.version = code_->codeversion;
  code.features = code_.features();
  return code;
}

auto
SmxV1Image::DescribeData() const -> Data
{
  Data data;
  data.bytes = data_.blob();
  data.length = data_.length();
  return data;
}

size_t
SmxV1Image::NumNatives() const
{
  return natives_.length();
}

const char*
SmxV1Image::GetNative(size_t index) const
{
  assert(index < natives_.length());
  return names_ + natives_[index].name;
}

bool
SmxV1Image::FindNative(const char* name, size_t* indexp) const
{
  for (size_t i = 0; i < natives_.length(); i++) {
    const char* candidate = names_ + natives_[i].name;
    if (strcmp(candidate, name) == 0) {
      if (indexp)
        *indexp = i;
      return true;
    }
  }
  return false;
}

size_t
SmxV1Image::NumPublics() const
{
  return publics_.length();
}

void
SmxV1Image::GetPublic(size_t index, uint32_t* offsetp, const char** namep) const
{
  assert(index < publics_.length());
  if (offsetp)
    *offsetp = publics_[index].address;
  if (namep)
    *namep = names_ + publics_[index].name;
}

bool
SmxV1Image::FindPublic(const char* name, size_t* indexp) const
{
  int high = publics_.length() - 1;
  int low = 0;
  while (low <= high) {
    int mid = (low + high) / 2;
    const char* candidate = names_ + publics_[mid].name;
    int diff = strcmp(candidate, name);
    if (diff == 0) {
      if (indexp) {
        *indexp = mid;
        return true;
      }
    }
    if (diff < 0)
      low = mid + 1;
    else
      high = mid - 1;
  }
  return false;
}

size_t
SmxV1Image::NumPubvars() const
{
  return pubvars_.length();
}

void
SmxV1Image::GetPubvar(size_t index, uint32_t* offsetp, const char** namep) const
{
  assert(index < pubvars_.length());
  if (offsetp)
    *offsetp = pubvars_[index].address;
  if (namep)
    *namep = names_ + pubvars_[index].name;
}

bool
SmxV1Image::FindPubvar(const char* name, size_t* indexp) const
{
  int high = pubvars_.length() - 1;
  int low = 0;
  while (low <= high) {
    int mid = (low + high) / 2;
    const char* candidate = names_ + pubvars_[mid].name;
    int diff = strcmp(candidate, name);
    if (diff == 0) {
      if (indexp) {
        *indexp = mid;
        return true;
      }
    }
    if (diff < 0)
      low = mid + 1;
    else
      high = mid - 1;
  }
  return false;
}

size_t
SmxV1Image::HeapSize() const
{
  return data_->memsize;
}

size_t
SmxV1Image::ImageSize() const
{
  return length_;
}

const char*
SmxV1Image::LookupFile(uint32_t addr) const
{
  int high = debug_files_.length();
  int low = -1;

  while (high - low > 1) {
    int mid = (low + high) / 2;
    if (debug_files_[mid].addr <= addr)
      low = mid;
    else
      high = mid;
  }

  if (low == -1)
    return nullptr;
  if (debug_files_[low].name >= debug_names_section_->size)
    return nullptr;

  return debug_names_ + debug_files_[low].name;
}

template <typename SymbolType, typename DimType>
const char*
SmxV1Image::lookupFunction(const SymbolType* syms, uint32_t addr) const
{
  const uint8_t* cursor = reinterpret_cast<const uint8_t*>(syms);
  const uint8_t* cursor_end = cursor + debug_symbols_section_->size;
  for (uint32_t i = 0; i < debug_info_->num_syms; i++) {
    if (cursor + sizeof(SymbolType) > cursor_end)
      break;

    const SymbolType* sym = reinterpret_cast<const SymbolType*>(cursor);
    if (sym->ident == sp::IDENT_FUNCTION &&
        sym->codestart <= addr &&
        sym->codeend > addr)
    {
      if (sym->name >= debug_names_section_->size)
        return nullptr;
      return debug_names_ + sym->name;
    }

    if (sym->dimcount > 0)
      cursor += sizeof(DimType) * sym->dimcount;
    cursor += sizeof(SymbolType);
  }
  return nullptr;
}

const char*
SmxV1Image::LookupFunction(uint32_t code_offset) const
{
  if (auto method = GetMethodRttiByOffset(code_offset))
    return names_ + method->name;

  if (debug_syms_) {
    auto name = lookupFunction<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>(debug_syms_, code_offset);
    if (name)
      return name;
  }
  if (debug_syms_unpacked_) {
    auto name =
      lookupFunction<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>(debug_syms_unpacked_, code_offset);
    if (name)
      return name;
  }

  for (size_t i = 0; i < publics_.length(); i++) {
    if (code_offset == publics_[i].address)
      return names_ + publics_[i].name;
  }

  return nullptr;
}

bool
SmxV1Image::HasRtti() const
{
  return rtti_data_ != nullptr;
}

const smx_rtti_method*
SmxV1Image::GetMethodRttiByOffset(uint32_t pcode_offset) const
{
  if (!rtti_methods_)
    return nullptr;

  for (uint32_t i = 0; i < rtti_methods_->row_count; i++) {
    const smx_rtti_method* method = getRttiRow<smx_rtti_method>(rtti_methods_, i);
    if (method->pcode_start <= pcode_offset && method->pcode_end > pcode_offset)
      return method;
  }
  return nullptr;
}

bool
SmxV1Image::LookupLine(uint32_t addr, uint32_t* line) const
{
  int high = debug_lines_.length();
  int low = -1;

  while (high - low > 1) {
    int mid = (low + high) / 2;
    if (debug_lines_[mid].addr <= addr)
      low = mid;
    else
      high = mid;
  }

  if (low == -1)
    return false;

  // Lines are zero-indexed for some reason.
  *line = debug_lines_[low].line + 1;
  return true;
}

size_t
SmxV1Image::NumFiles() const
{
  return debug_files_.length();
}

const char*
SmxV1Image::GetFileName(size_t index) const
{
  if (index >= debug_files_.length())
    return nullptr;

  if (debug_files_[index].name >= debug_names_section_->size)
    return nullptr;

  return debug_names_ + debug_files_[index].name;
}

template <typename SymbolType, typename DimType>
bool
SmxV1Image::getFunctionAddress(const SymbolType* syms, const char* function, ucell_t* funcaddr, uint32_t& index) const
{
  const uint8_t* cursor = reinterpret_cast<const uint8_t *>(syms);
  const uint8_t* cursor_end = cursor + debug_symbols_section_->size;
  for (uint32_t i = index; i < debug_info_->num_syms; i++) {
    if (cursor + sizeof(SymbolType) > cursor_end)
      break;

    const SymbolType *sym = reinterpret_cast<const SymbolType *>(cursor);
    if (sym->ident == sp::IDENT_FUNCTION &&
        sym->name < debug_names_section_->size &&
        !strcmp(debug_names_ + sym->name, function))
    {
      *funcaddr = sym->addr;
      return true;
    }

    if (sym->dimcount > 0)
      cursor += sizeof(DimType) * sym->dimcount;
    cursor += sizeof(SymbolType);
  }
  return false;
}

bool
SmxV1Image::LookupFunctionAddress(const char* function, const char* file, ucell_t* funcaddr) const
{
  *funcaddr = 0;
  if (rtti_methods_) {
    for (uint32_t i = 0; i < rtti_methods_->row_count; i++) {
      const smx_rtti_method* method = getRttiRow<smx_rtti_method>(rtti_methods_, i);
      const char* name = names_ + method->name;
      if (strcmp(name, function) != 0)
        continue;

      *funcaddr = method->pcode_start;
      // verify that this function is defined in the appropriate file
      const char* tgtfile = LookupFile(*funcaddr);
      if (tgtfile != nullptr && !strcmp(file, tgtfile))
        break;
    }
  } else {
    for (;;) {
      // find (next) matching function
      uint32_t index = 0;
      if (debug_syms_) {
        getFunctionAddress<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>(debug_syms_, function, funcaddr, index);
      } else {
        getFunctionAddress<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>(debug_syms_unpacked_, function, funcaddr, index);
      }

      if (index >= debug_info_->num_syms)
        return false;

      // verify that this function is defined in the appropriate file
      const char* tgtfile = LookupFile(*funcaddr);
      if (tgtfile != nullptr && strcmp(file, tgtfile) == 0)
        break;
      index++;
      assert(index < debug_info_->num_syms);
    }
  }

  // now find the first line in the function where we can "break" on
  uint32_t index = 0;
  for (; index < debug_info_->num_lines && debug_lines_[index].addr < *funcaddr; index++)
    continue;

  if (index >= debug_info_->num_lines)
    return false;

  *funcaddr = debug_lines_[index].addr;
  return true;
}

bool
SmxV1Image::LookupLineAddress(const uint32_t line, const char* filename, uint32_t* addr) const
{
  // Find a suitable "breakpoint address" close to the indicated line (and in
  // the specified file). The address is moved up to the next "breakable" line
  // if no "breakpoint" is available on the specified line. You can use function
  // LookupLine() to find out at which precise line the breakpoint was set.

  // The filename comparison is strict (case sensitive and path sensitive).
  *addr = 0;

  uint32_t bottomaddr, topaddr;
  uint32_t file;
  uint32_t index = 0;
  for (file = 0; file < debug_info_->num_files; file++) {
    // find the (next) matching instance of the file
    if (debug_files_[file].name >= debug_names_section_->size ||
        strcmp(debug_names_ + debug_files_[file].name, filename) != 0)
    {
      continue;
    }

    // get address range for the current file
    bottomaddr = debug_files_[file].addr;
    topaddr = (file + 1 < debug_info_->num_files) ? debug_files_[file + 1].addr : (uint32_t)-1;

    // go to the starting address in the line table
    while (index < debug_info_->num_lines && debug_lines_[index].addr < bottomaddr)
      index++;

    // browse until the line is found or until the top address is exceeded
    while (index < debug_info_->num_lines &&
           debug_lines_[index].line < line &&
           debug_lines_[index].addr < topaddr)
    {
      index++;
    }

    if (index >= debug_info_->num_lines)
      return false;
    if (debug_lines_[index].line >= line)
      break;

    // if not found (and the line table is not yet exceeded) try the next
    // instance of the same file (a file may appear twice in the file table)
  }
  if (file >= debug_info_->num_files)
    return false;

  assert(index < debug_info_->num_lines);
  *addr = debug_lines_[index].addr;
  return true;
}
