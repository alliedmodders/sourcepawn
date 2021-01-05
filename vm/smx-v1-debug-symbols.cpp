// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2004-2018 AlliedModers LLC
//
// This file is part of SourcePawn. SourcePawn is licensed under the GNU
// General Public License, version 3.0 (GPL). If a copy of the GPL was not
// provided with this file, you can obtain it here:
//   http://www.gnu.org/licenses/gpl.html
//
#include "smx-v1-debug-symbols.h"
#include "smx-v1-image.h"
#include "rtti.h"

using namespace ke;
using namespace sp;
using namespace SourcePawn;

// Handle RTTI .dbg.globals and .dbg.locals sections
SmxV1SymbolIterator::SmxV1SymbolIterator(SmxV1Image* image, ucell_t scope_addr)
  : image_(image),
    scope_address_(scope_addr)
{
  Reset();
}

bool
SmxV1SymbolIterator::Done()
{
  return state_ == End;
}

IDebugSymbol*
SmxV1SymbolIterator::Next()
{
  assert(!Done());
  
  // Get current variable and move on to the next one.
  const smx_rtti_debug_var* debug_var = image_->getRttiRow<smx_rtti_debug_var>(rtti_section_, rtti_row_index_);
  rtti_row_index_++;
  // No next row available.
  if (!validateScopeOfRow()) {
    switch (state_)
    {
    case Globals:
      // Try to switch to the local variables now.
      if (image_->rtti_dbg_locals_ && image_->rtti_dbg_methods_) {
        state_ = Locals;
        if (findFirstLocal() && validateScopeOfRow())
          break;
      }
      // Fall through if no locals available.
    case Locals:
      state_ = End;
      rtti_section_ = nullptr;
      rtti_row_index_ = 0;
      rtti_end_index_ = 0;
      break;
    }
  }
  symbols_.push_back(std::make_unique<SmxV1DebugSymbol>(image_, debug_var));
  return symbols_.back().get();
}

void
SmxV1SymbolIterator::Reset()
{
  symbols_.clear();
  // Always start iterating the global symbols first if the debug information is available.
  if (image_->rtti_dbg_globals_) {
    state_ = Globals;
    rtti_section_ = image_->rtti_dbg_globals_;
    rtti_row_index_ = 0;
    rtti_end_index_ = rtti_section_->row_count;
    if (validateScopeOfRow())
      return;
  }

  // No global debug information available. Start with locals.
  if (image_->rtti_dbg_locals_ && image_->rtti_dbg_methods_) {
    state_ = Locals;
    if (findFirstLocal() && validateScopeOfRow())
      return;
  }
  
  // No variables to iterate.
  state_ = End;
  rtti_section_ = nullptr;
  rtti_row_index_ = 0;
  rtti_end_index_ = 0;
}

bool
SmxV1SymbolIterator::findFirstLocal()
{
  rtti_section_ = image_->rtti_dbg_locals_;

  // Run through all methods we have debug information about.
  for (uint32_t i = 0; i < image_->rtti_dbg_methods_->row_count; i++) {
    const smx_rtti_debug_method* dbg_method = image_->getRttiRow<smx_rtti_debug_method>(image_->rtti_dbg_methods_, i);
    if (dbg_method->method_index >= image_->rtti_methods_->row_count)
      continue;

    // See if this entry is for the correct function.
    const smx_rtti_method* method = image_->getRttiRow<smx_rtti_method>(image_->rtti_methods_, dbg_method->method_index);
    if (method->pcode_start <= scope_address_ && method->pcode_end > scope_address_) {
      // Save the index of the first local variable of this function
      // in the .dbg.locals section.
      rtti_row_index_ = dbg_method->first_local;

      // Find the length of locals list.
      if (i == image_->rtti_dbg_methods_->row_count - 1) {
        rtti_end_index_ = image_->rtti_dbg_locals_->row_count;
      }
      else {
        dbg_method = image_->getRttiRow<smx_rtti_debug_method>(image_->rtti_dbg_methods_, i + 1);
        rtti_end_index_ = dbg_method->first_local;
      }
      return true;
    }
  }
  return false;
}

bool
SmxV1SymbolIterator::validateScopeOfRow()
{
  assert(!Done());
  // Find next variable in scope.
  for (; rtti_row_index_ < rtti_end_index_; rtti_row_index_++) {
    const smx_rtti_debug_var* debug_var = image_->getRttiRow<smx_rtti_debug_var>(rtti_section_, rtti_row_index_);
    if (debug_var->code_start <= scope_address_ &&
      debug_var->code_end >= scope_address_)
      return true;
  }
  return false;
}

// Iterate old .dbg.symbols section.
SmxV1LegacySymbolIterator::SmxV1LegacySymbolIterator(SmxV1Image* image, ucell_t scope_addr)
  : image_(image),
    scope_address_(scope_addr),
    start_(nullptr),
    cursor_(nullptr),
    cursor_end_(nullptr)
{
  if (image_->debug_syms_) {
    start_ = cursor_ = (const uint8_t*)image_->debug_syms_;
    cursor_end_ = cursor_ + image_->debug_symbols_section_->size;
    findFirstSymbol<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>();
  }
  else {
    start_ = cursor_ = (const uint8_t*)image_->debug_syms_unpacked_;
    cursor_end_ = cursor_ + image_->debug_symbols_section_->size;
    findFirstSymbol<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>();
  }
}

bool
SmxV1LegacySymbolIterator::Done()
{
  if (!cursor_)
    return true;
  return cursor_ >= cursor_end_;
}

const IDebugSymbol*
SmxV1LegacySymbolIterator::Next()
{
  assert(!Done());
  if (image_->debug_syms_)
    return nextSymbol<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>();
  else if (image_->debug_syms_unpacked_)
    return nextSymbol<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>();
  return nullptr;
}

void
SmxV1LegacySymbolIterator::Reset()
{
  cursor_ = start_;
  symbols_.clear();
}

template <typename SymbolType, typename DimType>
IDebugSymbol*
SmxV1LegacySymbolIterator::nextSymbol()
{
  const SymbolType *sym = reinterpret_cast<const SymbolType *>(cursor_);
  advanceCursor<SymbolType, DimType>();
  symbols_.push_back(std::make_unique<SmxV1DebugSymbol>(image_, sym));
  return symbols_.back().get();
}

template <typename SymbolType, typename DimType>
void
SmxV1LegacySymbolIterator::advanceCursor()
{
  // Skip until the next variable symbol.
  while (cursor_ < cursor_end_) {
    // Always advance at least one symbol when calling this function.
    const SymbolType *sym = reinterpret_cast<const SymbolType *>(cursor_);
    if (sym->dimcount > 0)
      cursor_ += sizeof(DimType) * sym->dimcount;
    cursor_ += sizeof(SymbolType);

    // Ignore function symbol entries.
    sym = reinterpret_cast<const SymbolType *>(cursor_);
    if (sym->ident == IDENT_FUNCTION)
      continue;

    // Check if this symbol is visible at the requested address.
    if (sym->codestart <= scope_address_ &&
        sym->codeend >= scope_address_)
      break;
  }
}

template <typename SymbolType, typename DimType>
void
SmxV1LegacySymbolIterator::findFirstSymbol()
{
  // Move the cursor to the first valid symbol for the requested scope address.
  const SymbolType *sym = reinterpret_cast<const SymbolType *>(cursor_);
  if (sym->ident == IDENT_FUNCTION ||
      sym->codestart > scope_address_ ||
      sym->codeend < scope_address_)
    advanceCursor<SymbolType, DimType>();
}

SmxV1DebugSymbol::SmxV1DebugSymbol(SmxV1Image* image, const smx_rtti_debug_var* sym)
  : address_(sym->address),
    codestart_(sym->code_start),
    codeend_(sym->code_end)
{
  std::unique_ptr<const Rtti> type(image->rttidata()->typeFromTypeId(sym->type_id));
  type_ = std::make_unique<const SmxV1SymbolType>(image, type.get());

  switch (sym->vclass & 3)
  {
  case kVarClass_Global:
    scope_ = Global;
    break;
  case kVarClass_Local:
    scope_ = Local;
    break;
  case kVarClass_Static:
    scope_ = Static;
    break;
  case kVarClass_Arg:
    scope_ = Argument;
    break;
  }

  name_ = image->GetDebugName(sym->name);
}

SmxV1DebugSymbol::SmxV1DebugSymbol(const SmxV1Image* image, const sp_fdbg_symbol_t* sym)
  : address_(sym->addr),
    codestart_(sym->codestart),
    codeend_(sym->codeend)
{
  type_ = std::make_unique<const SmxV1SymbolType>(image, sym);

  switch (sym->vclass)
  {
  case VCLASS_GLOBAL:
    scope_ = Global;
    break;
  case VCLASS_LOCAL:
    scope_ = Local;
    break;
  case VCLASS_STATIC:
    scope_ = Static;
    break;
  }

  if (scope_ == Local && address_ > 0)
    scope_ = Argument;

  name_ = image->GetDebugName(sym->name);
}

SmxV1DebugSymbol::SmxV1DebugSymbol(const SmxV1Image* image, const sp_u_fdbg_symbol_t* sym)
  : address_(sym->addr),
    codestart_(sym->codestart),
    codeend_(sym->codeend)
{
  type_ = std::make_unique<const SmxV1SymbolType>(image, sym);

  switch (sym->vclass)
  {
  case VCLASS_GLOBAL:
    scope_ = Global;
    break;
  case VCLASS_LOCAL:
    scope_ = Local;
    break;
  case VCLASS_STATIC:
    scope_ = Static;
    break;
  }

  if (scope_ == Local && address_ > 0)
    scope_ = Argument;

  name_ = image->GetDebugName(sym->name);
}

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* image, const Rtti* type) {
  reference_ = type->isByRef();
  constant_ = type->isConst();
  type_ = fromRttiType(image, type);
}

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* image, const sp_fdbg_symbol_t* sym)
  : type_(Integer),
    reference_(false),
    constant_(false)
{
  guessLegacyType<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>(image, sym);
}

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* image, const sp_u_fdbg_symbol_t* sym)
  : type_(Integer),
    reference_(false),
    constant_(false)
{
  guessLegacyType<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>(image, sym);
}

SmxV1SymbolType::BaseType
SmxV1SymbolType::fromRttiType(const SmxV1Image* image, const Rtti* type)
{
  switch (type->type()) {
  case cb::kBool:
    return Boolean;
  case cb::kInt32:
    return Integer;
  case cb::kFloat32:
    return Float;
  case cb::kChar8:
    return Character;
  case cb::kAny:
    return Any;
  case cb::kTopFunction:
    return Function;
  case cb::kVoid:
    return Void;
  case cb::kFixedArray:
  case cb::kArray:
  {
    const Rtti* inner = type;
    while (inner->inner()) {
      assert(inner->type() == cb::kArray || inner->type() == cb::kFixedArray);
      dimensions_.insert(dimensions_.begin() + 0, inner->index());
      inner = inner->inner();
    }
    return fromRttiType(image, inner);
  }
  case cb::kEnum:
  {
    const smx_rtti_enum* enumType = image->GetRttiEnum(type->index());
    if (enumType)
      name_ = image->GetName(enumType->name);
    return Enum;
  }
  case cb::kTypedef:
  {
    const smx_rtti_typedef* typedefType = image->GetRttiTypedef(type->index());
    if (typedefType)
      name_ = image->GetName(typedefType->name);
    return Typedef;
  }
  case cb::kTypeset:
  {
    const smx_rtti_typeset* typesetType = image->GetRttiTypeset(type->index());
    if (typesetType)
      name_ = image->GetName(typesetType->name);
    return Typeset;
  }
  case cb::kClassdef:
  {
    const smx_rtti_classdef* classdef = image->GetRttiClassdef(type->index());
    if (classdef) {
      name_ = image->GetName(classdef->name);
      uint8_t type = classdef->flags & kClassDefFlags_TypeMask;
      if (type == kClassDefType_Struct)
        return Struct;
    }
    return Object;
  }
  case cb::kFunction:
    // TODO: Expose argument type information and return type.
    return Function;
  case cb::kEnumStruct:
  {
    const smx_rtti_enumstruct* enumstruct = image->GetRttiEnumStruct(type->index());
    if (enumstruct)
      name_ = image->GetName(enumstruct->name);
    // Enum structs are just arrays.
    dimensions_.push_back(enumstruct->size);

    uint32_t last_field = image->GetRttiEnumStructLastFieldIndex(type->index());
    for (uint32_t field = enumstruct->first_field; field < last_field; field++) {
      const smx_rtti_es_field* enumstruct_field = image->GetRttiEnumStructField(field);
      if (!enumstruct_field)
        break;

      es_fields_.push_back(std::make_unique<const SmxV1EnumStructField>(image, enumstruct_field));
    }
    return EnumStruct;
  }
  }
  return Any;
}

// Tag masks from pre-rtti compiler.
#define FIXEDTAG     0x40000000
#define FUNCTAG      0x20000000
#define OBJECTTAG    0x10000000
#define ENUMTAG      0x08000000
#define METHODMAPTAG 0x04000000
#define STRUCTTAG    0x02000000

template <typename SymbolType, typename DimType>
void
SmxV1SymbolType::guessLegacyType(const SmxV1Image* image, const SymbolType* sym)
{
  // Save the dimension sizes first.
  assert(sym->dimcount <= sDIMEN_MAX);
  assert(sym->dimcount == 0 || sym->ident == IDENT_ARRAY || sym->ident == IDENT_REFARRAY);
  const DimType* dim = (const DimType*)(sym + 1);
  for (int i = 0; i < sym->dimcount; i++) {
    dimensions_.push_back(dim->size);
    dim++;
  }

  if (sym->ident == IDENT_REFERENCE || sym->ident == IDENT_REFARRAY)
    reference_ = true;

  // Try to guess the type from the tag.
  const sp_file_tag_t* tag = image->GetTagById(sym->tagid);
  if (tag) {
    const char* tagname = image->GetName(tag->name);
    if (!stricmp(tagname, "bool")) {
      type_ = Boolean;
      return;
    }
    if (!stricmp(tagname, "float")) {
      type_ = Float;
      return;
    }
    if (!stricmp(tagname, "String")) {
      type_ = Character;
      return;
    }
    if (!stricmp(tagname, "any")) {
      type_ = Any;
      return;
    }
    if ((tag->tag_id & FUNCTAG) > 0) {
      type_ = Function;
      name_ = tagname;
      return;
    }
    if ((tag->tag_id & OBJECTTAG) > 0) {
      type_ = Object;
      name_ = tagname;
      return;
    }
    // TODO: A tag can be a methodmap AND an enum at the same time.
    if ((tag->tag_id & METHODMAPTAG) > 0) {
      type_ = Enum;
      name_ = tagname;
      return;
    }
    if ((tag->tag_id & ENUMTAG) > 0) {
      type_ = Enum;
      name_ = tagname;
      return;
    }
    if ((tag->tag_id & STRUCTTAG) > 0) {
      type_ = Struct;
      name_ = tagname;
      return;
    }
  }
}

SmxV1EnumStructField::SmxV1EnumStructField(const SmxV1Image* image, const smx_rtti_es_field* es_field)
{
  name_ = image->GetName(es_field->name);
  offset_ = es_field->offset;
  std::unique_ptr<const Rtti> type(image->rttidata()->typeFromTypeId(es_field->type_id));
  type_ = std::make_unique<const SmxV1SymbolType>(image, type.get());
}
