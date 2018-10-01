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
  symbols_.append(new SmxV1DebugSymbol(image_, debug_var));
  return symbols_.back();
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

IDebugSymbol*
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
  symbols_.append(new SmxV1DebugSymbol(image_, sym));
  return symbols_.back();
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

SmxV1DebugSymbol::SmxV1DebugSymbol(const SmxV1Image* image, const smx_rtti_debug_var* sym)
  : address_(sym->address),
    codestart_(sym->code_start),
    codeend_(sym->code_end),
    type_(image, sym)
{
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
    scope_ = Arg;
    break;
  }

  name_ = image->GetDebugName(sym->name);
}

#define sGLOBAL   0     /* global variable/constant class (no states) */
#define sLOCAL    1     /* local variable/constant */
#define sSTATIC   2     /* global life, local scope */

SmxV1DebugSymbol::SmxV1DebugSymbol(const SmxV1Image* image, const sp_fdbg_symbol_t* sym)
  : address_(sym->addr),
    codestart_(sym->codestart),
    codeend_(sym->codeend),
    type_(image, sym)
{
  switch (sym->vclass)
  {
  case sGLOBAL:
    scope_ = Global;
    break;
  case sLOCAL:
    scope_ = Local;
    break;
  case sSTATIC:
    scope_ = Static;
    break;
  }

  if (scope_ == Local && address_ > 0)
    scope_ = Arg;

  name_ = image->GetDebugName(sym->name);
}

SmxV1DebugSymbol::SmxV1DebugSymbol(const SmxV1Image* image, const sp_u_fdbg_symbol_t* sym)
  : address_(sym->addr),
    codestart_(sym->codestart),
    codeend_(sym->codeend),
    type_(image, sym)
{
  switch (sym->vclass)
  {
  case sGLOBAL:
    scope_ = Global;
    break;
  case sLOCAL:
    scope_ = Local;
    break;
  case sSTATIC:
    scope_ = Static;
    break;
  }

  if (scope_ == Local && address_ > 0)
    scope_ = Arg;

  name_ = image->GetDebugName(sym->name);
}

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* image, const smx_rtti_debug_var* sym)
  : type_(Integer)
{
  dimcount_ = 0;
  // TODO: Parse type info from .rtti.data
}

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* image, const sp_fdbg_symbol_t* sym)
  : dimcount_(sym->dimcount),
    type_(Integer)
{
  guessLegacyType<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>(image, sym);
}

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* image, const sp_u_fdbg_symbol_t* sym)
  : dimcount_(sym->dimcount),
    type_(Integer)
{
  guessLegacyType<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>(image, sym);
}

template <typename SymbolType, typename DimType>
void
SmxV1SymbolType::guessLegacyType(const SmxV1Image* const image, const SymbolType* sym)
{
  // Save the dimension sizes first.
  assert(sym->dimcount <= sDIMEN_MAX);
  assert(sym->dimcount == 0 || sym->ident == IDENT_ARRAY || sym->ident == IDENT_REFARRAY);
  dimensions_ = new uint32_t[sym->dimcount];
  const DimType* dim = (const DimType*)(sym + 1);
  for (int i = 0; i < sym->dimcount; i++) {
    dimensions_[i] = dim->size;
    dim++;
  }

  // Try to guess the type from the tag.
  const char *tagname = image->GetTagName(sym->tagid);
  if (tagname != nullptr) {
    if (!stricmp(tagname, "bool")) {
      type_ = Boolean;
      return;
    }

    if (!stricmp(tagname, "float")) {
      type_ = Float;
      return;
    }

    if (!stricmp(tagname, "String")) {
      type_ = String;
      return;
    }
  }
  // Untagged array with a single dimension, walk through all elements
  // and check whether this could be a string.
  if ((sym->ident == IDENT_ARRAY || sym->ident == IDENT_REFARRAY) && sym->dimcount == 1) {
    const char* str = nullptr; //GetString(sym); // FIXME
    if (str) {
      uint32_t i = 0;
      for (; str[i] != '\0'; i++) {
        // TODO: Better heuristics for utf8 strings.
        if ((str[i] < ' ' && str[i] != '\n' && str[i] != '\r' && str[i] != '\t'))
          break; // non-ASCII character

                 // Require a letter at the start.
        if (i == 0 && !isalpha(str[i]))
          break;
      }

      if (i > 0 && str[i] == '\0') {
        type_ = String;
        return;
      }
    }
  }
}
