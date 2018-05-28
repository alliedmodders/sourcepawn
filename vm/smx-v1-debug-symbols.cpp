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

SmxV1SymbolIterator::SmxV1SymbolIterator(const SmxV1Image* const image, ucell_t address)
  : image_(image),
    address_(address),
    start_(nullptr),
    cursor_(nullptr),
    cursor_end_(nullptr)
{
  // Using old .dbg.symbols section.
  if (image_->debug_symbols_section_) {
    if (image_->debug_syms_) {
      start_ = cursor_ = (const uint8_t*)image_->debug_syms_;
      cursor_end_ = cursor_ + image_->debug_symbols_section_->size;
      skipFunctions<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>();
    } else {
      start_ = cursor_ = (const uint8_t*)image_->debug_syms_unpacked_;
      cursor_end_ = cursor_ + image_->debug_symbols_section_->size;
      skipFunctions<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>();
    }
  }
}

bool
SmxV1SymbolIterator::Done()
{
  if (!cursor_)
    return true;
  return cursor_ >= cursor_end_;
}

IDebugSymbol*
SmxV1SymbolIterator::Next()
{
  assert(!Done());
  if (image_->debug_syms_)
    return nextSymbol<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>();
  else if (image_->debug_syms_unpacked_)
    return nextSymbol<sp_u_fdbg_symbol_t, sp_u_fdbg_arraydim_t>();
  return nullptr;
}

void
SmxV1SymbolIterator::Reset()
{
  cursor_ = start_;
  symbols_.clear();
}

template <typename SymbolType, typename DimType>
IDebugSymbol*
SmxV1SymbolIterator::nextSymbol()
{
  const SymbolType *sym = reinterpret_cast<const SymbolType *>(cursor_);
  advanceCursor<SymbolType, DimType>();
  symbols_.append(new SmxV1DebugSymbol(image_, sym));
  return symbols_.back();
}

template <typename SymbolType, typename DimType>
void
SmxV1SymbolIterator::advanceCursor()
{
  // Skip until the next variable symbol.
  while (cursor_ < cursor_end_) {
    const SymbolType *sym = reinterpret_cast<const SymbolType *>(cursor_);
    if (sym->dimcount > 0)
      cursor_ += sizeof(DimType) * sym->dimcount;
    cursor_ += sizeof(SymbolType);

    if (sym->ident != IDENT_FUNCTION)
      break;
  }
}

template <typename SymbolType, typename DimType>
void
SmxV1SymbolIterator::skipFunctions()
{
  // Only move the cursor if we're currently pointing at a function entry.
  const SymbolType *sym = reinterpret_cast<const SymbolType *>(cursor_);
  if (sym->ident == IDENT_FUNCTION)
    advanceCursor<SymbolType, DimType>();
}

#define sGLOBAL   0     /* global variable/constant class (no states) */
#define sLOCAL    1     /* local variable/constant */
#define sSTATIC   2     /* global life, local scope */

SmxV1DebugSymbol::SmxV1DebugSymbol(const SmxV1Image* const image, const sp_fdbg_symbol_t* sym)
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

  name_ = image->GetDebugName(sym->name);
}

SmxV1DebugSymbol::SmxV1DebugSymbol(const SmxV1Image* const image, const sp_u_fdbg_symbol_t* sym)
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

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* const image, const sp_fdbg_symbol_t* sym)
  : dimcount_(sym->dimcount)
{
  guessLegacyType<sp_fdbg_symbol_t, sp_fdbg_arraydim_t>(image, sym);
}

SmxV1SymbolType::SmxV1SymbolType(const SmxV1Image* const image, const sp_u_fdbg_symbol_t* sym)
  : dimcount_(sym->dimcount)
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
    const char* str = nullptr; //GetString(sym);
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
