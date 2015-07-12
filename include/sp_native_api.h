// vim: set ts=4 sw=4 tw=99 et:
// 
// Copyright (C) 2006-2015 AlliedModders LLC
// 
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#ifndef _include_sourcepawn_includes_native_api_h_
#define _include_sourcepawn_includes_native_api_h_

namespace SourcePawn {

// These structures are part of the ABI.
namespace NativeSpecFlags
{
  static const uint8_t HasPluginContext = (1 << 0);
  static const uint8_t UsesLegacySignature = (1 << 1);
};

struct NativeSpec
{
  const uint8_t* signature;
  size_t length;
  void* method;
  uint8_t flags;
};

struct NativeDef
{
  const char* name;
  const NativeSpec* spec;
};

// Wrapper around |const cell_t *params| for type deduction.
//
// New legacy native signature:
//   cell_t (*)(IPluginContext *cx, const CellArgs& args)
//
// Unlike the old signature, arguments are indexed from 0 here.
class CellArgs
{
 public:
  size_t count() const {
    return params_[0];
  }
  cell_t operator[](size_t at) const {
    assert(at < count());
    return params_[at + 1];
  }

 private:
  const cell_t* params_;
};

} // namespace SourcePawn

#endif // _include_sourcepawn_includes_native_api_h_