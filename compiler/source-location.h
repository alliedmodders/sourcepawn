// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2022 AlliedModders LLC
// 
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#pragma once

#include <stdint.h>

namespace sp {
namespace cc {

class SourceManager;

// An encoded referece to a location in a source file. We keep this structure
// as small as feasible since our average script can have hundreds of thousands
// of source locations.
class SourceLocation
{
  friend class MacroLexer;
  friend class SourceFile;
  friend class SourceManager;
  friend struct LocationRange;
  friend struct Macro;

  static const uint32_t kInMacro = 0x80000000;

  static SourceLocation FromFile(uint32_t sourceId, uint32_t offset) {
    SourceLocation loc;
    loc.id_ = sourceId + offset;
    return loc;
  }
  static SourceLocation FromMacro(uint32_t sourceId, uint32_t offset) {
    SourceLocation loc;
    loc.id_ = sourceId + offset;
    loc.id_ |= kInMacro;
    return loc;
  }

 public:
  SourceLocation()
   : id_(0)
  {
  }
  SourceLocation(const SourceLocation&) = default;

  bool valid() const {
    return id_ != 0;
  }
  bool operator ==(const SourceLocation& other) {
    return id_ == other.id_;
  }
  bool operator !=(const SourceLocation& other) {
    return id_ != other.id_;
  }

  bool IsInMacro() const {
    return !!(id_ & kInMacro);
  }

  uint32_t id() const {
    return id_;
  }

 private:
  uint32_t offset() const {
    return id_ & ~kInMacro;
  }

 private:
  uint32_t id_;
};

} // namespace cc
} // namespace sp
