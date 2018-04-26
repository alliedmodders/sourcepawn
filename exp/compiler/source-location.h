// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 David Anderson
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
#ifndef _include_spcomp_source_location_h_
#define _include_spcomp_source_location_h_

#include <stdint.h>

namespace sp {

class FileContext;

// An encoded referece to a location in a source file. We keep this structure
// as small as feasible since our average script can have hundreds of thousands
// of source locations.
class SourceLocation
{
  friend struct Macro;
  friend class SourceFile;
  friend class SourceManager;
  friend struct LREntry;
  friend class MacroLexer;

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

  bool isSet() const {
    return id_ != 0;
  }
  bool operator ==(const SourceLocation& other) {
    return id_ == other.id_;
  }
  bool operator !=(const SourceLocation& other) {
    return id_ != other.id_;
  }

  bool isInMacro() const {
    return !!(id_ & kInMacro);
  }

  uint32_t opaqueId() const {
    return id_;
  }

 private:
  uint32_t offset() const {
    return id_ & ~kInMacro;
  }

 private:
  uint32_t id_;
};

struct SourceRange
{
  SourceLocation start;
  SourceLocation end;

  SourceRange()
  {}
  SourceRange(const SourceLocation& start, const SourceLocation& end)
   : start(start),
     end(end)
  {}
};

}

#endif // _include_spcomp_source_location_h_
