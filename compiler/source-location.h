// vim: set ts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2022-2026 AlliedModders LLC
//
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
