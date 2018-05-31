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
#ifndef _sourcepawn_compiler_smx_ssa_h_
#define _sourcepawn_compiler_smx_ssa_h_

namespace sp {

// Destinations for values.
enum class ValueDest {
  Pri,
  Alt,
  Stack,
  Error,
  None
};

// This is an extremely rigged SSA model to assist with optimal register
// and opcode assignment. Anything that produces a value returns one of these
// nodes.
class SValue
{
  friend class SmxCompiler;

 public:
  explicit SValue(ValueDest where)
   : id_(++sSequenceNo),
     where_(where)
  {}
  ~SValue() {
  }

  explicit operator bool() const {
    return where_ != ValueDest::Error && where_ != ValueDest::None;
  }

  uint64_t id() const {
    return id_;
  }
  ValueDest where() const {
    return where_;
  }

 private:
  void kill() {
    where_ = ValueDest::None;
  }

 private:
  static uint64_t sSequenceNo;

 private:
  // The unique identifier of the value.
  uint64_t id_;

  // The location of the identifier, as last known.
  ValueDest where_;
};

} // namespace sp

#endif // _sourcepawn_compiler_smx_ssa_h_
