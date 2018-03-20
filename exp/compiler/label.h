// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#ifndef _include_spcomp2_label_h_
#define _include_spcomp2_label_h_

namespace sp {

// A label is a lightweight object to assist in managing relative jumps. It
// exists in three states:
//   * Unbound, Unused: The label has no incoming jumps, and its position has
//     not yet been fixed in the instruction stream.
//   * Unbound, Used: The label has not yet been fixed at a position in the
//     instruction stream, but it has incoming jumps.
//   * Bound: The label has been fixed at a position in the instruction stream.
//
// When a label is unbound and used, the offset stored in the Label is a linked
// list threaded through each individual jump. When the label is bound, each
// jump instruction in this list is immediately patched with the correctly
// computed relative distance to the label.
//
// We keep sizeof(Label) == 4 to make it embeddable within code streams if
// need be (for example, SourcePawn mirrors the source code to maintain jump
// maps).
class Label
{
  // If set on status_, the label is bound.
  static const int32_t kBound = (1 << 0);

 public:
  Label()
   : status_(0)
  {
  }
  ~Label()
  {
    assert(!used() || bound());
  }

  static inline bool More(uint32_t status) {
    return status != 0;
  }
  static inline uint32_t ToOffset(uint32_t status) {
    return status >> 1;
  }

  bool used() const {
    return bound() || !!(status_ >> 1);
  }
  bool bound() const {
    return !!(status_ & kBound);
  }
  uint32_t offset() const {
    assert(bound());
    return ToOffset(status_);
  }
  uint32_t status() const {
    assert(!bound());
    return status_;
  }
  uint32_t addPending(uint32_t pc) {
    assert(pc <= INT_MAX / 2);
    uint32_t prev = status_;
    status_ = pc << 1;
    return prev;
  }
  void bind(uint32_t offset) {
    assert(!bound());
    status_ = (offset << 1) | kBound;
    assert(this->offset() == offset);
  }

  void reset() {
    assert(!used() || bound());
    status_ = 0;
  }

 protected:
  // Note that 0 as an invalid offset is okay, because the offset we save for
  // pending jumps are after the jump opcode itself, and therefore 0 is never
  // valid, since there are no 0-byte jumps.
  uint32_t status_;
};

// Similar to Label, but used for mutating a single instruction to a value
// that can only be computed later.
class DataLabel
{
  static const int32_t kBound = (1 << 0);

 public:
  DataLabel()
   : status_(0)
  {}
  ~DataLabel()
  {
    assert(!used() || bound());
  }
  static inline uint32_t ToOffset(uint32_t status) {
    return status >> 1;
  }

  bool used() const {
    return bound() || !!(status_ >> 1);
  }
  bool bound() const {
    return !!(status_ & kBound);
  }
  uint32_t offset() const {
    assert(bound());
    return ToOffset(status_);
  }
  uint32_t status() const {
    assert(!bound());
    return status_;
  }
  void use(uint32_t pc) {
    assert(!used() && !bound());
    assert(pc <= INT_MAX / 2);
    status_ = pc << 1;
  }
  void bind() {
    assert(used() && !bound());
    status_ |= kBound;
  }

  void reset() {
    assert(!used() || bound());
    status_ = 0;
  }

 private:
  uint32_t status_;
};

} // namespace ke

#endif // _include_spcomp2_label_h_
