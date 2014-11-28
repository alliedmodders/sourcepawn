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

// A label represents a local control-flow target in a stream. A label
// may be in three states:
//
// (1) Empty - the label is not used yet.
// (2) Pending - the label has incoming jumps, but the actual location of
//         is not yet known. The list of jumps is threaded through
//         the four bytes reserved for each emitted jump target. In
//         this case, the offset field is the location of the most
//         recently emitted incoming jump.
// (3) Bound   - the label's position has been pinned.
//
// A jump can be emitted to a label in any state. Binding a label
// immediately backpatches all pending incoming jumps. A label must be
// bound if it is used.
class Label
{
  int offset_ : 31;
  bool bound_ : 1;

  void setOffset(int offset) {
    offset_ = offset;
    assert(offset_ == offset);
  }

 public:
  static const int INVALID_OFFSET = -1;

 public:
  Label()
    : offset_(INVALID_OFFSET),
    bound_(false)
  {
  }
  ~Label()
  {
    assert(bound_ || offset_ == INVALID_OFFSET);
  }
  
  bool bound() const {
    return bound_;
  }
  int offset() const {
    return offset_;
  }
  int value() const {
    assert(bound());
    return offset_;
  }
  void bind(int offset) {
    assert(!bound_);
    setOffset(offset);
    bound_ = true;
  }
  int link(int offset) {
    assert(!bound_);
    int old = offset_;
    setOffset(offset);
    return old;
  }
};

} // namespace ke

#endif // _include_spcomp2_label_h_
