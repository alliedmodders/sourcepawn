// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// SourcePawn is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with SourcePawn.  If not, see <http://www.gnu.org/licenses/>.
#ifndef _include_sourcepawn_outofline_asm_h__
#define _include_sourcepawn_outofline_asm_h__

#include <assert.h>
#include "pool-allocator.h"

namespace sp {

class Compiler;

class OutOfLinePath : public PoolObject
{
public:
  virtual ~OutOfLinePath() {
    // Pool objects are not destructed.
    assert(false);
  }

  virtual bool emit(Compiler* cc) = 0;

  Label* label() {
    return &label_;
  }

private:
  Label label_;
};

class ErrorPath : public OutOfLinePath
{
 public:
  ErrorPath(const cell_t* cip, int err)
  : cip(cip),
    err(err)
  {}

  bool emit(Compiler* cc) override;

  const cell_t* cip;
  int err;
};

class OutOfBoundsErrorPath : public OutOfLinePath
{
 public:
  OutOfBoundsErrorPath(const cell_t* cip, cell_t bounds)
   : cip(cip),
     bounds(bounds)
  {}

  bool emit(Compiler* cc) override;

  const cell_t* cip;
  cell_t bounds;
};

} // namespace sp

#endif // _include_sourcepawn_outofline_asm_h__
