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
#ifndef _include_sourcepawn_jit_null_frames_h_
#define _include_sourcepawn_jit_null_frames_h_

#include <sp_vm_types.h>
#include <amtl/am-platform.h>
#if defined(SP_HAS_JIT)
# error "Wrong architecture!"
#endif

namespace sp {

using namespace SourcePawn;

// We create x86 stack frames like:
//   [return address]
//   [prev_ebp]
//       ^--- ebp is captured here.
//   [frame_type]
//   [function_id]
//
struct FrameLayout
{
  intptr_t function_id;
  intptr_t frame_type;
  intptr_t* prev_fp;
  void* return_address;

  // This is -offsetof(FrameLayout, prev_ebp).
  static const intptr_t kOffsetFromFp = 0;

  static inline FrameLayout* FromFp(intptr_t* fp) {
    return nullptr;
  }
};

} // namespace sp

#endif // _include_sourcepawn_jit_null_frames_h_

