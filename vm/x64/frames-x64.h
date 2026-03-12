// vim: set ts=8 sts=4 sw=4 tw=99 et:
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
#ifndef _include_sourcepawn_jit_frames_x64_h_
#define _include_sourcepawn_jit_frames_x64_h_

#include <amtl/am-platform.h>
#include <sp_vm_types.h>

namespace sp {

using namespace SourcePawn;

class PluginContext;

// We create x64 stack frames like:
//   [return address]
//   [prev_ebp]
//       ^--- ebp is captured here.
//   [function_id<<32 | frame_type]
//
struct FrameLayout {
    intptr_t function_id_and_frame_type_;
    intptr_t* prev_fp;
    void* return_address;

    // This is -offsetof(FrameLayout, prev_ebp).
    static const intptr_t kOffsetFromFp = -1;

    int32_t function_id() { return function_id_and_frame_type_ >> 32; }
    int32_t frame_type() { return (int32_t)function_id_and_frame_type_; }

    static inline FrameLayout* FromFp(intptr_t* fp) {
        return reinterpret_cast<FrameLayout*>(fp + kOffsetFromFp);
    }
};

} // namespace sp

#endif // _include_sourcepawn_jit_frames_x64_h_
