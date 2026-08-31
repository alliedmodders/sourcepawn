// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_jit_frames_x64_h_
#define _include_sourcepawn_jit_frames_x64_h_

#include <amtl/am-platform.h>
#include <sp_vm_types.h>

namespace sp {

using namespace SourcePawn;

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

struct JitScriptedFrameLayout {
    void* saved_frm;
    FrameLayout layout;

    static JitScriptedFrameLayout* FromLayout(FrameLayout* layout) {
        return reinterpret_cast<JitScriptedFrameLayout*>(
            reinterpret_cast<uint8_t*>(layout) - sizeof(void*));
    }
};

} // namespace sp

#endif // _include_sourcepawn_jit_frames_x64_h_
