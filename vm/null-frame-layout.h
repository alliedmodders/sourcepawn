// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_jit_null_frames_h_
#define _include_sourcepawn_jit_null_frames_h_

#include <amtl/am-platform.h>
#include <sp_vm_types.h>
#if defined(SP_JIT_V1) || defined(SP_JIT_V2)
#    error "Wrong architecture!"
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
struct FrameLayout {
    intptr_t function_id() { return 0; }
    intptr_t frame_type() { return 0; }
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
