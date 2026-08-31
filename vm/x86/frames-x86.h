// vim: set ts=8 sts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#ifndef _include_sourcepawn_jit_frames_x86_h_
#define _include_sourcepawn_jit_frames_x86_h_

#include <amtl/am-platform.h>
#include <sp_vm_types.h>
#if !defined(KE_ARCH_X86)
#    error "Wrong platform!"
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
    intptr_t function_id_;
    intptr_t frame_type_;
    intptr_t* prev_fp;
    void* return_address;

    // This is -offsetof(FrameLayout, prev_ebp).
    static const intptr_t kOffsetFromFp = -2;

    intptr_t function_id() { return function_id_; }
    intptr_t frame_type() { return frame_type_; }

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

#endif // _include_sourcepawn_jit_frames_x86_h_
