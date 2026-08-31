// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#include "features-x64.h"

#include <inttypes.h>
#if defined(__GNUC__)
#    include <cpuid.h>
#elif defined(_MSC_VER)
#    include <intrin.h>
#endif

#include <mutex>
#include <optional>

namespace sp {

struct cpuid_t {
    uint32_t eax;
    uint32_t ebx;
    uint32_t ecx;
    uint32_t edx;
};

#if defined(__GNUC__)
static cpuid_t
do_cpuid(int leaf) {
    cpuid_t regs;
    __get_cpuid(leaf, &regs.eax, &regs.ebx, &regs.ecx, &regs.edx);
    return regs;
}
#elif defined(_MSC_VER)
static cpuid_t
do_cpuid(int leaf) {
    cpuid_t regs;
    int out[4];
    __cpuid(out, leaf);
    regs.eax = out[0];
    regs.ebx = out[1];
    regs.ecx = out[2];
    regs.edx = out[3];
    return regs;
}
#endif

const FeaturesX64& FeaturesX64::Get() {
    static FeaturesX64 features{};
    static bool initialized = false;

    if (!initialized) {
        cpuid_t leaf0 = do_cpuid(0);

        if (leaf0.eax >= 1) {
            cpuid_t leaf1 = do_cpuid(1);
            features.sse3 = !!(leaf1.ecx & (1 << 0));
            features.ssse3 = !!(leaf1.ecx & (1 << 9));
            features.sse4_1 = !!(leaf1.ecx & (1 << 19));
            features.sse4_2 = !!(leaf1.ecx & (1 << 20));
            features.avx = !!(leaf1.ecx & (1 << 28));
        }
        if (leaf0.eax >= 7) {
            cpuid_t leaf7 = do_cpuid(7);
            features.avx2 = !!(leaf7.ebx & (1 << 5));
        }
        initialized = true;
    }
    return features;
}

} // namespace sp
