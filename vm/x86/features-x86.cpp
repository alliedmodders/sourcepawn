// vim: set ts=8 sts=4 sw=4 tw=99 et:
//
//  Copyright (c) 2026 AlliedModders LLC
//
//  This software is provided "as-is", without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from
//  the use of this software.
//
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//
//  1.  The origin of this software must not be misrepresented; you must not
//      claim that you wrote the original software. If you use this software in
//      a product, an acknowledgment in the product documentation would be
//      appreciated but is not required.
//  2.  Altered source versions must be plainly marked as such, and must not be
//      misrepresented as being the original software.
//  3.  This notice may not be removed or altered from any source distribution.
#include "features-x86.h"

#include <inttypes.h>
#if defined(__GNUC__)
#    include <cpuid.h>
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

const FeaturesX86&
FeaturesX86::Get() {
    static FeaturesX86 features{};
    static bool initialized = false;

    if (!initialized) {
        cpuid_t leaf0 = do_cpuid(0);

        if (leaf0.eax >= 1) {
            cpuid_t leaf1 = do_cpuid(1);
            features.fpu = !!(leaf1.edx & (1 << 0));
            features.mmx = !!(leaf1.edx & (1 << 23));
            features.sse = !!(leaf1.edx & (1 << 25));
            features.sse2 = !!(leaf1.edx & (1 << 26));
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
