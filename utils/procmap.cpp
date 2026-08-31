// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// Copyright (C) 2026 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#include "procmap.h"

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#include <memory>

#include <amtl/am-raii.h>

namespace sp {

using FilePtr = std::unique_ptr<FILE, decltype(&::fclose)>;

std::optional<uintptr_t> FindNextMmapCandidate(uintptr_t start, size_t size, uintptr_t end) {
    FilePtr fp(fopen("/proc/self/maps", "rb"), ::fclose);
    if (!fp)
        return {};

    uintptr_t last_end = start;
    char line[256];
    while (fgets(line, sizeof(line), fp.get())) {
        unsigned long long start_ll, end_ll;
        if (sscanf(line, "%llx-%llx", &start_ll, &end_ll) == 2) {
            uintptr_t vma_start = (uintptr_t)start_ll;
            uintptr_t vma_end = (uintptr_t)end_ll;

            if (vma_start > last_end && (vma_start - last_end) >= size) {
                if (last_end + size <= end)
                    return {last_end};
            }

            if (vma_end > last_end)
                last_end = vma_end;
            if (last_end >= end)
                break;
        }
    }

    if (last_end + size <= end)
        return {last_end};

    return {};
}

} // namespace sp
