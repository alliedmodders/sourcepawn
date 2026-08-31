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
#include "virtmem-32bit.h"
#include <mimalloc.h>

#include <memory>
#include <vector>

#if defined(_WIN32)
#    include <windows.h>
#    include <memoryapi.h>
#else
#    include <sys/mman.h>
#    include <unistd.h>
#    include <stdio.h>
#    include <errno.h>
#endif

#include "../utils/procmap.h"

#include "environment.h"
#include "heap-defaults.h"

namespace sp {

// For 32-bit platforms, we need the pointer address to remain below 2GB
// so that we can tag the high bit. We request the OS to keep allocations in this range.
[[maybe_unused]] static constexpr uintptr_t kMaxMemoryAddress = 0x80000000;
[[maybe_unused]] static constexpr uintptr_t kSearchStartHint = 0x10000;

#if defined(KE_POSIX)
static void* sp_mmap(void* addr, size_t size, int prot, int flags, int fd, size_t offset);
#elif defined(_WIN32)
static void* __stdcall sp_VirtualAlloc(void* addr, size_t size, DWORD alloc_type, DWORD protect);
static void* __stdcall sp_VirtualAlloc2(void* process, void* addr, size_t size, DWORD alloc_type, DWORD protect, void* params, DWORD param_count);

typedef PVOID(WINAPI* VirtualAlloc2_t)(HANDLE, PVOID, SIZE_T, ULONG, ULONG, MEM_EXTENDED_PARAMETER*, ULONG);
static VirtualAlloc2_t pVirtualAlloc2 = nullptr;
#endif

bool VirtMem32::Initialize() {
    mi_option_set(mi_option_limit_os_alloc, 0);
    mi_option_set(mi_option_allow_large_os_pages, 0);

#if defined(KE_POSIX)
    mi_register_mmap_callback(sp_mmap);
#elif defined(_WIN32)
    mi_register_virtualalloc_callback(sp_VirtualAlloc);

    HMODULE hMod = GetModuleHandleA("kernelbase.dll");
    if (!hMod) hMod = GetModuleHandleA("kernel32.dll");
    if (hMod) {
        pVirtualAlloc2 = (VirtualAlloc2_t)GetProcAddress(hMod, "VirtualAlloc2");
        if (pVirtualAlloc2)
            mi_register_virtualalloc2_callback(sp_VirtualAlloc2);
    }
#endif

    return true;
}

#if defined(KE_POSIX)
static void* sp_mmap(void* addr, size_t size, int prot, int flags, int fd, size_t offset) {
    if (addr != nullptr && (uintptr_t)addr + size > sp::kMaxMemoryAddress) {
        if (flags & MAP_FIXED) {
            errno = EINVAL;
            return MAP_FAILED;
        }
        addr = nullptr;
    }

    if (flags & MAP_FIXED)
        return mmap(addr, size, prot, flags, fd, offset);

    uintptr_t search_start = addr ? (uintptr_t)addr : sp::kSearchStartHint;

    while (search_start + size <= sp::kMaxMemoryAddress) {
#if defined(MAP_FIXED_NOREPLACE)
        std::optional<uintptr_t> candidate =
            sp::FindNextMmapCandidate(search_start, size, sp::kMaxMemoryAddress);
        if (!candidate.has_value())
            return MAP_FAILED;

        void* ptr = mmap((void*)*candidate, size, prot, flags | MAP_FIXED_NOREPLACE, fd, offset);
        if (ptr != MAP_FAILED)
            return ptr;

        if (errno != EEXIST)
            return MAP_FAILED;
        search_start = *candidate + sp::kSearchStartHint;
#else
        void* ptr = mmap((void*)search_start, size, prot, flags, fd, offset);
        if (ptr != MAP_FAILED) {
            if ((uintptr_t)ptr + size <= sp::kMaxMemoryAddress)
                return ptr;
            munmap(ptr, size);
        }
        search_start += sp::kSearchStartHint;
#endif
    }
    errno = ENOMEM;
    return MAP_FAILED;
}

#elif defined(_WIN32)

static void* __stdcall sp_VirtualAlloc(void* addr, size_t size, DWORD alloc_type, DWORD protect) {
    if (addr != nullptr) {
        if ((uintptr_t)addr + size > sp::kMaxMemoryAddress) {
            SetLastError(ERROR_INVALID_ADDRESS);
            return nullptr;
        }
        return VirtualAlloc(addr, size, alloc_type, protect);
    }

    uintptr_t search = sp::kSearchStartHint;
    while (search + size <= sp::kMaxMemoryAddress) {
        if (void* p = VirtualAlloc((void*)search, size, alloc_type, protect))
            return p;
        search += sp::kSearchStartHint;
    }
    SetLastError(ERROR_OUTOFMEMORY);
    return nullptr;
}

static void* __stdcall sp_VirtualAlloc2(void* process, void* addr, size_t size, DWORD alloc_type, DWORD protect, void* params, DWORD param_count) {
    if (addr != nullptr) {
        if ((uintptr_t)addr + size > sp::kMaxMemoryAddress) {
            SetLastError(ERROR_INVALID_ADDRESS);
            return nullptr;
        }
        return pVirtualAlloc2(process, addr, size, alloc_type, protect, (MEM_EXTENDED_PARAMETER*)params, param_count);
    }

    MEM_ADDRESS_REQUIREMENTS req{};
    req.HighestEndingAddress = (PVOID)0x7FFFFFFF;

    std::vector<MEM_EXTENDED_PARAMETER> new_params;

    if (params && param_count > 0) {
        MEM_EXTENDED_PARAMETER* orig_params = (MEM_EXTENDED_PARAMETER*)params;
        for (DWORD i = 0; i < param_count; i++) {
            if (orig_params[i].Type == MemExtendedParameterAddressRequirements) {
                MEM_ADDRESS_REQUIREMENTS* orig_req = (MEM_ADDRESS_REQUIREMENTS*)orig_params[i].Pointer;
                req.Alignment = orig_req->Alignment;
                req.LowestStartingAddress = orig_req->LowestStartingAddress;
            } else {
                new_params.push_back(orig_params[i]);
            }
        }
    }

    MEM_EXTENDED_PARAMETER address_req{};
    address_req.Type = MemExtendedParameterAddressRequirements;
    address_req.Pointer = &req;
    new_params.push_back(address_req);

    return pVirtualAlloc2(process, addr, size, alloc_type, protect, new_params.data(), new_params.size());
}
#endif

} // namespace sp
