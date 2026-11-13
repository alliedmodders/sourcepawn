// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#include "platform-helpers.h"

#include <memory>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace sp {

class UniqueHandle {
  public:
    UniqueHandle() = default;
    explicit UniqueHandle(HANDLE handle) : handle_(handle) {}

    UniqueHandle(const UniqueHandle&) = delete;
    void operator =(const UniqueHandle&) = delete;

    explicit operator bool() const {
        return handle_.get() != nullptr && handle_.get() != INVALID_HANDLE_VALUE;
    }

    HANDLE get() const { return handle_.get(); }

  private:
    struct CloseHandleDeleter {
        void operator()(HANDLE handle) const {
            if (handle != INVALID_HANDLE_VALUE)
                CloseHandle(handle);
        }
    };

    std::unique_ptr<void, CloseHandleDeleter> handle_;
};

static UniqueHandle OpenPathForQuery(const std::string& path) {
    DWORD share = FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;
    return UniqueHandle{CreateFileA(path.c_str(), 0, share, nullptr, OPEN_EXISTING,
                                    FILE_ATTRIBUTE_NORMAL, nullptr)};
}

std::optional<FileIdentity> GetFileIdentity(const std::string& path) {
    // Note: the CRT stat() implementation always returns 0 for st_ino, except
    // for drive roots, so it cannot be used to identify files.
    auto handle = OpenPathForQuery(path);
    if (!handle)
        return {};

    BY_HANDLE_FILE_INFORMATION info;
    if (GetFileInformationByHandle(handle.get(), &info) == 0)
        return {};

    return FileIdentity{
        static_cast<uint64_t>(info.dwVolumeSerialNumber),
        (uint64_t(info.nFileIndexHigh) << 32) | uint64_t(info.nFileIndexLow),
    };
}

} // namespace sp
