// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#include "platform-helpers.h"

#include <sys/stat.h>

namespace sp {

std::optional<FileIdentity> GetFileIdentity(const std::string& path) {
    struct stat st;
    if (stat(path.c_str(), &st) != 0)
        return {};

    return FileIdentity{
        static_cast<uint64_t>(st.st_dev),
        static_cast<uint64_t>(st.st_ino),
    };
}

} // namespace sp
