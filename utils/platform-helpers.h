// vim: set sts=4 ts=8 sw=4 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2026 AlliedModders LLC
//
#pragma once

#include <stdint.h>

#include <functional>
#include <optional>
#include <string>

#include <amtl/am-hashtable.h>

namespace sp {

// Identity of a file on disk. Two paths can refer to the same file via
// symlinks, hard links, or case insensitive paths.
struct FileIdentity {
    uint64_t dev;
    uint64_t ino;

    bool operator ==(const FileIdentity& other) const {
        return dev == other.dev && ino == other.ino;
    }
};

// Get the on-disk identity of a file. Returns nothing if the file does not
// exist or cannot be stat-ed.
std::optional<FileIdentity> GetFileIdentity(const std::string& path);

} // namespace sp

namespace std {

template <>
struct hash<sp::FileIdentity> {
    size_t operator()(const sp::FileIdentity& id) const {
        uintptr_t dev = ke::HashInt64(static_cast<int64_t>(id.dev));
        uintptr_t ino = ke::HashInt64(static_cast<int64_t>(id.ino));
        return ke::HashCombine(dev, ino);
    }
};

} // namespace std
