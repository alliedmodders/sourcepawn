// vim: set ts=4 sw=4 tw=99 et:
// 
// Copyright (C) 2022 David Anderson
// 
// This file is part of SourcePawn.
// 
// SourcePawn is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option)
// any later version.
// 
// SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
#include "source-manager.h"

#include <filesystem>
#include <limits>

#include <amtl/am-arithmetic.h>
#include "errors.h"
#include "lexer.h"

using namespace sp;

namespace sp {
namespace cc {

SourceManager::SourceManager(CompileContext& cc)
  : cc_(cc)
{
    loc_ranges_.emplace_back();
}

std::shared_ptr<SourceFile> SourceManager::Open(const token_pos_t& from, const std::string& path) {
    for (const auto& other : opened_files_) {
        std::error_code ec;
        if (std::filesystem::equivalent(path, other->path(), ec))
            return other;
    }

    auto file = std::make_shared<SourceFile>();
    if (!file->Open(path))
        return nullptr;
    if (!Open(from, file))
        return nullptr;
    return file;
}

bool SourceManager::Open(const token_pos_t& from, std::shared_ptr<SourceFile> file) {
    if (opened_files_.size() >= std::numeric_limits<int>::max()) {
        report(from, 422);
        return {};
    }

    file->set_sources_index(opened_files_.size());
    opened_files_.emplace_back(file);
    return true;
}

std::shared_ptr<SourceFile> SourceManager::Open(const std::string& name, tr::string&& data) {
    auto file = std::make_shared<SourceFile>(name, std::move(data));
    if (!Open({}, file))
        return nullptr;
    return file;
}

LocationRange SourceManager::EnterFile(std::shared_ptr<SourceFile> file, const token_pos_t& from) {
    size_t loc_index;
    if (!TrackExtents(file->size(), &loc_index)) {
        report(from, 422);
        return {};
    }

    loc_ranges_[loc_index].init(from, file.get());
    return loc_ranges_[loc_index];
}

LocationRange SourceManager::EnterMacro(const token_pos_t& from, SourceLocation expansion_loc,
                                        Atom* text)
{
    assert(expansion_loc.valid());

    size_t lr_index;
    if (!TrackExtents(text->length(), &lr_index)) {
        report(from, 422);
        return {};
    }

    loc_ranges_[lr_index].init(from, expansion_loc, text);
    return loc_ranges_[lr_index];
}

bool SourceManager::TrackExtents(uint32_t length, size_t* index) {
    // We allocate an extra 2 so we can refer to the end-of-file position without
    // colliding with the next range.
    uint32_t next_source_id;
    if (!ke::TryUint32Add(next_source_id_, length, &next_source_id) ||
        !ke::TryUint32Add(next_source_id, 2, &next_source_id) ||
        next_source_id > INT_MAX)
    {
        return false;
    }

    *index = loc_ranges_.size();

    LocationRange tracker;
    tracker.id = next_source_id_;
    loc_ranges_.push_back(tracker);

    next_source_id_ = next_source_id;
    return true;
}

bool SourceManager::IsSameSourceFile(const SourceLocation& a, const SourceLocation& b) {
    return GetSourceFileIndex(a) == GetSourceFileIndex(b);
}

size_t SourceManager::FindLocRangeSlow(const SourceLocation& loc) {
    assert(loc.valid());

    if (loc_ranges_[last_lr_lookup_].owns(loc))
        return last_lr_lookup_;

    size_t lower = 1;
    size_t upper = loc_ranges_.size();
    while (lower < upper) {
        size_t mid = (lower + upper) / 2;
        const auto& range = loc_ranges_[mid];
        if (loc.offset() < range.id) {
            upper = mid;
        } else if (loc.offset() > range.id + range.length() + 1) {
            // Note +1 for the terminal offset.
            lower = mid + 1;
        } else {
            assert(range.owns(loc));
            last_lr_lookup_ = mid;
            return mid;
        }
    }

    assert(false);
    return 0;
}

size_t SourceManager::FindSourceFileRangeIndex(SourceLocation loc, SourceLocation* expansion_loc) {
    size_t lr_index = FindLocRange(loc);
    if (lr_index == 0)
        return 0;

    if (expansion_loc)
        *expansion_loc = loc;

    while (lr_index && loc_ranges_[lr_index].is_macro()) {
        if (expansion_loc)
            *expansion_loc = loc_ranges_[lr_index].expansion_loc();
        lr_index = FindLocRange(loc_ranges_[lr_index].expansion_loc());
    }

    if (!loc_ranges_[lr_index].file())
        return 0;

    return lr_index;
}

uint32_t SourceManager::GetSourceFileIndex(const SourceLocation& loc) {
    auto lr_index = FindSourceFileRangeIndex(loc, nullptr);
    if (!lr_index)
        return 0; // Default to the main file.
    return loc_ranges_[lr_index].file()->sources_index();
}

uint32_t SourceManager::GetLineAndCol(SourceLocation loc, uint32_t* col) {
    if (col)
        *col = 0;

    if (!loc.valid())
        return 0;

    SourceLocation expansion_loc;
    auto lr_index = FindSourceFileRangeIndex(loc, &expansion_loc);
    if (!lr_index)
        return 0;

    auto file = loc_ranges_[lr_index].file();
    uint32_t offset = loc_ranges_[lr_index].ToOffset(expansion_loc);

    uint32_t line;
    if (!file->OffsetToLineAndCol(offset, &line, col))
        return 0;
    return line;
}

} // namespace cc
} // namespace sp
