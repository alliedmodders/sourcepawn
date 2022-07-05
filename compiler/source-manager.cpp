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

#include <amtl/am-arithmetic.h>
#include "errors.h"
#include "lexer.h"

SourceManager::SourceManager(CompileContext& cc)
  : cc_(cc)
{
}

std::shared_ptr<SourceFile> SourceManager::Open(const std::string& path,
                                                const token_pos_t& from)
{
    auto file = std::make_shared<SourceFile>();
    if (!file->Open(path))
	      return nullptr;

    size_t loc_index;
    if (!TrackExtents(file->size(), &loc_index)) {
        report(from, 422);
        return nullptr;
    }

    if (opened_files_.size() >= UINT_MAX) {
        report(from, 422);
        return nullptr;
    }

    file->set_sources_index(opened_files_.size());
    opened_files_.emplace_back(file);

    // :TODO: fix
    locations_[loc_index].init({}, file);
    file->set_location_index(loc_index);
    return file;
}

bool SourceManager::TrackExtents(uint32_t length, size_t* index) {
    // We allocate an extra 2 so we can refer to the end-of-file position without
    // colling with the next range.
    uint32_t next_source_id;
    if (!ke::TryUint32Add(next_source_id_, length, &next_source_id) ||
        !ke::TryUint32Add(next_source_id, 2, &next_source_id) ||
        next_source_id > INT_MAX)
    {
        return false;
    }

    *index = locations_.size();

    LREntry tracker;
    tracker.id = next_source_id_;
    locations_.push_back(tracker);

    next_source_id_ = next_source_id;
    return true;
}

LREntry SourceManager::GetLocationRangeEntryForFile(const std::shared_ptr<SourceFile>& file) {
    return locations_[file->location_index()];
}
