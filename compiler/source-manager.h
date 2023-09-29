// vim: set ts=2 sw=2 tw=99 et:
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
#pragma once

#include <limits.h>

#include <memory>

#include "stl/stl-unordered-map.h"
#include "stl/stl-vector.h"
#include "source-file.h"
#include "source-location.h"

class CompileContext;

struct token_pos_t {
    int file_ = 0;
    int line = 0;
};

// An LREntry is created each time we register a range of locations (it is
// short for LocationRangeEntry). For a file, an LREntry covers each character
// in the file, including a position for EOF. For macros, it covers the number
// of characters in its token stream, with a position for EOF.
//
// LREntries are allocated by calling trackFile() or trackMacro() in the
// SourceManager.
//
// LREntries are not malloc'd, so references must not be held past calls to
// trackFile() or trackMacro().
struct LREntry
{
    // Starting id for this source range.
    uint32_t id;

 private:
    // If we're creating a range from an #include, this is the location in the
    // parent file we were #included from, if any.
    //
    // If we're creating a range for macro insertion, this is where we started
    // inserting tokens.
    SourceLocation parent_;

    // If we included from a file, this is where we included.
    std::shared_ptr<SourceFile> file_;

 public:
    LREntry()
     : id(0)
    {}

    bool valid() const {
        return id != 0;
    }

    void init(const SourceLocation& parent, std::shared_ptr<SourceFile> file) {
        this->parent_ = parent;
        this->file_ = std::move(file);
    }

    std::shared_ptr<SourceFile> file() const {
        return file_;
    }
    const SourceLocation& parent() const {
        return parent_;
    }

    uint32_t length() const {
        return file_->size();
    }

    bool owns(const SourceLocation& loc) const {
        if (loc.offset() >= id && loc.offset() <= id + length())
            return true;
        return false;
    }

    SourceLocation FilePos(uint32_t offset) const {
        assert(file_);
        assert(offset <= file_->size());
        return SourceLocation::FromFile(id, offset);
    }
};

class SourceManager final
{
  public:
    explicit SourceManager(CompileContext& cc);

    std::shared_ptr<SourceFile> Open(const std::string& path, const token_pos_t& from);

    LREntry GetLocationRangeEntryForFile(const std::shared_ptr<SourceFile>& file);

    // For a given token location, retrieve the nearest source file index it maps to.
    uint32_t GetSourceFileIndex(const token_pos_t& pos) {
      return pos.file_;
    }
    bool IsSameSourceFile(const token_pos_t& current, const token_pos_t& next) {
      return current.file_ == next.file_;
    }

    const tr::vector<std::shared_ptr<SourceFile>>& opened_files() const {
      return opened_files_;
    }

  private:
    bool TrackExtents(uint32_t length, size_t* index);

  private:
    CompileContext& cc_;
    tr::vector<std::shared_ptr<SourceFile>> opened_files_;
    tr::vector<LREntry> locations_;

    // Source ids start from 1. The source file id is 1 + len(source) + 1. This
    // lets us store source locations as a single integer, as we can always
    // bisect to a particular file, and from there, to a line number and column.
    uint32_t next_source_id_ = 1;
};
