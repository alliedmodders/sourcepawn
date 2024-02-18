// vim: set ts=4 sw=4 tw=99 et:
// 
// Copyright (C) 2022 AlliedModders LLC
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

#include <limits>
#include <memory>

#include "shared/string-pool.h"
#include "stl/stl-unordered-map.h"
#include "stl/stl-vector.h"
#include "source-file.h"
#include "source-location.h"

namespace sp {
namespace cc {

class CompileContext;

// Of course, we'd love if tokens could just be SourceLocations. But peek_same_line()
// is an extremely hot function, and line checks need to be fast, so we track
// it explicitly.
struct token_pos_t : public SourceLocation {
    int line = 0;

    token_pos_t() {}
    token_pos_t(const SourceLocation& loc, int line)
      : SourceLocation(loc),
        line(line)
    {}
};

// Location Range. Design is taken from LLVM.
//
// An LocationRange is created each time we register a range of locations (it is
// short for LocationRangeEntry). For a file, an LocationRange covers each character
// in the file, including a position for EOF. For macros, it covers the number
// of characters in its token stream, with a position for EOF.
//
// LREntries are allocated by calling TrackExtents.
struct LocationRange
{
    // Starting id for this source range.
    uint32_t id;

 private:
    // If we're creating a range from an #include, this is the location in the
    // parent file we were #included from, if any.
    //
    // If we're creating a range for macro insertion, this is where the macro was
    // defined.
    SourceLocation parent_;

    // If we're creating a range from a macro, this is the insertion point.
    SourceLocation expansion_loc_;

    // If we included from a file, this is where we included. No refcount
    // needed since SourceFiles are held open by CompileContext.
    union {
        SourceFile* file_;
        Atom* text_;
    };

 public:
    LocationRange()
     : id(0)
    {}

    bool valid() const {
        return id != 0;
    }

    void init(const SourceLocation& parent, SourceFile* file) {
        this->parent_ = parent;
        this->file_ = file;
    }

    void init(SourceLocation parent, SourceLocation expansion_loc, Atom* text) {
        this->parent_ = parent;
        this->expansion_loc_ = expansion_loc;
        this->text_ = text;
    }

    std::shared_ptr<SourceFile> file() const {
        if (!is_file())
            return nullptr;
        return file_->to_shared();
    }

    const SourceLocation& parent() const { return parent_; }
    const SourceLocation& expansion_loc() const { return expansion_loc_; }

    bool is_macro() const { return expansion_loc_.valid(); }
    bool is_file() const { return !expansion_loc_.valid(); }

    uint32_t length() const {
        if (!valid())
            return 0;
        if (is_macro())
            return text_->length();
        return file_->size();
    }

    bool owns(const SourceLocation& loc) const {
        if (!loc.valid())
            return false;
        if (loc.offset() >= id && loc.offset() <= id + length())
            return true;
        return false;
    }

    SourceLocation FilePos(uint32_t offset) const {
        assert(file_);
        assert(offset <= file_->size());
        return SourceLocation::FromFile(id, offset);
    }

    SourceLocation MacroPos(uint32_t offset) const {
        assert(text_);
        assert(offset <= text_->length());
        return SourceLocation::FromMacro(id, offset);
    }

    uint32_t ToOffset(SourceLocation loc) {
        assert(owns(loc));
        return loc.offset() - id;
    }

    bool operator ==(const LocationRange& other) const {
        return id == other.id;
    }
};

class SourceManager final
{
  public:
    explicit SourceManager(CompileContext& cc);

    std::shared_ptr<SourceFile> Open(const token_pos_t& from, const std::string& path);
    std::shared_ptr<SourceFile> Open(const std::string& name, tr::string&& data);

    LocationRange EnterFile(std::shared_ptr<SourceFile> file, const token_pos_t& from);

    // Return a location range for a macro. If the macro has unique text, the
    // location range will not be cached.
    LocationRange EnterMacro(const token_pos_t& from, SourceLocation expansion_loc,
                             Atom* text);

    // For a given token location, retrieve the nearest source file index it maps to.
    uint32_t GetSourceFileIndex(const SourceLocation& loc);

    // Return the closest line and column number for a given location. If the
    // location is a macro expansion, the expansion location is used. If the
    // location is invalid, 0 is returned.
    uint32_t GetLineAndCol(SourceLocation loc, uint32_t* col);

    // Checks whether two tokens are in the same file. Runtime is O(log n) for
    // n = # of files opened.
    bool IsSameSourceFile(const SourceLocation& a, const SourceLocation& b);

    const tr::vector<std::shared_ptr<SourceFile>>& opened_files() const {
      return opened_files_;
    }

    // Find the index of the owning LocationRange. 0 is an invalid range.
    size_t FindLocRange(const SourceLocation& loc) {
        if (loc_ranges_[last_lr_lookup_].owns(loc))
            return last_lr_lookup_;
        return FindLocRangeSlow(loc);
    }

  private:
    bool Open(const token_pos_t& from, std::shared_ptr<SourceFile> file);
    bool TrackExtents(uint32_t length, size_t* index);
    size_t FindLocRangeSlow(const SourceLocation& loc);
    size_t FindSourceFileRangeIndex(SourceLocation loc, SourceLocation* expansion_loc);

  private:
    CompileContext& cc_;
    tr::vector<std::shared_ptr<SourceFile>> opened_files_;
    tr::vector<LocationRange> loc_ranges_;

    // Source ids start from 1. The source file id is 1 + len(source) + 1. This
    // lets us store source locations as a single integer, as we can always
    // bisect to a particular file, and from there, to a line number and column.
    uint32_t next_source_id_ = 1;

    // One-entry cache for SL lookup.
    size_t last_lr_lookup_ = 0;
};

} // namespace cc
} // namespace sp
