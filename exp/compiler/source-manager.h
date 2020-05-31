// vim: set ts=2 sw=2 tw=99 et:
// 
// Copyright (C) 2012-2014 David Anderson
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
#ifndef _include_spcomp_source_cache_h_
#define _include_spcomp_source_cache_h_

#include <memory>

#include <amtl/am-string.h>
#include <amtl/am-refcounting.h>
#include <amtl/am-vector.h>
#include <amtl/am-fixedarray.h>
#include "macros.h"
#include "token-kind.h"

namespace sp {

using namespace ke;

struct ReportingContext;
class ReportManager;
class SourceFile;

// We place some kind of reasonable cap on the size of source files. This cap
// also applies to the offsets we can allocate in the source manager. Capping
// the maximum amount of sources at INT_MAX places an upper bound on the size
// of the AST, and in turn, on other data structures as well.
static const size_t kMaxTotalSourceFileLength = (INT_MAX / 4) - 1;

typedef FixedArray<uint32_t> LineExtents;

class SourceFile : public Refcounted<SourceFile>
{
  friend class SourceManager;

  SourceFile(char* chars, uint32_t length, const char* path)
   : chars_(chars),
     length_(length),
     path_(path)
  {}

 public:
  const char* chars() const {
    return chars_.get();
  }
  uint32_t length() const {
    return length_;
  }
  const char* path() const {
    return path_.c_str();
  }

  LineExtents* lineCache() {
    return line_cache_.get();
  }
  void computeLineCache();

 protected:
  std::unique_ptr<char[]> chars_;
  uint32_t length_;
  std::unique_ptr<LineExtents> line_cache_;
  std::string path_;
};

struct FullMacroRef
{
  Macro* macro;
  unsigned offset;

  FullMacroRef()
   : macro(nullptr),
     offset(0)
  {}
  FullMacroRef(Macro* macro, unsigned offset)
   : macro(macro),
     offset(offset)
  {}
};

struct FullSourceRef
{
  RefPtr<SourceFile> file;
  unsigned line;
  unsigned col;
  unsigned offset;

  FullSourceRef()
   : line(0),
     col(0),
     offset(0)
  {}
  FullSourceRef(SourceFile* buffer, unsigned line, unsigned col, unsigned offset)
   : file(buffer),
     line(line),
     col(col),
     offset(offset)
  {}
};

struct TokenHistory
{
  std::vector<FullMacroRef> macros;
  std::vector<FullSourceRef> files;
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
  SourceLocation parent;

  // If we included from a file, this is where we included.
  RefPtr<SourceFile> file;

  // If we included from a macro, this is is the macro definition.
  Macro* macro;

 public:
  bool isFile() const {
    return !macro;
  }
  bool isMacro() const {
    return !!macro;
  }
  bool valid() const {
    return id != 0;
  }

  void init(const SourceLocation& parent, SourceFile* file) {
    this->parent = parent;
    this->file = file;
  }
  void init(const SourceLocation& parent, Macro* macro) {
    this->parent = parent;
    this->macro = macro;
  }

  SourceFile* getFile() const {
    assert(isFile());
    return *file;
  }
  Macro* getMacro() const {
    assert(isMacro());
    return macro;
  }
  const SourceLocation& getParent() const {
    return parent;
  }

  LREntry()
   : id(0),
     macro(nullptr)
  {}

  uint32_t length() const {
    return isFile() ? file->length() : macro->length();
  }

  bool owns(const SourceLocation& loc) const {
    if (loc.offset() >= id && loc.offset() <= id + length()) {
      assert(isMacro() == loc.isInMacro());
      return true;
    }
    return false;
  }

  SourceLocation filePos(uint32_t offset) const {
    assert(isFile());
    assert(offset <= file->length());
    return SourceLocation::FromFile(id, offset);
  }
  SourceLocation macroPos(uint32_t offset) const {
    assert(isMacro());
    assert(offset <= macro->length());
    return SourceLocation::FromMacro(id, offset);
  }
};

class SourceManager
{
 public:
  SourceManager(StringPool& strings, ReportManager& reports);

  RefPtr<SourceFile> open(ReportingContext& cc, const char* path);

  RefPtr<SourceFile> createFromBuffer(std::unique_ptr<char[]>&& buffer, uint32_t length, const char* path);

  // Returns whether two source locations ultimately originate from the same
  // file (i.e., ignoring macros).
  bool sameFiles(const SourceLocation& a, const SourceLocation& b);

  // Returns whether two source locations are in the same token buffer.
  bool sameSourceId(const SourceLocation& a, const SourceLocation& b);

  // Returns whether two locations were inserted from the same line.
  bool areLocationsInsertedOnSameLine(const SourceLocation& a, const SourceLocation& b);

  // Create a location tracker for a file. If out of bits, returns an invalid
  // tracker and reports an error;
  LREntry trackFile(const SourceLocation& from, RefPtr<SourceFile> file);

  // Create a location tracker for a macro. If out of bits, returns an invalid
  // tracker and reports an error;
  LREntry trackMacro(const SourceLocation& from, Macro* macro);

  // Computes the full file origin of a location - that is, this skips past
  // any macro expansions that caused the location to be generated.
  //
  // This is the same as calling getTokenHistory() and reading files[0]. If
  // the location is invalid, |FullSourceRef::file| will be null.
  //
  // Computing a full source location is mildly expensive - it requires a
  // O(log n) binary search where n is the number of lines in the file and
  // number of files.
  FullSourceRef decode(const SourceLocation& loc);

  // These will be removed once we overhaul error reporting.
  RefPtr<SourceFile> getSource(const SourceLocation& loc);
  unsigned getLine(const SourceLocation& loc);
  unsigned getCol(const SourceLocation& loc);

  void getTokenHistory(const SourceLocation& loc, TokenHistory* history);

  FullSourceRef getOrigin(const FullMacroRef& ref);

 private:
  unsigned getLine(const LREntry& range, const SourceLocation& loc);
  unsigned getCol(const LREntry& range, const SourceLocation& loc, unsigned line);

  FullSourceRef fullSourceRef(const LREntry& range, const SourceLocation& loc);

 private:
  bool trackExtents(uint32_t length, size_t* index);

  // Find the LREntry for a source location.
  bool findLocation(const SourceLocation& loc, size_t* index);

  // Normalizes a macro SourceLocation to its first insertion point. For a
  // file, this returns the file. For a macro, it finds the nearest file that
  // caused the macro to expand.
  SourceLocation normalize(const SourceLocation& loc);

 private:
  StringPool& strings_;
  ReportManager& rr_;
  AtomMap<RefPtr<SourceFile>> file_cache_;
  std::vector<LREntry> locations_;

  // Source ids start from 1. The source file id is 1 + len(source) + 1. This
  // lets us store source locations as a single integer, as we can always
  // bisect to a particular file, and from there, to a line number and column.
  uint32_t next_source_id_;

  // One-entry caches. This one is for getSource().
  uint32_t last_lookup_;
};

}

#endif // _include_spcomp_source_cache_h_
