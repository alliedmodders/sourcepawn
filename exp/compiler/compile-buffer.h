/* vim: set ts=2 sw=2 tw=99 et:
 *
 * Copyright (C) 2012 David Anderson
 *
 * This file is part of SourcePawn.
 *
 * SourcePawn is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 * 
 * SourcePawn is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * SourcePawn. If not, see http://www.gnu.org/licenses/.
 */
#ifndef _include_sourcepawn_compiler_buffer_h_
#define _include_sourcepawn_compiler_buffer_h_

#include <am-allocator-policies.h>

namespace sp {

class CompileBuffer : public SystemAllocatorPolicy
{
  static const size_t kInitialLength = 1024;

 public:
  CompileBuffer()
  : begin_(NULL),
    end_(NULL),
    pos_(NULL)
  {
    begin_ = (char*)malloc(kInitialLength);
    end_ = begin_ + kInitialLength;
    pos_ = begin_;
  }

  ~CompileBuffer()
  {
    free(begin_);
  }

  void append(char c) {
    if (pos_ + 1 >= end_)
      grow(1);
    *pos_++ = c;
  }

  void append(const char* text) {
    append(text, strlen(text));
  }

  void append(const char* text, size_t length) {
    if (pos_ + length >= end_)
      grow(length);
    memcpy(pos_, text, length);
    pos_ += length;
  }

  size_t length() const {
    return pos_ - begin_;
  }

  char* finish() {
    char* copy = (char*)malloc(length() + 1);
    memcpy(copy, begin_, length());
    copy[length()] = '\0';
    return copy;
  }

 private:
  void grow(size_t length) {
    size_t max = (end_ - begin_);
    size_t needed = max + length;
    size_t requested = max;
    while (requested < needed) {
      if (!IsUintPtrMultiplySafe(requested, 2)) {
        reportAllocationOverflow();
        return;
      }
      requested *= 2;
    }
    char* newbegin = (char*)realloc(begin_, requested);
    if (!newbegin) {
      reportOutOfMemory();
      abort();
    }
    if (newbegin != begin_) {
      char* newend = newbegin + requested;
      char* newpos = newbegin + (pos_ - begin_);

      begin_ = newbegin;
      end_ = newend;
      pos_ = newpos;
    } else {
      end_ = begin_ + requested;
    }
  }

 private:
  char* begin_;
  char* end_;
  char* pos_;
};

}

#endif // _include_sourcepawn_compiler_buffer_h_

