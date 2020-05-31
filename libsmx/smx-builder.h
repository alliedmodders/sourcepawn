// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2012-2018 AlliedModders LLC, David Anderson
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
#ifndef _include_spcomp2_smx_builder_h_
#define _include_spcomp2_smx_builder_h_

#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <amtl/am-hashmap.h>
#include <amtl/am-refcounting.h>
#include <smx/smx-headers.h>
#include <smx/smx-typeinfo.h>
#include <stdlib.h>
#include "shared/string-atom.h"
#include "smx-buffer.h"

namespace sp {

class StringPool;

// An SmxSection is a named blob of data.
class SmxSection : public ke::Refcounted<SmxSection>
{
 public:
  SmxSection(const char* name)
   : name_(name)
  {
  }
  virtual ~SmxSection()
  {}

  virtual bool write(ISmxBuffer* buf) = 0;
  virtual size_t length() const = 0;
  virtual bool empty() const {
    return false;
  }

  const std::string& name() const {
    return name_;
  }

 private:
  std::string name_;
};

// An SmxBlobSection is a section that has some kind of header structure
// (specified as a template parameter), and then an arbitrary run of bytes
// immediately after.
template <typename T>
class SmxBlobSection : public SmxSection
{
 public:
  SmxBlobSection(const char* name)
   : SmxSection(name),
     extra_(nullptr),
     extra_len_(0)
  {
    memset(&t_, 0, sizeof(t_));
  }

  T& header() {
    return t_;
  }
  void setBlob(const uint8_t* blob, size_t len) {
    extra_ = blob;
    extra_len_ = len;
  }
  bool write(ISmxBuffer* buf) override {
    if (!buf->write(&t_, sizeof(t_)))
      return false;
    if (!extra_len_)
      return true;
    return buf->write(extra_, extra_len_);
  }
  size_t length() const override {
    return sizeof(t_) + extra_len_;
  }

 private:
  T t_;
  const uint8_t* extra_;
  size_t extra_len_;
};

// An SmxBlobSection without headers.
template <>
class SmxBlobSection<void> : public SmxSection
{
 public:
  SmxBlobSection(const char* name)
   : SmxSection(name)
  {
  }

  void add(const void* bytes, size_t len) {
    buffer_.writeBytes(bytes, len);
  }
  bool write(ISmxBuffer* buf) override {
    return buf->write(buffer_.bytes(), buffer_.size());
  }
  size_t length() const override {
    return buffer_.size();
  }

 private:
  ByteBuffer buffer_;
};

// An SmxListSection is a section that is a simple table of uniformly-sized
// structures. It has no header of its own.
template <typename T>
class SmxListSection : public SmxSection
{
 public:
  SmxListSection(const char* name)
   : SmxSection(name)
  {
  }

  void push_back(const T& t) {
    list_.push_back(t);
  }
  T& add() {
    list_.push_back(T());
    return list_.back();
  }
  void add(const T& t) {
    list_.push_back(t);
  }
  T& at(size_t index) {
    return list_[index];
  }
  virtual bool write(ISmxBuffer* buf) override {
    return buf->write(list_.data(), list_.size() * sizeof(T));
  }
  virtual size_t length() const override {
    return count() * sizeof(T);
  }
  size_t count() const {
    return list_.size();
  }
  bool empty() const override {
    return list_.empty();
  }

 private:
  std::vector<T> list_;
};

// An SmxRttiTable is an SmxListSection that is preceded by an RTTI header.
template <typename T>
class SmxRttiTable : public SmxListSection<T>
{
  typedef SmxListSection<T> Base;

 public:
  SmxRttiTable(const char* name)
   : SmxListSection<T>(name)
  {
    header_.header_size = sizeof(header_);
    header_.row_size = sizeof(T);
  }

  bool write(ISmxBuffer* buf) override {
    header_.row_count = (uint32_t)Base::count();
    if (!buf->write(&header_, sizeof(header_)))
      return false;
    return Base::write(buf);
  }
  size_t length() const override {
    return sizeof(header_) + Base::length();
  }

 private:
  smx_rtti_table_header header_;
};

// A name table is a blob of zero-terminated strings. Strings are entered
// into the table as atoms, so duplicate stings are not emitted.
class SmxNameTable : public SmxSection
{
 public:
  SmxNameTable(const char* name)
   : SmxSection(name),
     buffer_size_(0)
  {
    name_table_.init(64);
  }

  uint32_t add(StringPool& pool, const char* str);

  uint32_t add(Atom* str) {
    NameTable::Insert i = name_table_.findForAdd(str);
    if (i.found())
      return i->value;

    if (!ke::IsUint32AddSafe(buffer_size_, str->length() + 1)) {
      fprintf(stderr, "out of memory in nametable\n");
      abort();
    }

    uint32_t index = buffer_size_;
    name_table_.add(i, str, index);
    names_.push_back(str);
    buffer_size_ += str->length() + 1;
    return index;
  }

  bool write(ISmxBuffer* buf) override;
  size_t length() const override {
    return buffer_size_;
  }

 private:
  struct HashPolicy {
    static uint32_t hash(Atom* str) {
      return ke::HashPointer(str);
    }
    static bool matches(Atom* a, Atom* b) {
      return a == b;
    }
  };
  typedef ke::HashMap<Atom*, size_t, HashPolicy> NameTable;

  NameTable name_table_;
  std::vector<Atom*> names_;
  uint32_t buffer_size_;
};

class SmxBuilder
{
 public:
  SmxBuilder();

  bool write(ISmxBuffer* buf);

  void add(const ke::RefPtr<SmxSection>& section) {
    sections_.push_back(section);
  }
  void addIfNotEmpty(const ke::RefPtr<SmxSection>& section) {
    if (!section->empty())
      sections_.push_back(section);
  }

 private:
  std::vector<ke::RefPtr<SmxSection>> sections_;
};

} // namespace sp

#endif // _include_spcomp2_smx_builder_h_
