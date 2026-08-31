// vim: set ts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC
//
#ifndef _include_jitcraft_string_pool_h_
#define _include_jitcraft_string_pool_h_

#include <string>

#include <amtl/am-hashtable.h>
#include <amtl/am-hashset.h>
#include <amtl/am-hashmap.h>
#include <amtl/am-string.h>
#include <string.h>
#include "string-atom.h"

namespace sp {

using namespace ke;

class CharsAndLength
{
 public:
  CharsAndLength()
   : str_(nullptr),
     length_(0)
  {
  }

  CharsAndLength(const char* str, size_t length)
   : str_(str),
     length_(length)
  {
  }

  const char* str() const {
    return str_;
  }
  size_t length() const {
    return length_;
  }

 private:
  const char* str_;
  size_t length_;
};

class StringPool
{
 public:
  StringPool()
    : table_(SystemAllocatorPolicy())
  {
    table_.init(256);
  }

  ~StringPool()
  {
    if (!table_.elements())
      return;
    for (Table::iterator i(&table_); !i.empty(); i.next())
      delete* i;
  }

  Atom* add(const std::string& str) {
    return add(str.c_str(), str.size());
  }

  Atom* add(const char* str, size_t length) {
    CharsAndLength chars(str, length);
    Table::Insert p = table_.findForAdd(chars);
    if (!p.found() && !table_.add(p, new Atom(str, length)))
      return nullptr;
    return *p;
  }

  Atom* add(const char* str) {
    return add(str, strlen(str));
  }

 private:
  struct Policy {
    typedef Atom* Payload;

    static uint32_t hash(const char* key) {
      return HashCharSequence(key, strlen(key));
    }

    static uint32_t hash(const CharsAndLength& key) {
      return HashCharSequence(key.str(), key.length());
    }

    static bool matches(const CharsAndLength& key, const Payload& e) {
      if (key.length() != e->length())
        return false;
      return strncmp(key.str(), e->chars(), key.length()) == 0;
    }
  };

  typedef ke::HashTable<Policy> Table;

 private:
   Table table_;
};

template <typename T>
struct PointerHashPolicy {
  static uint32_t hash(T* p) {
    return HashPointer(p);
  }
  static bool matches(T* a, T* b) {
    return a == b;
  }
};

template <typename T>
class PointerSet : public HashSet<T*, PointerHashPolicy<T>>
{
  typedef HashSet<T*, PointerHashPolicy<T>> Base;

 public:
  typedef typename Base::Insert Insert;

  PointerSet() {
    this->init(16);
  }

  void add(T* ptr) {
    Insert p = findForAdd(ptr);
    if (!p.found())
      add(p, ptr);
  }

  void add(Insert p, T* ptr) {
    Base::add(p, ptr);
  }
};

typedef PointerSet<Atom> AtomSet;

template <typename T>
class AtomMap : public HashMap<Atom*, T, PointerHashPolicy<Atom>>
{
 public:
  AtomMap() {
    this->init(16);
  }
};

} // namespace ke

#endif // _include_jitcraft_string_pool_h_
