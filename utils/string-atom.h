// vim: set ts=2 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2012-2026 AlliedModders LLC
//
#ifndef _include_sp_shared_string_atom_h
#define _include_sp_shared_string_atom_h

#include <amtl/am-string.h>

#include <string>

namespace sp {

class StringPool;

// An interned string.
class Atom
{
  friend class StringPool;

 private:
  Atom(const char* str, size_t len)
   : str_(str, len)
  {}
  Atom(const Atom&) = delete;

  Atom& operator =(const Atom&) = delete;

 public:
  size_t length() const {
    return str_.size();
  }
  const char* chars() const {
    return str_.c_str();
  }
  const std::string& str() const {
    return str_;
  }

 private:
  std::string str_;
};

} // namespace sp

#endif // _include_sp_shared_string_atom_h
