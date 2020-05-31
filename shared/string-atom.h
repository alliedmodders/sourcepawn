// vim: set ts=2 sw=2 tw=99 et:
//
// Copyright (C) 2012-2014 AlliedModders LLC, David Anderson
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

 private:
  std::string str_;
};

} // namespace sp

#endif // _include_sp_shared_string_atom_h
