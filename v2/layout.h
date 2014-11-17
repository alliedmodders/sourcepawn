// vim: set sts=2 ts=8 sw=2 tw=99 et:
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
#ifndef _include_spcomp_methodmap_h_
#define _include_spcomp_methodmap_h_

#include "pool-allocator.h"
#include "string-pool.h"

namespace ke {

class Methodmap : public PoolObject
{
 public:
  Methodmap()
   : parent_(nullptr)
  {}

  void setParent(EnumType *parent) {
    assert(!parent_);
    parent_ = parent;
  }
  EnumType *parent() const {
    return parent_;
  }

 private:
  EnumType *parent_;
};

#undef LAYOUT_DECL_TYPE_MAP

}

#endif // _include_spcomp_methodmap_h_
