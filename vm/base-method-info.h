// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// Copyright (C) 2006-2015 AlliedModders LLC
//
// This file is part of SourcePawn. SourcePawn is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
//
// You should have received a copy of the GNU General Public License along with
// SourcePawn. If not, see http://www.gnu.org/licenses/.
//
#ifndef _INCLUDE_SOURCEPAWN_BASE_METHOD_INFO_H_
#define _INCLUDE_SOURCEPAWN_BASE_METHOD_INFO_H_

#include <stdint.h>
#include <amtl/am-refcounting.h>

namespace sp {

class CompiledFunction;

class BaseMethodInfo : public ke::Refcounted<BaseMethodInfo>
{
  public:
    virtual ~BaseMethodInfo() {}

    virtual uint32_t pcode_offset() const = 0;
    virtual CompiledFunction* jit() const = 0;
};

} // namespace sp

#endif // _INCLUDE_SOURCEPAWN_BASE_METHOD_INFO_H_
