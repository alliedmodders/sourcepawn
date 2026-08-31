// vim: set sts=2 ts=8 sw=2 tw=99 et:
//
// SPDX-License-Identifier: BSD-3-Clause
//
// Copyright (c) 2006-2026 AlliedModders LLC
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

    virtual uint32_t frame_id() const = 0;
    virtual CompiledFunction* jit() const = 0;
    virtual uint32_t TranslateInterpCip(const uint8_t* cip) const = 0;
    virtual uint32_t TranslateJitCip(uint32_t cip) const = 0;

    virtual const char* GetName() const = 0;
    virtual const char* GetFilePath() const = 0;
};

} // namespace sp

#endif // _INCLUDE_SOURCEPAWN_BASE_METHOD_INFO_H_
